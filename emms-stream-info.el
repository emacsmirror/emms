;;; emms-stream-info.el --- Info from streaming audio

;; Copyright (C) 2004, 2005, 2006, 2007 Free Software Foundation, Inc.

;; Author: Yoni Rabkin <yonirabkin@member.fsf.org>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; 'emms-stream-info' establishes a TCP connection with the server and
;; sends an HTTP request string.  The server (hopefully) responds with
;; some header information describing the streaming audio channel,
;; some audio data and then the name of the song being played (usually
;; in that order).
;;
;; Some stations like WCPE [http://wcpe.org], while giving excellent
;; broadcasts do not support title streaming over MP3 or Ogg.  Using
;; this software on such stations will only result in general station
;; information and not the artist name or title of the track being
;; played.

;;; Functionality:
;;
;; Currently supports Icecast and Shoutcast servers with Ogg and MP3
;; streams.

;;; Use:
;;
;; Look at the documentation strings for the three interactive
;; functions: 'emms-stream-info-get', 'emms-stream-info-message' and
;; 'emms-stream-info-insert'.

;;; Important Notes:
;;
;; 1) This software does not parse, cache or save audio data at
;;    all. This software downloads a limited amount of data from a
;;    given streaming audio channel per call. This software is
;;    optimized to download as little as possible from a given
;;    streaming audio channel and then to immediately disconnect.
;;
;; 2) This software disregards and then discards all audio data
;;    automatically after each call.
;; 
;; 3) This software connects for a maximum of 10 seconds and then
;;    immediately disconnects. Usually the software will disconnect
;;    long before the 10 second limit is reached.
;;
;; 4) It is the responsibility of the user to read the Terms of
;;    Service of the streaming audio channel before running this
;;    software on that channel's service. Some streaming audio
;;    channels explicitly request 3rd party applications not to
;;    connect to their service. This is their prerogative. Respect it.

;; $Id: emms-stream-info.el,v 1.8 2005/07/09 11:56:00 forcer Exp $

;;; Code:

(require 'emms)

;; A higher value for 'emms-stream-info-max' this gives us a
;; correspondingly higher chance of grabbing the title information
;; from a stream but incurs a price in the additional time it takes to
;; download.
;;
;; This value is not relevant for Ogg streams since the title info in
;; Ogg streams arrives almost immediately.
;;
;; Do not set under 30000 since the typical value of 'metaint' on most
;; streaming audio servers is either 8192 or 24576
(defconst emms-stream-info-max 120000
  "Byte limit for downloads.")

(defconst emms-stream-info-timeout 10
  "Seconds to timeout connection (dead or alive).")

(defconst emms-stream-info-verbose t
  "Output real-time information about the connection.")

(defconst emms-stream-info-version
  "$Revision: 1.8 $"
  "Software version.")

(defconst emms-stream-info-char-alter-regexp "[-,'=:%+&0-9A-Za-z\.()/ ]"
  "Unified character alternative clause for regular expressions.")

(defconst emms-stream-info-shoutcast-regexp 
  (concat emms-stream-info-char-alter-regexp ".*?")
  "Regular expression for Shoutcast.")

(defconst emms-stream-info-icecast-regexp 
  (concat emms-stream-info-char-alter-regexp "+")
  "Regular expression for Icecast.")

(defconst emms-stream-info-shoutcast-title-regexp 
  (concat "StreamTitle='\\(" emms-stream-info-shoutcast-regexp "\\)';")
  "Regular expression for Shoutcast.")

;; Reference: http://www.xiph.org/ogg/vorbis/doc/framing.html
(defconst emms-stream-info-icecast-capture-pattern "Oggs\\(.*\\)BCV"
  "Regular Expression for the beggining of an Ogg bitstream page.")

;; For all servers
(defconst emms-stream-info-stream-header-regexp 
  (concat emms-stream-info-char-alter-regexp "+")
  "Regular expression for metainformation headers.")

(defconst emms-stream-info-streamlist-regexp
  "\\(^http://.*\\)\\|^File.=\\(http://.*\\)"
  "Regular expression for streamlist URLs.")

;; When t output debugging info
(defconst emms-stream-info-debugging nil
  "If t then emms-stream-info will spill the stream into a buffer.
Set to NIL unless you want a buffer filled with binary junk.")

(defconst emms-stream-info-debug-buffer "*emms-stream-info-debug*"
  "Buffer for debugging information.")

(defconst emms-stream-info-vocab (list "name" 
				       "genre" 
				       "pub" 
				       "metaint" 
				       "br" 
				       "bitrate" 
				       "description" 
				       "public" 
				       "audio-info")
  "List of header keys.")

(defconst emms-stream-info-format-string 
  "Now streaming:%s, %c %bKb/sec"
  "The following %-sequences are supported:

%b Bitrate
%s Song title and artist name
%c Station/Channel name and short description
%t Song title
%g Station/Channel genre
%a Artist name

Note that some stations do not supply artist and song title
information.")

(defconst emms-stream-info-format-string-notitle
  "Now streaming: %c %bKb/sec %g"
  "Some streaming audio stations/channels do not provide artist
and songtitle information. This string specifies an alternate
format for those stations.")

(defconst emms-stream-info-pls-regexp ".*\.pls"
  "Regular expression for a .pls streamlist file.")

(defconst emms-stream-info-m3u-regexp ".*\.m3u"
  "Regular expression for a .m3u streamlist file.")

(defvar emms-stream-info-url nil
  "Server URL.")

(defvar emms-stream-info-port nil
  "Server port.")

(defvar emms-stream-info-found nil
  "Results of our search.")

(defvar emms-stream-info-streamlist-found nil
  "Results of our streamlist search.")

(defvar emms-stream-info-procname "emms-stream-info-process"
  "Name of network connection process.")

(defvar emms-stream-info-downloaded 0
  "Amount of stream data downloaded.")

(defvar emms-stream-info-read-inhibit nil
  "When t do not attempt to read 'emms-stream-info-found'.")

(defvar emms-stream-info-return-hook nil
  "Activated after the disconnection from the streaming audio server.")

(defvar emms-stream-info-read-hook nil
  "Activated after the disconnection from the streaming audio
server. This hook is for integration purposes, for general user
functions use 'emms-stream-info-return-hook'.")

(defvar emms-stream-info-header-flag nil
  "Non-nil means header information has been captured.")

(defvar emms-stream-info-title-flag nil
  "Non-nil means title information has been captured.")

(defvar emms-stream-info-streamlist-flag nil
  "Non-nil means streamlist information has been captured.")

(defvar emms-stream-info-request-string nil
  "String sent to streaming audio server.")

(defun emms-stream-info-decompose-url (urlstr)
  "Return a vector containing the elements of the URI URLSTR."
  (let ((host nil)
	(file nil)
	(port nil)
	(protocol nil)
	(user nil)			; nil
	(pass nil)			; nil
	(refs nil)			; nil
	(attr nil)			; nil
	(full nil)
	(pos 1))
    (with-temp-buffer
      (insert urlstr)
      (goto-char (point-min))
      (if (looking-at "http")
	  (progn
	    (forward-char 4)
	    (setq protocol (buffer-substring-no-properties pos (point)))
	    (setq pos (point))))
      (skip-chars-forward "://")
      (setq pos (point))
      (skip-chars-forward "^/")
      (setq host (buffer-substring pos (point)))
      (if (string-match ":\\([0-9+]+\\)" host)
	  (setq port (string-to-number (match-string 1 host))
		host (substring host 0 (match-beginning 0))))
      (setq pos (point))
      (setq file (buffer-substring pos (point-max)))
      (setq full (buffer-substring (point-min) (point-max))))
    ;; Return in format compatible with 'url-generic-parse-url'. 
    (vector protocol user pass host port file refs attr full)))

;; This is our tiny state machine for keeping track across multiple
;; connections.
(defvar emms-stream-info-state-bv
  (if (fboundp 'make-bool-vector)
      (make-bool-vector 3 nil)
    (make-vector 3 nil))
  "State of sequential connections.
true at index 0 means output formatted message.
true at index 1 means insert formatted message.
trye at index 2 means continue to next connection.")

;; This bit is ugly and non-lispish, but asynchronous communications
;; need a state machine. Better to do it with a macro. and once
;; everything works I will too!
(defun emms-stream-info-set-message ()
  (aset emms-stream-info-state-bv 0 t))
(defun emms-stream-info-unset-message ()
  (aset emms-stream-info-state-bv 0 nil))
(defun emms-stream-info-message-p ()
  (aref emms-stream-info-state-bv 0))

(defun emms-stream-info-set-insert ()
  (aset emms-stream-info-state-bv 1 t))
(defun emms-stream-info-unset-insert ()
  (aset emms-stream-info-state-bv 1 nil))
(defun emms-stream-info-insert-p ()
  (aref emms-stream-info-state-bv 1))

(defun emms-stream-info-set-continue ()
  (aset emms-stream-info-state-bv 2 t))
(defun emms-stream-info-unset-continue ()
  (aset emms-stream-info-state-bv 2 nil))
(defun emms-stream-info-continue-p ()
  (aref emms-stream-info-state-bv 2))

(defun emms-stream-info-streamlist-type (str)
  (if (stringp str)
      (cond ((string-match emms-stream-info-pls-regexp str)
	     'pls)
	    ((string-match emms-stream-info-m3u-regexp str)
	     'm3u)
	    (t nil))
    nil))

(defun emms-stream-info-format (str format-alist)
  (let ((key-list (mapcar 'car format-alist)))
    (setq key-list (mapcar 'car format-alist))
    (mapc (lambda (e)
	    (setq str 
		  (emms-replace-regexp-in-string 
		   e 
		   (cdr (assoc e format-alist))
		   str)))
	  key-list))
  str)
    
;; Output a human readable message
(defun emms-stream-info-pretty-print (&optional string-out)
  "Output a human readable message. If STRING-OUT is non-nil, do
not output a message and only return a string."
  (let (str
	(format-string emms-stream-info-format-string)
	(format-alist
	 (list
	  (cons "%b" (or (emms-stream-info-get-key "br")
			 (emms-stream-info-get-key "bitrate")
			 ""))
	  (cons "%s" (or (emms-stream-info-get-key "songtitle") ""))
	  (cons "%c" (or (emms-stream-info-get-key "name") ""))
	  (cons "%t" (or (emms-stream-info-get-key "title") ""))
	  (cons "%g" (or (emms-stream-info-get-key "genre") ""))
	  (cons "%a" (or (emms-stream-info-get-key "artist") ""))
	  (cons "%. " ""))))		; clean untreated tags
    
    ;; Choose alternate string format if necessary
    (unless (emms-stream-info-get-key "title")
      (setq format-string emms-stream-info-format-string-notitle))

    ;; format according to the format-string
    (setq str
	  (emms-stream-info-format 
	   format-string
	   format-alist))
    
    ;; Escape rougue percent signs hiding in our string.
    (setq str (emms-replace-regexp-in-string "%" "%%" str))

    ;; Either output a message or return a string. But only if it is
    ;; an identifiable station/channel
    (when (emms-stream-info-get-key "name")
      (if string-out
	  str
	(message "%s" str)))))

(defun emms-stream-info-pretty-print-insert ()
  "Insert the formatted output of 'emms-stream-info-get' at point."
  (insert (or (emms-stream-info-pretty-print t) "")))

(defun emms-stream-info-continue ()
  (emms-stream-info-unset-continue)
  (if emms-stream-info-streamlist-found
      (emms-stream-info-get emms-stream-info-streamlist-found
			    (emms-stream-info-message-p)
			    (emms-stream-info-insert-p)
			    nil)
    (error "No streamlist found at URL")))

;; Useful
(defun list-to-string (l)
  "Return a STRING which is the concatenation of the elements of
L."
  (if (not l)
      nil
    (if (stringp (car l))
	(concat (car l) (list-to-string (cdr l)))
      (list-to-string (cdr l)))))

(defun emms-stream-info-get-key (key)
  "Return STRING associated with KEY."
  (unless emms-stream-info-read-inhibit
    (cdr (assoc key emms-stream-info-found))))

(defun emms-stream-info-get-keys (keys)
  "Return a list of strings associated with each key in
KEYS. KEYS should be a list of strings."
  (mapcar (lambda (e)
	    (emms-stream-info-get-key e))
	  keys))

;; BEGIN to END should typically be a segment of about 250 Bytes
;; length for Ogg streams.
(defun emms-stream-info-decode-ogg (begin end)
  "Parse Ogg stream segment from BEGIN to END."
  (let ((artist nil)
	(title nil))

    (goto-char begin)
    (re-search-forward (concat "artist=\\(" 
			       emms-stream-info-icecast-regexp 
			       "\\)") end t)
    (setq artist (emms-match-string-no-properties 1))
    
    (goto-char begin)
    (re-search-forward (concat "title=\\(" 
			       emms-stream-info-icecast-regexp 
			       "\\)") end t)
    (setq title (emms-match-string-no-properties 1))

    ;; ugh
    (if (or artist title)
	(list (cons "songtitle" (concat artist
					(if (and artist title)
					    " - "
					  " ")
					title))
	      (cons "artist" artist)
	      (cons "title" title))
      nil)))

;; BEGIN to END should be about 20 Bytes long
(defun emms-stream-info-decode-mp3 (begin end)
  "Parse Shoutcast/Icecast-MP3 segment from BEGIN to END."
  (let ((split nil)
	(songtitle nil)
	(artist nil)
	(title nil))

    (goto-char begin)
    (setq songtitle (buffer-substring begin end)
	  split (split-string songtitle "-"))

    (if (cdr split)
	(setq artist (car split)
	      title (list-to-string (cdr split))))

    (list (cons "songtitle" songtitle)
	  (cons "artist" artist)
	  (cons "title" title))))

(defun emms-stream-info-filter (proc str)
  "Filter function for the network process.
Argument PROC Process.
Argument STR Quanta of data."

  ;; Debugging flag dependent
  (if emms-stream-info-debugging
      (with-current-buffer emms-stream-info-debug-buffer
	(insert str)))

  (with-temp-buffer
    (setq emms-stream-info-downloaded (+ emms-stream-info-downloaded 
					 (length str)))

    ;; Insert a quanta of data.
    (insert str)

    ;; Look for headers
    (unless emms-stream-info-header-flag
      (mapcar (lambda (term)
		(goto-char (point-min))
		(if (re-search-forward 
		     (concat (regexp-opt 
			      (list "icy-" "ice-"))
			     term
			     ":\\(" 
			     emms-stream-info-stream-header-regexp
			     "\\)")
		     (point-max) t)
		    (progn
		      (add-to-list 'emms-stream-info-found 
				   (cons term 
					 (emms-match-string-no-properties 1)))
		      (setq emms-stream-info-header-flag t))))
	      emms-stream-info-vocab))

    ;; Look for title
    (unless emms-stream-info-title-flag
      (goto-char (- (point)
		    (length str)))
      (cond ((re-search-forward 
	      emms-stream-info-icecast-capture-pattern 
	      (point-max) 
	      t)
	     (setq emms-stream-info-found 
		   (append 
		    emms-stream-info-found 
		    (emms-stream-info-decode-ogg 
		     (match-beginning 1) 
		     (match-end 1))))
	     (setq emms-stream-info-title-flag t))
	    ;; In retrospect this section mimics input_http.c from
	    ;; the Xine project only that it uses buffer searching.
	    ((re-search-forward 
	      emms-stream-info-shoutcast-title-regexp 
	      (point-max) 
	      t)
	     (setq emms-stream-info-found 
		   (append emms-stream-info-found 
			   (emms-stream-info-decode-mp3 
			    (match-beginning 1) 
			    (match-end 1))))
	     (setq emms-stream-info-title-flag t))))

    ;; Too many nested conditions
    (if (emms-stream-info-set-continue)
	(unless emms-stream-info-streamlist-flag
	  (goto-char (point-min))
	  (if (re-search-forward  
	       emms-stream-info-streamlist-regexp
	       (point-max) t)
	      (progn
		(setq emms-stream-info-streamlist-found
		      (or (emms-match-string-no-properties 1)
			  (emms-match-string-no-properties 2)))
		(setq emms-stream-info-streamlist-flag t))))))

  ;; Be chatty at the user
  (if emms-stream-info-verbose
      (message "Connection %s. Downloaded %d/%d bytes."
	       (process-status proc) 
	       emms-stream-info-downloaded 
	       emms-stream-info-max))

  ;; Find out if we need to kill the connection
  (if (or (> emms-stream-info-downloaded emms-stream-info-max) ; maxed out?
	  ;; Captured header and title info?
	  (and emms-stream-info-header-flag emms-stream-info-title-flag)
	  ;; Captured streamlist info?
	  emms-stream-info-streamlist-flag)
      (emms-stream-info-kill-process proc)))

;; Closing the connection proves to be the most difficult part of the
;; program. There is a difference in the way emacs21 vs. emacs22
;; behave.
(defun emms-stream-info-kill-process (proc)
  "Hold Emacs while trying to close the connection.
Argument PROC Process."
  (while (not (equal (process-status proc) 'closed))
    (delete-process proc))
  (if (process-filter proc)
      (set-process-filter proc nil))
  ;; Workaround Emacs 21 sentinel problems
  (when (= emacs-major-version 21)
    (emms-stream-info-after-function)))

(defun emms-stream-info-after-function ()
  "Evalutated when the connection ends."
  (setq emms-stream-info-read-inhibit nil) ; allow reading
  (run-hooks 'emms-stream-info-read-hook)
  (run-hooks 'emms-stream-info-return-hook))

(defun emms-stream-info-sentinel (proc ev)
  "Sentinel function for network process.
Argument PROC Process.
Argument EV Event string."
  ;; Workaround Emacs 21 sentinel problems
  (unless (= emacs-major-version 21)
    (emms-stream-info-after-function)))

(defun emms-stream-info-make-request-string (file)
  "Return a valid HTTP request string with FILE as a URI."
  (concat "GET "
	  (if (equal file "")
	      "/"
	    file)
	  " HTTP/1.0\r\n"
	  "User-Agent: Free software (see www.gnu.org), reads title of currently playing track (discards audio).\r\n"
	  "Icy-MetaData:1\r\n"
	  "\r\n"))

(defun emms-stream-info-parse-url (urlstring)
  "Set the global variables for connecting to the streaming audio
server at URLSTRING."
  (let* ((url (emms-stream-info-decompose-url urlstring))
	 (hostname (elt url 3))
	 (port (elt url 4))
	 (file (elt url 5))
	 (protocol (elt url 0)))

    (cond ((or (not (equal protocol "http"))
	       (equal hostname ""))
	   (error "Invalid URL"))

	  ;; eg. "http://music.station.com:8014"
	  ((and (empty-string-p file)
		port)
	   (setq emms-stream-info-port port))

	  ;; eg. "http://ogg.smgradio.com/vr96.ogg"
	  ((and (not (empty-string-p file))
		(or (equal port "")
		    (equal port nil)
		    (equal port 0)))
	   (setq emms-stream-info-port 80))

	  ;; eg. "http://audio.ibiblio.org:8010/wcpe.ogg"
	  ((and (not (empty-string-p file))
		port)
	   (setq emms-stream-info-port port))

	  (t (error "Invalid URL")))

    (setq emms-stream-info-url hostname
	  emms-stream-info-request-string 
	  (emms-stream-info-make-request-string file))))

(defun empty-string-p (str)
  "Return t if STR is equal to the empty string."
  (equal str ""))

(defun emms-stream-info-reset-state ()
  (setq emms-stream-info-downloaded 0)		 ; restart fallback
  (setq emms-stream-info-title-flag nil)	 ; forget title flag
  (setq emms-stream-info-header-flag nil)	 ; forget header flag
  (setq emms-stream-info-found nil)		 ; forget output
  (setq emms-stream-info-streamlist-found nil)	 ; forget streamlist
  (setq emms-stream-info-streamlist-flag nil)	 ; forget streamlist
  (setq emms-stream-info-read-inhibit t)         ; do not read output

  ;; Reset state machine
  (emms-stream-info-unset-message)
  (emms-stream-info-unset-insert)
  (emms-stream-info-unset-continue)

  ;; forget hooks
  (remove-hook 'emms-stream-info-return-hook  
	       'emms-stream-info-pretty-print)   
  (remove-hook 'emms-stream-info-return-hook 
	       'emms-stream-info-continue)
  (remove-hook 'emms-stream-info-return-hook 
	       'emms-stream-info-pretty-print-insert))

;; -------------------------------------------------------------------
;; Interactive functions
;; -------------------------------------------------------------------

(defun emms-stream-info-get (&optional urlstring say write cont)
  "Get streaming audio server header metadata and song title from stream at URL.
Argument URLSTRING Address of streaming audio server as a string.
If URLSTRING is nil then get the latest stream played via emms.
Optional argument SAY boolean.
Optional argument WRITE boolean.
Optional argument CONT boolean."
  (interactive)

  (if urlstring
      (emms-stream-info-parse-url urlstring)
    (and (boundp 'emms-stream-last-stream)
         (fboundp 'emms-stream-url)
         emms-stream-last-stream
         (emms-stream-info-parse-url
          (emms-stream-url emms-stream-last-stream))))

  (emms-stream-info-reset-state)

  ;; Output formatted text as a message.
  (if say
      (progn
	(add-hook 'emms-stream-info-return-hook 
		  'emms-stream-info-pretty-print)
	(emms-stream-info-set-message)))
  ;; Insert formatted text into the current buffer.
  (if write
      (progn
	(add-hook 'emms-stream-info-return-hook 
		  'emms-stream-info-pretty-print-insert)
	(emms-stream-info-set-insert)))
  ;; Continue to the next connection after this one.
  (if cont
      (progn
	(add-hook 'emms-stream-info-return-hook 
		  'emms-stream-info-continue)
	(emms-stream-info-set-continue)))

  ;; Debugging flag dependent
  (if emms-stream-info-debugging
      (progn
	(if (get-buffer emms-stream-info-debug-buffer)
	    (kill-buffer emms-stream-info-debug-buffer))
	(get-buffer-create emms-stream-info-debug-buffer)))

  ;; Open connection
  (condition-case nil
      (if (fboundp 'make-network-process)
	  (make-network-process :name emms-stream-info-procname 
				:buffer nil 
				:host emms-stream-info-url 
				:service emms-stream-info-port)
	(open-network-stream emms-stream-info-procname
			     nil
			     emms-stream-info-url
			     emms-stream-info-port))
    (error
     (emms-stream-info-reset-state)
     (message "Error connecting to streaming audio sever at %s" 
	      emms-stream-info-url)))

  (let ((proc (get-process emms-stream-info-procname)))
    (when proc

      ;; Connection timeone
      (run-at-time emms-stream-info-timeout 
		   nil 
		   'emms-stream-info-kill-process 
		   proc)

      ;; Start download
      (process-send-string emms-stream-info-procname 
			   emms-stream-info-request-string)
      (set-process-sentinel proc 
			    'emms-stream-info-sentinel)
      (set-process-filter proc 
			  'emms-stream-info-filter)
      (unless (process-sentinel proc)
	(error "No process sentinel")))))

;; Should be phased out.
;; (defun emms-stream-info-input-sanity (&optional urlstring)
;;   (let ((type (emms-track-type (emms-playlist-selected-track))))
;;     (cond ((null urlstring)
;; 	   (if (or (equal type 'streamlist)
;; 		   (equal type 'url))
;; 	       (emms-track-name (emms-playlist-selected-track))))
;; 	  ((not (stringp urlstring))
;; 	   (error "URL must be in string format"))
;; 	  ((stringp url) urlstring))))

(defun emms-stream-info-input-sanity (&optional urlstring)
  (if (stringp urlstring)
      urlstring
    (error "URL must be in string format")))

(defun emms-stream-info-message (&optional urlstring)
  "Get information from streaming audio server at URLSTRING.
Return a formatted message.
URLSTRING should be a string."
  (interactive)
  (let ((url (emms-stream-info-input-sanity urlstring)))
    (cond ((equal (emms-stream-info-streamlist-type url) 'pls)
	   (emms-stream-info-get url t nil t))
	  ((equal (emms-stream-info-streamlist-type url) 'm3u)
	   (emms-stream-info-get url t nil t))
	  (t (emms-stream-info-get url t)))))

;; Insertion does not work for sequential connections.
(defun emms-stream-info-insert (&optional urlstring)
  "Get information from streaming audio server at URLSTRING.
Insert a formatted message at point.
URLSTRING should be a string."
  (interactive)
  (let ((url (emms-stream-info-input-sanity urlstring)))
    (cond ((equal (emms-stream-info-streamlist-type url) 'pls)
	   (emms-stream-info-get url nil t t))
	  ((equal (emms-stream-info-streamlist-type url) 'm3u)
	   (emms-stream-info-get url nil t t))
	  (t (emms-stream-info-get url nil t)))))

(provide 'emms-stream-info)

;;; emms-stream-info.el ends here
