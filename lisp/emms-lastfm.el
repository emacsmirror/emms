;;; emms-lastfm.el --- add your listened songs to your profile at last.fm

;; Copyright (C) 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Author: Tassilo Horn <tassilo@member.fsf.org>

;; Keywords: emms, mp3, mpeg, multimedia

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.

;; EMMS is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; EMMS; see the file COPYING.  If not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; This code sends information about what music you are playing to last.fm.
;; See <URL:http://www.last.fm> and
;; <URL:http://www.audioscrobbler.net/wiki/Protocol1.1>.

;;; Sample configuration:

;; (setq emms-lastfm-username "my-user-name"
;;       emms-lastfm-password "very-secret!")

;;; Usage:

;; To activate the last.fm emms plugin, run:
;;   `M-x emms-lastfm-enable'

;; Now all music you listen to will be submitted to Last.fm to enhance your
;; profile.

;; To deactivate the last.fm emms plugin, run:
;;   `M-x emms-lastfm-disable'

;; Beside submitting the tracks you listen to, you can also listen to Last.fm
;; radio. Simply copy the lastfm:// URL and run & paste:
;;   `M-x emms-lastfm-radio RET lastfm://artist/Britney Spears/fans'
;; (Of course you don't need to use _this_ URL. :-))

;; You can also insert Last.fm streams into playlists (or use
;; emms-streams.el to listen to them) by activating the player as
;; follows.
;;   (add-to-list 'emms-player-list 'emms-player-lastfm-radio)
;; To insert a Last.fm stream into a playlist, do
;;   (emms-insert-lastfm "lastfm://rest-of-url")

;; There are some functions for conveniently playing the Similar
;; Artists, Fan Radio, and the Global Tag Radio. Here you only need to
;; enter the band's name (for the first two) or the tag.
;;   `M-x emms-play-lastfm-similar-artists RET Britney Spears'
;;   `M-x emms-play-lastfm-artist-fan RET Modest Mouse'
;;   `M-x emms-play-lastfm-global-tag RET pop'

;; When you're listening to a Last.fm radio station you have the possibility to
;; give feedback to them. If you like the current song, type
;;   `M-x emms-lastfm-radio-love'.
;; If it's not that good, or it just happens to not fit to your actual mood,
;; type
;;   `M-x emms-lastfm-radio-skip'
;; and this song will be skipped.
;; If you really hate that song and you never want to hear it again, ban it by
;; typing
;;   `M-x emms-lastfm-radio-ban'.

;;; TODO
;;
;; - Get the last.fm radio stuff right again.  Currently the rating stuff seems
;;   to be broken.  There seems to be no official API, so one needs to look
;;   into the sources of the official client which can be found at
;;   http://www.audioscrobbler.net/development/client/.

;; -----------------------------------------------------------------------

(require 'url)
(require 'emms)
(require 'emms-mode-line)
(require 'emms-playing-time)
(require 'emms-source-file)
(require 'emms-url)

;;; Variables

(defgroup emms-lastfm nil
  "Interaction with the services offered by http://www.last.fm."
  :prefix "emms-lastfm-"
  :group 'emms)

(defcustom emms-lastfm-username ""
  "Your last.fm username"
  :type 'string
  :group 'emms-lastfm)

(defcustom emms-lastfm-password ""
  "Your last.fm password"
  :type 'string
  :group 'emms-lastfm)

(defcustom emms-lastfm-submission-verbose-p nil
  "If non-nil, display a message every time we submit a track to Last.fm."
  :type 'boolean
  :group 'emms-lastfm)

(defcustom emms-lastfm-submit-track-types '(file)
  "Specify what types of tracks to submit to Last.fm.
The default is to only submit files.

To submit every track to Last.fm, set this to t.

Note that it is not very meaningful to submit playlists,
streamlists, or Last.fm streams to Last.fm."
  :type '(choice (const :tag "All" t)
                 (set :tag "Types"
                      (const :tag "Files" file)
                      (const :tag "URLs" url)
                      (const :tag "Playlists" playlist)
                      (const :tag "Streamlists" streamlist)
                      (const :tag "Last.fm streams" lastfm)))
  :group 'emms-lastfm)

(defconst emms-lastfm-server "http://post.audioscrobbler.com/"
  "The last.fm server responsible for the handshaking
procedure. Only for internal use.")
(defconst emms-lastfm-client-id "ems"
  "The client ID of EMMS. Don't change it!")
(defconst emms-lastfm-client-version 0.2
  "The version registered at last.fm. Don't change it!")
(defconst emms-lastfm-protocol-version 1.2
  "The version of the supported last.fm protocol.  Don't change it.")

;; used internally
(defvar emms-lastfm-process nil "-- only used internally --")
(defvar emms-lastfm-session-id nil "-- only used internally --")
(defvar emms-lastfm-now-playing-url nil "-- only used internally --")
(defvar emms-lastfm-submit-url nil "-- only used internally --")
(defvar emms-lastfm-current-track nil "-- only used internally --")
(defvar emms-lastfm-timer nil "-- only used internally --")
(defvar emms-lastfm-current-track-starting-time-string nil "-- only used internally --")

;;; Scrobbling

(defun emms-lastfm-new-track-function ()
  "This function should run whenever a new track starts (or a
paused track resumes) and sets the track submission timer."
  (setq emms-lastfm-current-track
        (emms-playlist-current-selected-track))
  (setq emms-lastfm-current-track-starting-time-string
        (emms-lastfm-current-unix-time-string))
  ;; Tracks should be submitted, if they played 240 secs or half of their
  ;; length, whichever comes first.
  (let ((secs (emms-track-get emms-lastfm-current-track 'info-playing-time))
        (type (emms-track-type emms-lastfm-current-track)))
    (when (and secs
               (or (eq emms-lastfm-submit-track-types t)
                   (and (listp emms-lastfm-submit-track-types)
                        (memq type emms-lastfm-submit-track-types))))
      (when (> secs 240)
        (setq secs 240))
      (unless (< secs 30) ;; Skip titles shorter than 30 seconds
        (setq secs (- (/ secs 2) emms-playing-time))
        (unless (< secs 0)
          (setq emms-lastfm-timer
                (run-with-timer secs nil 'emms-lastfm-submit-track))))))
  ;; Update the now playing info displayed on the user's last.fm page.  This
  ;; doesn't affect the user's profile, so it can be done even for tracks that
  ;; should not be submitted.
  (emms-lastfm-submit-now-playing))

(defun emms-lastfm-http-POST (url string sentinel &optional sentinel-args)
  "Perform a HTTP POST request to URL using STRING as data.
STRING will be encoded to utf8 before the request.  Call SENTINEL
with the result buffer."
  (let ((url-http-attempt-keepalives nil)
        (url-show-status emms-lastfm-submission-verbose-p)
        (url-request-method "POST")
        (url-request-extra-headers
         '(("Content-type"
            . "application/x-www-form-urlencoded; charset=utf-8")))
        (url-request-data (encode-coding-string string 'utf-8)))
  (url-retrieve url sentinel sentinel-args)))

(defun emms-lastfm-http-GET (url sentinel &optional sentinel-args)
  "Perform a HTTP GET request to URL.
Call SENTINEL with SENTINEL-ARGS and the result buffer."
  (let ((url-show-status emms-lastfm-submission-verbose-p)
        (url-request-method "GET"))
    (url-retrieve url sentinel sentinel-args)))

(defun emms-lastfm-submit-now-playing ()
  "Submit now-playing infos to last.fm.
These will be displayed on the user's last.fm page."
  (let* ((artist (emms-track-get emms-lastfm-current-track 'info-artist))
         (title  (emms-track-get emms-lastfm-current-track 'info-title))
         (album  (emms-track-get emms-lastfm-current-track 'info-album))
         (track-number (emms-track-get emms-lastfm-current-track
                                       'info-tracknumber))
         (musicbrainz-id "")
         (track-length (number-to-string
                        (or (emms-track-get emms-lastfm-current-track
                                            'info-playing-time)
                            0))))
    ;; wait up to 5 seconds to submit np infos in order to finish handshaking.
    (dotimes (i 5)
      (when (not (and emms-lastfm-session-id
                      emms-lastfm-now-playing-url))
        (sit-for 1)))
    (when (and emms-lastfm-session-id
               emms-lastfm-now-playing-url)
      (emms-lastfm-http-POST emms-lastfm-now-playing-url
                             (concat "&s="    emms-lastfm-session-id
                                     "&a[0]=" (emms-url-quote artist)
                                     "&t[0]=" (emms-url-quote title)
                                     "&b[0]=" (emms-url-quote album)
                                     "&l[0]=" track-length
                                     "&n[0]=" track-number
                                     "&m[0]=" musicbrainz-id)
                             'emms-lastfm-submit-now-playing-sentinel))))

(defun emms-lastfm-submit-now-playing-sentinel (&rest args)
  "Parses the server reponse and inform the user if all worked
well or if an error occured."
  (let ((buffer (current-buffer)))
    (emms-http-decode-buffer buffer)
    (goto-char (point-min))
    ;; skip to the first empty line and go one line further.  There the last.fm
    ;; response starts.
    (re-search-forward "^$" nil t)
    (forward-line)
    (if (re-search-forward "^OK$" nil t)
        (progn
          (when emms-lastfm-submission-verbose-p
            (message "EMMS: Now playing infos submitted to last.fm"))
          (kill-buffer buffer))
      (message "EMMS: Now playing infos couldn't be submitted to last.fm: %s"
               (emms-read-line)))))

(defun emms-lastfm-cancel-timer ()
  "Cancels `emms-lastfm-timer' if it is running."
  (emms-cancel-timer emms-lastfm-timer)
  (setq emms-lastfm-timer nil))

(defun emms-lastfm-pause ()
  "Handles things to be done when the player is paused or
resumed."
  (if emms-player-paused-p
      ;; the player paused
      (emms-lastfm-cancel-timer)
    ;; The player resumed
    (emms-lastfm-new-track-function)))

(defun emms-lastfm (&optional ARG)
  "Start submitting the tracks you listened to to
http://www.last.fm, if ARG is positive. If ARG is negative or
zero submission of the tracks will be stopped. This applies to
the current track, too."
  (interactive "p")
  (cond
   ((not (and emms-lastfm-username emms-lastfm-password))
    (message "%s"
             (concat "EMMS: In order to activate the last.fm plugin you "
                     "first have to set both `emms-lastfm-username' and "
                     "`emms-lastfm-password'")))
   ((not emms-playing-time-p)
    (message "%s"
             (concat "EMMS: The last.fm plugin needs the functionality "
                     "provided by `emms-playing-time'. It seems that you "
                     "disabled it explicitly in your init file using code "
                     "like this: `(emms-playing-time -1)'. Delete that "
                     "line and have a look at `emms-playing-time's doc "
                     "string")))
   (t
    (if (and ARG (> ARG 0))
        (progn
          ;; Append it. Else the playing time could be started a bit too late.
          (add-hook 'emms-player-started-hook
                    'emms-lastfm-handshake-if-needed t)
          ;; Has to be appended, because it has to run after
          ;; `emms-playing-time-start'
          (add-hook 'emms-player-started-hook
                    'emms-lastfm-new-track-function t)
          (add-hook 'emms-player-stopped-hook
                    'emms-lastfm-cancel-timer)
          (add-hook 'emms-player-paused-hook
                    'emms-lastfm-pause)
          ;; Clean up after EMMS radio
          (remove-hook 'emms-player-started-hook
                       'emms-lastfm-cancel-timer-after-stop)
          (message "EMMS Last.fm plugin activated"))
      (remove-hook 'emms-player-started-hook
                   'emms-lastfm-handshake-if-needed)
      (remove-hook 'emms-player-started-hook
                   'emms-lastfm-new-track-function)
      (remove-hook 'emms-player-stopped-hook
                   'emms-lastfm-cancel-timer)
      (remove-hook 'emms-player-paused-hook
                   'emms-lastfm-pause)
      (when emms-lastfm-timer (emms-cancel-timer emms-lastfm-timer))
      (setq emms-lastfm-session-id nil
            emms-lastfm-submit-url    nil
            emms-lastfm-process       nil
            emms-lastfm-current-track nil)
      (message "EMMS Last.fm plugin deactivated")))))

(defalias 'emms-lastfm-activate 'emms-lastfm)
(emms-make-obsolete 'emms-lastfm-activate 'emms-lastfm "EMMS 2.2")

(defun emms-lastfm-enable ()
  "Enable the emms last.fm plugin."
  (interactive)
  (emms-lastfm 1))

(defun emms-lastfm-disable ()
  "Disable the emms last.fm plugin."
  (interactive)
  (emms-lastfm -1))

(defun emms-lastfm-restart ()
  "Disable and reenable the last.fm plugin. This will cause a new
handshake."
  (emms-lastfm-disable)
  (emms-lastfm-enable))

(defun emms-lastfm-handshake-if-needed ()
  (when (not (and emms-lastfm-session-id
                  emms-lastfm-submit-url
                  emms-lastfm-now-playing-url))
    (emms-lastfm-handshake)))

(defun emms-lastfm-current-unix-time-string ()
  (replace-regexp-in-string "\\..*" "" (number-to-string (float-time))))

(defun emms-lastfm-handshake ()
  "Handshakes with the last.fm server."
  (let ((timestamp (emms-lastfm-current-unix-time-string)))
    (emms-lastfm-http-GET
     (concat emms-lastfm-server
             "?hs=true"
             "&p=" (number-to-string emms-lastfm-protocol-version)
             "&c=" emms-lastfm-client-id
             "&v=" (number-to-string emms-lastfm-client-version)
             "&u=" (emms-url-quote emms-lastfm-username)
             "&t=" timestamp
             "&a=" (md5 (concat (md5 emms-lastfm-password) timestamp)))
     'emms-lastfm-handshake-sentinel)))

(defun emms-lastfm-handshake-sentinel (&rest args)
  "Parses the server reponse and inform the user if all worked
well or if an error occured."
  (let ((buffer (current-buffer)))
    (emms-http-decode-buffer buffer)
    (goto-char (point-min))
    ;; skip to the first empty line and go one line further.  There the last.fm
    ;; response starts.
    (re-search-forward "^$" nil t)
    (forward-line)
    (let ((response (emms-read-line)))
      (if (not (string-match (rx (or "OK")) response))
          (message "EMMS: Handshake failed: %s" response)
        (forward-line)
        (setq emms-lastfm-session-id (emms-read-line))
        (forward-line)
        (setq emms-lastfm-now-playing-url (emms-read-line))
        (forward-line)
        (setq emms-lastfm-submit-url (emms-read-line))
        (message "EMMS: Handshaking with server done")
        (kill-buffer buffer)))))

(defun emms-lastfm-submit-track ()
  "Submits the current track (`emms-lastfm-current-track') to
last.fm."
  (let* ((artist (emms-track-get emms-lastfm-current-track 'info-artist))
         (title  (emms-track-get emms-lastfm-current-track 'info-title))
         (album  (emms-track-get emms-lastfm-current-track 'info-album))
         (track-number (emms-track-get emms-lastfm-current-track 'info-tracknumber))
         (musicbrainz-id "")
         (track-length (number-to-string
                        (emms-track-get emms-lastfm-current-track
                                        'info-playing-time))))
    (emms-lastfm-http-POST
     emms-lastfm-submit-url
     (concat "&s="    emms-lastfm-session-id
             "&a[0]=" (emms-url-quote artist)
             "&t[0]=" (emms-url-quote title)
             "&i[0]=" emms-lastfm-current-track-starting-time-string
             "&o[0]=P" ;; TODO: Maybe support others.  See the API.
             "&r[0]="  ;; The rating.  Empty if not applicable (for P it's not)
             "&l[0]=" track-length
             "&b[0]=" (emms-url-quote album)
             "&n[0]=" track-number
             "&m[0]=" musicbrainz-id)
     'emms-lastfm-submission-sentinel)))

(defun emms-lastfm-submission-sentinel (&rest args)
  "Parses the server reponse and inform the user if all worked
well or if an error occured."
  (let ((buffer (current-buffer)))
    (emms-http-decode-buffer buffer)
    (goto-char (point-min))
    ;; skip to the first empty line and go one line further.  There the last.fm
    ;; response starts.
    (re-search-forward "^$" nil t)
    (forward-line)
    (if (re-search-forward "^OK$" nil t)
        (progn
          (when emms-lastfm-submission-verbose-p
            (message "EMMS: \"%s\" submitted to last.fm"
                     (emms-track-description emms-lastfm-current-track)))
          (kill-buffer buffer))
      (message "EMMS: Song couldn't be submitted to last.fm: %s"
               (emms-read-line)))))

;;; Playback of lastfm:// streams

(defgroup emms-player-lastfm-radio nil
  "EMMS player for Last.fm streams."
  :group 'emms-player
  :prefix "emms-player-lastfm-")

(defcustom emms-player-lastfm-radio (emms-player 'emms-lastfm-radio-start
                                                 'ignore ; no need to stop
                                                 'emms-lastfm-radio-playable-p)
  "*Parameters for the Last.fm radio player."
  :type '(cons symbol alist)
  :group 'emms-player-lastfm-radio)

(defconst emms-lastfm-radio-base-url "http://ws.audioscrobbler.com/radio/"
  "The base URL for playing lastfm:// stream.
-- only used internally --")

(defvar emms-lastfm-radio-session nil "-- only used internally --")
(defvar emms-lastfm-radio-stream-url nil "-- only used internally --")

(defun emms-lastfm-radio-get-handshake-url ()
  (concat emms-lastfm-radio-base-url
          "handshake.php?version=" (number-to-string
                                    emms-lastfm-client-version)
          "&platform="              emms-lastfm-client-id
          "&username="              (emms-url-quote emms-lastfm-username)
          "&passwordmd5="           (md5 emms-lastfm-password)
          "&debug="                 (number-to-string 9)))

(defun emms-lastfm-radio-handshake (fn radio-url)
  "Handshakes with the last.fm server.
Calls FN when done with RADIO-URL as its only argument."
  (emms-lastfm-http-GET (emms-lastfm-radio-get-handshake-url)
                        'emms-lastfm-radio-handshake-sentinel
                        (list fn radio-url)))

(defun emms-lastfm-radio-handshake-sentinel (status fn radio-url)
  (let ((buffer (current-buffer)))
    (emms-http-decode-buffer buffer)
    (setq emms-lastfm-radio-session    (emms-key-value "session"))
    (setq emms-lastfm-radio-stream-url (emms-key-value "stream_url"))
    (kill-buffer buffer)
    (if (and emms-lastfm-radio-session emms-lastfm-radio-stream-url)
        (progn
          (message "EMMS: Handshaking for Last.fm playback successful")
          (funcall fn radio-url))
      (message "EMMS: Failed handshaking for Last.fm playback"))))

(defun emms-lastfm-radio-1 (lastfm-url)
  "Internal function used by `emms-lastfm-radio'."
  (if (and emms-lastfm-radio-session
           emms-lastfm-radio-stream-url)
      (progn
        (emms-lastfm-http-GET
         (concat emms-lastfm-radio-base-url
                 "adjust.php?"
                 "session=" emms-lastfm-radio-session
                 "&url="    (emms-url-quote lastfm-url)
                 "&debug="  (number-to-string 0))
         'emms-lastfm-radio-sentinel))
    (message "EMMS: Cannot play Last.fm stream")))

(defun emms-lastfm-radio (lastfm-url)
  "Plays the stream associated with the given Last.fm URL. (A
Last.fm URL has the form lastfm://foo/bar/baz, e.g.

  lastfm://artist/Manowar/similarartists

or

  lastfm://globaltags/metal."
  (interactive "sLast.fm URL: ")
  ;; Streamed songs must not be added to the lastfm profile
  (emms-lastfm-disable)
  (if (not (and emms-lastfm-radio-session
                emms-lastfm-radio-stream-url))
      (emms-lastfm-radio-handshake #'emms-lastfm-radio-1 lastfm-url)
    (emms-lastfm-radio-1 lastfm-url)))

(defun emms-lastfm-radio-playable-p (track)
  "Determine whether the Last.fm player can play this track."
  (let ((name (emms-track-get track 'name))
        (type (emms-track-get track 'type)))
    (and (eq type 'lastfm)
         (string-match "^lastfm://" name))))

(defun emms-lastfm-radio-start (track)
  "Start playing TRACK."
  (when (emms-lastfm-radio-playable-p track)
    (let ((name (emms-track-get track 'name)))
      (emms-lastfm-radio name))))

(defcustom emms-lastfm-radio-metadata-period 15
  "When listening to Last.fm Radio every how many seconds should
emms-lastfm poll for metadata? If set to nil, there won't be any
polling at all.

The default is 15: That means that the mode line will display the
wrong (last) track's data for a maximum of 15 seconds. If your
network connection has a big latency this value may be too
high. (But then streaming a 128KHz mp3 won't be fun anyway.)"
  :type '(choice integer
                 (const :tag "Disable" nil))
  :group 'emms-lastfm)

(defun emms-lastfm-cancel-timer-after-stop ()
  (add-hook 'emms-player-stopped-hook
            'emms-lastfm-cancel-timer))

(defun emms-lastfm-radio-sentinel (&rest args)
  (let ((buffer (current-buffer)))
    (emms-http-decode-buffer buffer)
    (if (string= (emms-key-value "response" buffer) "OK")
        (progn
          (kill-buffer buffer)
          (add-hook 'emms-player-started-hook
                    'emms-lastfm-cancel-timer-after-stop)
          (emms-play-url emms-lastfm-radio-stream-url)
          (when emms-lastfm-radio-metadata-period
            (when emms-lastfm-timer
              (emms-lastfm-cancel-timer))
            (setq emms-lastfm-timer
                  (run-with-timer 0 emms-lastfm-radio-metadata-period
                                  'emms-lastfm-radio-request-metadata)))
          (message "EMMS: Playing Last.fm stream"))
      (kill-buffer buffer)
      (message "EMMS: Bad response from Last.fm"))))

(defun emms-lastfm-np (&optional insertp callback)
  "Show the currently-playing lastfm radio tune.

If INSERTP is non-nil, insert the description into the current
buffer instead.

If CALLBACK is a function, call it with the current buffer and
description as arguments instead of displaying the description or
inserting it."
  (interactive "P")
  (emms-lastfm-radio-request-metadata
   (lambda (status insertp buffer callback)
     (let ((response-buf (current-buffer))
           artist title)
       (emms-http-decode-buffer response-buf)
       (setq artist (emms-key-value "artist" response-buf)
             title  (emms-key-value "track" response-buf))
       (kill-buffer response-buf)
       (let ((msg (if (and title artist)
                      (format emms-show-format
                              (format "%s - %s" artist title))
                    "Nothing playing right now")))
         (cond ((functionp callback)
                (when (and title artist)
                  (funcall callback buffer msg)))
               ((and insertp title artist)
                (with-current-buffer buffer
                  (insert msg)))
               (t (message msg))))))
   (list insertp (current-buffer) callback)))

(defun emms-lastfm-read-artist ()
  "Read an artist name from the user."
  (let ((artists nil))
    (when (boundp 'emms-cache-db)
      (maphash
       #'(lambda (file track)
           (let ((artist (emms-track-get track 'info-artist)))
             (when artist
               (add-to-list 'artists artist))))
       emms-cache-db))
    (if artists
        (emms-completing-read "Artist: " artists)
      (read-string "Artist: "))))

(defun emms-play-lastfm-similar-artists (artist)
  "Plays the similar artist radio of ARTIST."
  (interactive (list (emms-lastfm-read-artist)))
  (emms-lastfm-radio (concat "lastfm://artist/"
                             artist
                             "/similarartists")))

(defun emms-play-lastfm-global-tag (tag)
  "Plays the global tag radio of TAG."
  (interactive "sGlobal Tag: ")
  (emms-lastfm-radio (concat "lastfm://globaltags/" tag)))

(defun emms-play-lastfm-artist-fan (artist)
  "Plays the artist fan radio of ARTIST."
  (interactive (list (emms-lastfm-read-artist)))
  (emms-lastfm-radio (concat "lastfm://artist/" artist "/fans")))

(defun emms-lastfm-radio-love ()
  "Inform Last.fm that you love the currently playing song."
  (interactive)
  (emms-lastfm-radio-rating "love"))

(defun emms-lastfm-radio-skip ()
  "Inform Last.fm that you want to skip the currently playing
song."
  (interactive)
  (emms-lastfm-radio-rating "skip"))

(defun emms-lastfm-radio-ban ()
  "Inform Last.fm that you want to ban the currently playing
song."
  (interactive)
  (emms-lastfm-radio-rating "ban"))

(defun emms-lastfm-radio-rating (command)
  (emms-lastfm-http-GET
   (concat emms-lastfm-radio-base-url
           "control.php?"
           "session="  emms-lastfm-radio-session
           "&command=" command
           "&debug="   (number-to-string 0))
   'emms-lastfm-radio-rating-sentinel))

(defun emms-lastfm-radio-rating-sentinel (&rest args)
  (let ((buffer (current-buffer)))
    (emms-http-decode-buffer buffer)
    (if (string= (emms-key-value "response" buffer) "OK")
        (message "EMMS: Rated current track")
      (message "EMMS: Rating failed"))
    (kill-buffer buffer)))

(defun emms-lastfm-radio-request-metadata (&optional fn data)
  "Request the metadata of the current song and display it.

If FN is given, call it instead of
`emms-lastfm-radio-request-metadata-sentinel', with DATA as its
first parameter.

If DATA is given, it should be a list."
  (interactive)
  (emms-lastfm-http-GET
   (concat emms-lastfm-radio-base-url
           "np.php?"
           "session=" emms-lastfm-radio-session
           "&debug="  (number-to-string 0))
   (or fn 'emms-lastfm-radio-request-metadata-sentinel)
   data))

(defun emms-lastfm-radio-request-metadata-sentinel (&rest args)
  (let ((buffer (current-buffer)))
    (emms-http-decode-buffer buffer)
    (let ((artist (emms-key-value "artist" buffer))
          (title  (emms-key-value "track" buffer))
          (track (emms-playlist-current-selected-track)))
      (kill-buffer buffer)
      (emms-track-set track 'info-artist artist)
      (emms-track-set track 'info-title title)
      (emms-track-updated track))))


;;; Utility functions

(defun emms-read-line ()
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun emms-key-value (key &optional buffer)
  "Returns the value of KEY from BUFFER.
If BUFFER is nil, use the current buffer.

BUFFER has to contain a key-value list like:

foo=bar
x=17"
  (unless (and buffer (not (buffer-live-p buffer)))
    (with-current-buffer (or buffer (current-buffer))
      (goto-char (point-min))
      (when (re-search-forward (concat "^" key "=") nil t)
        (buffer-substring-no-properties (point) (line-end-position))))))

(provide 'emms-lastfm)
;;; emms-lastfm.el ends here
