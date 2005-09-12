;;; emms-lyric.el --- Display lyrics synchronically

;; Copyright (C) 2005 William XWL

;; Author: William XWL <william.xwl@gmail.com>
;; Keywords: emms music lyric

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;; This package enables you to play music files and display lyrics
;; synchronically! :-) It requires `emms-player-extensions'.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;             (require 'emms-lyric)

;; Take a look at the "User Customizable" part for possible personal
;; customizations.

;;; Change Log:

;; v 0.3 [2005/07/19 17:53:25] Add `emms-lyric-find-lyric' for find
;;       lyric files in local repository `emms-lyric-dir'. Rewrite
;;       `emms-lyric-setup' to support more lyric formats.

;; v 0.2 [2005/07/18 16:10:02] Fix `emms-lyric-pause' bug. Now it works
;;       fine. Add `emms-lyric-seek', but which does not work very well
;;       currently.

;; v 0.1 [2005/07/17 20:07:30] Initial version.

;;; Known bugs:

;; 1. Sometimes music playing would be blocked by some process, like
;;    startup Gnus, while emms-lyric still goes on, thus make music and
;;    lyrics asynchronical.

;;; Todo:

;; 1. Maybe the lyric setup should run before `emms-start'.
;; 2. Give a user a chance to choose when finding out multiple lyrics.
;; 3. Search lyrics from internet ?

;;; Code:

(defvar emms-lyric-version "0.4 $Revision: 1.14 $"
  "EMMS lyric version string.")
;; $Id: emms-lyric.el,v 1.14 2005/08/25 13:03:02 xwl Exp $

(require 'emms)
(require 'emms-player-simple)
(require 'emms-source-file)
(require 'emms-player-extensions)

;;; User Customizations
(defvar emms-lyric-display-p t
  "Whether to diplay lyrics or not.")

(defvar emms-lyric-display-on-modeline t
  "Display lyrics on mode line.")

(defvar emms-lyric-display-on-minibuffer nil
  "Display lyrics on minibuffer.")

(defvar emms-lyric-dir ""
  "The directory of local lyric files. `emms-lyric-find-lyric' will look
for lyrics in current directory and here.")

(defvar emms-lyric-display-format " %s "
  "Format for displaying lyric on mode-line.")

;;; Variables
(defvar emms-lyric-alist nil
  "a list of the form: '((time0 lyric0) (time1 lyric1)...)). In short,
at time-i, display lyric-i.")

(defvar emms-lyric-timers nil
  "timers for displaying lyric.")

(defvar emms-lyric-start-time nil
  "emms lyric start time.")

(defvar emms-lyric-pause-time nil
  "emms lyric pause time.")

(defvar emms-lyric-elapsed-time 0
  "How long time has emms lyric played.")

(defvar emms-lyric-mode-line-string ""
  "current lyric.")

;;; emms lyric control

(defun emms-lyric-read-file (file)
  "Read a lyric file(LRC format). File should end up with \".lrc\", its
contents look like:

    [1:39]I love you, Emacs!
    [00:39]I love you, Emacs!
    [00:39.67]I love you, Emacs!

To find FILE, first look up in current directory, if not found, continue
looking up in `emms-lyric-dir'."
  (when emms-lyric-display-p
    (unless (file-exists-p file)
      (setq file (emms-lyric-find-lyric file)))
    (when (and file (not (string= file "")) (file-exists-p file))
      (with-temp-buffer
	(insert-file-contents file)
	(while (search-forward-regexp "\\[[0-9:.]+\\].*" nil t)
	  (let ((lyric-string (match-string 0))
		(time 0)
		(lyric ""))
	    (setq lyric
		  (replace-regexp-in-string ".*\\]" "" lyric-string))
	    (while (string-match "\\[[0-9:.]+\\]" lyric-string)
	      (let* ((time-string (match-string 0 lyric-string))
		     (semi-pos (string-match ":" time-string)))
		(setq time
		      (+ (* (string-to-number
			     (substring time-string 1 semi-pos))
			    60)
			 (string-to-number
			  (substring time-string
				     (1+ semi-pos)
				     (1- (length time-string))))))
		(setq lyric-string
		      (substring lyric-string (length time-string)))
		(setq emms-lyric-alist
		      (append emms-lyric-alist `((,time ,lyric))))
		(setq time 0)))))
	t))))

(defun emms-lyric-start ()
  "Start displaying lryics."
  (setq emms-lyric-start-time (current-time)
	emms-lyric-pause-time nil
	emms-lyric-elapsed-time 0)
  (when (and emms-lyric-display-p
	     (let ((file (cdaddr (emms-playlist-selected-track))))
	       (emms-lyric-read-file
		(replace-regexp-in-string
		 (file-name-extension file) "lrc" file))))
    (emms-lyric-set-timer)))

(add-hook 'emms-player-started-hook 'emms-lyric-start)

(defun emms-lyric-stop ()
  "Stop displaying lyrics."
  (interactive)
  (when (and emms-lyric-display-p
	     emms-lyric-alist)
    (cancel-function-timers 'emms-lyric-display)
    (if (or (not emms-player-paused-p)
	    emms-player-stopped-p)
	(setq emms-lyric-alist nil
	      emms-lyric-timers nil
	      emms-lyric-mode-line-string ""))))

(add-hook 'emms-player-stopped-hook 'emms-lyric-stop)
(add-hook 'emms-player-finished-hook 'emms-lyric-stop)

(defun emms-lyric-pause ()
  "Pause displaying lyrics."
  (if emms-player-paused-p
      (setq emms-lyric-pause-time (current-time))
    (when emms-lyric-pause-time
      (setq emms-lyric-elapsed-time
	    (+ (time-to-seconds
		(time-subtract emms-lyric-pause-time
			       emms-lyric-start-time))
	       emms-lyric-elapsed-time)))
    (setq emms-lyric-start-time (current-time)))
  (when (and emms-lyric-display-p
	     emms-lyric-alist)
    (if emms-player-paused-p
	(emms-lyric-stop)
      (emms-lyric-set-timer))))

(add-hook 'emms-player-paused-hook 'emms-lyric-pause)

(defun emms-lyric-seek (sec)
  "Seek forward or backward SEC seconds lyrics."
  (setq emms-lyric-elapsed-time
	(+ emms-lyric-elapsed-time
	   (time-to-seconds
	    (time-subtract (current-time)
			   emms-lyric-start-time))
	   sec))
  (when (< emms-lyric-elapsed-time 0)	; back to start point
    (setq emms-lyric-elapsed-time 0))
  (setq emms-lyric-start-time (current-time))
  (when (and emms-lyric-display-p
	     emms-lyric-alist)
    (let ((paused-orig emms-player-paused-p))
      (setq emms-player-paused-p t)
      (emms-lyric-stop)
      (setq emms-player-paused-p paused-orig))
    (emms-lyric-set-timer)))

(add-hook 'emms-player-seeked-hook 'emms-lyric-seek)

(defun emms-lyric-toggle-display-on-minibuffer ()
  "Toggle display lyric on minibbufer."
  (interactive)
  (if emms-lyric-display-on-minibuffer
      (progn
	(setq emms-lyric-display-on-minibuffer nil)
	(message "Disable lyric on minibufer."))
    (setq emms-lyric-display-on-minibuffer t)
    (message "Enable lyric on minibufer.")))

(defun emms-lyric-toggle-display-on-modeline ()
  "Toggle display lyric on modeline."
  (interactive)
  (if emms-lyric-display-on-modeline
      (progn
	(setq emms-lyric-display-on-modeline nil
	      emms-lyric-mode-line-string "")
	(message "Disable lyric on modeline."))
    (setq emms-lyric-display-on-modeline t)
    (message "Enable lyric on modeline.")))

(defun emms-lyric-set-timer ()
  "Set timers for displaying lyrics."
  (setq emms-lyric-timers
	(mapcar
	 '(lambda (arg)
	    (let ((time (- (car arg) emms-lyric-elapsed-time))
		  (lyric (cadr arg)))
	      (when (>= time 0)
		(run-at-time (format "%d sec" time)
			     nil
			     'emms-lyric-display
			     lyric))))
	 emms-lyric-alist)))

(defun emms-lyric-mode-line ()
  "Add lyric to the mode line."
  (unless (member 'emms-lyric-mode-line-string
		  global-mode-string)
    (setq global-mode-string
	  (append global-mode-string
		  '(emms-lyric-mode-line-string)))))

(defun emms-lyric-display (lyric)
  "Display lyric.

LYRIC is current playing lyric.

See `emms-lyric-display-on-modeline' and
`emms-lyric-display-on-minibuffer' on how to config where to
display."
  (when (and emms-lyric-display-p
	     emms-lyric-alist)
    (when emms-lyric-display-on-modeline
      (emms-lyric-mode-line)
      (setq emms-lyric-mode-line-string 
	    (format emms-lyric-display-format lyric))
      (force-mode-line-update))
    (when emms-lyric-display-on-minibuffer
      (message lyric))))

(defun emms-lyric-find-lyric (file)
  "Use `emms-source-file-gnu-find' to find lrc FILE. You should specify
a valid `emms-lyric-dir'."
  (unless (string= emms-lyric-dir "")
    ;; If find two or more lyric files, only return the first one. Good
    ;; luck! :-)
    (car (split-string
	  (shell-command-to-string
	   (concat emms-source-file-gnu-find " "
		   emms-lyric-dir " -name "
		   "'"			; wrap up whitespaces
		   (replace-regexp-in-string
		    "'" "*"		; FIX ME, '->\'
		    (file-name-nondirectory file))
		   "'"))
	  "\n"))))

;;; emms-lyric-mode

(defun emms-lyric-insert-time ()
  "Insert lyric time in the form: [01:23.21], then goto the
beginning of next line."
  (interactive)
  (let* ((total (+ (time-to-seconds
		    (time-subtract (current-time)
				   emms-lyric-start-time))
		   emms-lyric-elapsed-time))
	 (min (/ (* (floor (/ total 60)) 100) 100))
	 (sec (/ (floor (* (rem* total 60) 100)) 100.0)))
    (insert (replace-regexp-in-string
	     " " "0" (format "[%2d:%2d]" min sec))))
  (emms-lyric-next-line))

(defun emms-lyric-next-line ()
  "Goto the beginning of next line."
  (interactive)
  (forward-line 1))

(defun emms-lyric-previous-line ()
  "Goto the beginning of previous line."
  (interactive)
  (forward-line -1))

(defvar emms-lyric-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" 'emms-lyric-previous-line)
    (define-key map "n" 'emms-lyric-next-line)
    (define-key map "i" 'emms-lyric-insert-time)
    map)
  "Keymap for `emms-lyric-mode'.")

(defvar emms-lyric-mode-hook nil
  "Normal hook run when entering Emms Lyric mode.")

(define-derived-mode emms-lyric-mode nil "Emms Lyric"
  "Major mode for creating lyric files.
\\{emms-lyric-mode-map}"
  (run-hooks 'emms-lyric-mode-hook))


(provide 'emms-lyric)

;;; emms-lyric.el ends here
