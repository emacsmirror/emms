;;; emms-lyrics.el --- Display lyrics synchronically

;; Copyright (C) 2005, 2006 Free Software Foundation, Inc.

;; Author: William Xu <william.xwl@gmail.com>
;; Keywords: emms music lyric

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EMMS; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package enables you to play music files and display lyrics
;; synchronically! :-)

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;             (require 'emms-lyrics)
;;
;; Then either `M-x emms-lyrics-enable' or add (emms-lyrics 1) in
;; your .emacs to enable.

;;; Known bugs:

;; 1. Sometimes music playing would be blocked by some process, like
;;    startup Gnus, while emms-lyrics still goes on, thus make music and
;;    lyrics asynchronical.

;;; Todo:

;; 1. Maybe the lyric setup should run before `emms-start'.
;; 2. Give a user a chance to choose when finding out multiple lyrics.
;; 3. Search lyrics from internet ?

;;; Code:

(require 'emms)
(require 'emms-player-simple)
(require 'emms-source-file)
(require 'time-date)

;;; User Customization

(defgroup emms-lyrics nil
  "Lyrics module for EMMS."
  :group 'emms)

(defcustom emms-lyrics-display-on-modeline t
  "If non-nil, display lyrics on mode line."
  :type 'boolean
  :group 'emms-lyrics)

(defcustom emms-lyrics-display-on-minibuffer nil
  "If non-nil, display lyrics on minibuffer."
  :type 'boolean
  :group 'emms-lyrics)

(defcustom emms-lyrics-dir ""
  "Local lyrics repository. `emms-lyrics-find-lyric' will look
for lyrics in current directory and this directory."
  :type 'string
  :group 'emms-lyrics)

(defcustom emms-lyrics-display-format " %s "
  "Format for displaying lyrics."
  :type 'string
  :group 'emms-lyrics)

(defcustom emms-lyrics-coding-system 'utf-8
  "Coding system used in the output of lyrics."
  :type 'coding-system
  :group 'emms-lyrics)

(defcustom emms-lyrics-mode-hook nil
  "Normal hook run after entering Emms Lyric mode."
  :type 'hook
  :group 'emms-lyrics)


;;; Emms Lyrics

(defvar emms-lyrics-display-p t
  "If non-nil, will diplay lyrics.")

(defvar emms-lyrics-alist nil
  "a list of the form: '((time0 lyric0) (time1 lyric1)...)). In short,
at time-i, display lyric-i.")

(defvar emms-lyrics-timers nil
  "timers for displaying lyric.")

(defvar emms-lyrics-start-time nil
  "emms lyric start time.")

(defvar emms-lyrics-pause-time nil
  "emms lyric pause time.")

(defvar emms-lyrics-elapsed-time 0
  "How long time has emms lyric played.")

(defvar emms-lyrics-mode-line-string ""
  "current lyric.")

(defun emms-lyrics-read-file (file)
  "Read a lyric file(LRC format).
FILE should end up with \".lrc\", its content looks like one of the
following:

    [1:39]I love you, Emacs!
    [00:39]I love you, Emacs!
    [00:39.67]I love you, Emacs!

FILE should be under the same directory as the music file, or under
`emms-lyrics-dir'."
  (when (eq 'file (emms-track-get
                   (emms-playlist-current-selected-track)
                   'type))
    (unless (file-exists-p file)
      (setq file (emms-lyrics-find-lyric file)))
    (when (and file (not (string= file "")) (file-exists-p file))
      (with-temp-buffer
        (let ((coding-system-for-read emms-lyrics-coding-system))
          (insert-file-contents file)
          (while (search-forward-regexp "\\[[0-9:.]+\\].*" nil t)
            (let ((lyric-string (match-string 0))
                  (time 0)
                  (lyric ""))
              (setq lyric
                    (emms-replace-regexp-in-string ".*\\]" "" lyric-string))
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
                  (setq emms-lyrics-alist
                        (append emms-lyrics-alist `((,time ,lyric))))
                  (setq time 0))))))
        t))))

(defun emms-lyrics-start ()
  "Start displaying lryics."
  (setq emms-lyrics-start-time (current-time)
	emms-lyrics-pause-time nil
	emms-lyrics-elapsed-time 0)
  (when (let ((file
	       (emms-track-get
		(emms-playlist-current-selected-track)
		'name)))
	  (emms-lyrics-read-file
	   (emms-replace-regexp-in-string
	    (file-name-extension file) "lrc" file)))
    (emms-lyrics-set-timer)))

(defun emms-lyrics-stop ()
  "Stop displaying lyrics."
  (interactive)
  (when emms-lyrics-alist
    (mapc #'emms-cancel-timer emms-lyrics-timers)
    (if (or (not emms-player-paused-p)
	    emms-player-stopped-p)
	(setq emms-lyrics-alist nil
	      emms-lyrics-timers nil
	      emms-lyrics-mode-line-string ""))))

(defun emms-lyrics-pause ()
  "Pause displaying lyrics."
  (if emms-player-paused-p
      (setq emms-lyrics-pause-time (current-time))
    (when emms-lyrics-pause-time
      (setq emms-lyrics-elapsed-time
	    (+ (time-to-seconds
		(time-subtract emms-lyrics-pause-time
			       emms-lyrics-start-time))
	       emms-lyrics-elapsed-time)))
    (setq emms-lyrics-start-time (current-time)))
  (when emms-lyrics-alist
    (if emms-player-paused-p
	(emms-lyrics-stop)
      (emms-lyrics-set-timer))))

(defun emms-lyrics-seek (sec)
  "Seek forward or backward SEC seconds lyrics."
  (setq emms-lyrics-elapsed-time
	(+ emms-lyrics-elapsed-time
	   (time-to-seconds
	    (time-subtract (current-time)
			   emms-lyrics-start-time))
	   sec))
  (when (< emms-lyrics-elapsed-time 0)	; back to start point
    (setq emms-lyrics-elapsed-time 0))
  (setq emms-lyrics-start-time (current-time))
  (when emms-lyrics-alist
    (let ((paused-orig emms-player-paused-p))
      (setq emms-player-paused-p t)
      (emms-lyrics-stop)
      (setq emms-player-paused-p paused-orig))
    (emms-lyrics-set-timer)))

(defun emms-lyrics-sync (sec)
  "Synchronize the lyric display at SEC seconds."
  (setq emms-lyrics-start-time (current-time)
        emms-lyrics-elapsed-time 0)
  (emms-lyrics-seek sec))

(defun emms-lyrics-toggle-display-on-minibuffer ()
  "Toggle display lyrics on minibbufer."
  (interactive)
  (if emms-lyrics-display-on-minibuffer
      (progn
	(setq emms-lyrics-display-on-minibuffer nil)
	(message "Disable lyrics on minibufer."))
    (setq emms-lyrics-display-on-minibuffer t)
    (message "Enable lyrics on minibufer.")))

(defun emms-lyrics-toggle-display-on-modeline ()
  "Toggle display lyrics on mode line."
  (interactive)
  (if emms-lyrics-display-on-modeline
      (progn
	(setq emms-lyrics-display-on-modeline nil
	      emms-lyrics-mode-line-string "")
	(message "Disable lyrics on mode line."))
    (setq emms-lyrics-display-on-modeline t)
    (message "Enable lyrics on mode line.")))

(defun emms-lyrics (arg)
  "Turn on emms lyrics display if ARG is positive, off otherwise."
  (interactive "p")
  (if (and arg (> arg 0))
      (progn
        (setq emms-lyrics-display-p t)
        (add-hook 'emms-player-started-hook     'emms-lyrics-start)
        (add-hook 'emms-player-stopped-hook     'emms-lyrics-stop)
        (add-hook 'emms-player-finished-hook    'emms-lyrics-stop)
        (add-hook 'emms-player-paused-hook      'emms-lyrics-pause)
        (add-hook 'emms-player-seeked-functions 'emms-lyrics-seek)
        (add-hook 'emms-player-time-set-functions 'emms-lyrics-sync))
    (emms-lyrics-stop)
    (setq emms-lyrics-display-p nil)
    (emms-lyrics-restore-mode-line)
    (remove-hook 'emms-player-started-hook     'emms-lyrics-start)
    (remove-hook 'emms-player-stopped-hook     'emms-lyrics-stop)
    (remove-hook 'emms-player-finished-hook    'emms-lyrics-stop)
    (remove-hook 'emms-player-paused-hook      'emms-lyrics-pause)
    (remove-hook 'emms-player-seeked-functions 'emms-lyrics-seek)
    (remove-hook 'emms-player-time-set-functions 'emms-lyrics-sync)))

;;;###autoload
(defun emms-lyrics-enable ()
  "Enable displaying emms lyrics."
  (interactive)
  (emms-lyrics 1)
  (message "emms lyrics enabled."))

;;;###autoload
(defun emms-lyrics-disable ()
  "Disable displaying emms lyrics."
  (interactive)
  (emms-lyrics -1)
  (message "emms lyrics disabled."))

;;;###autoload
(defun emms-lyrics-toggle ()
  "Toggle displaying emms lyrics."
  (interactive)
  (if emms-lyrics-display-p
      (emms-lyrics-disable)
    (emms-lyrics-enable)))

(defun emms-lyrics-set-timer ()
  "Set timers for displaying lyrics."
  (setq emms-lyrics-timers
	(mapcar
	 '(lambda (arg)
	    (let ((time (- (car arg) emms-lyrics-elapsed-time))
		  (lyric (cadr arg)))
	      (when (>= time 0)
		(run-at-time (format "%d sec" time)
			     nil
			     'emms-lyrics-display
			     lyric))))
	 emms-lyrics-alist)))

(defun emms-lyrics-mode-line ()
  "Add lyric to the mode line."
  (or global-mode-string (setq global-mode-string '("")))
  (unless (member 'emms-lyrics-mode-line-string
		  global-mode-string)
    (setq global-mode-string
	  (append global-mode-string
		  '(emms-lyrics-mode-line-string)))))

(defun emms-lyrics-restore-mode-line ()
  "Restore the mode line."
  (setq global-mode-string
	(remove 'emms-lyrics-mode-line-string global-mode-string))
  (force-mode-line-update))

(defun emms-lyrics-display (lyric)
  "Display lyric.

LYRIC is current playing lyric.

See `emms-lyrics-display-on-modeline' and
`emms-lyrics-display-on-minibuffer' on how to config where to
display."
  (when emms-lyrics-alist
    (when emms-lyrics-display-on-modeline
      (emms-lyrics-mode-line)
      (setq emms-lyrics-mode-line-string
	    (format emms-lyrics-display-format lyric))
      (force-mode-line-update))
    (when emms-lyrics-display-on-minibuffer
      (message lyric))))

(defun emms-lyrics-find-lyric (file)
  "Use `emms-source-file-gnu-find' to find lrc FILE. You should specify
a valid `emms-lyrics-dir'."
  (unless (string= emms-lyrics-dir "")
    ;; If find two or more lyric files, only return the first one. Good
    ;; luck! :-)
    (car (split-string
	  (shell-command-to-string
	   (concat emms-source-file-gnu-find " "
		   emms-lyrics-dir " -name "
		   "'"			; wrap up whitespaces
		   (emms-replace-regexp-in-string
		    "'" "*"		; FIX ME, '->\'
		    (file-name-nondirectory file))
		   "'"))
	  "\n"))))


;;; emms-lyrics-mode

(defvar emms-lyrics-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" 'emms-lyrics-previous-line)
    (define-key map "n" 'emms-lyrics-next-line)
    (define-key map "i" 'emms-lyrics-insert-time)
    map)
  "Keymap for `emms-lyrics-mode'.")

(defun emms-lyrics-rem* (x y)
  "The remainder of X divided by Y, with the same sign as X."
  (let* ((q (floor x y))
         (rem (- x (* y q))))
    (if (= rem 0)
        0
      (if (eq (>= x 0) (>= y 0))
          rem
        (- rem y)))))

(defun emms-lyrics-insert-time ()
  "Insert lyric time in the form: [01:23.21], then goto the
beginning of next line."
  (interactive)
  (let* ((total (+ (time-to-seconds
		    (time-subtract (current-time)
				   emms-lyrics-start-time))
		   emms-lyrics-elapsed-time))
	 (min (/ (* (floor (/ total 60)) 100) 100))
	 (sec (/ (floor (* (emms-lyrics-rem* total 60) 100)) 100.0)))
    (insert (emms-replace-regexp-in-string
	     " " "0" (format "[%2d:%2d]" min sec))))
  (emms-lyrics-next-line))

(defun emms-lyrics-next-line ()
  "Goto the beginning of next line."
  (interactive)
  (forward-line 1))

(defun emms-lyrics-previous-line ()
  "Goto the beginning of previous line."
  (interactive)
  (forward-line -1))

(define-derived-mode emms-lyrics-mode nil "Emms Lyric"
  "Major mode for creating lyric files.
\\{emms-lyrics-mode-map}"
  (run-hooks 'emms-lyrics-mode-hook))

(provide 'emms-lyrics)

;;; emms-lyrics.el ends here
