;;; emms-playing-time.el --- Display emms playing time on mode line

;; Copyright (C) 2005 William XWL

;; Author: William XWL <william.xwl@gmail.com>

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

;; Display playing time on mode line, it looks like: 01:32/04:09.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;   (require 'emms-playing-time)

;;; Code:

(defvar emms-playing-time-version "0.1 $Revision: 1.7 $"
  "EMMS playing time version string.")
;; $Id: emms-playing-time.el,v 1.7 2005/09/08 16:07:20 xwl Exp $

(eval-when-compile (require 'cl))
(require 'emms-info)
(require 'emms-info-mp3info)
(require 'emms-player-simple)
(require 'emms-player-extensions)

;;; Customizations
(defvar emms-playing-time-display-p t
  "Whether to display playing time on mode line or not.")

(defvar emms-playing-time-display-short-p nil
  "Only display elapsed time, don't display total playing time,
e.g., display 02:37 instead of 02:37/05:49.")

(defvar emms-playing-time-display-format " %s "
  "String used for displaying playing time on mode-line.")

;;; Variables
(defvar emms-playing-time 0
  "How long has EMMS run up to now.")

(defvar emms-playing-time-string "")

;;; Functions
(defun emms-playing-time-start ()
  "Get ready for display playing time."
  (when emms-playing-time-display-p
    (setq emms-playing-time 0)
    (emms-playing-time-mode-line)
    (run-at-time t 1 'emms-playing-time-display)))

(add-hook 'emms-player-started-hook 'emms-playing-time-start)

(defun emms-playing-time-stop ()
  "Remove playing time on the mode line."
  (when emms-playing-time-display-p
    (if (or (not emms-player-paused-p)
	    emms-player-stopped-p)
	(progn
	  (setq emms-playing-time-string "")
	  (force-mode-line-update)))
    (cancel-function-timers 'emms-playing-time-display)))

(add-hook 'emms-player-stopped-hook 'emms-playing-time-stop)
(add-hook 'emms-player-finished-hook 'emms-playing-time-stop)

(defun emms-playing-time-pause ()
  "Pause playing time."
  (when emms-playing-time-display-p
    (if emms-player-paused-p
	(emms-playing-time-stop)
      (run-at-time t 1 'emms-playing-time-display))))

(add-hook 'emms-player-paused-hook 'emms-playing-time-pause)

(defun emms-playing-time-seek (sec)
  "Seek forward or backward SEC playing time."
  (when emms-playing-time-display-p
    (setq emms-playing-time (+ emms-playing-time sec))
    (when (< emms-playing-time 0)	; back to start point
      (setq emms-playing-time 0))))

(add-hook 'emms-player-seeked-hook 'emms-playing-time-seek)

(defun emms-playing-time-display ()
  "Display playing time on the mode line."
  (setq emms-playing-time (1+ emms-playing-time))
  (let* ((min (/ emms-playing-time 60))
	 (sec (% emms-playing-time 60))
	 ;; How to adapt `emms-info-format-info' here?
	 (struct
	  (emms-info-get (emms-playlist-current-track)))
	 (total-min-only
	  (when struct (emms-info-playing-time-min struct)))
	 (total-sec-only
	  (when struct (emms-info-playing-time-sec struct))))
    (setq emms-playing-time-string
	  (format
	   emms-playing-time-display-format
	   (replace-regexp-in-string
	    " " "0"
	    (if (or emms-playing-time-display-short-p
		    ;; unable to get total time info
		    (not total-min-only)
		    (not total-sec-only))
		(format "%2d:%2d" min sec)
	      (format "%2d:%2d/%2s:%2s"
		      min sec total-min-only total-sec-only)))))
    (force-mode-line-update)))

(defun emms-playing-time-mode-line ()
  "Add playing time to the mode line."
  (unless (member 'emms-playing-time-string
		  global-mode-string)
    (setq global-mode-string
	  (append global-mode-string
		  '(emms-playing-time-string)))))


(provide 'emms-playing-time)

;;; emms-playing-time.el ends here
