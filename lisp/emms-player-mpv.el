;;; emms-player-mpv.el --- mpv support for EMMS

;; Copyright (C) 2013-2018 ZHANG Weiyi
;; Copyright (C) 2014 Alex Kost
;; Copyright (C) 2018 stardiviner <numbchild@gmail.com>
;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Authors: ZHANG Weiyi <dochang@gmail.com>,
;;          Alex Kost <alezost@gmail.com>,
;;          stardiviner <numbchild@gmail.com>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; This provides a player that uses mpv.  It supports pause and
;; seeking.  See mpv manual for more.
;;
;; To load subtitles automatically,
;; put "`sub-auto=fuzzy"` in the mpv's config file.
;;
;; To disable/enable the cover display when playing music, modify
;;   `emms-player-mpv-parameters'. For example: (add-to-list
;;   'emms-player-mpv-parameters "--no-audio-display") Alternatively
;;   you can also add "audio-display=no" to mpv's config file.
;;
;; This file is based on `emms-player-mplayer.el'.  It was originally hosted at
;; https://github.com/dochang/emms-player-mpv.

;;; Code:

(require 'emms-compat)
(require 'emms-player-simple)

(defcustom emms-player-mpv-input-file
  (expand-file-name (locate-user-emacs-file "emms-mpv-input-file"))
  "The file to send command to mpv."
  :type 'file
  :group 'emms)

(define-emms-simple-player mpv '(file url streamlist playlist)
  (concat "\\`\\(https?\\|mms\\)://\\|"
	  (apply #'emms-player-simple-regexp
		 emms-player-base-format-list))
  "mpv" "--no-audio-display" "--quiet" "--really-quiet")

(defadvice emms-player-mpv-start (around append-arguments activate)
  (unless (file-exists-p emms-player-mpv-input-file)
    (call-process "mkfifo" nil nil nil emms-player-mpv-input-file))
  (let* ((input-file (format "--input-file=%s" emms-player-mpv-input-file))
         (track-arg (let* ((track (ad-get-arg 0))
                       (track-type (emms-track-get track 'type))
                       (track-name (emms-track-name track)))
                  (if (memq track-type '(streamlist playlist))
                      (format "--playlist=%s" track-name)
                    track-name)))
         (process (apply 'start-process
                         emms-player-simple-process-name
                         nil
                         emms-player-mpv-command-name
                         (append emms-player-mpv-parameters
                                 (list input-file track-arg)))))
    (set-process-sentinel process 'emms-player-simple-sentinel))
  (emms-player-started emms-player-mpv))

(emms-player-set emms-player-mpv
                 'pause
                 'emms-player-mpv-pause)

(emms-player-set emms-player-mpv
                 'resume
                 'emms-player-mpv-resume)

(emms-player-set emms-player-mpv
                 'seek
                 'emms-player-mpv-seek)

(emms-player-set emms-player-mpv
                 'seek-to
                 'emms-player-mpv-seek-to)

(defun emms-player-mpv--format-command (fmt &rest args)
  "Generate shell command to control mpv."
  (let ((mpv-cmd (apply 'format fmt args)))
    (format "echo %s > %s"
            (shell-quote-argument mpv-cmd)
            (shell-quote-argument emms-player-mpv-input-file))))

(defun emms-player-mpv-pause ()
  "Depends on mpv's --input-file option."
  (let ((cmd (emms-player-mpv--format-command "set pause yes")))
    (call-process-shell-command cmd nil nil nil)))

(defun emms-player-mpv-resume ()
  "Depends on mpv's --input-file option."
  (let ((cmd (emms-player-mpv--format-command "set pause no")))
    (call-process-shell-command cmd nil nil nil)))

(defun emms-player-mpv-seek (sec)
  "Depends on mpv's --input-file option."
  (let ((cmd (emms-player-mpv--format-command "seek %d" sec)))
    (call-process-shell-command cmd nil nil nil)))

(defun emms-player-mpv-seek-to (sec)
  "Depends on mpv's --input-file option."
  (let ((cmd (emms-player-mpv--format-command "seek %d absolute" sec)))
    (call-process-shell-command cmd nil nil nil)))

(provide 'emms-player-mpv)
;;; emms-player-mpv.el ends here
