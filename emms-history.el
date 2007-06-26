;;; emms-history.el -- save playlist when exit emacs

;; Copyright (C) 2006, 2007 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@163.com>

;; This file is part of EMMS.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Save playlists when exit emacs.
;; Next time use M-x emms-history-load to load saved playlist

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'emms-history)

;;; Code:

(provide 'emms-history)
(require 'emms)
(eval-when-compile
  (require 'cl))

(defvar emms-history-file "~/.emacs.d/.emms-history"
  "File to save playlists")

(defun emms-history-save ()
  "Save all playlists that open in this emacs session when exit. Use
`emms-history-load' to load saved playlists."
  (interactive)
  (when (stringp emms-history-file)
    (let ((oldbuf emms-playlist-buffer)
          ;; print with no limit
          print-length print-level
          emms-playlist-buffer playlists)
      (save-excursion
        (dolist (buf (emms-playlist-buffer-list))
          (set-buffer buf)
          (when (> (buffer-size) 0) ; make sure there is track in the buffer
            (setq emms-playlist-buffer buf
                  playlists
                  (cons
                   (list (buffer-name)
                         (or
                          (and emms-playlist-selected-marker
                               (marker-position emms-playlist-selected-marker))
                          (point-min))
                         (save-excursion
                           (widen)
                           (nreverse
                            (emms-playlist-tracks-in-region (point-min)
                                                            (point-max)))))
                   playlists))))
        (with-temp-buffer
          (insert "(\n;; active playlist\n")
          (prin1 (buffer-name oldbuf) (current-buffer))
          (insert "\n;; playlists: ((BUFFER_NAME SELECT_POSITION TRACKS) ...)\n")
          (prin1 playlists (current-buffer))
          (insert "\n;; play method\n")
          (prin1 `((emms-repeat-track . ,emms-repeat-track)
                   (emms-repeat-playlist . ,emms-repeat-playlist))
                 (current-buffer))
          (insert "\n)")
          (write-file emms-history-file))))))

(add-hook 'kill-emacs-hook 'emms-history-save)

(defun emms-history-load ()
  (interactive)
  (when (and (stringp emms-history-file)
             (file-exists-p emms-history-file))
    (let (history buf)
      (with-temp-buffer
        (insert-file-contents emms-history-file)
        (setq history (read (current-buffer)))
        (dolist (playlist (cadr history))
          (with-current-buffer (emms-playlist-new (car playlist))
            (setq emms-playlist-buffer (current-buffer))
            (if (string= (car playlist) (car history))
                (setq buf (current-buffer)))
            (mapc 'emms-playlist-insert-track
                  (nth 2 playlist))
            (ignore-errors
              (emms-playlist-select (cadr playlist)))))
        (setq emms-playlist-buffer buf)
        (dolist (method (nth 2 history))
          (set (car method) (cdr method)))
        (ignore-errors
          (emms-start))))))

;;; emms-history.el ends here
