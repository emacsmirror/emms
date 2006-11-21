;;; emms-history.el.gz ---

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Time-stamp: <Ye Wenbin 2006-11-21 16:57:59>
;; Version: $Id: emms-history.el,v 0.0 <2006-11-21 16:30:22> ywb Exp $
;; Keywords: 
;; X-URL: not distributed yet

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
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Save playlists when exit emacs.
;; Next time use M-x emms-playlist-load-saved-list to load saved
;; playlist

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'emms-history)

;;; Code:

(provide 'emms-history)
(require 'emms)
(eval-when-compile
  (require 'cl))

(defvar emms-history-saved-list-file "~/.emacs.d/.emms-history")

(defun emms-history-save-on-exit ()
  (when (stringp emms-history-saved-list-file)
    (let ((oldbuf emms-playlist-buffer)
          emms-playlist-buffer playlists)
      (save-excursion
        (dolist (buf (remove-if-not 'buffer-live-p
                                    (emms-playlist-buffer-list)))
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
          (write-file emms-history-saved-list-file))))))

(add-hook 'kill-emacs-hook 'emms-history-save-on-exit)

(defun emms-history-load-saved-list ()
  (interactive)
  (when (and (stringp emms-history-saved-list-file)
             (file-exists-p emms-history-saved-list-file))
    (let (history buf)
      (with-temp-buffer
        (insert-file-contents emms-history-saved-list-file)
        (setq history (read (current-buffer)))
        (dolist (playlist (cadr history))
          (with-current-buffer (emms-playlist-new (car playlist))
            (setq emms-playlist-buffer (current-buffer))
            (if (string= (car playlist) (car history))
                (setq buf (current-buffer)))
            (mapc 'emms-playlist-insert-track
                  (nth 2 playlist))
            (condition-case nil
                (emms-playlist-select (cadr playlist))
              (error nil))))
        (setq emms-playlist-buffer buf)
        (dolist (method (nth 2 history))
          (set (car method) (cdr method)))
        (emms-start)))))

;;; emms-history.el ends here
