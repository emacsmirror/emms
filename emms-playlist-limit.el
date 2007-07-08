;;; emms-playlist-limit.el --- limit playlist by various info

;; Copyright (C) 2007 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Keywords: emms, limit

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

;;; Code:

(require 'emms-playlist-mode)

;;; User Interfacs

;;;###autoload
(defun emms-playlist-limit (arg)
  "Turn on emms limit if ARG is positive, off otherwise."
  (interactive "p")
  (if (and arg (> arg 0))
      (add-hook 'emms-playlist-source-inserted-hook
                'emms-playlist-limit-update-tracks)
    (remove-hook 'emms-playlist-source-inserted-hook
                 'emms-playlist-limit-update-tracks)))

(define-key emms-playlist-mode-map (kbd "/ n") 'emms-playlist-limit-to-name)
(define-key emms-playlist-mode-map (kbd "/ a") 'emms-playlist-limit-to-info-artist)
(define-key emms-playlist-mode-map (kbd "/ t") 'emms-playlist-limit-to-info-title)
(define-key emms-playlist-mode-map (kbd "/ b") 'emms-playlist-limit-to-info-album)
(define-key emms-playlist-mode-map (kbd "/ y") 'emms-playlist-limit-to-info-year)
(define-key emms-playlist-mode-map (kbd "/ g") 'emms-playlist-limit-to-info-genre)
(define-key emms-playlist-mode-map (kbd "/ /") 'emms-playlist-limit-to-all)

(defun emms-playlist-limit-to-info-artist (regexp)
  "Limit to playlists that have artist that matches REGEXP."
  (interactive
   (list
    (read-string
     (format "Limit to artist (regexp = %s): "
             (emms-track-get (emms-playlist-track-at) 'info-artist)))))
  (when (string= regexp "")
    (setq regexp (emms-track-get (emms-playlist-track-at) 'info-artist)))
  (emms-playlist-limit-do 'info-artist regexp))

(defun emms-playlist-limit-to-info-album (regexp)
  "Limit to playlists that have album that matches REGEXP."
  (interactive
   (list
    (read-string
     (format "Limit to album (regexp = %s): "
             (emms-track-get (emms-playlist-track-at) 'info-album)))))
  (when (string= regexp "")
    (setq regexp (emms-track-get (emms-playlist-track-at) 'info-album)))
  (emms-playlist-limit-do 'info-album regexp))

(defun emms-playlist-limit-to-info-title (regexp)
  "Limit to playlists that have title that matches REGEXP."
  (interactive
   (list
    (read-string
     (format "Limit to title (regexp = %s): "
             (emms-track-get (emms-playlist-track-at) 'info-title)))))
  (when (string= regexp "")
    (setq regexp (emms-track-get (emms-playlist-track-at) 'info-title)))
  (emms-playlist-limit-do 'info-title regexp))

(defun emms-playlist-limit-to-info-year (regexp)
  "Limit to playlists that have year that matches REGEXP."
  (interactive
   (list
    (read-string
     (format "Limit to year (regexp = %s): "
             (emms-track-get (emms-playlist-track-at) 'info-year)))))
  (when (string= regexp "")
    (setq regexp (emms-track-get (emms-playlist-track-at) 'info-year)))
  (emms-playlist-limit-do 'info-year regexp))

(defun emms-playlist-limit-to-info-genre (regexp)
  "Limit to playlists that have genre that matches REGEXP."
  (interactive
   (list
    (read-string
     (format "Limit to genre (regexp = %s): "
             (emms-track-get (emms-playlist-track-at) 'info-genre)))))
  (when (string= regexp "")
    (setq regexp (emms-track-get (emms-playlist-track-at) 'info-genre)))
  (emms-playlist-limit-do 'info-genre regexp))

(defun emms-playlist-limit-to-name (regexp)
  "Limit to playlists that have name that matches REGEXP."
  (interactive
   (list
    (read-string
     (format "Limit to genre (regexp = %s): "
             (emms-track-get (emms-playlist-track-at) 'name)))))
  (when (string= regexp "")
    (setq regexp (emms-track-get (emms-playlist-track-at) 'name)))
  (emms-playlist-limit-do 'name regexp))

(defun emms-playlist-limit-to-all ()
  "Show all tracks again."
  (interactive)
  (emms-playlist-limit-do nil nil))


;;; Low Level Functions

(defvar emms-playlist-limit-tracks nil
  "All tracks in playlist buffer.")

(defun emms-playlist-limit-update-tracks ()
  "Update `emms-playlist-limit-tracks'."
  (setq emms-playlist-limit-tracks
        (append emms-playlist-limit-tracks
                (emms-playlist-tracks-in-region
                 (point-min) (point-max)))))

(defun emms-playlist-limit-do (name value)
  "Limit by NAME with VALUE.
e.g.,
    (emms-playlist-limit-do 'info-artist \"Jane Zhang\")

When NAME is nil, show all tracks again.

See `emms-info-mp3find-arguments' for possible options."
  (with-current-emms-playlist
    (save-excursion
      (emms-playlist-ensure-playlist-buffer)
      (let ((curr (emms-playlist-current-selected-track))
            (tracks
             (emms-playlist-tracks-in-region (point-min) (point-max))))
        (erase-buffer)
        (run-hooks 'emms-playlist-cleared-hook)
        (if name
            (mapc (lambda (track)
                    (let ((track-value (emms-track-get track name)))
                      (when (and track-value (string-match value track-value))
                        (emms-playlist-insert-track track))))
                  tracks)
          (mapc (lambda (track)
                  (emms-playlist-insert-track track))
                emms-playlist-limit-tracks))
        (let ((pos (text-property-any (point-min) (point-max)
                                      'emms-track curr)))
          (if pos
              (emms-playlist-select pos)
            (emms-playlist-first)))))))


(provide 'emms-playlist-limit)

;;; emms-playlist-limit.el ends here
