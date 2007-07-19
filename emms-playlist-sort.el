;;; emms-playlist-sort.el --- sort emms playlist

;; Copyright (C) 2005, 2006, 2007 Free Software Foundation, Inc.

;; Author: William Xu <william.xwl@gmail.com>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;;; Code:

(require 'emms-score)

(eval-when-compile (require 'cl))

;;; User Customizations

(defgroup emms-playlist-sort nil
  "Sorting Emacs Multimedia System playlists."
  :prefix "emms-playlist-sort-"
  :group 'emms)

(defcustom emms-playlist-sort-list '(info-artist info-album)
  "Sorting list used by `emms-playlist-sort-by-list'.
Currently it understands the following fields: name info-artist
info-title info-album info-genre info-playing-time
info-tracknumber."
  :type 'symbol
  :group 'emms-playlist-sort)


;;; User Interfaces

(defmacro define-emms-playlist-sort (attribute)
  "Macro for defining emms playlist sort functions."
  `(defun ,(intern (format "emms-playlist-sort-by-%s" attribute)) ()
     ,(format "Sort emms playlist by %s, increasingly." attribute)
     (interactive)
     (emms-playlist-sort
      (lambda (a b)
	(emms-string< (emms-track-get a (quote ,attribute))
                      (emms-track-get b (quote ,attribute)))))))

(define-emms-playlist-sort name)
(define-emms-playlist-sort info-artist)
(define-emms-playlist-sort info-title)
(define-emms-playlist-sort info-album)
(define-emms-playlist-sort info-year)
(define-emms-playlist-sort info-note)

(defun emms-playlist-sort-by-score ()
  "Sort emms playlist by score, decreasingly."
  (interactive)
  (emms-playlist-sort
   (lambda (a b)
     (> (emms-score-get-score (emms-track-get a 'name))
	(emms-score-get-score (emms-track-get b 'name))))))

(defun emms-playlist-sort-by-natural-order ()
  "Sort emms playlist by natural order.
See `emms-sort-natural-order-less-p'."
  (interactive)
  (emms-playlist-sort 'emms-sort-natural-order-less-p))

(defun emms-playlist-sort-by-list ()
  "Sort emms playlist by `emms-playlist-sort-list'.
The sort will be carried out until comparsion succeeds,
increasingly."
  (interactive)
  (emms-playlist-sort 'emms-playlist-sort-by-list-p))

(eval-after-load 'emms-playlist-mode
  '(progn
     ;; Steal this key from emms-playlist-mode..
     (define-key emms-playlist-mode-map (kbd "s") nil)
     (define-key emms-playlist-mode-map (kbd "s n") 'emms-playlist-sort-by-name)
     (define-key emms-playlist-mode-map (kbd "s a") 'emms-playlist-sort-by-info-artist)
     (define-key emms-playlist-mode-map (kbd "s t") 'emms-playlist-sort-by-info-title)
     (define-key emms-playlist-mode-map (kbd "s b") 'emms-playlist-sort-by-info-album)
     (define-key emms-playlist-mode-map (kbd "s y") 'emms-playlist-sort-by-info-year)
     (define-key emms-playlist-mode-map (kbd "s o") 'emms-playlist-sort-by-info-note)
     (define-key emms-playlist-mode-map (kbd "s N") 'emms-playlist-sort-by-natural-order)
     (define-key emms-playlist-mode-map (kbd "s l") 'emms-playlist-sort-by-list)
     (define-key emms-playlist-mode-map (kbd "s s") 'emms-playlist-sort-by-score)))


;;; Low Level Functions

(defun emms-playlist-sort (predicate &optional start end)
  "Sort the playlist buffer by PREDICATE.
If START and END are not provided, the whole buffer will be sorted."
  (unless start (setq start (point-min)))
  (unless end (setq end (point-max)))
  (with-current-emms-playlist
    (save-excursion
      (emms-playlist-ensure-playlist-buffer)
      (widen)
      (let ((current (emms-playlist-selected-track))
            (tracks
             (emms-playlist-tracks-in-region start end)))
        (delete-region start end)
        (run-hooks 'emms-playlist-cleared-hook)
        (mapc 'emms-playlist-insert-track
              (sort tracks predicate))
        (let ((pos (text-property-any start end
                                      'emms-track current)))
          (if pos
              (emms-playlist-select pos)
            (emms-playlist-first)))))))

(defun emms-string> (a b)
  (not (or (emms-string< a b)
	   (string= a b))))

(defun emms-sort-natural-order-less-p (a b)
  "Sort two tracks by natural order.
This is the order in which albums where intended to be played.
ie. by album name and then by track number."
  (or (emms-string> (emms-track-get a 'info-album)
		    (emms-track-get b 'info-album))
      (and (string= (emms-track-get a 'info-album)
		    (emms-track-get b 'info-album))
	   (< (string-to-number (or (emms-track-get a 'info-tracknumber)
				    "0"))
	      (string-to-number (or (emms-track-get b 'info-tracknumber)
				    "0"))))))

(defun emms-playlist-sort-by-list-p (a b)
  (catch 'return
    (dolist (info emms-playlist-sort-list)
      (case info
        ((name info-artist info-title info-album info-genre)
         (when (emms-string< (emms-track-get a info)
                        (emms-track-get b info))
           (throw 'return t)))
        ((info-playing-time)
         (when (< (emms-track-get a info)
                  (emms-track-get b info))
           (throw 'return t)))
        ((info-tracknumber)
         (when (< (string-to-number (or (emms-track-get a info) "0"))
                  (string-to-number (or (emms-track-get b info) "0")))
           (throw 'return t)))))))

(defun emms-string< (s1 s2)
  "Same as `string<' except this is case insensitive."
  (string< (and s1 (downcase s1)) (and s2 (downcase s2))))


(provide 'emms-playlist-sort)

;;; emms-playlist-sort.el ends here
