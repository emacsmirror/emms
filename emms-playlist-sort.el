;;; emms-playlist-sort.el --- sort emms playlist

;; Copyright (C) 2005 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; $Id: emms-playlist-sort.el,v 0.1 2005/10/06 00:29:36 xwl Exp $

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

;; Provide various playlist sort functions for emms, such as
;; `emms-playlist-sort-by-info-title'.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;            (require 'emms-playlist-sort)

;;; Code:

;;; sort macro

(defmacro define-emms-playlist-sort (attribute)
  "Macro for defining emms playlist sort functions."
  `(defun ,(intern (format "emms-playlist-sort-by-%s" attribute)) ()
     ,(format "Sort emms playlist by %s." attribute)
     (interactive)
     (emms-playlist-sort
      (lambda (a b)
	(string< (emms-track-get a (quote ,attribute))
		 (emms-track-get b (quote ,attribute)))))))

(define-emms-playlist-sort name)
(define-emms-playlist-sort info-artist)
(define-emms-playlist-sort info-title)
(define-emms-playlist-sort info-album)
(define-emms-playlist-sort info-year)
(define-emms-playlist-sort info-note)

(define-key emms-playlist-mode-map (kbd "S n") 'emms-playlist-sort-by-name)
(define-key emms-playlist-mode-map (kbd "S a") 'emms-playlist-sort-by-info-artist)
(define-key emms-playlist-mode-map (kbd "S t") 'emms-playlist-sort-by-info-title)
(define-key emms-playlist-mode-map (kbd "S b") 'emms-playlist-sort-by-album)
(define-key emms-playlist-mode-map (kbd "S y") 'emms-playlist-sort-by-info-year)
(define-key emms-playlist-mode-map (kbd "S o") 'emms-playlist-sort-by-info-note)

(defun emms-playlist-sort (predicate)
  "Sort the whole playlist buffer by PREDICATE."
  (with-current-emms-playlist
    (save-excursion
      (emms-playlist-ensure-playlist-buffer)
      (widen)
      (let ((current (emms-playlist-selected-track))
	    (tracks (emms-playlist-tracks-in-region (point-min)
						    (point-max))))
	(delete-region (point-min)
		       (point-max))
	(run-hooks 'emms-playlist-cleared-hook)
	(mapc 'emms-playlist-insert-track
	      (sort tracks predicate))
	(let ((pos (text-property-any (point-min)
				      (point-max)
				      'emms-track current)))
	  (if pos
	      (emms-playlist-select pos)
	    (emms-playlist-first)))))))


(provide 'emms-playlist-sort)

;;; emms-playlist-sort.el ends here
