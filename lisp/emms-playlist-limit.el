;;; emms-playlist-limit.el --- Limit playlist by various info -*- lexical-binding: t -*-

;; Copyright (C) 2007, 2008, 2009 William Xu, 2018 Free Software Foundation, Inc.
;; 

;; Author: William Xu <william.xwl@gmail.com>, Fran Burstall <fran.burstall@gmail.com>
;; Keywords: emms, limit

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;  This code allows you to "limit" the current playlist by creating a
;;  derived playlist containing only those tracks with a type
;;  (info-artist and the like) matching a regexp.  The derived
;;  playlist then becomes current.

;;  Usage:
;;  ------------------------------------------------------------------

;;  This code is activated by (emms-all).  Otherwise simply do:

;;  (require 'emms-playlist-limit)

;;  Key bindings
;;  ------------------------------------------------------------------

;;  / /		emms-playlist-limit-to-all
;;  / a		emms-playlist-limit-to-info-artist
;;  / b		emms-playlist-limit-to-info-album
;;  / c		emms-playlist-limit-to-info-composer
;;  / g		emms-playlist-limit-to-info-genre
;;  / n		emms-playlist-limit-to-name
;;  / p		emms-playlist-limit-to-info-performer
;;  / t		emms-playlist-limit-to-info-title
;;  / y		emms-playlist-limit-to-info-year

;;; Code:

(require 'seq)
(require 'emms-playlist-mode)

;; User Interfaces

(defgroup emms-playlist-limit nil
  "Playlist limit module for EMMS."
  :group 'emms)

(defcustom emms-playlist-limit-hook nil
  "Hooks to run after each limit operations."
  :type 'symbol
  :group 'emms-playlist-limit)

(defmacro define-emms-playlist-limit (attribute)
  "Macro for defining emms playlist limit to ATTRIBUTE function."
  `(defun ,(intern (format "emms-playlist-limit-to-%s" attribute)) (regexp)
     ,(format "Limit playlist to tracks that have %s matching REGEXP." attribute)
     (interactive
      (list
       (let* ((curr
               (or (emms-track-get
                    (or  (emms-playlist-track-at)
			 (emms-playlist-track-at (max 1 (1- (point))))) ; at eol
		    (quote ,attribute))
		   (emms-track-get
                    (emms-playlist-selected-track) (quote ,attribute))))
              (attr-name ,(emms-replace-regexp-in-string
                           "info-" "" (symbol-name attribute)))
              (fmt (if curr
                       (format "Limit to %s (regexp = %s): " attr-name curr)
                     (format "Limit to %s (regexp): " attr-name))))
         (read-string fmt))))
     (when (string= regexp "")
       (setq regexp (or (emms-track-get
			 (or (emms-playlist-track-at)
			     (emms-playlist-track-at (max 1 (1- (point))))) ; at eol
			 (quote ,attribute))
			(emms-track-get
			 (emms-playlist-selected-track) (quote ,attribute)))))
     (if regexp
	 (emms-playlist-limit-do (quote ,attribute) regexp)
       (message "Limit cancelled: no regexp."))))



(define-emms-playlist-limit info-artist)
(define-emms-playlist-limit info-composer)
(define-emms-playlist-limit info-performer)
(define-emms-playlist-limit info-title)
(define-emms-playlist-limit info-album)
(define-emms-playlist-limit info-year)
(define-emms-playlist-limit info-genre)
(define-emms-playlist-limit name)

(defvar-local emms-playlist-limit-original-playlist nil
  "Playlist buffer we are filtering.")

(defun emms-playlist-limit-to-all ()
  "Show all tracks again."
  (interactive)
  (when (and  emms-playlist-limit-original-playlist
	      (eq (current-buffer) emms-playlist-buffer))
    (let ((old-buf (current-buffer)))
      (switch-to-buffer emms-playlist-limit-original-playlist)
      (emms-playlist-set-playlist-buffer)
      (kill-buffer old-buf))))

(define-key emms-playlist-mode-map (kbd "/ n") 'emms-playlist-limit-to-name)
(define-key emms-playlist-mode-map (kbd "/ a") 'emms-playlist-limit-to-info-artist)
(define-key emms-playlist-mode-map (kbd "/ c") 'emms-playlist-limit-to-info-composer)
(define-key emms-playlist-mode-map (kbd "/ p") 'emms-playlist-limit-to-info-performer)
(define-key emms-playlist-mode-map (kbd "/ t") 'emms-playlist-limit-to-info-title)
(define-key emms-playlist-mode-map (kbd "/ b") 'emms-playlist-limit-to-info-album)
(define-key emms-playlist-mode-map (kbd "/ y") 'emms-playlist-limit-to-info-year)
(define-key emms-playlist-mode-map (kbd "/ g") 'emms-playlist-limit-to-info-genre)
(define-key emms-playlist-mode-map (kbd "/ /") 'emms-playlist-limit-to-all)


;;; Low Level Functions

(defun emms-playlist-limit-track-get (track type)
  "Return the value of TYPE from TRACK.

This is a wrapper around `emms-track-get' that also tries
'info-originalyear, 'info-originaldate and 'info-date to get a
usable date when TYPE is 'info-year."
  (cond ((eq type 'info-year)
	 (let ((date (or (emms-track-get track 'info-originaldate)
			 (emms-track-get track 'info-originalyear)
			 (emms-track-get track 'info-date)
			 (emms-track-get track 'info-year))))
	   (or  (emms-format-date-to-year date)
		"<unknown year>")))
	(t (emms-track-get track type))))

(defun emms-playlist-limit--limit-playlist (playlist type regexp)
  "Return new playlist containing the tracks of PLAYLIST with TYPE matching REGEXP."
  (let* ((bufname (concat (buffer-name playlist)
			  (format "/%s=%s"
				  (emms-replace-regexp-in-string "info-" "" (symbol-name type)) regexp)))
	 (tracks (nreverse (with-current-buffer playlist
			     (save-excursion (emms-playlist-tracks-in-region (point-min) (point-max))))))
	 (filtered-tracks (seq-filter
			   (lambda (track) (let ((field (emms-playlist-limit-track-get track type)))
					     (and field (string-match regexp field))))
			   tracks))
	 (new-playlist (emms-playlist-new bufname)))
    (with-current-buffer new-playlist
      (mapc #'emms-playlist-insert-track filtered-tracks))
    new-playlist))


(defun emms-playlist-limit-do (type regexp)
  "Limit by TYPE with REGEXP.
e.g.,
    (emms-playlist-limit-do 'info-artist \"Jane Zhang\")

See `emms-info-mp3find-arguments' for possible options for TYPE."
  (with-current-emms-playlist
    (emms-playlist-ensure-playlist-buffer)
    (let ((curr (emms-playlist-current-selected-track))
	  (old-buf (current-buffer))
	  (buf (emms-playlist-limit--limit-playlist (current-buffer) type regexp)))
      (with-current-buffer buf
	(if (= (point-min) (point-max))
	    (progn
	      (message "No matching tracks found!")
	      (kill-buffer))
	  (let ((pos (when curr (text-property-any (point-min) (point-max)
						   'emms-track curr))))
	    (emms-playlist-select (or pos 1)))
	  (emms-playlist-mode-center-current)
	  (setq emms-playlist-limit-original-playlist old-buf)
	  (emms-playlist-set-playlist-buffer)
	  (run-hooks 'emms-playlist-limit-hook)
	  (switch-to-buffer buf))))))

(provide 'emms-playlist-limit)

;;; emms-playlist-limit.el ends here
