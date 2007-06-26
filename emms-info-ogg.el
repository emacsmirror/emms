;;; emms-info-ogg.el --- ogg-comment.el info-interface for EMMS

;; Copyright (C) 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.

;; Authors: Yoni Rabkin <yonirabkin@member.fsf.org>,
;;          Ulrik Jensen <terryp@daimi.au.dk>

;; Keywords: ogg, emms, info

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file provides an interface to retrieving comments from
;; ogg-files, using Lawrence Mitchells ogg-comment.el.

;; To activate, put something like this in your ~/.emacs:

;; (require 'emms-info-ogg)
;; (add-to-list 'emms-info-methods-list 'emms-info-ogg)

;; You'll of course need to also have a player if you want to actually
;; play the files.

;;; Code:

(require 'emms-info)
(require 'ogg-comment)

(defvar emms-info-ogg-version "0.2 $Revision: 1.14 $"
  "EMMS info ogg version string.")
;; $Id: emms-info-ogg.el,v 1.14 2005/07/09 11:56:00 forcer Exp $

(defgroup emms-info-ogg nil
  "An EMMS-info method for getting/setting ogg-comments, using
ogg-comments.el"
  :group 'emms-info-methods
  :prefix "emms-info-ogg-")

(defun emms-info-ogg-get-comment (field info)
  (let ((comment (cadr (assoc field (cadr info)))))
    (if comment
        comment
      "")))

(defun emms-info-ogg (track)
  "Retrieve an emms-info structure as an ogg-comment"
  (when (and (eq 'file (emms-track-type track))
             (string-match "\\.[Oo][Gg][Gg]\\'" (emms-track-name track)))
    (let ((info (oggc-read-header (emms-track-name track)))
	  (file (emms-track-get track 'name))
	  ptime-total ptime-min ptime-sec)
      (with-temp-buffer
	(call-process "ogginfo" nil t nil file)
	(goto-char (point-min))
	(re-search-forward "Playback length: \\([0-9]*\\)m:\\([0-9]*\\)")
	(let ((minutes (string-to-number (match-string 1)))
	      (seconds (string-to-number (match-string 2))))
	  (setq ptime-total (+ (* minutes 60) seconds)
		ptime-min minutes
		ptime-sec seconds)))

      (emms-track-set track 'info-title (emms-info-ogg-get-comment "title" info))
      (emms-track-set track 'info-artist (emms-info-ogg-get-comment "artist" info))
      (emms-track-set track 'info-album (emms-info-ogg-get-comment "album" info))
      (emms-track-set track 'info-note (emms-info-ogg-get-comment "comment" info))
      (emms-track-set track 'info-year (emms-info-ogg-get-comment "date" info))
      (emms-track-set track 'info-genre (emms-info-ogg-get-comment "genre" info))
      (emms-track-set track 'info-playing-time ptime-total)
      (emms-track-set track 'info-playing-time-min ptime-min)
      (emms-track-set track 'info-playing-time-sec ptime-sec)
      (emms-track-set track 'info-file (emms-track-name track)))))

(provide 'emms-info-ogg)
;;; emms-info-ogg.el ends here
