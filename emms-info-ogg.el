;;; emms-info-ogg.el --- ogg-comment.el info-interface for EMMS

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Ulrik Jensen <terryp@daimi.au.dk>
;; Keywords: ogg, emms, info

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301 USA

;;; Commentary:

;; This file provides an interface to retrieving comments from
;; ogg-files, using Lawrence Mitchells ogg-comment.el.

;; To activate, put something like this in your ~/.emacs:

;; (require 'emms-info-ogg)
;; (add-to-list 'emms-info-methods-list 'emms-info-ogg-comment)

;; You'll of course need to also have a player if you want to actually
;; play the files.

;;; Code:

(require 'emms-info)
(require 'ogg-comment)

(defvar emms-info-ogg-version "0.2 $Revision: 1.14 $"
  "EMMS info ogg version string.")
;; $Id: emms-info-ogg.el,v 1.14 2005/07/09 11:56:00 forcer Exp $

(defgroup emms-info-ogg-comments nil
  "An EMMS-info method for getting/setting ogg-comments, using
ogg-comments.el"
  :group 'emms-info-methods
  :prefix "emms-info-ogg-")

;; Doesn't implement set yet
(define-emms-info-method emms-info-ogg-comment
  :providep 'emms-info-ogg-comment-providep
  :get 'emms-info-ogg-comment-get)

(defun emms-info-ogg-comment-providep (track)
  "Return non-nil if this info-method provides info for the track."
  (if (and track (emms-track-name track)
	   (string-match "\\.ogg$" (emms-track-name track)))
      t
    nil))

(defun emms-info-ogg-get-comment (field info)
  (let ((comment (cadr (assoc field (cadr info)))))
    (if comment
	comment
      "")))

(defun emms-info-ogg-comment-get (track)
  "Retrieve an emms-info strucutre as an ogg-comment"
  (let ((info (oggc-read-header (emms-track-name track))))
    (make-emms-info :title (emms-info-ogg-get-comment "title" info)
		    :artist (emms-info-ogg-get-comment "artist" info)
		    :album (emms-info-ogg-get-comment "album" info)
		    :note (emms-info-ogg-get-comment "comment" info)
		    :year (emms-info-ogg-get-comment "date" info)
		    :genre (emms-info-ogg-get-comment "genre" info)
		    :file (emms-track-name track))))

(provide 'emms-info-ogg)
;;; emms-info-ogg.el ends here
