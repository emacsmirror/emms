;;; emms-info-mp3info.el --- Info-method for EMMS using mp3info

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Ulrik Jensen <terryp@daimi.au.dk>
;; Keywords:

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

;; This code has been adapted from code found in mp3player.el, written
;; by Jean-Philippe Theberge (jphiltheberge@videotron.ca), Mario
;; Domgoergen (kanaldrache@gmx.de) and Jorgen Schäfer
;; <forcer@forcix.cx>

;; To activate this method for getting info, use something like:

;; (require 'emms-info-mp3info)
;; (add-to-list 'emms-info-methods-list 'emms-info-mp3info)

;;; Code:
(eval-when-compile (require 'cl))
(require 'emms-info)

(defvar emms-info-mp3info-version "0.2 $Revision: 1.10 $"
  "EMMS info mp3info version string.")
;; $Id: emms-info-mp3info.el,v 1.10 2005/08/12 18:01:16 xwl Exp $

(defgroup emms-info-mp3info nil
  "An EMMS-info method for getting/setting ID3v1 tags, using a
external mp3info program"
  :group 'emms-info-methods)

(defcustom emms-info-mp3info-program-name "mp3info"
  "*The name/path of the mp3info tag program."
  :type 'string
  :group 'emms-info-mp3info)

(defcustom emms-info-mp3info-field-names
  '(title "title"
	  artist "artist"
	  album "album"
	  note "description"
	  year "date"
	  genre "genre"
	  file (emms-track-name track)
	  playing-time "playing-time"
	  playing-time-min "playing-time-min"
	  playing-time-sec "playing-time-sec")
  "Plist of field names."
  :type 'plist
  :group 'emms-info-mp3info)

(defcustom emms-info-mp3info-set-parameter
  '(title "-t"
	  artist "-a"
	  album "-l"
	  note "-c"
	  year "-y"
	  genre "-g")
  "Parameter list"
  :type 'plist
  :group 'emms-info-mp3info)

(defcustom emms-info-field-delemiter "="
  "Delemiter between field and content."
  :type 'string
  :group 'emms-info-mp3info)

(defcustom emms-info-field-seperator "\n"
  "Seperator between different fields."
  :type 'string
  :group 'emms-info-mp3info)


(define-emms-info-method emms-info-mp3info
  :providep 'emms-info-mp3info-providep
  :get 'emms-info-mp3info-get
  :set 'emms-info-mp3info-set)

(defun emms-info-mp3info-providep (track)
  "Return non-nil if this info-method provides info for the track."
  (if (and track (emms-track-name track)
	   (string-match "\\.mp3$" (emms-track-name track)))
      t
    nil))

(defun emms-info-mp3info-set (track info)
  "Set the id3v1 tag of file TRACK to id3info INFO, using the
mp3info-program"
  ;; 0 = discard & don't wait for termination.
  (call-process emms-info-mp3info-program-name nil 0 nil
		"-a" (emms-info-artist info)
		"-t" (emms-info-title info)
 		"-l" (emms-info-album info)
		"-c" (emms-info-note info)
		"-y" (emms-info-year info)
		(emms-track-name track)))

(defun emms-info-retrieve (field)
  "Search for field and get content.
If there is no field return an empty string"
  (goto-char (point-min))
  (save-match-data
    (or (progn
	  (re-search-forward
	   (concat field emms-info-field-delemiter "\\(.*\\)"
		   emms-info-field-seperator)
	   nil t)
	  (match-string 1))
	"")))


(defun emms-info-mp3info-get (track)
  "Get the id3v1 tag of file TRACK, using the mp3info-program and
return an emms-info structure representing it."
  (with-temp-buffer
    (call-process emms-info-mp3info-program-name nil t nil
		  "-p"
		  (concat "title=%t\n" "artist=%a\n" "album=%l\n"
			  "date=%y\n" "genre=%G\n" "description=%c\n"
			  "playing-time=%S\n" "playing-time-min=%m\n"
			  "playing-time-sec=%s\n")
		  (emms-track-name track))
    (flet ((retrieve-info (field)
			  (goto-char (point-min))
 			  (save-match-data
 			    (when (re-search-forward (concat field "=\\(.*\\)")
						     nil t)
			      (or (match-string 1) "")))))
      (make-emms-info
       :title (retrieve-info "title")
       :artist (retrieve-info "artist")
       :album (retrieve-info "album")
       :note (retrieve-info "description")
       :year (retrieve-info "date")
       :genre (retrieve-info "genre")
       :file (emms-track-name track)
       :playing-time (retrieve-info "playing-time")
       :playing-time-min (retrieve-info "playing-time-min")
       :playing-time-sec (retrieve-info "playing-time-sec")))))

(provide 'emms-info-mp3info)
;;; emms-info-mp3info.el ends here
