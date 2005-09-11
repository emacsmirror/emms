;;; emms-info.el --- Info system for EMMS 

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

;; This file provides an interface for different methods of reading
;; info about the files that EMMS is playing, and displaying it.

;; To create a method for retrieving info about a file, you create an
;; object like this:

;; (define-emms-info-method emms-info-mp3info
;;   :providep 'emms-info-mp3info-providep
;;   :get 'emms-info-mp3info-get
;;   :set 'emms-info-mp3info-set)

;; Then you register it with emms-info, by adding it to
;; `emms-info-methods-list'.

;; (add-to-list 'emms-info-methods-list 'emms-info-ogg-comment)

;;; Code:
(require 'emms)

(eval-when-compile (require 'cl))

(defvar emms-info-version "0.2 $Revision: 1.16 $"
  "EMMS info version string.")
;; $Id: emms-info.el,v 1.16 2005/08/12 17:59:30 xwl Exp $

;; Customizations
(defgroup emms-info nil
  "*Info system for EMMS"
  :prefix "emms-info-"
  :group 'emms)

(defgroup emms-info-methods nil
  "*Methods to get info for EMMS-info"
  :group 'emms-info
  :prefix "emms-info-")

(defcustom emms-info-methods-list nil
  "List of info-methods. You need to set this."
  :group 'emms-info
  :type '(repeat function))

;; Caching
(defcustom emms-info-cache t
  "Boolean value, indicating whether or not to use a cache for
info-structures."
  :group 'emms-info
  :type 'boolean)

(defcustom emms-info-format '("%s - %s" (emms-info-artist emms-info-title))
  "Format the info string.
The first element of the list is a string with typical format
instructions, the cdr is a list of functions that get called with
the struct as argument."
  :type '(list string (repeat sexp))
  :group 'emms-pbi)

(defvar emms-info-cache-hash-table nil
  "A hash-table storing the cached info.

Uses tracks as keys and the emms-info structures as
values.")

 ;; The structure for info about files:
(defstruct emms-info title artist album note year genre file
  playing-time playing-time-min playing-time-sec)

;; Interface
(defmacro define-emms-info-method (name &rest alist)
  `(defun ,name (action)
     (plist-get ',(mapcar (lambda (keyword)
                            (if (not (keywordp keyword))
                                (cadr keyword)
                              (intern (substring (symbol-name keyword) 1))))
                          alist)
                action)))

;; Methods for the cache
(defun emms-info-get-cached (track)
  "Return cached info for the track TRACK, nil of no cache."
  (if emms-info-cache-hash-table
      (gethash track emms-info-cache-hash-table nil)
    nil))

(defun emms-info-set-cached (track info)
  "Set cached info for TRACK to INFO"
  (unless emms-info-cache-hash-table
    ;; No hash-table yet, create one
    (setq emms-info-cache-hash-table (make-hash-table :test 'equal)))
  (puthash track info emms-info-cache-hash-table))

;; Retrieve 
(defun emms-info-method-for (track)
  "Return an info-method suitable for TRACK."
  (unless emms-info-methods-list 
    (error "There are no info-methods defined at all. You should customize `emms-info-methods-alist'."))
  ;; find an info-method capable of providing info for this file
  (let ((curmethod emms-info-methods-list))
    (while (and curmethod
		(not (funcall (funcall (car curmethod) 'providep) track)))
      (setq curmethod (cdr curmethod)))
    (when curmethod
      (car curmethod))))

(defun emms-info-get (track &optional dont-use-cached)
  "Return an emms-info structure representing the track TRACK.
if DONT-USE-CACHED is non-nil, then always read from the file."
  ;; extend with methods for caching
  (let ((method (emms-info-method-for track))
	(cached (emms-info-get-cached track)))
    (if (or dont-use-cached (not emms-info-cache) (not cached))
	;; read from the file
	(when method
	  (let ((readinfo (funcall (funcall method 'get) track)))
	    (when emms-info-cache
	      ;; save the cache
	      (emms-info-set-cached track readinfo))
	    ;; return the read version
	    readinfo))
      ;; else just use the cached
      cached)))

(defun emms-info-set (track info)
  "Set the info of the file TRACK to the emms-info structure INFO."
  (let ((method (emms-info-method-for track)))
    (when method
      (when emms-info-cache
	(emms-info-set-cached track info))
      (funcall (funcall method 'set) track info))))

(defun emms-info-format-info (format struct)
  "Take FORMAT and format it with STRUCT.
For the formaz of FORMAT see `emms-info-format')"
  (apply 'format (car format) 
	 (mapcar (lambda (func) 
		   (funcall func struct)) 
		 (cadr format))))

;; Functions suitable as values of
;; `emms-playlist-get-file-name-function':

(defun emms-info-file-info-song-artist (track)
  "Returns a description of TRACK, build from it's comments.

If `emms-info-methods-list' indicates how to retrieve special info
about it, use this. Otherwise returns the name alone."
  (if (not (and track (emms-track-name track)))
      "Invalid track!"
    (if (emms-info-method-for track)
	;; read the info
	(let ((info (emms-info-get track)))
	  (if (and info (not (string= (emms-info-artist info) "")) (not (string= (emms-info-title info) "")))
	      (concat (emms-info-artist info)  " - " (emms-info-title info))
	    (file-name-sans-extension (file-name-nondirectory (emms-track-name track)))))
      ;; we can't read info for this file, default to the name
      (file-name-sans-extension (file-name-nondirectory (emms-track-name track))))))
    
(provide 'emms-info)
;;; emms-info.el ends here
