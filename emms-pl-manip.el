;;; emms-pl-manip.el --- Advanced playlist manipulation for EMMS

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

;; This file offers various advanced playlist-manipulations functions
;; for EMMS.

;; Basically just load up this file, and check out some of these
;; functions:

;; `emms-playlist-sort' - for sorting

;; `emms-playlist-sort-by-info-artist' - if you have `emms-info', this
;; well let you sort based on the artist.

;; Todo

;; `emms-playlist-add-file' - adds a single file to the playlist.
;; `emms-playlist-add-dir'  - adds a directory to the playlist

;;; Code:

(defvar emms-pl-manip-version "0.2 $Revision: 1.16 $"
  "EMMS pl manip version string.")
;; $Id: emms-pl-manip.el,v 1.16 2005/07/09 11:56:00 forcer Exp $


(defvar emms-playlist-get-file-name-function 'emms-track-name)

(defun vector-sort (vec pred &optional beg end)
  "Sort a vector VEC, using the predicate PRED, and return the new
vector. If BEG and END are specified, sort only this subrange.

PRED is called with 2 elements and should return true, if the first is
less than the other."
  (let ((begidx (or beg 0))
	(endidx (or end (1- (length vec)))))
    (if (= begidx endidx)
	;; return a vector of this element
	(make-vector 1 (aref vec begidx))
      ;; split the vector
      (let ((mididx (/ (- endidx begidx) 2)))
	;; sort the two sub-vectors
	(vector-sort vec pred begidx mididx)
	(vector-sort vec pred mididx endidx)
	;; merge the vectors - *this* can't be
	(let ((result (make-vector (length vec) nil))
	      (lowidx begidx)
	      (highidx mididx)
	      (residx 0))
	  ;; first merge while there are still elements left in both
	  (while (and (< lowidx mididx) (< highidx endidx))
	    (if (funcall pred (aref vec lowidx) (aref vec highidx))
		(progn
		  (aset result residx (aref vec lowidx))
		  (setq lowidx (1+ lowidx)))
	      (aset result residx (aref vec highidx))
	      (setq highidx (1+ highidx)))
	    (setq residx (1+ residx)))
	  ;; now only one of the ranges have elements left, merge those
	  (let ((idx -1)
		(idxterm endidx))
	    (if (< lowidx mididx)
		(setq idxterm mididx)
	      (setq idx mididx))
	    (while (< idx idxterm)
	      (aset residx result (aref vec idx))
	      (setq residx (1+ residx)
		    idx (1+ idx))))
	  ;; return
	  result)))))

(defun emms-pl-manip-sort (by pred)
  "Sorts the EMMS-playlist, by applying BY as a function to each
filename in the list, and then comparing the results with PRED."
  ;; convert to a list
  (let ((listplaylist (append emms-playlist nil)))
    ;; sort the list
    (emms-playlist-set-playlist
     (vconcat
      (sort listplaylist
	    (lambda (arg-one arg-two)
	      (funcall pred
		       (funcall by arg-one)
		       (funcall by arg-two))))))))

(defun emms-pl-manip-sort-by-filename ()
  (interactive)
  (emms-pl-manip-sort (lambda (x) x) 'string<))


(defun emms-pl-manip-sort-by-name ()
  (interactive)
  (emms-pl-manip-sort emms-playlist-get-file-name-function 'string<))

(defun emms-pl-manip-sort-by-info-artist ()
  "Sort the playlist, using "
  (interactive)
  (unless (featurep 'emms-info)
    (error "You have to load emms-info before using emms-pl-manip-sort-by-info-artist."))
  (emms-pl-manip-sort (lambda (entry)
                        (emms-info-artist (emms-info-get entry)))
                      'string<))

(defun emms-playlist-reshuffle ()
  "Reshuffle the playlist."
  (interactive)
  (emms-playlist-set-playlist (emms-playlist-shuffle)))

(provide 'emms-pl-manip)
;;; emms-pl-manip.el ends here
