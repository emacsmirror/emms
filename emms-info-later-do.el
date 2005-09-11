;;; emms-info-later-do.el --- Using later-do.el to load info

;; Copyright (C) 2004  Free Software Foundation, Inc.

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

;; This probably *won't* work if you have the info-cache disabled!

;; Also, you need to have Jorgen Schäfers later-do.el in your
;; loadpath. That file can be found here:

;; <http://www.emacswiki.org/cgi-bin/wiki.pl/LaterDo>

;; Possible problems: if the init-playlist hook gets called *after*
;; the playlists hook for the same thing gets called, we're doomed.

;; To use this, add the following to your EMMS-configuration

;; (require 'emms-info-later-do)
;; (emms-info-later-do-mode 1)

;; If you want to use the playlist-buffer interface with this, you
;; need to add the following as well:

;; (add-hook 'emms-info-later-do-read-info-functions
;;           'emms-pbi-entry-update-track)

;; Which will make the PBI update it's entries as their info is being
;; loaded.

;; To change the speed this loading happens with, see
;; `later-do-interval'.

;;; Code:
(eval-when-compile (require 'cl))
(require 'emms-info)
(condition-case nil
    (require 'later-do)
  (error nil))

(defvar emms-info-later-do-track-queue nil
  "A list of tracks whose info needs to be updated.")

(defun emms-info-later-do-mode (arg)
  "Activate later-do-mode for loading info."
  (interactive "p")
  (if (and (numberp arg) (> arg 0))
      ;; this hook needs to be run before any hooks that uses the
      ;; info-cache to do something productive.
      (add-hook 'emms-playlist-changed-hook
		'emms-info-later-do-new-playlist)
    (remove-hook 'emms-playlist-changed-hook
		 'emms-info-later-do-new-playlist)))

(defvar emms-info-later-do-read-info-functions nil
  "Functions to run when the info for a track has been read.

The functions will be called with one argument, TRACK, the track just
updated.")

(defun emms-info-later-do-clean-queue ()
  "Remove all emms-info jobs from the later-do queue."
  (let ((funcs later-do-list)
	(everything-up-to nil))
    (while funcs
      (let ((queued-job (car funcs)))
	;; if this job is an emms-info-later-do-process
	(if (equal (car queued-job) 'emms-info-later-do-process)
	    (progn
	      ;; Remove this item from the list, and continue in a clean
	      ;; way!	      
	      (let ((everything-after (cdr funcs)))
		(setq later-do-list (append everything-up-to
					    everything-after))
		(setq funcs everything-after)))
	  ;; Add this item to everything-up-to
	  (setq everything-up-to (append everything-up-to
					 (list queued-job)))
	  (setq funcs (cdr funcs)))))))

(defun emms-info-later-do-process ()
  "Load the info for one single item.

Also set up loading the next item, if there are anymore."
  (when emms-info-later-do-track-queue
    (let ((track (car emms-info-later-do-track-queue)))
      ;; remove track from queue
      (setq emms-info-later-do-track-queue
	    (cdr emms-info-later-do-track-queue))
      ;; load info for track -- this is kinda redundant, considering
      ;; that info-get will update the cache when forced to go around
      ;; it, but just in case.
      (emms-info-set-cached track (emms-info-get track t))
      ;; notify the world of our heroic conquest!
      (run-hook-with-args 'emms-info-later-do-read-info-functions
			  track)
      (if emms-info-later-do-track-queue
	  ;; we still have a queue, run us again!
	  (later-do 'emms-info-later-do-process)
	(message "Lazy loading of info complete!")))))

(defun emms-info-later-do-new-playlist ()
  "Function that gets ready to lazy-load a new playlist.

Should be called from `emms-playlist-changed-hook'."
  ;; Cleaning up the potential old playlist/process: 
  ;; Clean up any currently running emms-info-later-do-process lists
  (emms-info-later-do-clean-queue)
  ;; Remove the cache for any tracks currently containing placeholder
  ;; info-objects.
  (while emms-info-later-do-track-queue
    ;; This track still holds a placeholder info-object, remove that.
    (emms-info-set-cached (car emms-info-later-do-track-queue) nil)
    (setq emms-info-later-do-track-queue (cdr emms-info-later-do-track-queue)))
  (setq emms-info-later-do-track-queue nil)
  ;; Processing the new playlist:
  ;; For all tracks where info isn't already cached, create a
  ;; placeholder info object, so modules requesting info get
  ;; *something*. Accumulate indices for each track needing to be
  ;; reread, and add them to the queue.
  (let ((tracks (emms-playlist-get-playlist))
	(idx 0))
    (while (< idx (length tracks))
      (let* ((curtrack (aref tracks idx))
	     (curinfo (emms-info-get-cached curtrack)))
	(unless curinfo
	  ;; Mark this one as needing to be loaded
	  (add-to-list 'emms-info-later-do-track-queue curtrack t)
	  ;; Setup a placeholder-info object in the cache
	  (emms-info-set-cached
	   curtrack
	   (make-emms-info :title "" :album ""
			   :artist "" :note ""
			   :file (emms-track-name curtrack)))))
      (setq idx (1+ idx))))
  ;; Queue loading the info for each track, if any tracks need to be loaded.
  (when emms-info-later-do-track-queue
    ;; Process the first item synchronously to avoid having it displayed in
    ;; the mode line before its meta-data has been fetched.
    (emms-info-later-do-process)))

(provide 'emms-info-later-do)
;;; emms-info-later-do.el ends here
