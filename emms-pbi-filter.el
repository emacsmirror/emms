;;; emms-pbi-filter.el --- Filtering functions for the PBI

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

;; This file provides a way to filter the playlist to only show
;; certain parts of a big playlist, and only play after those.

;;; Code:

(require 'emms-pbi)

(defvar emms-pbi-not-filtered nil
  "A list of trackindexes in `emms-playlist' that aren't filtered
out.")

(defun emms-pbi-filter-region (beg end)
  "Hide the lines spanning the region from BEG to END."
  (interactive "r")
  (save-excursion
    (set-buffer emms-pbi-playlist-buffer-name)
    (goto-char beg)
    (let ((startofregion (point-at-bol)))
      (goto-char end)
      (let ((endofregion (point-at-eol)))
	(let ((hideoverlay (make-overlay startofregion  endofregion))
	      (newlineoverlay (make-overlay (- startofregion 1) startofregion))
	      (intangibleoverlay (make-overlay (- startofregion 1) (+  endofregion 1))))
	  (overlay-put newlineoverlay 'after-string "\n")
	  (overlay-put newlineoverlay 'invisible t)
	  (overlay-put newlineoverlay 'emms-id 'emms-pbi-filter-overlay)
	  (overlay-put intangibleoverlay 'intangible t)
	  (overlay-put intangibleoverlay 'emms-id 'emms-pbi-filter-overlay)
	  (overlay-put hideoverlay 'emms-id 'emms-pbi-filter-overlay)
	  (overlay-put hideoverlay 'invisible t))))))

(defun emms-pbi-unfilter-region (beg end)
  "Show all hidden lines between BEG and END."
  (interactive "r")
  (let ((overlays (overlays-in beg end)))
    (while overlays
      (when (equal (overlay-get (car overlays) 'emms-id) 'emms-pbi-filter-overlay)
	(delete-overlay (car overlays)))
      (setq overlays (cdr overlays)))))

(provide 'emms-pbi-filter)
;;; emms-pbi-filter.el ends here
