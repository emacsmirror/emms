;;; emms-pbi-mark.el --- Mark-functions for the EMMS playlist

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

;; This file provides a mark-facility for the EMMS playlist-buffer
;; interface.

;; To use it, add:

;; (require 'emms-pbi-mark)
;; (emms-pbi-mark 1)

;; To your ~/.emacs. 

;;; Code:

;; $Revision: 1.7 $
;; $Id: emms-pbi-mark.el,v 1.7 2005/07/09 11:56:00 forcer Exp $

;; Customs
(defgroup emms-pbi-mark nil
  "Marking utilities for the EMMS playlist"
  :prefix "emms-pbi-mark-"
  :group 'emms-pbi)

(defface emms-pbi-mark-marked-face
  '((((class color))
     (:foreground "green" :weight bold)))
  "Face used for marked files"
  :group 'emms-pbi-mark)

;; variables
(defvar emms-pbi-mark-indices nil
  "A list containing the indices of the marked elements.")

;; Entry
(defun emms-pbi-mark (arg)
  "Activate mark-mode for the EMMS-playlist."
  (interactive "p")
  (if (and arg (< 0 arg))
      (progn
	(add-hook 'emms-pbi-current-line-face-changed-hook 'emms-pbi-add-mark-face)
	(add-hook 'emms-playlist-changed-hook 'emms-pbi-mark-invalidate-marks)
	(add-hook 'emms-pbi-mode-hook 'emms-pbi-mark-add-keybindings))
    (remove-hook 'emms-pbi-current-line-face-changed-hook 'emms-pbi-add-mark-face)
    (remove-hook 'emms-pbi-mode-hook 'emms-pbi-mark-add-keybindings)
    (remove-hook 'emms-playlist-changed-hook 'emms-pbi-mark-invalidate-marks)))

(defun emms-pbi-mark-invalidate-marks ()
  "Force a reset of marked tracks."
  (when (get-buffer emms-pbi-playlist-buffer-name)
    (emms-pbi-mark-clear-marked))
  (setq emms-pbi-mark-indices nil))

(defun emms-pbi-mark-add-keybindings ()
  "Adds keybindings for the mark-functions to a *Playlist* buffer."
  (local-set-key (kbd "m") 'emms-pbi-mark-mark-file)
  (local-set-key (kbd "M-u") 'emms-pbi-mark-clear-marked)
  (local-set-key (kbd "u") 'emms-pbi-mark-unmark-file))

(defun emms-pbi-mark-current-line-marked-p ()
  "Return non-nil if the current line is marked, nil otherwise."
  (let ((idx (emms-pbi-return-current-line-index)))
    (if (not idx)
	nil
      (member idx emms-pbi-mark-indices))))

(defun emms-pbi-mark-current-line-mark ()
  "Mark the current line."
  (let ((idx (emms-pbi-return-current-line-index)))
    (when (emms-pbi-valid-index-p idx)
      (add-to-list 'emms-pbi-mark-indices idx)
      (emms-pbi-add-mark-face))))

(defun emms-pbi-mark-current-line-unmark ()
  "Unmark the current line."
  (let ((idx (emms-pbi-return-current-line-index)))
    (when (and (emms-pbi-valid-index-p idx)
	       (emms-pbi-mark-current-line-marked-p))
      (setq emms-pbi-mark-indices (remove idx emms-pbi-mark-indices))
      (emms-pbi-remove-mark-face))))

(defun emms-pbi-add-mark-face ()
  "Add a face to the current line, if it is marked."
  (when (emms-pbi-mark-current-line-marked-p)
    (put-text-property (point-at-bol) (point-at-eol) 'face 'emms-pbi-mark-marked-face)))

(defun emms-pbi-remove-mark-face ()
  "Remove the marked-face from the current-line, if it's no longer
marked."
  (when (not (emms-pbi-mark-current-line-marked-p))
    (let ((inhibit-read-only t))
      (remove-text-properties (point-at-bol) (point-at-eol) '(face))
      (emms-pbi-add-properties-current-line))))

;; Programming interface functions
(defun emms-pbi-mark-get-marked ()
  "Return a list of tracks marked in the playlist"
  (let ((indices emms-pbi-mark-indices)
	(tracks nil))
    (while indices
      (let ((idx (car indices)))
	(when (emms-pbi-valid-index-p idx)
	  (add-to-list 'tracks (emms-playlist-get-track idx))))
	(setq indices (cdr indices)))
    tracks))

;; User Interface functions
(defun emms-pbi-mark-clear-marked ()
  "Clear all marks from the playlist-buffer"
  (interactive)
  (save-excursion
    (set-buffer (get-buffer emms-pbi-playlist-buffer-name))
    (let ((indices emms-pbi-mark-indices)
	  (tracks nil))
      (while emms-pbi-mark-indices
	(let ((idx (car emms-pbi-mark-indices)))	  
	  (when (emms-pbi-valid-index-p idx)
	    (goto-line (1+ idx))
	    (emms-pbi-mark-current-line-unmark)))))))

(defun emms-pbi-mark-mark-file (arg)
  "Marks the current line in the playlist.

With prefix argument, mark the following ARG lines."
  (interactive "p")
  (let ((marksleft arg)
	(inhibit-read-only t))
    (while (> marksleft 0)
      (emms-pbi-mark-current-line-mark)
      (forward-line 1)
      (setq marksleft (1- marksleft)))))

(defun emms-pbi-mark-unmark-file (arg)
  "Removes a mark from the current line in the playlist.

With prefix argument, unmark the following ARG lines."
  (interactive "p")
  (let ((marksleft arg)
	(inhibit-read-only t))
    (while (> marksleft 0)
      (emms-pbi-mark-current-line-unmark)
      (forward-line 1)
      (setq marksleft (1- marksleft)))))

(provide 'emms-pbi-mark)
;;; emms-pbi-mark.el ends here
