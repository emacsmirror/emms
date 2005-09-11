;;; emms-pbi-popup.el --- Playlist-popup functionality for EMMS

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

;; 

;;; Code:
(require 'emms-pbi)

(defvar emms-pbi-popup-version "0.2 $Revision: 1.7 $"
  "EMMS pbi popup version string.")
;; $Id: emms-pbi-popup.el,v 1.7 2005/07/09 11:56:00 forcer Exp $

(defgroup emms-pbi-popup nil
  "*Module for popping up the playlist in a keystroke."
  :group 'emms-pbi
  :prefix "emms-pbi-popup-")

(defcustom emms-pbi-popup-default-width (+ (or emms-pbi-playlist-entry-max-length 36) 4)
  "*The default width of the window to popup the playlist in.

This defaults to `emms-pbi-playlist-entry-max-length' + 4, or 40 if
`emms-pbi-playlist-entry-max-length' is nil."
  :group 'emms-pbi-popup
  :type 'number)

(defcustom emms-pbi-popup-default-side-left nil
  "*Boolean determining whether to popup in the left-side as a
default. If nil, the popup will appear in the right side."
  :type 'boolean
  :group 'emms-pbi-popup)

(defvar emms-pbi-popup-old-conf nil
  "The window-configuration when popping up the playlist.")

(defun emms-pbi-popup-forget-conf ()
  "Forget the previously saved configuration, and make the changes
final."
  (setq emms-pbi-popup-old-conf nil)
  ;; Remove the special bindings
  (emms-pbi-popup-revert)
  ;; Remove this function again, it will get addded when a new
  ;; configuration is saved anyway
  (remove-hook 'window-configuration-change-hook 'emms-pbi-popup-forget-conf))

(defun emms-pbi-popup-revert ()
  "Revert to the window-configuration from before if there is one,
otherwise just remove the special bindings from the playlist."
  (interactive)
   (remove-hook 'emms-pbi-manually-change-song-hook 'emms-pbi-popup-revert)
  (let ((playlistbuffer (get-buffer emms-pbi-playlist-buffer-name)))
    (when playlistbuffer
      (save-excursion
	(set-buffer playlistbuffer)
	(local-unset-key (kbd "q"))
	(local-unset-key (kbd "TAB")))))
  (when emms-pbi-popup-old-conf
    (set-window-configuration emms-pbi-popup-old-conf)))

;; Entry-point
(defun emms-pbi-popup-playlist (&optional popup-left popup-width )
  "Pops up the playlist temporarily, for selecting a new song.

If POPUP-LEFT is non-nil, the window will appear in the left side of
the current window, otherwise it will appear in the right side. 

POPUP-WIDTH is the width of the new frame, defaulting to
`emms-pbi-popup-default-width'."
  (interactive)
  (setq popup-width (or popup-width emms-pbi-popup-default-width)
	popup-left (or popup-left emms-pbi-popup-default-side-left))
  ;; Split the current screen, and make the playlist popup
  (let ((new-window-width (- (window-width) popup-width)))
    (if (not (> new-window-width 0))
	;; consider just opening the playlist here instead of arguing
	;; semantics with the user?
	(error "Current window not wide enough to popup playlist!")
      ;; Negative value to popup in the left side
      (when popup-left
	(setq new-window-width (- new-window-width)))
      ;; Make sure EMMS is actually playing before continuing
      (if (or (not (emms-playlist-get-playlist)) (= (length (emms-playlist-get-playlist)) 0))
	  ;; we haven't got a playlist, exit.
	  (error "Can't popup playlist-buffer until a playlist has been loaded!")
	;; if 
	;; Save the current window-configuration
	(setq emms-pbi-popup-old-conf (current-window-configuration))
	;; Split and select the playlist
	(let ((buffer-on-the-right
	       (split-window-horizontally new-window-width)))
	  (unless popup-left
	    (select-window buffer-on-the-right)))
	(unless (get-buffer emms-pbi-playlist-buffer-name)
	  ;; No playlist-buffer yet, create it.
	  (emms-pbi 1))     
	(switch-to-buffer emms-pbi-playlist-buffer-name t)
	;; Now, modify the playlist functionality to revert to the
      ;; window-configuration from before when a song is selected
	(add-hook 'emms-pbi-manually-change-song-hook 'emms-pbi-popup-revert)
	(local-set-key (kbd "TAB") 'emms-pbi-popup-revert)
	(local-set-key (kbd "q") 'delete-window)
	;; Also, forget about the whole thing if the user does something
	;; to the window-configuration
	(add-hook 'window-configuration-change-hook 'emms-pbi-popup-forget-conf)))))


(provide 'emms-pbi-popup)
;;; emms-pbi-popup.el ends here
