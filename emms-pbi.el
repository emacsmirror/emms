;;; pbi.el --- Playlist-buffer interface for emms.el

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

;; This module provices a playlist-buffer interface (pbi) to EMMS.

;; To use it, just add the following to your emms-configuration
;; (.emacs, for example):

;; (require 'emms-pbi)

;; If you want the playlist to be generated automagically when you
;; start playing your music, use the following:

;; (emms-pbi 1)			       

;; Another common usage, is to just load the playlist when you need
;; it. This can be achieved by starting emms, and then typing M-x
;; emms-pbi RET.

;; The default look of the playlist depends on the value of
;; `emms-track-description-function'. This function takes a
;; track-value and returns a string, that will then be formatted
;; further, and inserted into the playlist-buffer. If you want a
;; different function to handle the base-description, but you only
;; want that function for emms-pbi, you can override
;; `emms-pbi-track-description-function', which will fall back to
;; `emms-track-description-function' when nil

;; You'll probably also want to customize the faces, in which case,
;; you do M-x customize-group RET emms-pbi RET and tweak.

;; To get id3-tags, and ogg-info, you should look at
;; emms-info.el. This file provides amongst other things, functions
;; suitable as values of `emms-track-description-function' (or if you
;; only want the info in emms-pbi, values of
;; `emms-pbi-track-description-function').

;; If you use a big playlist, and info, you probably don't want info
;; to load everything right at once. This can be accomplished through
;; the `emms-info-later-do'-module, which will gradually load the
;; playlist. A note on how to make it work with the PBI as well, is
;; included in that file (and also a part of emms-default.el,
;; currently the 'cvs-setup)

;; Linenumbering (Changed!)  
;;
;; Prior versions of emms-pbi had their own linenumbering
;; functions. But these functions were either error prone or damn
;; slow. And besides there was already a emacs mode that does exactly
;; the same: setnu.el So we remove the linenumbering functions in
;; favour of setnu. You can get setnu from
;; http://www.wonderworks.com/download/setnu.el. To get linenumbers
;; just put the following code in your ~/.emacs and put setnu.el
;; somewhere on your loadpath:
;;
;; (require 'setnu)
;; (add-hook 'emms-pbi-after-build-hook (lambda () (setnu-mode 1)))

;;; Code:

(require 'emms)
(require 'cl)

(defvar emms-pbi-version "0.2 $Revision: 1.37 $"
  "EMMS pbi version string.")
;; $Id: emms-pbi.el,v 1.37 2005/07/09 19:14:57 yonirabkin Exp $

;; Customizations

(defgroup emms-pbi nil
  "*A playlist-buffer user-interface for EMMS."
  :group 'emms
  :prefix "emms-pbi-")

(defcustom emms-pbi-playlist-entry-generate-function 'identity
  "*The function to call for generating a single item of the
playlist. This will be called with a string argument FILENAME, and
should return the text to be inserted in the playlist."
  :type 'function
  :group 'emms-pbi)

(defcustom emms-pbi-playlist-entry-max-length nil
  "*The maximum length of an entry in the playlist. If this is nil,
the entire string provided by `emms-track-description-function'.
will be used. Beware, the output of that function is cut off to fit
the max-length before running
`emms-pbi-playlist-entry-generate-function'."
  :type '(restricted-sexp :match-alternatives (integerp 'nil))
  :group 'emms-pbi)

(defcustom emms-pbi-playlist-buffer-name "*Playlist*"
  "Name of the buffer to use as a playlist-buffer"
  :type 'string
  :group 'emms-pbi)

(defcustom emms-pbi-track-description-function nil
  "Returns a description for the playlist.

Take track as only argument. If `emms-pbi-track-description-function' is nil,
`emms-track-description-function' is used instead."
  :type 'function
  :group 'emms-pbi)

;; Hooks

(defcustom emms-pbi-after-build-hook nil
  "Hook that is run after the playlist buffer is built.
That might be usefull to change the playlist buffer before the
buffer is set read-only."
  :type 'hook
  :group 'emms-pbi)

(defcustom emms-pbi-current-line-face-changed-hook nil
  "Hook that is called when the face of the current line changes."
  :type 'hook
  :group 'emms-pbi)

(defcustom emms-pbi-manually-change-song-hook nil
  "Hook that is called when the song is manually changed."
  :type 'hook
  :group 'emms-pbi)

;; Faces
(defface emms-pbi-song-face
  '((((class color) (background light))
     (:foreground "red"))
    (((class color) (background dark))
     (:foreground "red")))
  "Face used for songs"
  :group 'emms-pbi)

(defface emms-pbi-current-face
  '((((class color)(background light))
     (:foreground "blue" :weight bold))
    (((class color)(background dark))
     (:foreground "yellow" :weight bold)))
  "Face used for the currently played song"
  :group 'emms-pbi)

;; Variables
(defvar emms-pbi-suspend-hooks nil
  "When this variable is t, the hooks updating the playlist stop
reacting.

If you do something with `emms-playlist', and don't want to regenerate
the entire playlist-buffer, a good idea is to bind
`emms-pbi-suspend-hooks' to t when you set `emms-playlist'.")

(defvar emms-pbi-current-overlay nil
  "Overlay moving with the current track.")

(defvar emms-kill-ring ()
  "Kill-ring for the playlist buffer.")

(defvar emms-pbi-longest-line 0
  "The length of the longest line yet inserted.")

;; Entry points
(defun emms-pbi (arg)
  "Turn on emms-playlist if prefix argument ARG is a positive integer,
off otherwise."
  (interactive "p")
  (if (and arg (> arg 0))
      (progn
	(add-hook 'emms-player-started-hook 'emms-pbi-update-current-face)
	;; make sure this is appended!
	(add-hook 'emms-playlist-changed-hook 'emms-pbi-rebuild-playlist-buffer t)
	;; build the playlist, if we have a playlist
	(if (> (length emms-playlist) 0)
	    (progn 
	      (emms-pbi-build-playlist-buffer)
	      (switch-to-buffer emms-pbi-playlist-buffer-name))
	  (message "Empty playlist, won't build playlist-buffer!")))
    (remove-hook 'emms-player-stopped-hook 'emms-pbi-remove-current-face)
    (remove-hook 'emms-player-starter-hook 'emms-pbi-add-current-face)
    (remove-hook 'emms-playlist-changed-hook 'emms-pbi-rebuild-playlist-buffer)))

(defun emms-pbi-shorten-entry-to-max-length (entry)
  "Cut off an entry-text to make sure it's no longer than
`emms-pbi-playlist-entry-max-length' characters long."
  (if (and emms-pbi-playlist-entry-max-length
	   (> (length entry) emms-pbi-playlist-entry-max-length))
      (substring entry 0 emms-pbi-playlist-entry-max-length)
    entry))

(defun emms-position-vector (elt vector)
  "Returns the index of elt in vector"
  (let ((curidx 0)
	(residx nil))
    (while (and (< curidx (length vector)) (eq residx nil))
      (let ((curelt (aref vector curidx)))
	(when (equal elt curelt)
	  (setq residx curidx)))
      (setq curidx (1+ curidx)))
    residx))

;; This function should probably be phased out, since it depends too
;; much on emms-info. All uses should be replaced by the function
;; below:
(defun emms-pbi-entry-info-updated (track info)
  "Update the track-entry based on the info"
  (save-excursion
    (set-buffer emms-pbi-playlist-buffer-name)
    (let ((inhibit-read-only t)
	  (pos (emms-position-vector track emms-playlist)))
      ;; find the entry in the playlist, corresponding to TRACK    
      (when 
	  (goto-line (1+ pos))      
	;; update the text of it - by generating it again simply
	;; first, find the index of the entry in the playlist.
	;; and save the current properties
	(delete-region (point-at-bol) (point-at-eol))
	(emms-pbi-insert-entry (emms-playlist-get-track pos))
	;; and update them
	(emms-pbi-add-properties-current-line)))))

(defun emms-pbi-entry-update-track (track)
  "Update the playlist entry for TRACK."
  (let ((trackidx
	 (loop for i from 0 for a across (emms-playlist-get-playlist)
	       if (equal a track) return i)))
    (when (and trackidx (emms-pbi-valid-index-p trackidx))
      (emms-pbi-entry-update-idx trackidx))))

(defun emms-pbi-entry-update-idx (trackidx)
  "Update the playlist entry for the track at index TRACKIDX."
  (save-excursion
    (set-buffer emms-pbi-playlist-buffer-name)
    ;; Find the track
    (when (emms-pbi-valid-index-p trackidx)
      (let ((lineidx (1+ trackidx)))
	(let ((inhibit-read-only t))
	  ;; Erase the line
	  (goto-line lineidx)
	  (delete-region (point-at-bol) (point-at-eol))
	  ;; Insert the track and add properties
	  (emms-pbi-insert-entry (emms-playlist-get-track trackidx))
	  (emms-pbi-add-properties-current-line)
	  ;; Make sure the overlay is in place
	  (emms-pbi-update-current-face))))))

(defun emms-pbi-entry-generate (track)
  "Generate an entry for TRACK in the playlist-buffer.

This uses `emms-pbi-track-description-function', or if that is nil, it defaults
to `emms-track-description'."
  (if emms-pbi-track-description-function
      (funcall emms-pbi-track-description-function track)
    ;; default to the emms way
    (funcall emms-track-description-function track)))

(defun emms-pbi-insert-entry (track)
  "Insert an entry for TRACK in the playlist."
  (let ((inhibit-read-only t)
	(line (emms-pbi-shorten-entry-to-max-length
	       (emms-pbi-entry-generate track))))
    (insert line)
    (emms-pbi-add-properties-current-line)
    ;; for the convenience of other modules, keep track of the longest
    ;; line yet.
    (setq emms-pbi-longest-line (max emms-pbi-longest-line (length line)))))

(defun emms-pbi-rebuild-playlist-buffer ()
  "This function rebuilds the playlist-buffer if necessary."
  (unless emms-pbi-suspend-hooks
    (emms-pbi-build-playlist-buffer)))

;; Function for building the playlist

(defun emms-pbi-build-playlist-buffer ()
  "Build a playlist-buffer based on the current playlist."
  (save-excursion
    (set-buffer (get-buffer-create emms-pbi-playlist-buffer-name))
    (let ((playlist-length (length emms-playlist))
	  (idx 0)
	  (inhibit-read-only t))
      ;; reset the buffer
      (erase-buffer)
      ;; insert all elements
      (while (< idx playlist-length)
	(emms-pbi-insert-entry (emms-playlist-get-track idx))
	(insert "\n")
	(setq idx (1+ idx)))
      ;; Initialise the buffer variables
      ;; remove the last line
      (emms-pbi-update-current-face)
      (delete-backward-char 1)
      (run-hooks 'emms-pbi-after-build-hook)
      (setq buffer-read-only t)
      (emms-pbi-mode)
      ;; as the last thing we do, update the current-face.
      (when emms-player-playing-p
	(emms-pbi-update-current-face)))))

;; Updating the currently playing face
(defun emms-pbi-update-current-face ()
  "Updates the file line with the current-face"
  (when (get-buffer emms-pbi-playlist-buffer-name)
    (save-excursion
      (set-buffer emms-pbi-playlist-buffer-name)
      (let ((inhibit-read-only t))
	;; don't try to `1+' the value `nil' 
	(unless (null emms-playlist-current)
	  (goto-line (1+ emms-playlist-current))
	  (if (overlayp emms-pbi-current-overlay)
	      (move-overlay emms-pbi-current-overlay
			    (point-at-bol) (point-at-eol))
	    (setq emms-pbi-current-overlay 
		  (make-overlay (point-at-bol) (point-at-eol)))
	    (overlay-put emms-pbi-current-overlay 'face 'emms-pbi-current-face)))))))
			
;;Handling faces & properties
(defun emms-pbi-add-properties-current-line ()
  "Adds the correct faces and other properties to the current line"
  ;; Default face
  (let ((idx (emms-pbi-return-current-line-index)))
    (add-text-properties (point-at-bol) (point-at-eol)
			 '(face emms-pbi-song-face))
    (run-hooks 'emms-pbi-current-line-face-changed-hook)))

(defun emms-pbi-play-current-line ()
  "Play the current line"
  (interactive)  
  (let ((new-idx (emms-pbi-return-current-line-index)))
    ;; check boundaries
    (when (and new-idx (> new-idx -1) (< new-idx (length emms-playlist)))
      (emms-stop)
      (emms-playlist-set-current new-idx)
      (emms-start)
      (run-hooks 'emms-pbi-manually-change-song-hook))))

(defun emms-pbi-show-current-line ()
  "Show filename and info for track on current line."
  (interactive)  
  (let ((idx (emms-pbi-return-current-line-index)))
    (message "Filename: %s; Info: %s" 
	     (emms-track-name 
	      (emms-playlist-get-track idx)) 
	     (emms-playlist-get idx))))

;; Major-mode for the playlist-buffer
(define-derived-mode emms-pbi-mode fundamental-mode "EMMS playlist"
  (suppress-keymap emms-pbi-mode-map t)
  (define-key emms-pbi-mode-map (kbd "n")         'emms-next)
  (define-key emms-pbi-mode-map (kbd "p")         'emms-previous)
  (define-key emms-pbi-mode-map (kbd "c")         'emms-pbi-recenter)
  (define-key emms-pbi-mode-map (kbd "l")         'emms-pbi-recenter)
  (define-key emms-pbi-mode-map (kbd "C-x C-s")   'emms-pbi-export-playlist)
  (define-key emms-pbi-mode-map (kbd "C-k")       'emms-pbi-kill-line)
  (define-key emms-pbi-mode-map (kbd "d")         'emms-pbi-kill-line)
  (define-key emms-pbi-mode-map (kbd "C-y")       'emms-pbi-yank)
  (define-key emms-pbi-mode-map (kbd "s")         'emms-stop)
  (define-key emms-pbi-mode-map (kbd "f")         'emms-pbi-show-current-line)
  (define-key emms-pbi-mode-map (kbd "RET")       'emms-pbi-play-current-line)
  (define-key emms-pbi-mode-map (kbd "q")         'bury-buffer)
  (define-key emms-pbi-mode-map (kbd "<mouse-2>") 'emms-pbi-play-current-line)
  (define-key emms-pbi-mode-map (kbd "Q")         'emms-pbi-quit)
  (define-key emms-pbi-mode-map (kbd "?")         'describe-mode))

(defun emms-pbi-quit ()
  "Stops emms and kill the playlist buffer"
  (interactive)
  (emms-stop) 
  (kill-buffer emms-pbi-playlist-buffer-name))

(defun emms-playlist-empty-p ()
  (= (length emms-playlist) 0))

(defun emms-pbi-kill-line ()
  "Kill the playlist line the cursor is currently on and update
  the playlist accordingly."
  (interactive)
  (if (emms-playlist-empty-p)
      (message "One cannot remove what is not there grasshopper")
    (let ((idx (emms-pbi-return-current-line-index))
	  (inhibit-read-only t))
      ;; remove from buffer
      (goto-char (point-at-bol))
      (kill-line 1)
      ;; push track onto emms-kill-ring
      (push (aref emms-playlist idx)
	    emms-kill-ring)
      ;; now delete the entry from the playlist. - making sure that
      ;; the entire list isn't regenerated
      (let ((emms-pbi-suspend-hooks t))
	(emms-playlist-remove idx))
      (if (numberp emms-playlist-current)
	  ;; this deals with edge cases, explicit
	  (cond ((and emms-player-playing-p
		      (= idx emms-playlist-current))
		 (emms-stop) (emms-start)
		 (emms-pbi-update-current-face))
		((= idx emms-playlist-current)
		 (emms-pbi-update-current-face))
		((< idx emms-playlist-current)
		 (setq emms-playlist-current
		       (1- emms-playlist-current))))
	(emms-stop)))))	;; stop if playlist empty after kill

(defun emms-pbi-yank ()
  "Yank a filename from `kill-ring' into the playlist."
  (interactive)
  (let ((track (pop emms-kill-ring))
	(inhibit-read-only t)
	(idx (emms-pbi-return-current-line-index)))
    (if (and track (emms-player-for track))
	;; only insert files that actually exist, and can be played.
	;; insert it into the buffer
	(save-excursion
	  (goto-char (point-at-bol))
	  (insert "\n")
	  (forward-line -1)
	  (goto-char (point-at-bol))
	  (emms-pbi-insert-entry track)
	  ;; insert it into the playlist
	  (let ((emms-pbi-suspend-hooks t))
	    (emms-playlist-add (list track) idx))))
    (message "No playable track in emms-kill-ring!")))

(defun emms-pbi-return-current-line-index ()
  "Return the index position in the playlist of the current line." 
  (1- (count-lines (point-min) (point-at-eol))))

(defun emms-pbi-valid-index-p (idx)
  "Return non-nil if IDX is a valid index in the current playlist."
  (and idx (> idx -1) (< idx (length emms-playlist))))

(defun emms-pbi-recenter ()
  "Center on current playing track"
  (interactive)
  (let ((line (1+ emms-playlist-current)))
    (goto-line line)
    (recenter)))

(defun emms-pbi-export-playlist (file)
  (interactive "Fsave playlist:")
  (let ((buffer (find-file-noselect file t)))
    (set-buffer buffer)
    (prin1 emms-playlist buffer)
    (save-buffer)
    (kill-buffer buffer)))

(defun emms-pbi-open-playlist (file)
  (interactive "fOpen playlist:")
  (let ((buffer (find-file-noselect file)))
    (set-buffer buffer)
    (beginning-of-buffer)
    (emms-playlist-set-playlist (read buffer))
    (kill-buffer buffer)))

(provide 'emms-pbi)
;;; emms-pbi.el ends here
