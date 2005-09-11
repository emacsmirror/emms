;;; emms-tageditor.el --- Info-editor for EMMS

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

;; This file provides an info-editor for EMMS. It also provides, as an
;; option, functions for integrating this with the
;; playlist-buffer-interface as well as the mark-module for the pbi.

;; To activate the basic editor, use this in your EMMS-configuration:

;; (require 'emms-tageditor)

;; And call M-x emms-tageditor-edit-current RET, when you find a song
;; with a tag that needs to be changed.

;; To use the pbi-functionality, add the following to your
;; configuration:

;; (emms-tageditor-pbi-mode 1)

;; Which will add 'e' as a key, to edit the track under the point in
;; the pbi.

;; To use the extended pbi-functionality, with `emms-pbi-mark', use:

;; (emms-tageditor-pbi-mark-mode 1)

;; Which likewise will bind 'E' to edit all the tracks marked as a
;; whole, in a way that allows you to set the "album"-tag of each
;; track, or f.ex. to match the filename to a regexp, and set a group
;; from that regexp as the title-tag, or any other.

;;; Code:

(require 'emms)
(require 'emms-info)
(require 'emms-pbi)
(require 'emms-pbi-mark)
(require 'widget)
(require 'wid-edit)

;; Custom
(defgroup emms-tageditor nil
  "*A wizard-style tageditor for EMMS."
  :group 'emms
  :prefix "emms-tageditor-")

(defcustom emms-tageditor-buffer-name "*emms-tageditor*"
  "The name of the buffer used for tag-editing."
  :type 'string
  :group 'emms-tageditor)

;; Variables
(defvar emms-tageditor-current-tracks []
  "A vector of the tracks currently being edited.")

(defvar emms-tageditor-current-infos []
  "A vector of the info-ojects currently being edited.")

(defvar emms-tageditor-message nil
  "A message to show with the form, if non-nil.")

(defvar emms-tageditor-widgets nil
  "A hash map of the widgets on the screen.")

;; Hashtable interface
(defun emms-tageditor-get-widget (trackidx fieldname)
  "Return the widget of FIELDNAME, for the track with TRACKIDX"
  (let ((symbol (emms-tageditor-get-widget-id trackidx fieldname)))
    (gethash symbol emms-tageditor-widgets)))

(defun emms-tageditor-set-widget (trackidx fieldname widget)
  "Save a reference to WIDGET, as FIELDNAME of the track with TRACKIDX."
  (let ((symbol (emms-tageditor-get-widget-id trackidx fieldname)))
    (puthash symbol widget emms-tageditor-widgets)))

;; Helper function for the hashtable
(defun emms-tageditor-get-widget-id (trackidx fieldname)
  "Get a symbol ID of the FIELDNAME widget of TRACKIDX."
  (intern (concat "-trackidx-" (number-to-string trackidx)
		  "-" (symbol-name fieldname))))

(defun emms-tageditor-read-tag (trackidx)
  "Read the form for a single track, and parse it into an info-object."
  (let ((info (aref emms-tageditor-current-infos trackidx))
	(track (aref emms-tageditor-current-tracks trackidx))
	(tags '(title artist album note)))
    (while tags
      (let* ((tag (car tags))
	     (infotag (intern (concat "emms-info-" (symbol-name tag)))))
	(eval `(setf (,infotag ,info)
		     ,(widget-value (emms-tageditor-get-widget
				     trackidx
				     tag)))))
      (setq tags (cdr tags)))
    info))

;; Parsing the form
(defun emms-tageditor-read-tags (tracks)
  "Create a new list of info-object from the form, and return it."
  (mapcar 'emms-tageditor-read-tag tracks))

;; Event-handling
(defun emms-tageditor-save (widget &rest ignore)
  "Save the info of a single tag."
  ;; This function only saves a single form.  Figure out which track
  ;; this is bound to, by extracting the trackidx from the
  (let* ((trackidx (widget-get widget :trackidx))
	 (track (aref emms-tageditor-current-tracks trackidx)))
    ;; Let's make sure we have an info-source capable of writing tags.
    (if (funcall (emms-info-method-for track) 'set)
	(progn
	  ;; we have an info-method for it, let's set it.
	  (emms-info-set track
			 (emms-tageditor-read-tag trackidx))
	  (when (and (featurep 'emms-pbi)
		     (get-buffer emms-pbi-playlist-buffer-name))
	    ;; If a playlist is available, it's info might need to be updated
	    ;; for this track.
	    (emms-pbi-entry-update-track track)
	    (when (= (length emms-tageditor-current-tracks) 1)
	      ;; pressing save should kill the buffer when only one track is
	      ;; being edited.
	      (emms-tageditor-cleanup))))
      ;; if the above returned nil, no function to save info for this
      ;; track has been made! signal an error and escape!
      (message (format (concat "Track %s doesn't have an associated info-method "
			       " capable of saving data")
		       (emms-track-name track))))))

(defun emms-tageditor-save-all ()
  "Save all entries currently being edited."
  ;; Loop through all forms, and save them
  (let ((idx 0))
    (while (< idx (length emms-tageditor-current-tracks))
      (emms-tageditor-save (emms-tageditor-get-widget idx 'save))
      (setq idx (1+ idx))))
  ;; Always cleanup when saving everything
  (emms-tageditor-cleanup))

(defun emms-tageditor-cancel (&rest ignore)
  (emms-tageditor-cleanup))

(defun emms-tageditor-create-string (rep times)
  "Concat TIMES occurances of REP into a string and return it."
  (if (> times 1)
      (concat (emms-tageditor-create-string rep (1- times)) rep)
    rep))

;; Setting up the form, and destroying it
(defun emms-tageditor-create-widgets (trackidx info)
  "Create widgets for a single track-form"
  (let ((inhibit-read-only t)
	(track (aref emms-tageditor-current-tracks trackidx))
	(info (aref emms-tageditor-current-infos trackidx)))
    (goto-char (point-min))
    (widget-insert (format "Editing tag for track: %s (%s)\n"
			   (emms-track-name track)
			   (symbol-name
			    (emms-track-type track))))
    (widget-insert (concat "----------------------------------------"
			   "----------------------------------------"
			   "\n"))
    ;; Insert the tags
    (let ((tags '(title artist album note)))
      (while tags
	(let* ((tag (car tags))
	       (info-tag (intern (concat "emms-info-" (symbol-name tag))))
	       (tag-name
		(concat (upcase-initials (symbol-name tag)) ":"
			(emms-tageditor-create-string " "
						      (- 10 (1+ (length (symbol-name tag))))))))
	  (widget-insert tag-name)
	  (eval `(emms-tageditor-set-widget
		  trackidx (quote ,tag)
		  (widget-create 'editable-field
				 :size 69
				 :trackidx trackidx
				 :value (,info-tag ,info))))
	  (when (not (= (length tags) 1))
	    (widget-insert "\n")))
	(setq tags (cdr tags))))
    ;; Insert the rest
    (widget-insert (concat "\n"
			   "----------------------------------------"
			   "----------------------------------------"
			   "\n"))
    (emms-tageditor-set-widget
     trackidx 'save    
     (widget-create 'push-button
		    :notify 'emms-tageditor-save
		    :trackidx trackidx
		    :help-echo "Save changes to this tag"
		    "Save"))
    (widget-insert "                                                                  ")
    (widget-create 'push-button
		   :notify 'emms-tageditor-cancel
		   :help-echo "Cancel changes"
		   "Cancel")))

(defun emms-tageditor-cleanup ()
  "Clean up and exit the tageditor."
  ;; delete all widgets
  (let ((idx 0))
    (while (< idx (length emms-tageditor-current-tracks))
      (when emms-tageditor-widgets
	(let ((tags '(title artist album note save)))
	  (while tags
	    (let ((tag (car tags)))
	      (eval `(widget-delete (emms-tageditor-get-widget idx tag))))
	    (setq tags (cdr tags)))))
      ;; continue idx loop
      (setq idx (1+ idx))))
  ;; kill the buffer & delete the hashmap
  (setq emms-tageditor-widgets nil)
  (kill-buffer (get-buffer-create emms-tageditor-buffer-name)))

(defun emms-tageditor-replace-regexp (regexp rep string &optional fixedcase literal subexp start)
  "Compatibility wrapper for replace-regexp-in-string/replace-in-string."
  (if (featurep 'xemacs)
      (replace-in-string regexp rep string fixedcase literal subexp start)
    (replace-regexp-in-string regexp rep string fixedcase literal subexp start)))

(defun emms-tageditor-replace-create-replacement (replace-with trackidx)
  (let ((info (aref emms-tageditor-current-infos trackidx))
	(track (aref emms-tageditor-current-tracks trackidx)))
    (setq replace-with (emms-tageditor-replace-regexp "$TITLE" (emms-info-title info) replace-with))
    (setq replace-with (emms-tageditor-replace-regexp "$ALBUM" (emms-info-album info) replace-with))
    (setq replace-with (emms-tageditor-replace-regexp "$ARTIST" (emms-info-artist info) replace-with))
    (setq replace-with (emms-tageditor-replace-regexp "$NOTE" (emms-info-note info) replace-with))
    (setq replace-with (emms-tageditor-replace-regexp "$TRACKNAME" (emms-track-name track) replace-with)))
  replace-with)

(defun emms-tageditor-replace-tag (field regexp replace-with)
  "Replace REGEXP with REPLACE-WITH in all fields of type FIELD."
  (let ((idx 0))
    (while (< idx (length emms-tageditor-current-tracks))
      ;; Find the widget for the current track
      (let ((widget (emms-tageditor-get-widget idx field)))
	(let* ((str (widget-value widget))
	       (str (emms-tageditor-replace-regexp regexp replace-with str)))
	  (if (string= "$SET" regexp)
	      (widget-value-set
	       widget
	       (emms-tageditor-replace-create-replacement replace-with idx))
	    (widget-value-set
	     widget
	     (emms-tageditor-replace-create-replacement str idx)))))
      (setq idx (1+ idx)))))

(defun emms-tageditor-replace-tags (&optional field regexp replace-with)
  "Replace REGEXP with REPLACE-WITH in the widgets matching FIELD."
  (interactive)
  (setq field (or field (intern (completing-read
			       "Select which tags to replace in: "
			       '(("all" . all) ("title" . title)
				 ("artist" . artist) ("album" . album)
				 ("note" . note))
			       nil t "title"))))
  (setq regexp (or regexp (read-from-minibuffer "Regexp to replace: ")))
  (setq replace-with (or replace-with (read-from-minibuffer (concat "Replace regexp " regexp " with: "))))
  ;; Having all input, let's continue to act on it.
  (when (and field regexp replace-with)
    ;; two cases, 'all or something else
    (if (equal field 'all)
	(progn
	  ;; We need a sweep-search of all tag-fields
	  (let ((tags '(title artist album note)))
	    (while tags
	      (emms-tageditor-replace-tag (car tags) regexp replace-with)
	      (setq tags (cdr tags)))))
      ;; only search the field called field
      (emms-tageditor-replace-tag field regexp replace-with))
    ;; we've probably changed some widget values, so we need to make
    ;; them count.
    (widget-setup)))

;; Setting up the buffer
(defun emms-tageditor-edit (tracks &optional infos)
  "Open an editor for the vector TRACKS.

Optionally, use the vector INFOS as the default info for each track,
and use the function SAVEFUNCTION as the event-handler for each
save-button."
  ;; Save variables
  (setq emms-tageditor-current-tracks tracks)
  (if infos
      (setq emms-tageditor-current-infos infos)
    ;; Otherwise, create the vector of infos by loading them.
    (setq emms-tageditor-current-infos
	  (make-vector (length emms-tageditor-current-tracks)
		       nil))
    (let ((idx 0))
      (while (< idx (length emms-tageditor-current-tracks))
	(setf (aref emms-tageditor-current-infos idx)
	      ;; should we allow cache here?
	      (emms-info-get (aref emms-tageditor-current-tracks idx)))
	(setq idx (1+ idx)))))  
  ;; Kill the buffer, then recreate it. Otherwise, everything will be
  ;; in one big widget.
  (kill-buffer (get-buffer-create emms-tageditor-buffer-name))
  (switch-to-buffer (get-buffer-create emms-tageditor-buffer-name))  
  ;; Initialise buffer
  (kill-all-local-variables)
  (widget-minor-mode 1)
  ;; Setup widget hashmap,
  (setq emms-tageditor-widgets (make-hash-table :test 'equal))
  ;; and create the widgets 
  (let ((idx 0))
    (while (< idx (length emms-tageditor-current-tracks))
      (emms-tageditor-create-widgets idx
				     (aref emms-tageditor-current-infos idx))
      (widget-insert "\n\n")
      (setq idx (1+ idx)))
    ;; Create the save _all_ widget?
    ;; setup the help-message
    (when emms-tageditor-message
      (goto-char (point-max))
      (widget-insert (concat "\n"
			     "........................................"
			     "........................................"
			     "\n"))
      (widget-insert emms-tageditor-message)
      (setq emms-tageditor-message nil))
    (use-local-map widget-keymap)
    ;; Bind some additional keys
    (widget-setup)
    (local-set-key (kbd "C-x C-s") (lambda () (interactive) (emms-tageditor-save-all)))
    (local-set-key (kbd "C-c C-r") 'emms-tageditor-replace-tags)
    (local-set-key (kbd "ESC") (lambda () (interactive) (emms-tageditor-cancel)))))

;; Entry function
(defun emms-tageditor-edit-current ()
  "Edit the info of the currently playing track"
  (interactive)
  (emms-tageditor-edit (vconcat (list (emms-playlist-current-track)))))

;; Integrating with emms-pbi 
(defvar emms-tageditor-pbi-mark-message
  "When editing multiple files, some things works a bit
differently. First of all, to save *all* changes made to tracks, use
C-x C-s.

The changes to each individual track, can be saved by using the
corresponding Save-buttons.

To utilize the full power of this mode of editing, you should use
M-x emms-tageditor-replace-tags RET, bound to C-c C-r.

When using `emms-tageditor-replace-tags', you have the following
special keyword available:

For matching:

   $SET -- Attempting to replace this value with anything, will tell
           the function to simply override the previous value.

For what to replace with:

   $TRACKNAME -- The trackname (for file-type tracks, the full filename)
   $TITLE     -- The (saved) title of this track.
   $ARTIST    -- Likewise, with the artist
   $ALBUM     -- Likewise, with the album
   $ALBUM     -- Likewise, with the note.

NOT IMPLEMENTED YET:
If the power of that function doesn't fit your needs, you can use M-x
emms-tageditor-toggle-read-only RET, bound to C-c C-t. This function
will make the buffer read-only, which means you can use the regular
editing functions on the entire buffer. This means that doing an M-x
replace-regexp RET, won't halt if it matches any of the text outsie
widgets, as it would otherwise.")

(defun emms-tageditor-pbi-mode (&optional arg)
  "Register the intergration with the playlist-buffer interface for EMMS.

Turn the registration on, if and only if ARG is a positive integer,
off otherwise."
  (interactive "p")
  (if (not (featurep 'emms-pbi))
      (message "You need `emms-pbi' loaded to use this!")
    (if (and (numberp arg) (< 0 arg))
	(add-hook 'emms-pbi-mode-hook 'emms-tageditor-pbi-register)
      (remove-hook 'emms-pbi-mode-hook 'emms-tageditor-pbi-register))))

(defun emms-tageditor-pbi-register ()
  "Register keybindings for the playlist-buffer interface.

Should be run in `emms-pbi-mode-hook'."
  (local-set-key (kbd "e")
		 'emms-tageditor-pbi-edit-current-line))

(defun emms-tageditor-pbi-edit-current-line ()
  "Edit the track under point."
  (interactive)
  (if (not (featurep 'emms-pbi))
      (message "You need `emms-pbi' loaded to use this!")
    ;; Fetch track under current line
    (let ((curidx (emms-pbi-return-current-line-index)))
      (when (emms-pbi-valid-index-p curidx)
	(other-window 1)
	(emms-tageditor-edit (vconcat (list (emms-playlist-get-track curidx))))))))

;; Integrating with pbi-mark
(defun emms-tageditor-pbi-mark-mode (&optional arg)
  "Register the intergration with the playlist-buffer-marks for EMMS.

Turn the integration on, if and only if ARG is a positive integer, off
otherwise."
  (interactive "p")
  (if (not (featurep 'emms-pbi-mark))
      (message "You need `emms-pbi-mark' loaded to use this!")
    (if (and (numberp arg) (< 0 arg))
	(add-hook 'emms-pbi-mode-hook 'emms-tageditor-pbi-mark-register)
      (remove-hook 'emms-pbi-mode-hook 'emms-tageditor-pbi-mark-register))))

(defun emms-tageditor-pbi-mark-register ()
  "Register keybindings for the playlist-buffer interface marking functions.

Should be run in `emms-pbi-mode-hook'."
  (local-set-key (kbd "E")
		 'emms-tageditor-pbi-mark-edit-marked-entries))

(defun emms-tageditor-pbi-mark-edit-marked-entries ()
  "Edit all marked entries as one, using a special editor."
  (interactive)
  (if (not (featurep 'emms-pbi-mark))
      (message "You need `emms-pbi-mark' loaded to use this!")
    (other-window 1)
    (setq emms-tageditor-message emms-tageditor-pbi-mark-message)
    (emms-tageditor-edit (vconcat (emms-pbi-mark-get-marked)))
    (goto-char (point-min))))

(provide 'emms-tageditor)
;;; emms-tageditor.el ends here
