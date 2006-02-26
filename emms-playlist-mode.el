;;; emms-playlist-mode.el --- Playlist mode for Emms.

;;; Copyright 2005, 2006, Yoni Rabkin under the GNU General Public
;;; License.  This file is a part of the Emacs Multimedia system.

;;; Commentary:
;;;
;;; This is a method of displaying and manipulating the different Emms
;;; playlist buffers.
;;;
;;; Emms developer's motto: "When forcer says (require 'jump) we say
;;; (funcall #'jump height)"

;;; Code:

;;; --------------------------------------------------------
;;; Variables
;;; --------------------------------------------------------

(defvar emms-playlist-mode-hook nil
  "Emms playlist mode hook.")

(defvar emms-playlist-mode-selected-overlay-marker nil
  "Marker for last selected track.  Use for updating the display.")

(defvar emms-playlist-mode-switched-buffer nil
  "Last buffer visited before calling `emms-playlist-mode-switch-buffer'.")

(defvar emms-playlist-mode-window-width -25
  "Width for the emms-playlist-mode pop-up window.")

;; The marker is unique for each playlist buffer
(make-variable-buffer-local
 'emms-playlist-mode-selected-overlay-marker)

(make-variable-buffer-local
 'emms-playlist-mode-selected-overlay)

(defgroup emms-playlist-mode nil
  "*The Emacs Multimedia System playlist mode."
  :prefix "emms-playlist-mode-"
  :group 'multimedia)

;;; --------------------------------------------------------
;;; Faces
;;; --------------------------------------------------------

(defface emms-playlist-track-face
  '((((class color) (background dark))
     :foreground "DarkSeaGreen")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "Blue"))
  "Face for the tracks in a playlist buffer."
  :group 'emms-playlist-mode)

(defface emms-playlist-selected-face
  '((((class color) (background dark))
     :foreground "SteelBlue3")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "blue3"))
  "Face for highlighting the selected track."
  :group 'emms-playlist-mode)

;;; --------------------------------------------------------
;;; Keys
;;; --------------------------------------------------------

(defconst emms-playlist-mode-map
  (let ((emms-playlist-mode-map (make-sparse-keymap)))
    (set-keymap-parent emms-playlist-mode-map text-mode-map)
    (define-key emms-playlist-mode-map (kbd "C-x C-s") 'emms-playlist-mode-save-buffer)
    (define-key emms-playlist-mode-map (kbd "C-y") 'emms-playlist-mode-yank)
    (define-key emms-playlist-mode-map (kbd "C-k") 'emms-playlist-mode-kill-track)
    (define-key emms-playlist-mode-map (kbd "C-w") 'emms-playlist-mode-kill)
    (define-key emms-playlist-mode-map (kbd "C-_") 'emms-playlist-mode-undo)
    (define-key emms-playlist-mode-map (kbd "C-n") 'next-line)
    (define-key emms-playlist-mode-map (kbd "C-p") 'previous-line)
    (define-key emms-playlist-mode-map (kbd "C-j") 'emms-playlist-mode-insert-newline)
    (define-key emms-playlist-mode-map (kbd "M-y") 'emms-playlist-mode-yank-pop)
    (define-key emms-playlist-mode-map (kbd "M-<") 'emms-playlist-mode-first)
    (define-key emms-playlist-mode-map (kbd "M->") 'emms-playlist-mode-last)
    (define-key emms-playlist-mode-map (kbd "d") 'emms-playlist-mode-kill-track)
    (define-key emms-playlist-mode-map (kbd "n") 'emms-next)
    (define-key emms-playlist-mode-map (kbd "p") 'emms-previous)
    (define-key emms-playlist-mode-map (kbd "s") 'emms-stop)
    (define-key emms-playlist-mode-map (kbd "f") 'emms-show)
    (define-key emms-playlist-mode-map (kbd "c") 'emms-playlist-mode-center-current)
    (define-key emms-playlist-mode-map (kbd "q") 'bury-buffer)
    (define-key emms-playlist-mode-map (kbd "?") 'describe-mode)
    (define-key emms-playlist-mode-map (kbd "r") 'emms-random)
    (define-key emms-playlist-mode-map (kbd "<mouse-2>") 'emms-playlist-mode-play-current-track)
    (define-key emms-playlist-mode-map (kbd "RET") 'emms-playlist-mode-play-current-track)
    emms-playlist-mode-map)
  "Keymap for `emms-playlist-mode'.")

(defmacro emms-playlist-mode-move-wrapper (name fun)
  "Create a function NAME which is an `interactive' version of FUN.

NAME should be a symbol.
FUN should be a function."
  `(defun ,name ()
     ,(format "Interactive wrapper around `%s' for playlist-mode."
	      fun)
     (interactive)
     (,fun)))

(emms-playlist-mode-move-wrapper emms-playlist-mode-first
				 emms-playlist-first)

(emms-playlist-mode-move-wrapper emms-playlist-mode-last
				 emms-playlist-last)

(emms-playlist-mode-move-wrapper emms-playlist-mode-select-next
				 emms-playlist-next)

(emms-playlist-mode-move-wrapper emms-playlist-mode-select-previous
				 emms-playlist-previous)

(defun emms-playlist-mode-center-current ()
  "Move point to the currently selected track."
  (interactive)
  (goto-char (or emms-playlist-mode-selected-overlay-marker
		 (point-min))))

(defun emms-playlist-mode-selected-at ()
  "Return t if point is currently on the selected track."
  (eq (emms-playlist-track-at)
      (emms-playlist-selected-track)))

(defun emms-playlist-mode-play-current-track ()
  "Start playing track at point."
  (interactive)
  (emms-playlist-set-playlist-buffer)
  (emms-playlist-select (point))
  (when emms-player-playing-p
    (emms-stop))
  (emms-start))

(defun emms-playlist-mode-switch-buffer ()
  "Switch to the playlist buffer and then switch back if called again.

This function switches to the current Emms playlist buffer and
remembers the buffer switched from. When called again the
function switches back to the remembered buffer."
  (interactive)
  (if (eq (current-buffer)
	  emms-playlist-buffer)
      (switch-to-buffer emms-playlist-mode-switched-buffer)
    (setq emms-playlist-mode-switched-buffer (current-buffer))
    (switch-to-buffer emms-playlist-buffer)))

(defmacro with-inhibit-read-only-t (&rest body)
  "Simple wrapper around `inhibit-read-only'."
  `(let ((inhibit-read-only t))
     ,@body))

(defun emms-playlist-mode-insert-newline ()
  "Insert a newline at point."
  (interactive)
  (with-inhibit-read-only-t
   (newline)))

(defun emms-playlist-mode-undo ()
  "Wrapper around `undo'."
  (interactive)
  (with-inhibit-read-only-t
   (undo)))

;;; --------------------------------------------------------
;;; Overlay compatability
;;; --------------------------------------------------------

(defun find-overlay-emms-track ()
  "Return the position of the next emms track."
  (save-excursion
    (while (and (not (eobp))
		(not (get-char-property (point) 'emms-track)))
      (goto-char (min (next-overlay-change (point))
		      (next-single-property-change (point) 'emms-track))))
    (point)))

;; Taken from CVS Emacs (2005-12-24) and modified to support Emms2 on
;; Emacs 21.4. The modifications make this function not equivalent to
;; the original `remove-overlays' from which it was copied, so don't
;; try to use it in the same way.
(defun remove-all-overlays (&optional beg end)
  "Clear BEG and END of overlays."
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (if (< end beg)
      (setq beg (prog1 end (setq end beg))))
  (save-excursion
    (dolist (o (overlays-in beg end))
      (when (eq (overlay-get o nil) nil)
	(if (< (overlay-start o) beg)
	    (if (> (overlay-end o) end)
		(progn
		  (move-overlay (copy-overlay o)
				(overlay-start o) beg)
		  (move-overlay o end (overlay-end o)))
	      (move-overlay o (overlay-start o) beg))
	  (if (> (overlay-end o) end)
	      (move-overlay o end (overlay-end o))
	    (delete-overlay o)))))))

;;; --------------------------------------------------------
;;; Killing and yanking
;;; --------------------------------------------------------

(defun emms-playlist-mode-between-p (p a b)
  "Return t if P is a point between points A and B."
  (if (or (eq a b)
	  (and (> b a)
	       (= p b)))
      nil
    (eq p (cadr (sort (list a b p) #'<=)))))

;; C-k
;;
;; Currently this kills as regular GNU/Emacs would and not like a
;; typical music player would.
(defun emms-playlist-mode-kill-track ()
  "Kill track at point."
  (interactive)
  (with-inhibit-read-only-t
   (let ((track (emms-playlist-track-at)))
     (if track
	 (let ((track-region (emms-property-region (point)
						   'emms-track)))
	   (when (and emms-player-playing-p
		      (equal (emms-playlist-selected-track) track))
	     (emms-stop))
	   (remove-all-overlays (point-at-bol) (point-at-eol))
	   (kill-region (car track-region) (cdr track-region)))
       (kill-line)))))

;; C-w
(defun emms-playlist-mode-kill ()
  "Kill from mark to point."
  (interactive)
  (with-inhibit-read-only-t
   (let ((m (mark))			; throw error if no mark
	 (p (point))
	 (sm emms-playlist-selected-marker))
     ;; Are we killing the playing/selected track?
     (when (emms-playlist-mode-between-p
	    (marker-position sm) m p)
       (setq emms-playlist-selected-marker nil)
       (setq emms-playlist-mode-selected-overlay-marker nil)
       (emms-stop))
     (kill-region p m))))

;; C-y
(defun emms-playlist-mode-yank ()
  "Yank into the playlist buffer."
  (interactive)
  (with-inhibit-read-only-t
   (goto-char (point-at-bol))
   (yank))
  (emms-playlist-mode-overlay-refresh))

;; M-y
(defun emms-playlist-mode-yank-pop ()
  "Cycle through the kill-ring."
  (interactive)
  (with-inhibit-read-only-t
   (yank-pop)))

;;; --------------------------------------------------------
;;; Overlay
;;; --------------------------------------------------------

(defun emms-playlist-mode-overlay-face (ovly face priority)
  "Place the overlay OVLY with the face FACE and priority PRIORITY."
  (overlay-put ovly 'face face)
  (overlay-put ovly 'priority priority))

(defun emms-playlist-mode-overlay-track (start end face priority)
  "Place the overlay starting at START and ending at END over FACE.

START and END should points.
FACE should be a... face."
  (let ((overl (make-overlay start end)))
    (emms-playlist-mode-overlay-face overl face priority) overl))

(defun emms-playlist-mode-overlay-at-point (face priority)
  (let ((region (emms-property-region (point) 'emms-track)))
    (emms-playlist-mode-overlay-track (car region)
				      (cdr region)
				      face
				      priority)))

(defun emms-playlist-mode-overlay-selected ()
  "Place an overlay over the currently selected track."
  (emms-playlist-mode-remove-overlay-selected)
  (when (not (and (null emms-playlist-selected-marker)
		  (null emms-playlist-mode-selected-overlay-marker))) ; ugh
    (save-excursion
      (goto-char emms-playlist-selected-marker)
      (setq emms-playlist-mode-selected-overlay-marker
	    (point-marker))
      (setq emms-playlist-mode-selected-overlay
	    (emms-playlist-mode-overlay-at-point
	     'emms-playlist-selected-face 2)))))

(defun emms-playlist-mode-remove-overlay-selected ()
  "Remove the overlay from the currently selected track"
  (when (not (null emms-playlist-mode-selected-overlay-marker))
    (save-excursion
      (goto-char emms-playlist-mode-selected-overlay-marker)
      (remove-all-overlays (point-at-bol)
			   (point-at-eol))
      (emms-playlist-mode-overlay-at-point
       'emms-playlist-track-face 1))))

(defun emms-playlist-mode-overlay-all ()
  "Place an low-priority overlay over the entire buffer."
  (emms-playlist-mode-overlay-track (point-min)
				    (point-max)
				    'emms-playlist-track-face
				    1))

;; not graceful, but avoids growing as the number of tracks grow.
(defun emms-playlist-mode-overlay-refresh ()
  "Remove and re-apply all the overlays in the buffer."
  (remove-all-overlays (point-min)
		       (point-max))
  (emms-playlist-mode-overlay-all)
  (setq emms-playlist-mode-selected-overlay-marker nil)
  (emms-playlist-mode-overlay-selected))

;;; --------------------------------------------------------
;;; Saving/Restoring
;;; --------------------------------------------------------

(defun emms-playlist-mode-save-buffer (buffer filename)
  "Saves a playlist buffer in a file, including annotations."
  (interactive "bPlaylist buffer to save: \nFFile to save buffer as: ")
  (with-current-buffer (find-file-noselect filename)
    (erase-buffer)
    (prin1 (with-current-buffer buffer
	     (buffer-string))
	   (current-buffer))
    (save-buffer)
    (kill-buffer (current-buffer))))

(defun emms-playlist-mode-open-buffer (filename)
  "Opens a previously saved playlist buffer.

It creates a buffer called \"filename\", and restore the contents
of the saved playlist inside."
  (interactive "fFile: ")
  (let* ((s)
	 (buffer (find-file-noselect filename))
	 (name   (buffer-name buffer)))
    (with-current-buffer buffer
      (setq s (read (buffer-string))))
    (kill-buffer buffer)
    (with-current-buffer (emms-playlist-new name)
      (with-inhibit-read-only-t
       (insert s)
       (condition-case nil
	   (progn
	     (emms-playlist-first)
	     (emms-playlist-update-track)
	     (while t
	       (emms-playlist-next)
	       (emms-playlist-update-track)))
	 (error
	  nil)))
      (emms-playlist-first)
      (emms-playlist-select (point))
      (switch-to-buffer (current-buffer)))))

(defun emms-playlist-mode-save-active-buffer (filename)
  "Saves the active playlist buffer to a file."
  (interactive "FFile to save playlist buffer as: ")
  (emms-playlist-mode-save-buffer emms-playlist-buffer filename))

;;; --------------------------------------------------------
;;; Local functions
;;; --------------------------------------------------------

(defun emms-playlist-mode-insert-track (track)
  "Insert the description of TRACK at point."
  (emms-playlist-ensure-playlist-buffer)
  (with-inhibit-read-only-t
   (insert (propertize (emms-track-description track)
		       'emms-track track))
   (save-restriction
     (widen)
     (let ((p (emms-property-region (point-at-bol) 'emms-track))
	   (c (if (equal (emms-playlist-current-selected-track)
			 (get-text-property (point-at-bol) 'emms-track))
		  (cons 'emms-playlist-selected-face 2)
		(cons 'emms-playlist-track-face 1))))
       (emms-playlist-mode-overlay-track (car p) (cdr p)
					 (car c) (cdr c))))
   (insert "\n")))

(defun emms-playlist-mode-update-track-function ()
  "Update the track display at point."
  (emms-playlist-ensure-playlist-buffer)
  (with-inhibit-read-only-t
   (let ((track-region (emms-property-region (point)
					     'emms-track))
	 (track (get-text-property (point)
				   'emms-track)))
     (save-excursion
       (delete-region (car track-region)
		      ;; 1+ For the \n
		      (1+ (cdr track-region)))
       (emms-playlist-mode-insert-track track)))))

;;; --------------------------------------------------------
;;; Entry
;;; --------------------------------------------------------

(defun emms-playlist-mode-go ()
  "Switch to the current emms-playlist buffer and use emms-playlist-mode."
  (interactive)
  (if (or (null emms-playlist-buffer)
	  (not (buffer-live-p emms-playlist-buffer)))
      (error "No current Emms buffer")
    (switch-to-buffer emms-playlist-buffer)
    (when (and (not (eq major-mode 'emms-playlist-mode))
	       emms-playlist-buffer-p)
      (emms-playlist-mode))))

(defun emms-playlist-mode-go-popup (&optional window-width)
  "Popup emms-playlist buffer as a side window. Default value for
WINDOW-WIDTH is `emms-playlist-mode-window-width'."
  (interactive)
  (setq emms-playlist-mode-window-width
	(or window-width emms-playlist-mode-window-width))
  (split-window-horizontally emms-playlist-mode-window-width)
  (other-window 1)
  (emms-playlist-mode-go))

(defun emms-playlist-mode-startup ()
  "Instigate emms-playlist-mode on the current buffer."
  ;; when there is neither a current emms track or a playing one...
  (when (not (or emms-playlist-selected-marker
		 emms-player-playing-p))
    ;; ...then stop the player.
    (emms-stop)
    ;; why select the first track?
    (when emms-playlist-buffer-p
      (emms-playlist-select-first)))
  ;; when there is a selected track.
  (when emms-playlist-selected-marker
    (emms-playlist-mode-overlay-selected)
    (goto-char (or emms-playlist-mode-selected-overlay-marker
		   (point-min))))
  (setq buffer-read-only t))

;;;###autoload
(defun emms-playlist-mode ()
  "A major mode for Emms playlists.
\\{emms-playlist-mode-map}."
  (interactive)
  (let ((val emms-playlist-buffer-p))
    (kill-all-local-variables)
    (setq emms-playlist-buffer-p val))

  (use-local-map emms-playlist-mode-map)
  (setq major-mode 'emms-playlist-mode
	mode-name "Emms-Playlist")

  (setq emms-playlist-insert-track-function
	'emms-playlist-mode-insert-track)
  (setq emms-playlist-update-track-function
	'emms-playlist-mode-update-track-function)
  ;; Not used yet
  ;; (setq emms-playlist-delete-track-function
  ;;       ...)
  (add-hook 'emms-playlist-selection-changed-hook
	    'emms-playlist-mode-overlay-selected)

  (emms-playlist-mode-startup)

  (run-hooks 'emms-playlist-mode-hooks))

(provide 'emms-playlist-mode)

;;; emms-playlist-mode.el ends here
