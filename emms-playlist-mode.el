;;; emms-playlist-mode.el --- Playlist mode for Emms.

;;; Copyright Yoni Rabkin under the GNU General Public License.
;;; This file is a part of the Emacs Multimedia system.

;;; Commentary:
;;;
;;; I'm designing this as a method of displaying and manipulating the
;;; different Emms playlist buffers defined by the user.
;;;
;;; Emms developer's motto:
;;; "When forcer say (require 'jump) we (how-high-p)"
;;;
;;; Feature requests:
;;;
;;; (1) Lukhas wants `emms-list-playlist-buffers' to list all the
;;;     playlist buffers.
;;;
;;; (2) Arbitrary comment entry with color overlays.

;;; Code:

;;; --------------------------------------------------------
;;; Variables
;;; --------------------------------------------------------

(defvar emms-playlist-mode-hook nil
  "Emms playlist mode hook.")

(defvar emms-playlist-mode-selected-overlay-marker nil
  "Marker for last selected track.  Use for updating the display.")

;; The marker is unique for each playlist buffer
(make-variable-buffer-local
 'emms-playlist-mode-selected-overlay-marker)

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
  "Basic face for highlighting tracks in a playlist buffer."
  :group 'emms-playlist-mode)

(defface emms-playlist-selected-face
  '((((class color) (background dark))
     :foreground "SteelBlue3")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "blue3"))
  "Basic face for highlighting the selected track."
  :group 'emms-playlist-mode)

;;; --------------------------------------------------------
;;; Keys
;;; --------------------------------------------------------

(defconst emms-playlist-mode-map
  (let ((emms-playlist-mode-map (make-sparse-keymap)))
    (set-keymap-parent emms-playlist-mode-map text-mode-map)
    (define-key emms-playlist-mode-map (kbd "n") 'emms-next)
    (define-key emms-playlist-mode-map (kbd "p") 'emms-previous)
    (define-key emms-playlist-mode-map (kbd "C-x C-s") 'emms-playlist-save-buffer)
    (define-key emms-playlist-mode-map (kbd "C-k") 'emms-playlist-mode-kill-track)
    (define-key emms-playlist-mode-map (kbd "C-y") 
      #'(lambda () (interactive) (emms-playlist-mode-insert-last-killed-track kill-ring)))
    (define-key emms-playlist-mode-map (kbd "d") 'emms-playlist-mode-kill-track)
    (define-key emms-playlist-mode-map (kbd "s") 'emms-stop)
    (define-key emms-playlist-mode-map (kbd "f") 'emms-show)
    (define-key emms-playlist-mode-map (kbd "c") 'emms-playlist-mode-center-current)
    (define-key emms-playlist-mode-map (kbd "C") 'emms-playlist-clear)
    (define-key emms-playlist-mode-map (kbd "RET") 'emms-playlist-mode-play-current-track)
    (define-key emms-playlist-mode-map (kbd "q") 'bury-buffer)
    (define-key emms-playlist-mode-map (kbd "<mouse-2>") 'emms-playlist-mode-play-current-track)
    (define-key emms-playlist-mode-map (kbd "?") 'describe-mode)
    (define-key emms-playlist-mode-map (kbd "M-<") 'emms-playlist-mode-first)
    (define-key emms-playlist-mode-map (kbd "M->") 'emms-playlist-mode-last)
    (define-key emms-playlist-mode-map (kbd "C-n") 'emms-playlist-mode-select-next)
    (define-key emms-playlist-mode-map (kbd "C-p") 'emms-playlist-mode-select-previous)
    (define-key emms-playlist-mode-map (kbd "r") 'emms-random)
    emms-playlist-mode-map)
  "Keymap for `emms-playlist-mode'.")

;; We will add to this wrapper boundry checking as needed later.
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
  (emms-playlist-select (point))
  (when emms-player-playing-p
    (emms-stop))
  (emms-start))

(defun emms-playlist-mode-insert-last-killed-track (ring)
  (if (null ring)
      (error "No last killed track")
    (let ((track nil))
      (with-temp-buffer
	(insert (car ring))
	(setq track (get-text-property (point-min) 'emms-track)))
      (if track
	  (emms-playlist-insert-track track)
	(emms-playlist-mode-insert-last-killed-track (cdr ring))))))

;; The logic for killing tracks in an interactive manner is
;; suprisingly annoying
(defun emms-playlist-mode-kill-track ()
  "Kill the track at point."
  (interactive)
  (let ((region (emms-property-region (point) 'emms-track))
	(inhibit-read-only t))
    (cond ((not (emms-playlist-track-at))
	   (kill-line 1))
	  ((and (not (emms-playlist-mode-selected-at))
		(emms-playlist-track-at))
	   (kill-region (car region)
			(cdr region))
	   (emms-playlist-mode-kill-track))
	  ((and emms-player-playing-p
		(emms-playlist-mode-selected-at))
	   (emms-stop)
	   (emms-next-noerror)
	   (kill-region (car region)
			(cdr region))
	   (emms-playlist-mode-kill-track))
	  ((and (not emms-player-playing-p)
		(emms-playlist-mode-selected-at))
	   (kill-region (car region)
			(cdr region))
	   (emms-playlist-mode-kill-track))
	  (t (error "Cannot kill content at point")))))

;;; --------------------------------------------------------
;;; Overlay
;;; --------------------------------------------------------

(defun emms-playlist-mode-overlay-face (ovly face priority)
  (overlay-put ovly 'face face)
  (overlay-put ovly 'priority priority))

(defun emms-playlist-mode-overlay-track (start end face priority)
  "Place the overlay starting at START and ending at END over FACE.

START and END should points.
FACE should be a... face."
  (let ((o (make-overlay start end)))
    (emms-playlist-mode-overlay-face o face priority)))

(defun emms-playlist-mode-overlay-unselected ()
  ;; point-mix/max because -insert-source narrows the world
  (emms-playlist-mode-overlay-track (point-min)
				    (point-max)
				    'emms-playlist-track-face
				    1))

(defun emms-playlist-mode-overlay-selected ()
  "Place an overlay over the currently selected track."
  (unless (null emms-playlist-mode-selected-overlay-marker)
    (save-excursion
      (goto-char emms-playlist-mode-selected-overlay-marker)
      (remove-overlays (point-at-bol)
		       (point-at-eol))
      (emms-playlist-mode-overlay-track (point-at-bol)
					(point-at-eol)
					'emms-playlist-track-face
					2)))
  (save-excursion
    (goto-char emms-playlist-selected-marker)
    (setq emms-playlist-mode-selected-overlay-marker
	  (point-marker))
    (emms-playlist-mode-overlay-track (point-at-bol)
				      (point-at-eol)
				      'emms-playlist-selected-face
				      3))
  nil)

;;; --------------------------------------------------------
;;; Hooks
;;; --------------------------------------------------------

(add-hook 'emms-playlist-source-inserted-hook
	  'emms-playlist-mode-overlay-unselected)

(add-hook 'emms-playlist-selection-changed-hook
	  'emms-playlist-mode-overlay-selected)

;;; --------------------------------------------------------
;;; Playlists
;;; --------------------------------------------------------

(defun emms-playlist-buffers ()
  "Return a list of EMMS playlist buffers."
  (let ((lis nil))
    (mapc (lambda (buf)
	    (with-current-buffer buf
	      (when emms-playlist-buffer-p
		(setq lis (cons buf lis)))))
	  (buffer-list))
    lis))

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
      (let ((inhibit-read-only t))
        (insert s))
      (emms-playlist-first)
      (emms-playlist-select (point)))))

(defun emms-playlist-mode-save-active-buffer (filename)
  "Saves the active playlist buffer to a file."
  (interactive "FFile to save playlist buffer as: ")
  (emms-playlist-mode-save-buffer emms-playlist-buffer filename))

;;; --------------------------------------------------------
;;; Entry
;;; --------------------------------------------------------

(defun emms-playlist-mode-go ()
  "Switch to the current emms-playlist buffer and use emms-playlist-mode."
  (interactive)
  (if (null emms-playlist-buffer)
      (error "No current Emms buffer")
    (switch-to-buffer emms-playlist-buffer)
    (when (and (not (eq major-mode 'emms-playlist-mode))
	       emms-playlist-buffer-p)
      (emms-playlist-mode))))

(defun emms-playlist-mode-startup ()
  "Instigate emms-playlist-mode on the current buffer."
  (when (not (or emms-playlist-selected-marker
		 emms-player-playing-p))
    (emms-stop)
    (when emms-playlist-buffer-p
      (emms-playlist-select-first)))
  (when emms-playlist-selected-marker
    (emms-playlist-mode-overlay-selected)
    (goto-char (or emms-playlist-mode-selected-overlay-marker
		   (point-min))))
  (setq buffer-read-only t))

;;;###autoload
(defun emms-playlist-mode ()
  "A major mode for Emms playlists."
  (interactive)
  (kill-all-local-variables)

  (use-local-map emms-playlist-mode-map)
  (setq major-mode 'emms-playlist-mode
	mode-name "Emms-Playlist")

  (emms-playlist-mode-startup)

  (run-hooks 'emms-playlist-mode-hooks))

(provide 'emms-playlist-mode)

;;; emms-playlist-mode.el ends here
