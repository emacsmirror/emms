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
;;; (2) Add emms-info support which re-writes the track titles.
;;;     This will be implemented externally [forcer].
;;;
;;; (3) Add multi-line formatting and arbitrary comment entry.
;;;
;;; (4) Font-locking for unselected tracks via overlays.

;;; --------------------------------------------------------
;;; Variables
;;; --------------------------------------------------------

(defvar emms-playlist-mode-hook nil
  "Emms playlist mode hook.")

(defvar emms-playlist-mode-selected-overlay-marker nil)

(make-variable-buffer-local
 'emms-playlist-mode-selected-overlay-marker)

;; Is this needed at all?
(defvar emms-playlist-mode-prepend-show-string
  "Now Playling:")

(defgroup emms-playlist-mode nil
  "*The Emacs Multimedia System playlist mode."
  :prefix "emms-playlist-mode-"
  :group 'multimedia)

;;; --------------------------------------------------------
;;; Faces and font locking
;;; --------------------------------------------------------

;; change the eye-gouging colors before release
(defface emms-playlist-track-face
  '((((class color) (background dark))
     :background "yellow")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "red"))
  "Basic face for highlighting the selected track."
  :group 'emms-playlist-mode)

(defface emms-playlist-selected-face
  '((((class color) (background dark))
     :background "blue1")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "blue3"))
  "Basic face for highlighting the region of the track."
  :group 'emms-playlist-mode)

;; FIXME: automatic font-locking does not work. See below.
(defvar emms-playlist-mode-font-lock-keywords
  '(("\\(.*\\)" (1 emms-playlist-track-face)))
  "Keyword highlighting specification for `emms-playlist-mode'.")

;;; --------------------------------------------------------
;;; Keys
;;; --------------------------------------------------------

(defconst emms-playlist-mode-map
  (let ((emms-playlist-mode-map (make-sparse-keymap)))
    (set-keymap-parent emms-playlist-mode-map text-mode-map)
    (define-key emms-playlist-mode-map (kbd "n") 'emms-next)
    (define-key emms-playlist-mode-map (kbd "p") 'emms-previous)
    (define-key emms-playlist-mode-map (kbd "C-x C-s") 'emms-playlist-save)
    (define-key emms-playlist-mode-map (kbd "C-k") 'emms-playlist-mode-kill-track)
    (define-key emms-playlist-mode-map (kbd "d") 'emms-playlist-mode-kill-track)
    (define-key emms-playlist-mode-map (kbd "s") 'emms-stop)
    (define-key emms-playlist-mode-map (kbd "f") 'emms-show)
    (define-key emms-playlist-mode-map (kbd "c") 'emms-playlist-mode-center-current)
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
  "Keymap for emms-playlist-mode.")

;; We will add to this wrapper boundry checking as needed later.
(defmacro emms-playlist-mode-move-wrapper (name fun)
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
  (interactive)
  (goto-char (or emms-playlist-mode-selected-overlay-marker
		 (point-min))))

(defun emms-playlist-mode-selected-at ()
  (eq (emms-playlist-track-at) 
      (emms-playlist-selected-track)))

(defun emms-playlist-mode-play-current-track ()
  (interactive)
  (emms-playlist-select (point))
  (when emms-player-playing-p
    (emms-stop))
  (emms-start))

;; The logic for killing tracks in an interactive manner is
;; suprisingly annoying
(defun emms-playlist-mode-kill-track ()
  (interactive)
  (let ((region (emms-property-region (point) 'emms-track)))
    (cond ((not (emms-playlist-track-at)) 
	   (kill-line 1))	     ; Purposfully kills only one line
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

(defun emms-playlist-mode-overlay-track (start end face)
  (let ((o (make-overlay start end)))
    (overlay-put o 'track t)
    (overlay-put o 'face face)))

(defun emms-playlist-mode-overlay-at (face)
  (let ((o (make-overlay (point-at-bol) (point-at-eol))))
    (overlay-put o 'track t)
    (overlay-put o 'face face)))

;; Selected track overlaying track in constant time.
(defun emms-playlist-mode-overlay-selected ()
  (unless (null emms-playlist-mode-selected-overlay-marker)
    (save-excursion
      (goto-char emms-playlist-mode-selected-overlay-marker)
      (remove-overlays (point) 
		       (point-at-eol))))
  (save-excursion
    (goto-char emms-playlist-selected-marker)
    (setq emms-playlist-mode-selected-overlay-marker 
	  (point-marker)) 
    (emms-playlist-mode-overlay-at
     'emms-playlist-selected-face))
  nil)

;;; --------------------------------------------------------
;;; Hooks
;;; --------------------------------------------------------

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
    (with-current-buffer (get-buffer-create name)
      (insert s))))

(defun emms-playlist-mode-save-active-buffer (filename)
  "Saves the active playlist buffer to a file."
  (interactive "FFile to save playlist buffer as: ")
  (emms-playlist-mode-save-buffer emms-playlist-buffer filename))

;;; --------------------------------------------------------
;;; Entry
;;; --------------------------------------------------------

(defun emms-playlist-mode-go ()
  (interactive)
  (if (null emms-playlist-buffer)
      (error "No current Emms buffer")
    (switch-to-buffer emms-playlist-buffer)
    (when (and (not (eq major-mode 'emms-playlist-mode))
	       emms-playlist-buffer-p)
      (emms-playlist-mode))))

(defun emms-playlist-mode-startup ()
  (unless (or emms-playlist-selected-marker
	      emms-player-playing-p)
    (emms-stop)
    (emms-playlist-select-first))
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

  ;; FIXME: *automatic* font-locking does not work and I have no idea
  ;; why. Anyone who wants to fix it is more than welcomed to it.
  (set (make-local-variable 'font-lock-defaults)
       '(emms-playlist-mode-font-lock-keywords))

  (emms-playlist-mode-startup)

  (run-hooks 'emms-playlist-mode-hooks))

(provide 'emms-playlist-mode)

;;; emms-playlist-mode.el ends here.
