;;; emms-metaplaylist-mode.el --- A major mode for lists of Emms
;;; playlists

;;; Copyright 2005, by Yoni Rabkin under the GNU General Public
;;; License.

;;; Commentary:
;;
;; `emms-metaplaylist-mode' creates an interactive list of all the
;; Emms playlist buffers. The currently active buffer is
;; highlighted. You can choose a buffer from the list with RET and get
;; taken there.

;;; Code:

(require 'emms-playlist-mode)

;;; --------------------------------------------------------
;;; Variables, customisation and faces
;;; --------------------------------------------------------

(defgroup emms-metaplaylist-mode nil
  "*The Emacs Multimedia System meta-playlist mode."
  :prefix "emms-metaplaylist-mode-"
  :group 'multimedia)

(defcustom emms-metaplaylist-mode-buffer-name "*Emms Playlists*"
  "*Name of the buffer in which Emms playlists will be listed."
  :type 'string
  :group 'emms-metaplaylist-mode)

(defcustom emms-metaplaylist-mode-hooks nil
  "*List of hooks to run on entry to emms-metaplaylist-mode."
  :type 'list
  :group 'emms-metaplaylist-mode)

(defface emms-metaplaylist-mode-face
  '((((class color) (background dark))
     :foreground "AntiqueWhite3")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "WhiteSmoke"))
  "Face for the buffer names in the playlists buffer."
  :group 'emms-metaplaylist-mode)

(defface emms-metaplaylist-mode-current-face
  '((((class color) (background dark))
     :foreground "red2")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "red3"))
  "Face for the current buffer name in the playlists buffer."
  :group 'emms-metaplaylist-mode)

;;; --------------------------------------------------------
;;; Keymap
;;; --------------------------------------------------------

(defconst emms-metaplaylist-mode-map
  (let ((emms-metaplaylist-mode-map (make-sparse-keymap)))
    (set-keymap-parent emms-metaplaylist-mode-map text-mode-map)
    (define-key emms-metaplaylist-mode-map (kbd "n") 'next-line)
    (define-key emms-metaplaylist-mode-map (kbd "p") 'previous-line)
    (define-key emms-metaplaylist-mode-map (kbd "RET") 'emms-metaplaylist-mode-goto-current)
    (define-key emms-metaplaylist-mode-map (kbd "q") 'kill-buffer)
    (define-key emms-metaplaylist-mode-map (kbd "?") 'describe-mode)
    emms-metaplaylist-mode-map)
  "Keymap for `emms-metaplaylist-mode'.")

;;; --------------------------------------------------------
;;; Metaplaylist
;;; --------------------------------------------------------

(defun emms-metaplaylist-mode-goto-current ()
  "Switch to the buffer at point."
  (interactive)
  (switch-to-buffer
   (buffer-substring (point-at-bol)
		     (point-at-eol))))

(defun get-emms-playlist-buffers ()
  "Return a list of EMMS playlist buffers."
  (let ((lis nil))
    (mapc (lambda (buf)
	    (with-current-buffer buf
	      (when emms-playlist-buffer-p
		(setq lis (cons buf lis)))))
	  (buffer-list))
    lis))

;; Since there will never be a significantly large amount of playlist
;; buffers co-existing at once, we allow ourselves not to keep
;; state. We regenerate the playlists buffer anew on demand.
(defun emms-metaplaylist-mode-create ()
  "Create or recreate the meta-playlist buffer."
  (let ((name emms-metaplaylist-mode-buffer-name)
	(playlists (get-emms-playlist-buffers)))
    (if playlists
	(progn
	  (ignore-errors (kill-buffer name))
	  (get-buffer-create name)
	  (with-current-buffer name
	    (emms-metaplaylist-mode)
	    (save-excursion
	      (mapc (lambda (buf)
		      (let ((inhibit-read-only t))
			(insert (buffer-name buf))
			(emms-playlist-mode-overlay-track
			 (point-at-bol)
			 (point-at-eol)
			 (if (eq buf emms-playlist-buffer)
			     'emms-metaplaylist-mode-face
			   'emms-metaplaylist-mode-current-face
			   1)
			 (newline))))
		    playlists))
	    (current-buffer)))	       ; return the buffer as lisp obj
      (error "No Emms playlist buffers"))))

;;; --------------------------------------------------------
;;; Mode entry
;;; --------------------------------------------------------

(defun emms-metaplaylist-mode-go ()
  "Single entry point to the metaplaylist interface."
  (interactive)
  (emms-metaplaylist-mode-create)
  (switch-to-buffer emms-metaplaylist-mode-buffer-name))

(defun emms-metaplaylist-mode ()
  "A major mode for Emms playlists."
  (interactive)
  (kill-all-local-variables)

  (use-local-map emms-metaplaylist-mode-map)
  (setq major-mode 'emms-metaplaylist-mode
	mode-name "Emms-MetaPlaylist")

  (setq buffer-read-only t)

  (run-hooks 'emms-metaplaylist-mode-hooks))

(provide 'emms-metaplaylist-mode)

;;; emms-metaplaylist-mode.el ends here
