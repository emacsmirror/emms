;;; emms-browser.el --- provide a buffer to browse files

;; Copyright (C) 2006  Damien Elmes <emacs@repose.cx>

;; Author: Damien Elmes <emacs@repose.cx>
;; Keywords: emms, mp3, mpeg, multimedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code allows you to browse the metadata cache and add tracks to
;; your playlist. To be properly useful, you should M-x
;; emms-add-directory-tree to all the files you own at least once so
;; that the cache is fully populated.

;; To use, run (emms-devel) and then bind emms-smart-browse to a key,
;; like:

;; (global-set-key (kbd "<f2>") 'emms-smart-browse)

;; The 'smart browsing' code attempts to link the browser and playlist
;; windows together, so that closing one will close both. Activating
;; it will toggle between three states:

;; a) both windows displayed, with the browser focused
;; b) focus switched to the playlist window
;; c) the extra window closed, and both buffers buried

;; Some useful keybindings in the browser buffer:

;; SPC      - add all the tracks on the current line to the playlist
;; RET      - do the same, and start the first added track playing
;; /        - isearch through the available items
;; q        - bury both buffers (if you use emms-smart-browse)

;; If you just want access to the browser, try M-x
;; emms-browse-by-TYPE, where TYPE is one of artist, album, genre or
;; year.

;; If you don't want to activate the code with (emms-devel), you can
;; activate it manually with:

;; (require 'emms-browser)

;;; Code:

(require 'emms)
(require 'emms-cache)
(require 'emms-source-file)

;; --------------------------------------------------
;; Variables and configuration
;; --------------------------------------------------

(defgroup emms-browser nil
  "*The Emacs Multimedia System browser"
  :prefix "emms-browser-"
  :group 'multimedia
  :group 'applications)

(defcustom emms-browser-default-browsing-function
  'emms-browse-by-artist
  "The default browsing mode."
  :group 'emms-browser
  :type 'function)

(defcustom emms-browser-comparison-test
  'case-fold
  "A method for comparing entries in the cache.
The default is to compare case-insensitively."
  :group 'emms-browser
  :type 'symbol)

(defcustom emms-browser-sort-function
  'emms-sort-natural-order-less-p
  "How to sort tracks from the browser (nil for no sorting).
This is used to sort tracks when they are added to the playlist."
  :group 'emms-browser
  :type 'function)

(defcustom emms-browser-show-display-hook nil
  "Hooks to run when starting or switching to a browser buffer."
  :group 'emms-browser
  :type 'hook)

(defcustom emms-browser-hide-display-hook nil
  "Hooks to run when burying or removing a browser buffer."
  :group 'emms-browser
  :type 'hook)

(defcustom emms-browser-tracks-added-hook nil
  "Hooks to run when tracks are added to the playlist."
  :group 'emms-browser
  :type 'hook)

(defvar emms-browser-buffer nil
  "The current browser buffer, if any.")

(defvar emms-browser-buffer-name "*EMMS Browser*"
  "The default buffer name.")

(defvar emms-browser-current-mapping nil
  "The current mapping db, eg. artist -> track.")
(make-variable-buffer-local 'emms-browser-current-mapping)

(defconst emms-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "q") 'emms-browser-bury-buffer)
    (define-key map (kbd "/") 'emms-isearch-buffer)
    (define-key map (kbd "?") 'describe-mode)
    (define-key map (kbd "RET") 'emms-browser-add-tracks-and-play)
    (define-key map (kbd "SPC") 'emms-browser-add-tracks)
    map)
  "Keymap for `emms-browser-mode'.")

(defface emms-browser-tracks-face
  '((((class color) (background dark))
     (:foreground "plum"))
    (((class color) (background light))
     (:foreground "Blue"))
    (((type tty) (class mono))
     (:inverse-video t))
    (t (:background "Blue")))
  "Face for the tracks in a playlist buffer."
  :group 'emms-browser-mode)

;; --------------------------------------------------
;; General mode setup
;; --------------------------------------------------

(defun emms-browser ()
  "Launch or switch to the EMMS Browser."
  (interactive)
  (emms-browser-create-or-focus
   emms-browser-default-browsing-function))

(defun emms-browser-create-or-focus (browse-func)
  "Create a new browser buffer with BROWSE-FUNC, or switch.
BROWSE-FUNC should fill the buffer with something of interest. An
example function is `emms-browse-by-artist'."
  (let ((buf (emms-browser-get-buffer))
        wind)
    (if buf
        ;; if the buffer is displayed, switch the window instead
        (progn
          (setq wind (get-buffer-window buf))
          (if wind
              (select-window wind)
            (switch-to-buffer buf))
          (run-mode-hooks 'emms-browser-show-display-hook))
      ;; if there's no buffer, create a new window
      (emms-browser-create)
      (funcall browse-func))))

(defun emms-browser-create ()
  "Create a new emms-browser buffer and start emms-browser-mode."
  (emms-browser-new-buffer)
  (emms-browser-mode)
  (run-mode-hooks 'emms-browser-show-display-hook))

(defun emms-browser-mode ()
  "A major mode for the Emms browser.
\\{emms-browser-mode-map}"
      ;; create a new buffer
  (interactive)

  (use-local-map emms-browser-mode-map)
  (setq major-mode 'emms-browser-mode
        mode-name "Emms-Browser")

  (setq buffer-read-only t)
  (setq emms-browser-buffer (current-buffer)))

(defun emms-browser-new-buffer ()
  "Create a new browser buffer, and switch to it."
  (switch-to-buffer (generate-new-buffer
                     emms-browser-buffer-name)))

(defun emms-browser-clear ()
  "Create or switch to a browser buffer, clearing it."
  (let ((buf (emms-browser-get-buffer)))
    (if buf
        (progn
          (switch-to-buffer buf)
          (emms-with-inhibit-read-only-t
           (delete-region (point-min) (point-max))))
      (emms-browser-create))))

(defun emms-browser-get-buffer ()
  "Return the current buffer if it exists, or nil."
  (unless (or (null emms-browser-buffer)
              (not (buffer-live-p emms-browser-buffer)))
    emms-browser-buffer))

(defun emms-browser-ensure-browser-buffer ()
  (unless (eq major-mode 'emms-browser-mode)
    (error "Current buffer is not an emms-browser buffer")))

(defun emms-browser-bury-buffer ()
  "Bury the browser buffer, running hooks."
  (interactive)
  (run-mode-hooks 'emms-browser-hide-display-hook)
  (bury-buffer))

;; --------------------------------------------------
;; Browsing methods - by artist/album/etc
;; --------------------------------------------------

(defmacro emms-browser-add-category (name track-type)
  "Create an interactive function emms-browse-by-NAME."
  (let ((funname (intern (concat "emms-browse-by-" name)))
        (modedesc (concat "Browsing by: " name))
        (funcdesc (concat "Browse by " name ".")))
  `(defun ,funname ()
     ,funcdesc
     (interactive)
     (emms-browser-clear)
     (rename-buffer ,modedesc)
     (emms-browser-display-by ,track-type)
     (goto-char (point-min)))))

(emms-browser-add-category "artist" 'info-artist)
(emms-browser-add-category "album" 'info-album)
(emms-browser-add-category "genre" 'info-genre)
(emms-browser-add-category "year" 'info-year)

(defun emms-browser-make-by (field-type)
  "Make a mapping with FIELD-TYPE, eg artist -> tracks."
  (let ((db (make-hash-table
             :test emms-browser-comparison-test))
        field existing-entry)
    (maphash (lambda (path track)
               (setq field (emms-track-get track field-type "missing-tag"))
               (setq existing-entry (gethash field db))
               (if existing-entry
                   (puthash field (cons track existing-entry) db)
                 (puthash field (list track) db)))
             emms-cache-db)
    db))

(defun emms-browser-display-by (field-type)
  "Render a mapping into a browser buffer."
  (let ((db (emms-browser-make-by field-type)))
    (maphash (lambda (field track)
               (emms-browser-insert-entry field track))
             db)
    (setq emms-browser-current-mapping db)
    (emms-with-inhibit-read-only-t
     (let ((sort-fold-case t))
       (sort-lines nil (point-min) (point-max))))))

(defun case-fold-string= (a b)
  (compare-strings a nil nil b nil nil t))

(defun case-fold-string-hash (a)
  (sxhash (upcase a)))

(define-hash-table-test 'case-fold
  'case-fold-string= 'case-fold-string-hash)

;; --------------------------------------------------
;; Operations on individual lines
;; --------------------------------------------------

(defun emms-browser-insert-entry (entry tracks)
  "Add a single ENTRY -> TRACKS mapping to the buffer."
  (emms-browser-ensure-browser-buffer)
  (emms-with-inhibit-read-only-t
   (insert (emms-propertize entry
                            'emms-tracks tracks
                            'face 'emms-browser-tracks-face) "\n")))

(defun emms-browser-add-tracks ()
  "Add all the tracks on the current line to the playlist."
  (interactive)
  (let ((tracks (emms-browser-tracks-at))
        (count 0)
        old-max new-max type name)
    (unless tracks
      (error "No tracks on current line!"))
    (with-current-emms-playlist
      (setq old-max (point-max)))
    ;; add each of the tracks
    (dolist (track tracks)
      (setq type (emms-track-get track 'type))
      (setq name (emms-track-get track 'name))
      (cond
       ((eq type 'file)
        (emms-add-file name))
       ((eq type 'url)
        (emms-add-url name)))
      (setq count (1+ count)))
    ;; sort
    (when emms-browser-sort-function
      (with-current-emms-playlist
        (setq new-max (point-max)))
      (when (fboundp 'emms-playlist-sort)
        (emms-playlist-sort emms-browser-sort-function
                            old-max new-max)))
    (run-mode-hooks 'emms-browser-tracks-added-hook)
    (message "Added %d tracks." count)))

(defun emms-browser-add-tracks-and-play ()
  "Add all the tracks on the current line, play the first file."
  (interactive)
  (let (old-pos)
    (with-current-emms-playlist
      (setq old-pos (point-max)))
    (emms-browser-add-tracks)
    (with-current-emms-playlist
      (goto-char old-pos)
      (emms-playlist-select (point)))
    ;; FIXME: is there a better way of doing this?
    (emms-stop)
    (emms-start)))

(defun emms-browser-tracks-at (&optional pos)
  "Return the tracks at POS (point if not given), or nil if none."
  (emms-browser-ensure-browser-buffer)
  (save-excursion
    ;; move the point to the start of the line, since the trailing new
    ;; line is not propertized
    (move-beginning-of-line nil)
    (emms-with-widened-buffer
     (get-text-property (or pos (point))
                        'emms-tracks))))

(defun emms-isearch-buffer ()
  "Isearch through the buffer."
  (interactive)
  (goto-char (point-min))
  (call-interactively 'isearch-forward))

;; --------------------------------------------------
;; Linked browser and playlist windows (experimental)
;; --------------------------------------------------

(defcustom emms-browser-switch-to-playlist-on-add
  nil
  "Whether to switch to to the playlist after adding files."
  :group 'emms-browser
  :type 'boolean)

(defun emms-smart-browse ()
  "Display browser and playlist.
Toggle between selecting browser, playlist or hiding both. Tries
to behave sanely if the user has manually changed the window
configuration."
  (interactive)
  (add-to-list 'emms-browser-show-display-hook
               'emms-browser-display-playlist)
  (add-to-list 'emms-browser-hide-display-hook
               'emms-browser-hide-linked-window)
  ;; switch to the playlist window when adding tracks?
  (add-to-list 'emms-browser-tracks-added-hook
               (lambda () (interactive)
                 (when emms-browser-switch-to-playlist-on-add
                   (emms-smart-browse))
                 ;; recenter
                 (with-selected-window
                     (emms-browser-get-linked-window)
                   ;; FIXME: how do we achieve the same behaviour as
                   ;; c-u when calling interactively?
                   (recenter))))
  (let (wind buf)
  (cond
   ((eq major-mode 'emms-browser-mode)
    (setq buf (emms-browser-get-linked-buffer))
    (setq wind (emms-browser-get-linked-window))
    ;; if the playlist window is visible, select it
    (if wind
        (select-window wind)
      ;; otherwise display and select it
      (select-window (emms-browser-display-playlist))))
   ((eq major-mode 'emms-playlist-mode)
    (setq wind (emms-browser-get-linked-window))
    ;; if the playlist window is selected, and the browser is visible,
    ;; hide both
    (if wind
        (progn
          (select-window wind)
          (emms-browser-bury-buffer))
      ;; otherwise bury both
      (bury-buffer)
      (emms-browser-hide-linked-window)))
   (t
    ;; show both
    (emms-browser)))))

(defun emms-browser-get-linked-buffer ()
  "Return linked buffer (eg browser if playlist is selected."
  (cond
   ((eq major-mode 'emms-browser-mode)
    (car (emms-playlist-buffer-list)))
   ((eq major-mode 'emms-playlist-mode)
    emms-browser-buffer)))

(defun emms-browser-get-linked-window ()
  "Return linked window (eg browser if playlist is selected."
  (let ((buf (emms-browser-get-linked-buffer)))
    (when buf
      (get-buffer-window buf))))

(defun emms-browser-display-playlist ()
  "A hook to show the playlist when the browser is displayed.
Returns the playlist window."
  (interactive)
  (let ((pbuf (emms-browser-get-linked-buffer))
        (pwin (emms-browser-get-linked-window)))
    ;; if the window isn't alive..
    (unless (window-live-p pwin)
      (save-selected-window
        (split-window-horizontally)
        (other-window 1)
        (if pbuf
            (switch-to-buffer pbuf)
          ;; there's no playlist - create one
          (setq pbuf (emms-playlist-current-clear))
          (switch-to-buffer pbuf))
        ;; make q in the playlist window hide the linked browser
        (when (boundp 'emms-playlist-mode-map)
          (define-key emms-playlist-mode-map (kbd "q")
            (lambda ()
              (interactive)
              (emms-browser-hide-linked-window)
              (bury-buffer))))
        (setq pwin (get-buffer-window pbuf))
        (goto-char (point-max))))
    pwin))

(defun emms-browser-hide-linked-window ()
  "Delete a playlist or browser window when the other is hidden."
  (interactive)
  (let ((other-buf (emms-browser-get-linked-buffer))
        (other-win (emms-browser-get-linked-window)))
    (when (and other-win
               (window-live-p other-win))
      (delete-window other-win))
    ;; bury the buffer, or it becomes visible when we hide the
    ;; linked buffer
    (bury-buffer other-buf)))

(provide 'emms-browser)
;;; emms-browser.el ends here
