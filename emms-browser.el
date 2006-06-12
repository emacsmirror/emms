;;; emms-browser.el --- provide a buffer to browse files

;; Copyright (C) 2006  Damien Elmes <emacs@repose.cx>

;; Author: Damien Elmes <emacs@repose.cx>
;; Keywords: emms, mp3, mpeg, multimedia

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

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

;; SPC      - expand/contract current item
;; RET      - add current artist/album/title/etc
;; C-RET    - as above, but select the first added file and play
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

(defcustom emms-browser-track-sort-function
  'emms-sort-natural-order-less-p
  "How to sort tracks in the browser.
Ues nil for no sorting."
  :group 'emms-browser
  :type 'function)

(defcustom emms-browser-alpha-sort-function
  'string<
  "How to sort artists/albums/etc. in the browser.
Use nil for no sorting."
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

(defvar emms-browser-current-mapping-type nil
  "The current mapping type, eg. 'info-artist")
(make-variable-buffer-local 'emms-browser-current-mapping-type)

(defconst emms-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "q") 'emms-browser-bury-buffer)
    (define-key map (kbd "/") 'emms-isearch-buffer)
    (define-key map (kbd "?") 'describe-mode)
    (define-key map (kbd "C-/") 'emms-playlist-mode-undo)
    (define-key map (kbd "SPC") 'emms-browser-toggle-subitems)
    (define-key map (kbd "RET") 'emms-browser-add-tracks)
    (define-key map (kbd "<C-return>") 'emms-browser-add-tracks-and-play)
    map)
  "Keymap for `emms-browser-mode'.")

(defface emms-browser-tracks-face
  '((((class color) (background dark))
     (:foreground "#aaaaff"))
    (((class color) (background light))
     (:foreground "Blue"))
    (((type tty) (class mono))
     (:inverse-video t))
    (t (:background "Blue")))
  "Face for the tracks in a playlist buffer."
  :group 'emms-browser-mode)

(defface emms-browser-tracks-sub-face-1
  '((((class color) (background dark))
     (:foreground "#7777ff"))
    (((class color) (background light))
     (:foreground "Blue"))
    (((type tty) (class mono))
     (:inverse-video t))
    (t (:background "Blue")))
  "Face for the tracks in a playlist buffer."
  :group 'emms-browser-mode)

(defface emms-browser-tracks-sub-face-2
  '((((class color) (background dark))
     (:foreground "#4444ff"))
    (((class color) (background light))
     (:foreground "Blue"))
    (((type tty) (class mono))
     (:inverse-video t))
    (t (:background "Blue")))
  "Face for the tracks in a playlist buffer."
  :group 'emms-browser-mode)

(defface emms-browser-tracks-sub-face-3
  '((((class color) (background dark))
     (:foreground "#3333ff"))
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

(defmacro emms-browser-add-category (name track-type &optional expand-func)
  "Create an interactive function emms-browse-by-NAME."
  (let ((funname (intern (concat "emms-browse-by-" name)))
        (modedesc (concat "Browsing by: " name))
        (funcdesc (concat "Browse by " name ".")))
  `(defun ,funname ()
     ,funcdesc
     (interactive)
     (emms-browser-clear)
     (rename-buffer ,modedesc)
     (emms-browser-display-by ,track-type ,expand-func)
     (goto-char (point-min)))))

(emms-browser-add-category "artist" 'info-artist 'emms-browser-show-albums)
(emms-browser-add-category "album" 'info-album 'emms-browser-show-titles)
(emms-browser-add-category "genre" 'info-genre 'emms-browser-show-artists)
(emms-browser-add-category "year" 'info-year 'emms-browser-show-artists)

(defun emms-browser-make-by (field-type)
  "Make a mapping with FIELD-TYPE, eg artist -> tracks."
  (let ((db (make-hash-table
             :test emms-browser-comparison-test))
        field existing-entry)
    (maphash (lambda (path track)
               (setq field (emms-track-get track field-type "misc"))
               (setq existing-entry (gethash field db))
               (if existing-entry
                   (puthash field (cons track existing-entry) db)
                 (puthash field (list track) db)))
             emms-cache-db)
    db))

(defun emms-browser-display-by (field-type &optional expand-func)
  "Render a mapping into a browser buffer.
Optional EXPAND-FUNC is a function to call when expanding a
line."
  (let ((db (emms-browser-make-by field-type)))
    (maphash (lambda (desc data)
               (emms-browser-insert-entry desc data expand-func))
             db)
    ;; sort
    (setq emms-browser-current-mapping db)
    ;; FIXME: currently we use a hash for the 'level 1' information,
    ;; and an alist for subinfo. that means that this sorting is done
    ;; differently to subinfo..
    ;; should we use emms-browser-alpha-sort-function instead?
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

(defun emms-browser-insert-entry (entry tracks &optional expand-func)
  "Add a single ENTRY -> TRACKS mapping to the buffer.
EXPAND-FUNC is an optional func to call when expanding a line."
  (emms-browser-ensure-browser-buffer)
  (emms-with-inhibit-read-only-t
   (insert (emms-propertize entry
                            'emms-browser-data tracks
                            'emms-browser-level 1
                            'emms-browser-expand-func expand-func
                            'face 'emms-browser-tracks-face) "\n")))

(defun emms-browser-add-tracks ()
  "Add all the tracks on the current line to the playlist."
  (interactive)
  (let ((tracks (emms-browser-data-at))
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

(defun emms-browser-data-at (&optional pos)
  "Return the tracks at POS (point if not given), or nil if none."
  (emms-browser-ensure-browser-buffer)
  (save-excursion
    ;; move the point to the start of the line, since the trailing new
    ;; line is not propertized
    (move-beginning-of-line nil)
    (emms-with-widened-buffer
     (get-text-property (or pos (point))
                        'emms-browser-data))))

(defun emms-isearch-buffer ()
  "Isearch through the buffer."
  (interactive)
  (goto-char (point-min))
  (when (isearch-forward)
    (unless (emms-browser-subitems-visible)
      (emms-browser-show-subitems)
      (next-line))))

;; --------------------------------------------------
;; Expansion/subitem support (experimental)
;; --------------------------------------------------

(defmacro emms-browser-add-show-category (name field-type &optional
                                               expand-func sort-func)
  "Create an interactive function emms-browser-show-FIELD-TYPE.
EXPAND-FUNC is used to further expand subitems if not already
expanded.
SORT-FUNC is called to sort retrieved data."
  (let ((fname (intern (concat "emms-browser-show-" name)))
        (fdesc (concat "Show " name " under current line")))
  `(defun ,fname ()
     ,fdesc
     (interactive)
     (unless (emms-browser-subitems-visible)
       (let ((data (emms-browser-make-alist-from-field
                    ,field-type
                    (emms-browser-data-at))))
         (when ,sort-func
           (setq data (funcall ,sort-func data)))
         (emms-browser-insert-subitems data ,expand-func))))))

;;
;; create emms-browser-show-*
;;
(emms-browser-add-show-category
 "albums" 'info-album
 'emms-browser-show-titles
 'emms-browser-sort-by-name)

(emms-browser-add-show-category
 "artists" 'info-artist
 'emms-browser-show-albums
 'emms-browser-sort-by-name)

(emms-browser-add-show-category
 "titles" 'info-title
 nil
 'emms-browser-sort-by-tracks)

(defun emms-browser-level-at-point ()
  "Return the current level at point.
Actually this function returns the value of the first character
on the line, because if point is on a trailing \n it will fail.
Returns 0 if the current line is not an entry."
  (let ((val
         (get-text-property (line-beginning-position)
                            'emms-browser-level)))
    (if val
        val
      0)))

(defun emms-browser-find-entry-more-than-level (level)
  "Move point to next entry more than LEVEL and return point.
If no entry exits, return nil.
Returns point if currently on a an entry more than LEVEL."
  (let ((old-pos (point))
        level-at-point)
    (re-search-forward "\n" nil t)
    (if (> (emms-browser-level-at-point) level)
        (point)
      (goto-char old-pos)
      nil)))

(defun emms-browser-subitems-visible ()
  "True if there are any subentries under point."
  (let ((current-level (emms-browser-level-at-point))
        new-level)
    (save-excursion
      (re-search-forward "\n" nil t)
      (when (setq new-level (emms-browser-level-at-point))
        (> new-level current-level)))))

(defun emms-browser-toggle-subitems ()
  "Show or hide (kill) subitems under the current line."
  (interactive)
  (if (emms-browser-subitems-visible)
      (emms-browser-kill-subitems)
    (emms-browser-show-subitems)))

(defun emms-browser-show-subitems ()
  "Show subitems under the current line."
  (let ((func (get-text-property (line-beginning-position)
                                 'emms-browser-expand-func)))
  (if func
      (funcall func)
    (message "Can't expand further!"))))

(defun emms-browser-kill-subitems ()
  "Remove all subitems under the current line.
Stops at the next line at the same level, or EOF."
  (let ((current-level (emms-browser-level-at-point))
        (kill-whole-line t))
    (save-excursion
      (emms-with-inhibit-read-only-t
       (while (emms-browser-find-entry-more-than-level current-level)
         (kill-line)
         (previous-line))))))

(defun emms-browser-insert-subitems (subitems &optional expand-func)
  "Insert SUBITEMS under the current item.
SUBITEMS is a list of cons cells (desc . data).
emms-browser-level will be set to 1 more than the current level.
Don't add anything if there are already subitems below."
  (let ((new-level (1+ (emms-browser-level-at-point)))
        desc data)
    (save-excursion
      (next-line)
      (beginning-of-line)
      (emms-with-inhibit-read-only-t
       (dolist (item subitems)
         (setq desc (car item))
         (setq data (cdr item))
         (insert
          (emms-propertize (concat (make-string (* 2 (1- new-level)) ?\  ) desc)
                           'emms-browser-data data
                           'emms-browser-level new-level
                           'emms-browser-expand-func expand-func
                           'face
                           (intern
                            (concat
                             "emms-browser-tracks-sub-face-"
                             (int-to-string
                              (1- new-level)))))
          "\n"))))))

(defun emms-browser-make-alist-from-field (field-type tracks)
  "Make an alist mapping of FIELD-TYPE -> TRACKS.
Items with no metadata for FIELD-TYPE will be placed in 'misc'"
  (let (db key existing)
    (dolist (track tracks)
      (setq key (emms-track-get track field-type "misc"))
      (setq existing (assoc key db))
      (if existing
          (setcdr existing (cons track (cdr existing)))
        (push (cons key (list track)) db)))
    db))

;; --------------------------------------------------
;; Sorting expanded entries
;; --------------------------------------------------

(defmacro emms-browser-sort-cadr (sort-func)
  "Return a function to sort an alist using SORT-FUNC.
This sorting predicate will compare the cadr of each entry.
SORT-FUNC should be a playlist sorting predicate like
`emms-playlist-sort-by-natural-order'."
  `(lambda (a b)
     (funcall ,sort-func (cadr a) (cadr b))))

(defmacro emms-browser-sort-car (sort-func)
  "Return a function to sort an alist using SORT-FUNC.
This sorting predicate will compare the car of each entry.
SORT-FUNC should be a playlist sorting predicate like
`emms-playlist-sort-by-natural-order'."
  `(lambda (a b)
     (funcall ,sort-func (car a) (car b))))

(defun emms-browser-sort-by-tracks (data)
  "Sort an alist DATA by the tracks in each entry.
Uses `emms-browser-track-sort-function'."
  (if emms-browser-track-sort-function
      (sort data (emms-browser-sort-cadr
                  emms-browser-track-sort-function))
    data))

(defun emms-browser-sort-by-name (data)
  "Sort an alist DATA by keys.
Uses `emms-browser-alpha-sort-function'."
  (if emms-browser-alpha-sort-function
      (sort data (emms-browser-sort-car
                  emms-browser-alpha-sort-function))
    data))

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
                 (let (playlist-window)
                   (when emms-browser-switch-to-playlist-on-add
                     (emms-smart-browse))
                   ;; recenter
                   (when
                       (setq playlist-window
                             (emms-browser-get-linked-window))
                     (with-selected-window
                         playlist-window
                       ;; FIXME: how do we achieve the same behaviour as
                       ;; c-u when calling interactively?
                       (recenter))))))
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

;(defun emms
(provide 'emms-browser)
;;; emms-browser.el ends here
