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
;; year. These commands can also be used while smart browsing to
;; change the browsing category.

;; If you don't want to activate the code with (emms-devel), you can
;; activate it manually with:

;; (require 'emms-browser)

;; Note this code is very new and is still prone to big changes in the
;; API and breakage. Bug reports are welcome.

;;; Code:

(require 'emms)
(require 'emms-cache)
(require 'emms-source-file)
(require 'emms-playlist-sort)

(eval-when-compile
  (require 'cl))

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
  "*The default browsing mode."
  :group 'emms-browser
  :type 'function)

(defcustom emms-browser-make-name-function
  'emms-browser-make-name-standard
  "*A function to make names for entries and subentries.
Overriding this function allows you to customise how various elements
are displayed. It is called with two arguments - track and type."
  :group 'emms-browser
  :type 'function)

(defcustom emms-browser-insert-track-function
  'emms-browser-insert-track-standard
  "*A function to insert a track into the playlist.
The default behaviour indents tracks depending on whether you're
adding an album, artist, etc."
  :group 'emms-browser
  :type 'function)

(defcustom emms-browser-comparison-test
  'case-fold
  "*A method for comparing entries in the cache.
The default is to compare case-insensitively."
  :group 'emms-browser
  :type 'symbol)

(defcustom emms-browser-track-sort-function
  'emms-sort-natural-order-less-p
  "*How to sort tracks in the browser.
Ues nil for no sorting."
  :group 'emms-browser
  :type 'function)

(defcustom emms-browser-alpha-sort-function
  'string<
  "*How to sort artists/albums/etc. in the browser.
Use nil for no sorting."
  :group 'emms-browser
  :type 'function)

(defcustom emms-browser-show-display-hook nil
  "*Hooks to run when starting or switching to a browser buffer."
  :group 'emms-browser
  :type 'hook)

(defcustom emms-browser-hide-display-hook nil
  "*Hooks to run when burying or removing a browser buffer."
  :group 'emms-browser
  :type 'hook)

(defcustom emms-browser-tracks-added-hook nil
  "*Hooks to run when tracks are added to the playlist."
  :group 'emms-browser
  :type 'hook)

(defvar emms-browser-buffer nil
  "The current browser buffer, if any.")

(defvar emms-browser-buffer-name "*EMMS Browser*"
  "The default buffer name.")

(defvar emms-browser-top-level-hash nil
  "The current mapping db, eg. artist -> track.")
(make-variable-buffer-local 'emms-browser-top-level-hash)

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
    (define-key map (kbd "<tab>") 'emms-browser-next-non-track)
    (define-key map (kbd "<backtab>") 'emms-browser-prev-non-track)
    (define-key map (kbd "E") 'emms-browser-expand-all)
    (define-key map (kbd "1") 'emms-browser-collapse-all)
    (define-key map (kbd "2") 'emms-browser-expand-to-level-2)
    (define-key map (kbd "3") 'emms-browser-expand-to-level-3)
    (define-key map (kbd "4") 'emms-browser-expand-to-level-4)
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
;; Top-level browsing methods - by artist/album/etc
;; --------------------------------------------------

;; Since the number of tracks may be rather large, we use a hash to
;; sort the top level elements into various categories. All
;; subelements will be stored in a bdata alist structure.

(defmacro emms-browser-add-category (name type)
  "Create an interactive function emms-browse-by-NAME."
  (let ((funname (intern (concat "emms-browse-by-" name)))
        (modedesc (concat "Browsing by: " name))
        (funcdesc (concat "Browse by " name ".")))
  `(defun ,funname ()
     ,funcdesc
     (interactive)
     (let ((hash (emms-browser-make-hash-by ,type)))
       (emms-browser-clear)
       (rename-buffer ,modedesc)
       (emms-browser-render-hash hash ,type)
       (setq emms-browser-top-level-hash hash)
       (goto-char (point-min))))))

(emms-browser-add-category "artist" 'info-artist)
(emms-browser-add-category "album" 'info-album)
(emms-browser-add-category "genre" 'info-genre)
(emms-browser-add-category "year" 'info-year)

(defun emms-browser-make-hash-by (type)
  "Make a hash, mapping with TYPE, eg artist -> tracks."
  (let ((hash (make-hash-table
             :test emms-browser-comparison-test))
        field existing-entry)
    (maphash (lambda (path track)
               (setq field (emms-track-get track type "misc"))
               (setq existing-entry (gethash field hash))
               (if existing-entry
                   (puthash field (cons track existing-entry) hash)
                 (puthash field (list track) hash)))
             emms-cache-db)
    hash))

(defun emms-browser-render-hash (db type)
  "Render a mapping (DB) into a browser buffer."
  (maphash (lambda (desc data)
             ;; reverse the entries so that unsorted tracks are displayed in
             ;; ascending order
             (emms-browser-insert-top-level-entry desc (nreverse data) type))
           db)
  (emms-with-inhibit-read-only-t
   (let ((sort-fold-case t))
     (sort-lines nil (point-min) (point-max)))))

(defun case-fold-string= (a b)
  (compare-strings a nil nil b nil nil t))

(defun case-fold-string-hash (a)
  (sxhash (upcase a)))

(define-hash-table-test 'case-fold
  'case-fold-string= 'case-fold-string-hash)

(defun emms-browser-insert-top-level-entry (entry tracks type)
  "Insert a single top level entry into the buffer."
  (emms-browser-ensure-browser-buffer)
  (emms-with-inhibit-read-only-t
   (insert (emms-propertize
            entry
            'emms-browser-bdata
            (emms-browser-make-bdata-tree
             type 1 tracks)
            'face 'emms-browser-tracks-face) "\n")))

;; --------------------------------------------------
;; Building a subitem tree
;; --------------------------------------------------

(defun emms-browser-next-mapping-type (current-mapping)
  "Return the next sensible mapping.
Eg. if current-mapping is currently 'info-artist, return 'info-album."
  (cond
   ((eq current-mapping 'info-artist) 'info-album)
   ((eq current-mapping 'info-album) 'info-title)
   ((eq current-mapping 'info-genre) 'info-artist)
   ((eq current-mapping 'info-year) 'info-artist)))

(defun emms-browser-make-bdata-tree (type level tracks)
  "Build a tree of browser DB elements for tracks."
  (emms-browser-make-bdata
   (emms-browser-make-bdata-tree-recurse
    type level tracks)
   ;; with the current hash code, we're guaranteed to have only one
   ;; element at the top
   (emms-track-get (car tracks) type)
   type level))

(defun emms-browser-make-bdata-tree-recurse (type level tracks)
  "Build a tree of alists based on a list of tracks, TRACKS.
For example, if TYPE is 'info-year, return an alist like:
artist1 -> album1 -> *track* 1.."
  (let* ((next-type (emms-browser-next-mapping-type type))
         (next-level (1+ level))
         alist name new-db new-tracks)
    ;; if we're at a leaf, the db data is a list of tracks
    (if (eq type 'info-title)
        tracks
      ;; otherwise, make DBs from the sub elements
      (setq alist
            (emms-browser-make-sorted-alist
             next-type tracks))
      (mapcar (lambda (entry)
                (setq name (emms-browser-make-name
                            entry next-type))
                (setq new-tracks (cdr entry))
                (emms-browser-make-bdata
                 (emms-browser-make-bdata-tree-recurse
                  next-type next-level new-tracks)
                 name next-type next-level))
              alist))))

(defun emms-browser-make-name (entry type)
  "Return a name for ENTRY, used for making a bdata object.
This uses `emms-browser-make-name-function'"
  ;; we use cadr because we are guaranteed only one track in entry.
  (funcall emms-browser-make-name-function entry type))

(defun emms-browser-make-name-standard (entry type)
  "Add track numbers to track names.
Apart from tracks, names are displayed without modification."
  (if (eq type 'info-title)
      (emms-browser-make-name-with-track-number (cadr entry))
    (car entry)))

(defun emms-browser-make-name-with-track-number (track)
  "Concat a track number to the name of track, if one exists."
  (let ((tracknum (emms-track-get track 'info-tracknumber)))
    (concat
     (if (string= tracknum "0")
         ""
       (concat
        (if (eq (length tracknum) 1)
            (concat "0" tracknum)
          tracknum)
        ". "))
     (emms-track-get track 'info-title))))

(defun emms-browser-make-bdata (data name type level)
  "Return a browser data item from ALIST.
DATA should be a list of DB items, or a list of tracks.
NAME is a name for the DB item.
TYPE is a category the data is organised by, such as 'info-artist.
LEVEL is the number of the sublevel the db item will be placed in."
  (list (cons 'type type)
        (cons 'level level)
        (cons 'name name)
        (cons 'data data)))

(defun emms-browser-make-alist (type tracks)
  "Make an alist mapping of TYPE -> TRACKS.
Items with no metadata for TYPE will be placed in 'misc'"
  (let (db key existing)
    (dolist (track tracks)
      (setq key (emms-track-get track type "misc"))
      (setq existing (assoc key db))
      (if existing
          (setcdr existing (cons track (cdr existing)))
        (push (cons key (list track)) db)))
    ;; sort the entries we've built
    (dolist (item db)
      (setcdr item (nreverse (cdr item))))
    db))

(defun emms-browser-make-sorted-alist (type tracks)
  "Return a sorted alist of TRACKS.
TYPE is the metadata to make the alist by - eg. if it's
'info-artist, an alist of artists will be made."
  (emms-browser-sort-alist
   (emms-browser-make-alist type tracks)
   type))

;; --------------------------------------------------
;; BDATA accessors and predicates
;; --------------------------------------------------

(defun emms-browser-bdata-level (bdata)
  (cdr (assq 'level bdata)))

(defun emms-browser-bdata-name (bdata)
  (cdr (assq 'name bdata)))

(defun emms-browser-bdata-type (bdata)
  (cdr (assq 'type bdata)))

(defun emms-browser-bdata-data (bdata)
  (cdr (assq 'data bdata)))

(defun emms-browser-bdata-p (obj)
  "True if obj is a BDATA object."
  (consp (assq 'data obj)))

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

(defun emms-browser-sort-by-track (alist)
  "Sort an ALIST by the tracks in each entry.
Uses `emms-browser-track-sort-function'."
  (if emms-browser-track-sort-function
      (sort alist (emms-browser-sort-cadr
                  emms-browser-track-sort-function))
    alist))

(defun emms-browser-sort-by-name (alist)
  "Sort ALIST by keys alphabetically.
Uses `emms-browser-alpha-sort-function'."
  (if emms-browser-alpha-sort-function
      (sort alist (emms-browser-sort-car
                  emms-browser-alpha-sort-function))
    alist))

(defun emms-browser-sort-alist (alist type)
  "Sort ALIST using the sorting function for TYPE."
  (let ((sort-func
         (cond
          ((or
            (eq type 'info-album)
            (eq type 'info-artist)
            (eq type 'info-year)
            (eq type 'info-genre))
           'emms-browser-sort-by-name)
          ((eq type 'info-title)
           'emms-browser-sort-by-track)
          (t (message "Can't sort unknown mapping!")))))
    (funcall sort-func alist)))

;; --------------------------------------------------
;; Subitem operations on the buffer
;; --------------------------------------------------

(defun emms-browser-bdata-at-point ()
  "Return the bdata object at point.
Includes information at point (such as album name), and metadata."
  (get-text-property (line-beginning-position)
                     'emms-browser-bdata))

(defun emms-browser-data-at-point ()
  "Return the data stored under point.
This will be a list of DB items."
  (emms-browser-bdata-data (emms-browser-bdata-at-point)))

(defun emms-browser-level-at-point ()
  "Return the current level at point."
  (emms-browser-bdata-level (emms-browser-bdata-at-point)))

(defun emms-browser-expand-one-level ()
  "Expand the current line by one sublevel."
  (interactive)
  (let* ((data (emms-browser-data-at-point)))
    (save-excursion
      (next-line)
      (beginning-of-line)
      (dolist (data-item data)
        (emms-browser-insert-data-item data-item)))))

(defun emms-browser-insert-data-item (data-item)
  "Insert DATA-ITEM into the buffer.
This checks DATA-ITEM's level to determine how much to indent.
The line will have a property emms-browser-bdata storing subitem
information."
  (let* ((level (emms-browser-bdata-level data-item))
         (name (emms-browser-bdata-name data-item))
         (indent (emms-browser-make-indent-for-level level)))
    (emms-with-inhibit-read-only-t
     (insert
      (emms-propertize
       (concat indent name)
       'emms-browser-bdata data-item
       'face (emms-browser-face-from-level level))
      "\n"))))

(defun emms-browser-make-indent-for-level (level)
  (make-string (* 2 (1- level)) ?\  ))

(defun emms-browser-face-from-level (level)
  "Return a face appropriate for LEVEL."
  (intern
   (concat "emms-browser-tracks-sub-face-"
           (int-to-string (1- level)))))

(defun emms-browser-find-entry-more-than-level (level)
  "Move point to next entry more than LEVEL and return point.
If no entry exits, return nil.
Returns point if currently on a an entry more than LEVEL."
  (let ((old-pos (point))
        level-at-point)
    (forward-line 1)
    (setq level-at-point (emms-browser-level-at-point))
    (if (and level-at-point
             (> level-at-point level))
        (point)
      (goto-char old-pos)
      nil)))

(defun emms-browser-subitems-visible ()
  "True if there are any subentries visible point."
  (let ((current-level (emms-browser-level-at-point))
        new-level)
    (save-excursion
      (re-search-forward "\n" nil t)
      (when (setq new-level (emms-browser-level-at-point))
        (> new-level current-level)))))

(defun emms-browser-subitems-exist ()
  "True if it's possible to expand the current line."
  (not (eq (emms-browser-bdata-type
            (emms-browser-bdata-at-point))
           'info-title)))

(defun emms-browser-move-up-level (&optional direction)
  "Move up one level if possible.
Return true if we were able to move up.
If DIRECTION is 1, move forward, otherwise move backwards."
  (let ((moved nil)
        (continue t)
        (current-level (emms-browser-level-at-point)))
    (while (and
            continue
            (zerop (forward-line
                    (or direction -1))))
      (when (> current-level (emms-browser-level-at-point))
        (setq moved t)
        (setq continue nil)))
    moved))

(defun emms-browser-toggle-subitems ()
  "Show or hide (kill) subitems under the current line."
  (interactive)
  (if (emms-browser-subitems-visible)
      (emms-browser-kill-subitems)
    (if (emms-browser-subitems-exist)
        (emms-browser-show-subitems)
      (assert (emms-browser-move-up-level))
      (emms-browser-kill-subitems))))

(defun emms-browser-show-subitems ()
  "Show subitems under the current line."
  (unless (emms-browser-subitems-visible)
    (if (emms-browser-subitems-exist)
        (emms-browser-expand-one-level))))

(defun emms-browser-kill-subitems ()
  "Remove all subitems under the current line.
Stops at the next line at the same level, or EOF."
  (when (emms-browser-subitems-visible)
    (let ((current-level (emms-browser-level-at-point))
          (next-line (line-beginning-position 2)))
      (emms-with-inhibit-read-only-t
       (delete-region next-line
                      (save-excursion
                        (while
                            (emms-browser-find-entry-more-than-level
                             current-level))
                        (line-beginning-position 2)))))))

;; --------------------------------------------------
;; Dealing with the playlist (queuing songs, etc)
;; --------------------------------------------------

(defun emms-browser-insert-playlist-group (type group level)
  "Insert a group description into the playlist buffer.
Eg. [album] foo bar"
  (let ((short-type (substring (symbol-name type) 5)))
    (with-current-emms-playlist
      (goto-char (point-max))
      (insert
       (emms-browser-make-indent-for-level level)
       (format "[%s] %s\n" short-type group)))))

(defun emms-browser-insert-track (track name level)
  "Insert a track into the playlist buffer, called NAME.
LEVEL is used to control indentation."
  (funcall emms-browser-insert-track-function track name level))

(defun emms-browser-insert-track-standard (track name level)
  (with-current-emms-playlist
    (goto-char (point-max))
    (insert  (emms-propertize
              (concat
               (emms-browser-make-indent-for-level level)
               name)
              'face 'emms-playlist-track-face
              'emms-track track)
            "\n")))

(defun emms-browser-add-tracks ()
  "Add all tracks at point."
  (interactive)
  (let ((bdata (emms-browser-bdata-at-point)))
    (emms-browser-add-bdata-to-playlist
     bdata (emms-browser-bdata-level bdata)))
  (run-hooks 'emms-browser-tracks-added-hook))

(defun emms-browser-add-tracks-and-play ()
  "Add all tracks at point, and play the first added track."
  (interactive)
  (let (old-pos)
    (with-current-emms-playlist
      (setq old-pos (point-max)))
    (emms-browser-add-tracks)
    (with-current-emms-playlist
      (goto-char old-pos)
      (emms-playlist-next)
      (emms-playlist-select (point)))
    ;; FIXME: is there a better way of doing this?
    (emms-stop)
    (emms-start)))

(defun emms-browser-add-bdata-to-playlist (bdata starting-level)
  "Add all tracks in BDATA to the playlist."
  (let ((type (emms-browser-bdata-type bdata))
        (name (emms-browser-bdata-name bdata))
        (level (emms-browser-bdata-level bdata)))

    ;; adjust the indentation relative to the starting level
    (when starting-level
      (setq level (- level (1- starting-level))))

    (unless (eq type 'info-title)
      (emms-browser-insert-playlist-group
       type name level))

    (dolist (item (emms-browser-bdata-data bdata))
      (if (not (eq type 'info-title))
          (emms-browser-add-bdata-to-playlist item starting-level)
        ;; add full track name as there may not be enough context
        (setq name (concat (emms-track-get item 'info-artist)
                           " - "
                           ;; track numbers don't make much sense
                           ;; for individual files
                           (or (and (> level 1)
                                    name)
                               (emms-track-get item 'info-title))))
        (emms-browser-insert-track
         item name level)))))

(defun emms-isearch-buffer ()
  "Isearch through the buffer."
  (interactive)
  (goto-char (point-min))
  (when (isearch-forward)
    (unless (emms-browser-subitems-visible)
      (emms-browser-show-subitems))))

(defun emms-browser-next-non-track (&optional direction)
  "Jump to the next non-track element."
  (interactive)
  (let ((continue t))
    (while (and continue
                (forward-line (or direction 1)))
      (unless (eq (emms-browser-bdata-type
                   (emms-browser-bdata-at-point)) 'info-title)
        (setq continue)))))

(defun emms-browser-prev-non-track ()
  "Jump to the previous non-track element."
  (interactive)
  (emms-browser-next-non-track -1))

(defun emms-browser-expand-all ()
  "Expand everything."
  (interactive)
  (emms-browser-expand-to-level 99))

(defun emms-browser-expand-to-level-2 ()
  "Expand all top level items one level."
  (interactive)
  (emms-browser-expand-to-level 2))

(defun emms-browser-expand-to-level-3 ()
  "Expand all top level items two levels."
  (interactive)
  (emms-browser-expand-to-level 3))

(defun emms-browser-expand-to-level-4 ()
  "Expand all top level items three levels."
  (interactive)
  (emms-browser-expand-to-level 4))

(defun emms-browser-expand-to-level (level)
  "Expand to an a depth specified by LEVEL."
  (goto-char (point-min))
  (while (not (eq (buffer-end 1) (point)))
    (if (< (emms-browser-level-at-point) level)
        (emms-browser-show-subitems))
    (emms-browser-next-non-track))
  (goto-char (point-min)))

(defun emms-browser-collapse-all ()
  "Collapse everything."
  (interactive)
  (goto-char (point-max))
  (while (not (eq (buffer-end -1) (point)))
    (emms-browser-prev-non-track)
    (emms-browser-kill-subitems)))

;; --------------------------------------------------
;; Linked browser and playlist windows
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

(provide 'emms-browser)
;;; emms-browser.el ends here
