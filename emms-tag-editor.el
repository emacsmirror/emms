;;; emms-mp3tag.el --- Edit track tags.

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Keywords:
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'emms-mp3tag)

;;; Code:

(provide 'emms-mp3tag)
(eval-when-compile
  (require 'cl))
(require 'emms)
(require 'emms-info-mp3info)
(require 'emms-playlist-mode)
(require 'emms-mark)
(require 'format-spec)

(defvar emms-mp3tag-tags
  '((info-artist      . "a")
    (info-title       . "t")
    (info-album       . "l")
    (info-tracknumber . "n")
    (info-year        . "y")
    (info-genre       . "g")
    (info-date        . "d")
    (info-note        . "c"))
"A list to setup format.")

(defvar emms-mp3tag-edit-buffer "*EMMS-TAGS*"
  "Buffer name to edit mp3 tags")
(defvar emms-mp3tag-log-buffer "*EMMS-LOG*"
  "Buffer name of mp3tag edit log")

(defun emms-mp3tag-make-format (tags)
  (format "%%m\n%-16s = %%f\n%s\n\n" "name"
          (mapconcat
           (lambda (tag)
             (concat (propertize (format "%-16s = " (symbol-name tag))
                                 'read-only t 'rear-nonsticky t 'face 'bold)
                     "%" (cdr (assoc tag emms-mp3tag-tags))))
           tags "\n")))

(defvar emms-mp3tag-formats
  (let* ((tags (mapcar 'car emms-mp3tag-tags))
         (default (emms-mp3tag-make-format (remove 'info-date tags))))
    `(("mp3" . ,default)
      ("ogg" . ,(emms-mp3tag-make-format (remove 'info-year tags)))
      ("default" . ,default)))
  "Format to insert the track. The CAR part is the extension of the
track name, and the CDR part is the format template. The format
specification is like:
 m     --     Track description
 f     --     Track name
 a     --     Track info-artist
 t     --     Track info-title
 l     --     Track info-album
 n     --     Track info-tracknumber
 y     --     Track info-year
 g     --     Track info-genre
 c     --     Track info-note

You can add new specification in `emms-mp3tag-tags' and use
`emms-mp3tag-make-format' to help create a new format.

The CDR part also can be a function, which accept one parameter, the
track, and should return a string to insert to `emms-mp3tag-edit-buffer'.
")

(defvar emms-mp3tag-get-format-function 'emms-mp3tag-get-format
  "Function to decide which format to use for format the track.")

(defvar emms-mp3tag-parse-function 'emms-mp3tag-default-parser
  "Function to parse tags in `emms-mp3tag-edit-buffer'. It should find
all modified tags, and return all the tracks. The tracks which tag has
been modified should set a property 'tag-modified to t, and if the
track name have been change, the function should set a new property
'newname instead set the 'name directly.

See also `emms-mp3tag-default-parser'.")

(defvar emms-mp3tag-tagfile-functions
  '(("mp3" "mp3info"
     ((info-artist      . "a")
      (info-title       . "t")
      (info-album       . "l")
      (info-tracknumber . "n")
      (info-year        . "y")
      (info-genre       . "g")
      (info-note        . "c")))
    ("ogg" . emms-mp3tag-tag-ogg))
  "A List for change tag in files. If the extern program set tag by
command line options one by one such as mp3info, the list should like:
 (EXTENSION PROGRAM COMMAND_LINE_OPTIONS)

Otherwise, a function that accept a parameter, the track, should be
given.

See also `emms-mp3tag-tag-file' and `emms-mp3tag-tag-ogg'.
")

(defun emms-mp3tag-tag-ogg (track)
  (call-process "vorbiscomment" nil nil nil
                "-w" "-t"
                (mapconcat
                 (lambda (tag)
                   (concat tag "="
                           (emms-track-get track (intern (concat "info-" tag)))))
                 '("artist" "title" "album" "tracknumber" "date" "genre" "note")
                 "\n\t")
                (emms-track-name track)))

(defun emms-mp3tag-tag-file (track program tags)
  "Change tag in FILE use PROGRAM. The TAGS is given in `emms-mp3tag-tagfile-functions'."
  (let (args val)
    (mapc (lambda (tag)
            (when (> (length (setq val (emms-track-get track (car tag)))) 0)
              (setq args (append args (list (concat "-" (cdr tag)) val)))))
          tags)
    (apply 'call-process program
           nil nil nil
           filename args)))

(defun emms-mp3tag-get-format (track)
  (let ((format
         (assoc (file-name-extension (emms-track-name track))
                emms-mp3tag-formats)))
    (if format
        (cdr format)
      (cdr (assoc "default" emms-mp3tag-formats)))))

(defun emms-mp3tag-format-track (track)
  (let ((format (funcall emms-mp3tag-get-format-function track)))
    (if (functionp format)
        (funcall format track)
      (format-spec
       format
       (apply 'format-spec-make
              ?m (emms-propertize (emms-track-force-description track)
                                  'face 'emms-playlist-track-face
                                  'emms-track (copy-sequence track))
              ?f (emms-track-name track)
              (apply 'append
                     (mapcar (lambda (tag)
                               (list (string-to-char (cdr tag))
                                     (or (emms-track-get track (car tag)) "")))
                             emms-mp3tag-tags)))))))

(defun emms-mp3tag-track-at (&optional pos)
  (let ((track (emms-playlist-track-at pos))
        newtrack)
    (when track
      (setq newtrack (copy-sequence track))
      (emms-track-set newtrack 'position (point-marker))
      (emms-track-set newtrack 'orig-track track)
      newtrack)))

(defsubst emms-mp3tag-erase-buffer (&optional buf)
  (let ((inhibit-read-only t))
    (save-excursion
      (set-buffer (get-buffer-create buf))
      (erase-buffer))))

(defsubst emms-mp3tag-insert-track (track)
  (and track
       (insert (emms-mp3tag-format-track track))))

(defsubst emms-mp3tag-display-log-buffer-maybe ()
  (if (> (buffer-size (get-buffer emms-mp3tag-log-buffer)) 0)
      (display-buffer emms-mp3tag-log-buffer)))

(defun emms-mp3tag-insert-tracks (tracks)
  (save-excursion
    (emms-mp3tag-erase-buffer emms-mp3tag-log-buffer)
    (emms-mp3tag-erase-buffer emms-mp3tag-edit-buffer)
    (set-buffer (get-buffer emms-mp3tag-edit-buffer))
    (mapc 'emms-mp3tag-insert-track tracks)
    (emms-mp3tag-mode)
    (pop-to-buffer (current-buffer))
    (goto-char (point-min))
    (emms-mp3tag-display-log-buffer-maybe)))

(defun emms-mp3tag-edit-track (track)
  (interactive (list (emms-mp3tag-track-at)))
  (if (null track)
      (message "No track at point!")
    (emms-mp3tag-insert-tracks (list track))))

(defun emms-mp3tag-edit-marked-tracks ()
  (interactive)
  (let ((tracks (emms-mark-mapcar-marked-track 'emms-mp3tag-track-at t)))
    (if (null tracks)
        (message "No track marked!")
      (emms-mp3tag-insert-tracks tracks))))

(defun emms-mp3tag-edit ()
  "Edit tags of track at point or marked tracks"
  (interactive)
  (if (emms-mark-has-markedp)
      (emms-mp3tag-edit-marked-tracks)
    (emms-mp3tag-edit-track (emms-mp3tag-track-at))))

(defvar emms-mp3tag-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'emms-mp3tag-next-field)
    (define-key map [backtab] 'emms-mp3tag-prev-field)
    (define-key map "\C-c\C-n" 'emms-mp3tag-next-track)
    (define-key map "\C-c\C-p" 'emms-mp3tag-prev-track)
    (define-key map "\C-c\C-c" 'emms-mp3tag-submit-and-exit)
    (define-key map "\C-c\C-s" 'emms-mp3tag-submit)
    (define-key map "\C-c\C-r" 'emms-mp3tag-replace-all)
    map))
(define-key emms-playlist-mode-map "E" 'emms-mp3tag-edit)

(define-derived-mode emms-mp3tag-mode text-mode "Mp3tag"
  "Major mode to edit track tags.
\\{emms-mp3tag-mode-map}")

(defun emms-mp3tag-replace-all (name value)
  "Replace all track's NAME to VALUE. If turn transient-mark-mode on,
you can apply the command to a selected region."
  (interactive
   (list (completing-read "Replace tag: "
                          emms-mp3tag-tags nil t)
         (read-from-minibuffer "Set tag to: ")))
  (save-excursion
    (save-restriction
      (if (and mark-active transient-mark-mode)
          (narrow-to-region (region-beginning) (region-end)))
      (goto-char (point-min))
      (while (re-search-forward (concat "^" (regexp-quote name)) nil t)
        (skip-chars-forward " \t=")
        (delete-region (point) (line-end-position))
        (insert value)))))

(defun emms-mp3tag-next-field (arg)
  (interactive "p")
  (if (> arg 0)
      (re-search-forward "\\s-*=[ \t]*" nil nil arg)
    (emms-mp3tag-prev-field (- arg))))

(defun emms-mp3tag-prev-field (arg)
  (interactive "p")
  (if (< arg 0)
      (emms-mp3tag-next-field (- arg))
    (skip-chars-backward " \t=")
    (re-search-backward "\\s-*=[ \t]*" nil nil arg)
    (skip-chars-forward " \t=")))

(defun emms-mp3tag-prev-track ()
  (interactive)
  (let ((prev (previous-single-property-change (point)
                                               'emms-track)))
    (when (not prev)
      (error "No previous track"))
    (when (not (get-text-property prev 'emms-track))
      (setq prev (or (previous-single-property-change prev 'emms-track)
                     (point-min))))
    (when (or (not prev)
              (not (get-text-property prev 'emms-track)))
      (error "No previous track"))
    (goto-char prev)))

(defun emms-mp3tag-next-track ()
  (interactive)
  (let ((next (next-single-property-change (point)
                                           'emms-track)))
    (when (not next)
      (error "No next track"))
    (when (not (get-text-property next 'emms-track))
      (setq next (next-single-property-change next 'emms-track)))
    (when (or (not next)
              (= next (point-max)))
      (error "No next track"))
    (goto-char next)))

(defun emms-mp3tag-submit (arg)
  "Make modified tags take affect. With prefiex argument, bury tag
edit buffer."
  (interactive "P")
  (let ((tracks (funcall emms-mp3tag-parse-function))
        filename func exit old pos val need-sync)
    (if (not (and tracks (y-or-n-p "Submit changes? ")))
        (message "Nothing have to do!")
      (emms-mp3tag-erase-buffer emms-mp3tag-log-buffer)
      (message "Wait while set tags...")
      (save-excursion
        (dolist (track tracks)
          (when (emms-track-get track 'tag-modified)
            (setq filename (emms-track-name track)
                  old (emms-track-get track 'orig-track))
            ;; rename local file
            (when (and (emms-track-get track 'newname)
                       (eq (emms-track-get track 'type) 'file)
                       (file-writable-p (emms-track-name track)))
              (setq filename (emms-track-get track 'newname))
              (rename-file (emms-track-name track) filename)
              (emms-track-set old 'name filename)
              ;; for re-enter this function
              (emms-track-set track 'newname nil)
              (emms-track-set track 'name filename)
              (setq need-sync t)
              ;; register to emms-cache-db
              (funcall emms-cache-modified-function)
              (funcall emms-cache-set-function filename 'file old))
            ;; set tags to original track
            (dolist (tag emms-mp3tag-tags)
              (when (setq val (emms-track-get track (car tag)))
                (emms-track-set old (car tag) val)))
            ;; use mp3info to change tag in mp3 file
            (when (and (eq (emms-track-get track 'type) 'file)
                       (file-writable-p (emms-track-name track))
                       (setq func (assoc (file-name-extension filename) emms-mp3tag-tagfile-functions)))
              (setq exit
                    (if (functionp (cdr func))
                        (funcall (cdr func) track)
                      (emms-mp3tag-tag-file track (cadr func) (nth 2 func))))
              (if (zerop exit)
                  (emms-track-get track 'info-mtime (butlast (current-time)))
                (emms-mp3tag-log "Change tags of %s failed with exit value %d" filename exit)))
            ;; update track in playlist
            (when (and (setq pos (emms-track-get track 'position))
                       (marker-position pos))
              (set-buffer (marker-buffer pos))
              (goto-char pos)
              (funcall emms-playlist-update-track-function))
            ;; clear modified tag
            (emms-track-set track 'tag-modified nil))))
      (if (and need-sync (y-or-n-p "You have change some track names, sync the cache? "))
          (emms-cache-sync))
      (emms-mp3tag-display-log-buffer-maybe)
      (message "Set all mp3 tag done!")))
  (if arg (bury-buffer)))

(defun emms-mp3tag-submit-and-exit ()
  (interactive)
  (emms-mp3tag-submit t))

(defun emms-mp3tag-default-parser ()
  (let (next tracks track key val)
    (goto-char (point-min))
    (if (get-text-property (point) 'emms-track)
        (setq next (point))
      (setq next (next-single-property-change (point)
                                              'emms-track)))
    (when next
      (while
          (progn
            (goto-char next)
            (setq track (get-text-property (point) 'emms-track))
            (forward-line 1)
            (mapc (lambda (pair)
                    (when (string-match "\\s-*=\\s-*" pair)
                      (setq key (intern-soft (substring pair 0 (match-beginning 0)))
                            val (substring pair (match-end 0)))
                      (when (and key
                                 (> (length val) 0)
                                 (not (string= val (emms-track-get track key))))
                        (if (eq key 'name)
                            (emms-track-set track 'newname val)
                          (emms-track-set track key val))
                        (emms-track-set track 'tag-modified t))))
                  (split-string (buffer-substring (point)
                                                  (or
                                                   (setq next (next-single-property-change (point) 'emms-track))
                                                   (point-max)))
                                "\n"))
            (if (emms-track-get track 'tag-modified)
                (push track tracks))
            next))
      tracks)))

(defun emms-mp3tag-log (&rest args)
  (with-current-buffer (get-buffer-create emms-mp3tag-log-buffer)
    (goto-char (point-max))
    (insert (apply 'format args) "\n")))

;;; emms-mp3tagedit.el ends here
