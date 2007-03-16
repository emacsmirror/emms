;;; emms-tag-editor.el --- Edit track tags.

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
;;   (require 'emms-tag-editor)

;;; Code:

(provide 'emms-tag-editor)
(eval-when-compile
  (require 'cl))
(require 'emms)
(require 'emms-info-mp3info)
(require 'emms-playlist-mode)
(require 'emms-mark)
(require 'format-spec)

(defvar emms-tag-editor-tags
  '((info-artist      . "a")
    (info-title       . "t")
    (info-album       . "l")
    (info-tracknumber . "n")
    (info-year        . "y")
    (info-genre       . "g")
    (info-date        . "d")
    (info-note        . "c"))
"A list to setup format.")

(defvar emms-tag-editor-edit-buffer "*EMMS-TAGS*"
  "Buffer name to edit tags")
(defvar emms-tag-editor-log-buffer "*EMMS-LOG*"
  "Buffer name of tag edit log")

(defun emms-tag-editor-make-format (tags)
  (concat "%m\n" (emms-propertize (format "%-16s = " "name")
                             'read-only t 'rear-nonsticky t
                             'face 'bold)
          "%f\n"
          (mapconcat
           (lambda (tag)
             (concat (emms-propertize (format "%-16s = " (symbol-name tag))
                                 'read-only t 'rear-nonsticky t
                                 'face 'bold)
                     "%" (cdr (assoc tag emms-tag-editor-tags))))
           tags "\n")
          "\n\n"))

(defvar emms-tag-editor-formats
  (let* ((tags (mapcar 'car emms-tag-editor-tags))
         (default (emms-tag-editor-make-format (remove 'info-date tags))))
    `(("mp3" . ,default)
      ("ogg" . ,(emms-tag-editor-make-format (remove 'info-year tags)))
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

You can add new specification in `emms-tag-editor-tags' and use
`emms-tag-editor-make-format' to help create a new format.

The CDR part also can be a function, which accept one parameter, the
track, and should return a string to insert to `emms-tag-editor-edit-buffer'.
")

(defvar emms-tag-editor-get-format-function 'emms-tag-editor-get-format
  "Function to decide which format to use for format the track.")

(defvar emms-tag-editor-parse-function 'emms-tag-editor-default-parser
  "Function to parse tags in `emms-tag-editor-edit-buffer'. It should find
all modified tags, and return all the tracks. The tracks which tag has
been modified should set a property 'tag-modified to t, and if the
track name have been change, the function should set a new property
'newname instead set the 'name directly.

See also `emms-tag-editor-default-parser'.")

(defvar emms-tag-editor-tagfile-functions
  '(("mp3" "mp3info"
     ((info-artist      . "a")
      (info-title       . "t")
      (info-album       . "l")
      (info-tracknumber . "n")
      (info-year        . "y")
      (info-genre       . "g")
      (info-note        . "c")))
    ("ogg" . emms-tag-editor-tag-ogg))
  "A List for change tag in files. If the extern program set tag by
command line options one by one such as mp3info, the list should like:
 (EXTENSION PROGRAM COMMAND_LINE_OPTIONS)

Otherwise, a function that accept a parameter, the track, should be
given.

See also `emms-tag-editor-tag-file' and `emms-tag-editor-tag-ogg'.
")

(defun emms-tag-editor-tag-ogg (track)
  (let (args val)
    (mapc (lambda (tag)
            (let ((info-tag (intern (concat "info-" tag))))
              (when (> (length (setq val (emms-track-get track info-tag))) 0)
                (setq args (append (list "-t" (concat tag "=" val)) args)))))
          '("artist" "title" "album" "tracknumber" "date" "genre" "note"))
    (when args
      (apply #'call-process "vorbiscomment" nil
             (get-buffer-create emms-tag-editor-log-buffer)
             nil
             "-w"
             (append args (list (emms-track-name track)))))))

(defun emms-tag-editor-tag-file (track program tags)
  "Change tag in FILE use PROGRAM. The TAGS is given in `emms-tag-editor-tagfile-functions'."
  (let (args val)
    (mapc (lambda (tag)
            (when (> (length (setq val (emms-track-get track (car tag)))) 0)
              (setq args (append (list (concat "-" (cdr tag)) val) args))))
          tags)
    (apply 'call-process program
           nil (get-buffer-create emms-tag-editor-log-buffer) nil
           filename args)))

(defun emms-tag-editor-get-format (track)
  (let ((format
         (assoc (file-name-extension (emms-track-name track))
                emms-tag-editor-formats)))
    (if format
        (cdr format)
      (cdr (assoc "default" emms-tag-editor-formats)))))

(defun emms-tag-editor-format-track (track)
  (let ((format (funcall emms-tag-editor-get-format-function track)))
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
                             emms-tag-editor-tags)))))))

(defun emms-tag-editor-track-at (&optional pos)
  (let ((track (emms-playlist-track-at pos))
        newtrack)
    (when track
      (setq newtrack (copy-sequence track))
      (emms-track-set newtrack 'position (point-marker))
      (emms-track-set newtrack 'orig-track track)
      newtrack)))

(defsubst emms-tag-editor-erase-buffer (&optional buf)
  (let ((inhibit-read-only t))
    (save-excursion
      (set-buffer (get-buffer-create buf))
      (erase-buffer))))

(defsubst emms-tag-editor-insert-track (track)
  (and track
       (insert (emms-tag-editor-format-track track))))

(defsubst emms-tag-editor-display-log-buffer-maybe ()
  (if (> (buffer-size (get-buffer emms-tag-editor-log-buffer)) 0)
      (display-buffer emms-tag-editor-log-buffer)))

(defun emms-tag-editor-insert-tracks (tracks)
  (save-excursion
    (emms-tag-editor-erase-buffer emms-tag-editor-log-buffer)
    (emms-tag-editor-erase-buffer emms-tag-editor-edit-buffer)
    (set-buffer (get-buffer emms-tag-editor-edit-buffer))
    (mapc 'emms-tag-editor-insert-track tracks)
    (emms-tag-editor-mode)
    (pop-to-buffer (current-buffer))
    (goto-char (point-min))
    (emms-tag-editor-display-log-buffer-maybe)))

(defun emms-tag-editor-edit-track (track)
  (interactive (list (emms-tag-editor-track-at)))
  (if (null track)
      (message "No track at point!")
    (emms-tag-editor-insert-tracks (list track))))

(defun emms-tag-editor-edit-marked-tracks ()
  (interactive)
  (let ((tracks (emms-mark-mapcar-marked-track 'emms-tag-editor-track-at t)))
    (if (null tracks)
        (message "No track marked!")
      (emms-tag-editor-insert-tracks tracks))))

(defun emms-tag-editor-edit ()
  "Edit tags of track at point or marked tracks"
  (interactive)
  (if (emms-mark-has-markedp)
      (emms-tag-editor-edit-marked-tracks)
    (emms-tag-editor-edit-track (emms-tag-editor-track-at))))

(defvar emms-tag-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'emms-tag-editor-next-field)
    (define-key map [backtab] 'emms-tag-editor-prev-field)
    (define-key map "\C-c\C-n" 'emms-tag-editor-next-track)
    (define-key map "\C-c\C-p" 'emms-tag-editor-prev-track)
    (define-key map "\C-c\C-c" 'emms-tag-editor-submit-and-exit)
    (define-key map "\C-c\C-s" 'emms-tag-editor-submit)
    (define-key map "\C-x\C-s" 'emms-tag-editor-submit)
    (define-key map "\C-c\C-r" 'emms-tag-editor-set-all)
    (define-key map "\C-c\C-a" 'emms-tag-editor-replace-in-tag)
    (define-key map "\C-c\C-t" 'emms-tag-editor-transpose-tag)
    map))
(define-key emms-playlist-mode-map "E" 'emms-tag-editor-edit)

(define-derived-mode emms-tag-editor-mode text-mode "Tag-Edit"
  "Major mode to edit track tags.
\\{emms-tag-editor-mode-map}")

(defun emms-tag-editor-set-all (tag value)
  "Replace all track's TAG to VALUE. If turn transient-mark-mode on,
you can apply the command to a selected region."
  (interactive
   (list (completing-read "Set tag: "
                          emms-tag-editor-tags nil t)
         (read-from-minibuffer "To: ")))
  (save-excursion
    (save-restriction
      (if (and mark-active transient-mark-mode)
          (narrow-to-region (region-beginning) (region-end)))
      (goto-char (point-min))
      (while (re-search-forward (concat "^" (regexp-quote tag)) nil t)
        (skip-chars-forward " \t=")
        (delete-region (point) (line-end-position))
        (insert value)))))

(defun emms-tag-editor-replace-in-tag (tag from to)
  (interactive 
   (cons (completing-read "Replace in tag: "
                          emms-tag-editor-tags nil t)
         (let ((common (query-replace-read-args
                        (if (and transient-mark-mode mark-active)
                            "Query replace regexp in region"
                          "Query replace regexp")
                        t)))
           (butlast common))))
  (save-excursion
    (save-restriction
      (if (and mark-active transient-mark-mode)
          (narrow-to-region (region-beginning) (region-end)))
      (setq tag (concat (regexp-quote tag) "[ \t]+=[ \t]+"))
      (goto-char (point-min))
      (map-y-or-n-p
       (lambda (match)
         (format "Replace %s to %s" match to))
       (lambda (match)
         (delete-region (- (point) (length match)) (point))
         (insert to))
       (lambda ()
         (if (and (save-excursion
                    (re-search-backward tag (line-beginning-position) t))
                  (re-search-forward from (line-end-position) t))
             (match-string 0)
           (let (found)
             (while (and (not found)
                         (re-search-forward tag nil t))
               (if (re-search-forward from (line-end-position) t)
                   (setq found t)))
             (and found (match-string 0)))))))))

(defun emms-tag-editor-transpose-tag (tag1 tag2)
  (interactive
   (let* ((tag1 (intern (completing-read "Tag1: "
                                         emms-tag-editor-tags nil t)))
          (tag2 (intern (completing-read "Tag2: "
                                         (assq-delete-all tag1 (copy-sequence emms-tag-editor-tags))
                                         nil t))))
     (list tag1 tag2)))
  (save-excursion
    (save-restriction
      (if (and mark-active transient-mark-mode)
          (narrow-to-region (region-beginning) (region-end)))
      (let* ((emms-playlist-buffer-p t)
             (tracks (emms-playlist-tracks-in-region (point-min)
                                                     (point-max)))
             (inhibit-read-only t)
             temp)
        (erase-buffer)
        (dolist (track tracks)
          (setq temp (emms-track-get track tag1))
          (emms-track-set track tag1 (emms-track-get track tag2))
          (emms-track-set track tag2 temp)
          (emms-tag-editor-insert-track track))))))

(defun emms-tag-editor-next-field (arg)
  (interactive "p")
  (if (> arg 0)
      (re-search-forward "\\s-*=[ \t]*" nil nil arg)
    (emms-tag-editor-prev-field (- arg))))

(defun emms-tag-editor-prev-field (arg)
  (interactive "p")
  (if (< arg 0)
      (emms-tag-editor-next-field (- arg))
    (skip-chars-backward " \t=")
    (re-search-backward "\\s-*=[ \t]*" nil nil arg)
    (skip-chars-forward " \t=")))

(defun emms-tag-editor-prev-track ()
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

(defun emms-tag-editor-next-track ()
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

(defun emms-tag-editor-submit (arg)
  "Make modified tags take affect. With prefiex argument, bury tag
edit buffer."
  (interactive "P")
  (let ((tracks (funcall emms-tag-editor-parse-function))
        filename func exit old pos val need-sync)
    (if (not (and tracks (y-or-n-p "Submit changes? ")))
        (message "Nothing have to do!")
      (emms-tag-editor-erase-buffer emms-tag-editor-log-buffer)
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
            (dolist (tag emms-tag-editor-tags)
              (when (setq val (emms-track-get track (car tag)))
                (emms-track-set old (car tag) val)))
            ;; use mp3info to change tag in mp3 file
            (when (and (eq (emms-track-get track 'type) 'file)
                       (file-writable-p (emms-track-name track))
                       (setq func (assoc (file-name-extension filename) emms-tag-editor-tagfile-functions)))
              (setq exit
                    (if (functionp (cdr func))
                        (funcall (cdr func) track)
                      (emms-tag-editor-tag-file track (cadr func) (nth 2 func))))
              (if (zerop exit)
                  (emms-track-get track 'info-mtime (butlast (current-time)))
                (emms-tag-editor-log "Change tags of %s failed with exit value %d" filename exit)))
            ;; update track in playlist
            (when (and (setq pos (emms-track-get track 'position))
                       (marker-position pos))
              (set-buffer (marker-buffer pos))
              (goto-char pos)
              (funcall emms-playlist-update-track-function))
            ;; clear modified tag
            (emms-track-set track 'tag-modified nil))))
      (if (and (featurep 'emms-cache)
               need-sync
               (y-or-n-p "You have change some track names, sync the cache? "))
          (and (fboundp 'emms-cache-sync) ; silence byte-compiler
               (emms-cache-sync)))
      (unless (emms-tag-editor-display-log-buffer-maybe)
        (message "Set all mp3 tag done!"))))
  (if arg (bury-buffer)))

(defun emms-tag-editor-submit-and-exit ()
  (interactive)
  (emms-tag-editor-submit t))

(defun emms-tag-editor-default-parser ()
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

(defun emms-tag-editor-log (&rest args)
  (with-current-buffer (get-buffer-create emms-tag-editor-log-buffer)
    (goto-char (point-max))
    (insert (apply 'format args) "\n")))

;;; emms-tag-editor.el ends here
