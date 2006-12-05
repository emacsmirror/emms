;;; emms-mp3tag.el ---

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Time-stamp: <Ye Wenbin 2006-12-05 19:07:12>
;; Version: $Id: emms-mp3tag.el,v 1.5 2006/12/05 00:57:14 ywb Exp ywb $
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
    (info-note        . "c")))

(defvar emms-mp3tag-edit-buffer "*EMMS-TAGS*"
  "Buffer name to edit mp3 tags")
(defvar emms-mp3tag-log-buffer "*EMMS-LOG*"
  "Buffer name of mp3tag edit log")

(defvar emms-mp3tag-format
  `(("default"
     ,(format "%%m\n%s\n\n"
              (mapconcat
               (lambda (tag)
                 (concat (propertize (format "%-16s = " (symbol-name (car tag)))
                                     'read-only t 'rear-nonsticky t)
                         "%" (cdr tag)))
               (append '((name . "f")) emms-mp3tag-tags) "\n"))
     emms-mp3tag-default-parser))
  "The contents of this variable should look like:
 ((NAME FORMATER PARSER) ...)

The NAME is use to select proper format, and FORMATER should be a
string or a function. When it is a string, it can use specification
as:
 m     --     Track description
 f     --     Track name
 a     --     Track info-artist
 t     --     Track info-title
 l     --     Track info-album
 n     --     Track info-tracknumber
 y     --     Track info-year
 g     --     Track info-genre
 c     --     Track info-note

When it is a function, it recept a parameter, the track, and should
return a string that to insert to `emms-mp3tag-edit-buffer'.

The PARSER is a function to collect all track info in
`emms-mp3tag-edit-buffer'. It should return the new tracks. If
the track tag changed, it should add a new property tag-modified
and set to non-nil. If the track name change, it should set new
newname to the new file name.
")

(defvar emms-mp3tag-selected-format
  (assoc "default" emms-mp3tag-format))

(defun emms-mp3tag-select-format (format)
  (interactive
   (list (completing-read "Set edit format to: "
                          emms-mp3tag-format nil t)))
  (setq emms-mp3tag-select-format (assoc "default" emms-mp3tag-format)))

(defun emms-mp3tag-format-track (format track)
  (if (stringp format)
      (format-spec
       format
       (apply 'format-spec-make
              ?m (emms-propertize (emms-track-force-description track)
                                  'face 'emms-playlist-track-face
                                  'emms-track (copy-sequence track))
              ?f (emms-track-name track)
              (apply 'append
                     (mapcar
                      (lambda (pair)
                        (list (aref (cdr pair) 0)
                              (or (emms-track-get track (car pair)) "")))
                      emms-mp3tag-tags))))
    (funcall format track)))

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

;; (defun emms-mp3tag-insert-track (track)
;;   (cond ((null track) nil)
;;         ((not (eq (emms-track-get track 'type) 'file))
;;          (emms-mp3tag-log "Track %s is not a local file!" (emms-track-name track)))
;;         ((not (file-writable-p (emms-track-name track)))
;;          (emms-mp3tag-log "The file %s is not writable" (emms-track-name track)))
;;         (t (insert (emms-mp3tag-format-track
;;                     (cadr emms-mp3tag-selected-format) track)))))

(defsubst emms-mp3tag-insert-track (track)
  (and track
       (insert (emms-mp3tag-format-track
                (cadr emms-mp3tag-selected-format) track))))

(defun emms-mp3tag-insert-tracks (tracks)
  (save-excursion
    (emms-mp3tag-erase-buffer emms-mp3tag-log-buffer)
    (emms-mp3tag-erase-buffer emms-mp3tag-edit-buffer)
    (set-buffer (get-buffer emms-mp3tag-edit-buffer))
    (mapc 'emms-mp3tag-insert-track tracks)
    (emms-mp3tag-mode 1)
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
  (interactive)
  (if (emms-mark-has-markedp)
      (emms-mp3tag-edit-marked-tracks)
    (emms-mp3tag-edit-track (emms-mp3tag-track-at))))

(defsubst emms-mp3tag-display-log-buffer-maybe ()
  (if (> (buffer-size (get-buffer emms-mp3tag-log-buffer)) 0)
      (display-buffer emms-mp3tag-log-buffer)))

(defvar emms-mp3tag-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'emms-mp3tag-next-field)
    (define-key map [backtab] 'emms-mp3tag-prev-field)
    (define-key map "\C-c\C-n" 'emms-mp3tag-next-track)
    (define-key map "\C-c\C-p" 'emms-mp3tag-prev-track)
    (define-key map "\C-c\C-c" 'emms-mp3tag-submit)
    (define-key map "\C-c\C-r" 'emms-mp3tag-replace-all)
    map))
(define-key emms-playlist-mode-map "E" 'emms-mp3tag-edit)

(define-minor-mode emms-mp3tag-mode
  "A minor mode to edit mp3tag.
\\{emms-mp3tag-mode-map}"
  :lighter " MP3Tag"
  :keymap emms-mp3tag-mode-map)

(defun emms-mp3tag-replace-all (name value)
  (interactive
   (list (completing-read "Replace tag: "
                          emms-mp3tag-tags nil t)
         (read-from-minibuffer "Set tag to: ")))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (concat "^" (regexp-quote name)) nil t)
      (skip-chars-forward " \t=")
      (delete-region (point) (line-end-position))
      (insert value))))

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

(defun emms-mp3tag-submit ()
  (interactive)
  (let ((tracks (funcall (nth 2 emms-mp3tag-selected-format)))
        filename exit old pos args val need-sync)
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
            (setq args nil)
            (dolist (tag emms-mp3tag-tags)
              (when (setq val (emms-track-get track (car tag)))
                (emms-track-set old (car tag) val)
                (setq args (append args (list (concat "-" (cdr tag)) val)))))
            ;; use mp3info to change tag in mp3 file
            (if (and (eq (emms-track-get track 'type) 'file)
                     (file-writable-p (emms-track-name track))
                     (string-match filename "\\.mp3\\'"))
                (if (zerop (setq exit
                                 (apply 'call-process emms-info-mp3info-program-name
                                        nil nil nil
                                        filename args)))
                    ;; for `emms-cache-sync' not call `emms-info-functions' again
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
      (message "Set all mp3 tag done!"))))

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
