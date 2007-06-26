;;; emms-source-playlist.el --- EMMS sources from playlist files

;; Copyright (C) 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.

;; Author: Jorgen Schäfer <forcer@forcix.cx>
;; Keywords: emms, mp3, mpeg, multimedia

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file contains track sources for EMMS which read playlist
;; files.  EMMS' own playlist files are supported as well as .m3u and
;; .pls files.

;;; Code:

;; Version control
(defvar emms-source-playlist-version "0.5 $Revision: 1.30 $"
  "emms-source-playlist.el version string")
;; $Id: emms-source-file.el,v 1.30 2005/08/11 06:16:15 yonirabkin Exp $

(require 'emms)
(require 'emms-source-file)

(defcustom emms-source-playlist-formats '(native pls m3u)
  "*A list of playlist formats.
Each entry must have at least three corresponding functions.

First, a function named `emms-source-playlist-FORMAT-p' which
returns non-nil if the current buffer is of the type FORMAT.  It
is called with no arguments.

Second, a function named `emms-source-playlist-parse-FORMAT'
which parses the current buffer into tracks.  It is called with
no arguments.

Third, a function named `emms-source-playlist-unparse-FORMAT'
which creates an output file in the type FORMAT that contains the
tracks of a playlist buffer.  It is called with two arguments:
The playlist buffer and the file buffer.

It is also recommended to have a function named
`emms-source-playlist-FORMAT-files' which returns a list of the
files contained in the playlist."
  :type '(repeat (symbol :tag "Format"))
  :group 'emms)

(defcustom emms-source-playlist-default-format nil
  "*The default format to use for saving playlists.
If this is nil, you will be prompted for a format to use."
  :type '(choice (const :tag "Prompt each time" nil)
                 (const :tag "Native" native)
                 (const :tag "m3u" m3u)
                 (const :tag "pls" pls)
                 (symbol :tag "Other"))
  :group 'emms)

;;; General playlist

(defsubst emms-source-playlist-p-sym (format)
  (intern (concat "emms-source-playlist-" (symbol-name format) "-p")))

(defsubst emms-source-playlist-parse-sym (format)
  (intern (concat "emms-source-playlist-parse-" (symbol-name format))))

(defsubst emms-source-playlist-unparse-sym (format)
  (intern (concat "emms-source-playlist-unparse-" (symbol-name format))))

(defsubst emms-source-playlist-files-sym (format)
  (intern (concat "emms-source-playlist-" (symbol-name format) "-files")))

(defun emms-source-playlist-p (format &optional parse-files)
  (let ((sym (emms-source-playlist-p-sym format)))
    (when (and (functionp sym)
               (or (not parse-files)
                   (functionp (emms-source-playlist-files-sym format))))
      (funcall sym))))

(defun emms-source-playlist-parse (format)
  (funcall (emms-source-playlist-parse-sym format)))

(defun emms-source-playlist-unparse (format playlist file)
  (funcall (emms-source-playlist-unparse-sym format) playlist file))

(defun emms-source-playlist-files (format)
  (let ((sym (emms-source-playlist-files-sym format)))
    (if (functionp sym)
        (funcall sym)
      (error "The `%s' format cannot parse files from a playlist" format))))

(defvar emms-source-playlist-format-history nil
  "List of recently-entered formats; used by `emms-playlist-save'.")

(defun emms-source-playlist-read-format ()
  "Read a playlist format from the user.
If `emms-source-playlist-default-format' is non-nil, use it
instead of prompting the user."
  (or emms-source-playlist-default-format
      (intern
       (completing-read
        (concat "Playlist format: (default: "
                (if emms-source-playlist-format-history
                    (car emms-source-playlist-format-history)
                  "native")
                ") ")
        (mapcar #'symbol-name emms-source-playlist-formats)
        nil nil nil 'emms-source-playlist-format-history
        (if emms-source-playlist-format-history
            (car emms-source-playlist-format-history)
          "native")))))

(defun emms-playlist-save (format file)
  "Store the current playlist to FILE as the type FORMAT.
The default format is specified by `emms-source-playlist-default-format'."
  (interactive (list (emms-source-playlist-read-format)
                     (read-file-name "Store as: "
                                     emms-source-file-default-directory
                                     emms-source-file-default-directory
                                     nil)))
  (with-temp-buffer
    (emms-source-playlist-unparse format
                                  (with-current-emms-playlist
                                    (current-buffer))
                                  (current-buffer))
    (let ((backup-inhibited t))
      (write-file file))))

(defun emms-source-playlist-determine-format (&optional parse-files)
  "Determine the playlist format of the current buffer.
If PARSE-FILES is specified, the given format must be able to
return a list of the files contained in the playlist."
  (catch 'return
    (let ((formats emms-source-playlist-formats))
      (while formats
        (when (emms-source-playlist-p (car formats) parse-files)
          (throw 'return (car formats)))
        (setq formats (cdr formats))))))

;;;###autoload (autoload 'emms-play-playlist "emms-source-playlist" nil t)
;;;###autoload (autoload 'emms-add-playlist "emms-source-playlist" nil t)
(define-emms-source playlist (file)
  "An EMMS source for playlists.
See `emms-source-playlist-formats' for a list of supported formats."
  (interactive (list (read-file-name "Playlist file: "
                                     emms-source-file-default-directory
                                     emms-source-file-default-directory
                                     t)))
  (mapc #'emms-playlist-insert-track
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let ((format (emms-source-playlist-determine-format)))
            (if format
                (emms-source-playlist-parse format)
              (error "Not a recognized playlist format"))))))

;;; EMMS native playlists

;; Format:
;; ;;; This is an EMMS playlist file. Play it with M-x emms-play-playlist
;; <sexpr>

(defun emms-source-playlist-native-p ()
  "Return non-nil if the current buffer contains a native EMMS playlist."
  (save-excursion
    (goto-char (point-min))
    (looking-at "^;;; This is an EMMS playlist file")))

(defun emms-source-playlist-parse-native ()
  "Parse the native EMMS playlist in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (read (current-buffer))))

(defun emms-source-playlist-unparse-native (in out)
  "Unparse a native playlist from IN to OUT.
IN should be a buffer with a EMMS playlist in it.
OUT should be the buffer where tracks are stored in the native EMMS format."
  (with-current-buffer in ;; Don't modify the position
    (save-excursion       ;; in the IN buffer
      (with-current-buffer out
        (insert ";;; This is an EMMS playlist file."
                " Play it with M-x emms-play-playlist\n")
        (insert "(")
        (let ((track (emms-source-playlist-first in))
              (firstp t))
          (while track
            (if (not firstp)
                (insert "\n ")
              (setq firstp nil))
            (prin1 track (current-buffer))
            (setq track (emms-source-playlist-next in))))
        (insert ")\n")))))

;;;###autoload (autoload 'emms-play-native-playlist "emms-source-playlist" nil t)
;;;###autoload (autoload 'emms-add-native-playlist "emms-source-playlist" nil t)
(define-emms-source native-playlist (file)
  "An EMMS source for a native EMMS playlist file."
  (interactive (list (read-file-name "Playlist file: "
                                     emms-source-file-default-directory
                                     emms-source-file-default-directory
                                     t)))
  (mapc #'emms-playlist-insert-track
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (when (not (emms-source-playlist-native-p))
            (error "Not a native EMMS playlist file."))
          (emms-source-playlist-parse-native))))

;;; m3u files

;; Format:
;; Either a list of filename-per-line, ignore lines beginning with #
;; or:
;; #EXTM3U
;; #EXTINF:<length in seconds>,<name>
;; <filename>

; emms-source-playlist-m3u-p
; emms-source-playlist-parse-m3u
; emms-source-playlist-m3u-files
; emms-source-playlist-unparse-m3u

(defun emms-source-playlist-m3u-p ()
  "Return non-nil if the current buffer contains an m3u playlist.

We currently have no metric for determining whether a buffer is
an .m3u playlist based on its contents alone, so we assume that
the more restrictive playlist formats have already been
detected and simply return non-nil always."
  t)

(defun emms-source-playlist-parse-m3u ()
  "Parse the m3u playlist in the current buffer."
  (mapcar (lambda (file)
            (if (string-match "\\`http://\\|\\`mms://" file)
                (emms-track 'url file)
              (emms-track 'file file)))
          (emms-source-playlist-m3u-files)))

(defun emms-source-playlist-m3u-files ()
  "Extract a list of filenames from the given m3u playlist.

Empty lines and lines starting with '#' are ignored."
  (let ((files nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[^# \n].*$" nil t)
        (setq files (cons (match-string 0) files))))
    (nreverse files)))

(defun emms-source-playlist-unparse-m3u (in out)
  "Unparse an m3u playlist from IN to OUT.
IN should be a buffer containing an m3u playlist.
OUT should be the buffer where tracks are stored in m3u format."
  (with-current-buffer in ;; Don't modify the position
    (save-excursion       ;; in the IN buffer
      (with-current-buffer out
        (let ((track (emms-source-playlist-first in)))
          (while track
            (insert (emms-track-name track) ?\n)
            (setq track (emms-source-playlist-next in))))))))

;;;###autoload (autoload 'emms-play-m3u-playlist "emms-source-playlist" nil t)
;;;###autoload (autoload 'emms-add-m3u-playlist "emms-source-playlist" nil t)
(define-emms-source m3u-playlist (file)
  "An EMMS source for an m3u playlist file."
  (interactive (list (read-file-name "Playlist file: "
                                     emms-source-file-default-directory
                                     emms-source-file-default-directory
                                     t)))
  (mapc #'emms-playlist-insert-track
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (when (not (emms-source-playlist-m3u-p))
            (error "Not an m3u playlist file."))
          (emms-source-playlist-parse-m3u))))

;;; pls files

;; Format:
;; A list of one filename per line.
;; [playlist]
;; NumberOfEntries=<num_entries>
;; File<position>=<filename>

; emms-source-playlist-pls-p
; emms-source-playlist-parse-pls
; emms-source-playlist-pls-files
; emms-source-playlist-unparse-pls

(defun emms-source-playlist-pls-p ()
  "Return non-nil if the current buffer contains a pls playlist."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^File[0-9]*=.+$" nil t)
        t
      nil)))

(defun emms-source-playlist-parse-pls ()
  "Parse the pls playlist in the current buffer."
  (mapcar (lambda (file)
            (if (string-match "\\`http://\\|\\`mms://" file)
                (emms-track 'url file)
              (emms-track 'file file)))
          (emms-source-playlist-pls-files)))

(defun emms-source-playlist-pls-files ()
  "Extract a list of filenames from the given pls playlist.

Empty lines and lines starting with '#' are ignored."
  (let ((files nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^File[0-9]*=\\(.+\\)$" nil t)
        (setq files (cons (match-string 1) files))))
    (nreverse files)))

(defun emms-source-playlist-unparse-pls (in out)
  "Unparse a pls playlist from IN to OUT.
IN should be a buffer conatining a pls playlist.
OUT should be the buffer where tracks are stored in pls format."
  (with-current-buffer in ;; Don't modify the position
    (save-excursion       ;; in the IN buffer
      (with-current-buffer out
        (let ((pos 0))
          (insert "[playlist]\n")
          (save-restriction
            (narrow-to-region (point) (point))
            (let ((track (emms-source-playlist-first in)))
              (while track
                (setq pos (1+ pos))
                (insert "File" (number-to-string pos) "="
                        (emms-track-name track) ?\n)
                (setq track (emms-source-playlist-next in))))
            (goto-char (point-min))
            (insert "NumberOfEntries=" (number-to-string pos) ?\n)))))))

;;;###autoload (autoload 'emms-play-pls-playlist "emms-source-playlist" nil t)
;;;###autoload (autoload 'emms-add-pls-playlist "emms-source-playlist" nil t)
(define-emms-source pls-playlist (file)
  "An EMMS source for a pls playlist file."
  (interactive (list (read-file-name "Playlist file: "
                                     emms-source-file-default-directory
                                     emms-source-file-default-directory
                                     t)))
  (mapc #'emms-playlist-insert-track
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (when (not (emms-source-playlist-pls-p))
            (error "Not a pls playlist file."))
          (emms-source-playlist-parse-pls))))

;;; extm3u files

;; Format:
;; #EXTM3U
;; #EXTINF:<length in seconds>,<name>
;; <filename>

; emms-source-playlist-extm3u-p
; emms-source-playlist-parse-extm3u
; emms-source-playlist-unparse-extm3u

;;       (erase-buffer)
;;       (insert "#EXTM3U\n")
;;       (mapc (lambda (track)
;; 	      (let ((time (or (emms-track-get track 'info-mtime) ""))
;; 		    (artist (emms-track-get track 'info-artist))
;; 		    (title (emms-track-get track 'info-title))
;; 		    (name (emms-track-get track 'name)))
;; 		(insert (format "#EXTINF: %s,%s - %s\n%s\n"
;; 				time artist title name))))
;;             tracklist)
;;       (save-buffer)
;;       (kill-buffer (current-buffer)))))

;; Not implemented yet

;;; Helper functions

(defun emms-source-playlist-first (buf)
  "Return the first track in BUF.
This moves point."
  (with-current-buffer buf
    (condition-case nil
        (progn
          (emms-playlist-first)
          (emms-playlist-track-at (point)))
      (error
       nil))))

(defun emms-source-playlist-next (buf)
  "Return the next track in BUF.
This moves point."
  (with-current-buffer buf
    (condition-case nil
        (progn
          (emms-playlist-next)
          (emms-playlist-track-at (point)))
      (error
       nil))))

;;; Adding playlists as files

;;;###autoload (autoload 'emms-play-playlist-file "emms-source-playlist" nil t)
;;;###autoload (autoload 'emms-add-playlist-file "emms-source-playlist" nil t)
(define-emms-source playlist-file (file)
  "An EMMS source for playlist files.
This adds the given file to the current EMMS playlist buffer,
without adding its contents.

See `emms-source-playlist-formats' for a list of supported formats."
  (interactive (list (read-file-name "Playlist file: "
                                     emms-source-file-default-directory
                                     emms-source-file-default-directory
                                     t)))
  (emms-playlist-insert-track
   (emms-track 'playlist (expand-file-name file))))

;;;###autoload (autoload 'emms-play-playlist-directory
;;;###autoload           "emms-source-playlist" nil t)
;;;###autoload (autoload 'emms-add-playlist-directory
;;;###autoload           "emms-source-playlist" nil t)
(define-emms-source playlist-directory (dir)
  "An EMMS source for a whole directory tree of playlist files.
If DIR is not specified, it is queried from the user."
  (interactive (list
                (emms-read-directory-name "Play directory: "
                                          emms-source-file-default-directory
                                          emms-source-file-default-directory
                                          t)))
  (mapc (lambda (file)
          (unless (or (let ((case-fold-search nil))
                        (string-match emms-source-file-exclude-regexp file))
                      (file-directory-p file))
            (emms-playlist-insert-track
             (emms-track 'playlist (expand-file-name file)))))
        (directory-files dir t "^[^.]")))

;;;###autoload (autoload 'emms-play-playlist-directory-tree
;;;###autoload           "emms-source-playlist" nil t)
;;;###autoload (autoload 'emms-add-playlist-directory-tree
;;;###autoload           "emms-source-file" nil t)
(define-emms-source playlist-directory-tree (dir)
  "An EMMS source for multiple directory trees of playlist files.
If DIR is not specified, it is queried from the user."
  (interactive (list
                (emms-read-directory-name "Play directory tree: "
                                          emms-source-file-default-directory
                                          emms-source-file-default-directory
                                          t)))
  (mapc (lambda (file)
          (unless (let ((case-fold-search nil))
                    (string-match emms-source-file-exclude-regexp file))
            (emms-playlist-insert-track
             (emms-track 'playlist file))))
        (emms-source-file-directory-tree (expand-file-name dir) "^[^.]")))

(provide 'emms-source-playlist)
;;; emms-source-playlist.el ends here
