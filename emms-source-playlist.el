;;; emms-source-playlist.el --- EMMS sources from playlist files

;; Copyright (C) 2003, 2004, 2005, 2006 Free Software Foundation, Inc.

;; Author: Jorgen Schäfer <forcer@forcix.cx>
;; Keywords: emms, mp3, mpeg, multimedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301 USA

;;; Commentary:

;; This file contains track sources for EMMS which read playlist
;; files. EMMS' own playlist files are supported as well als the
;; typical .m3u and .pls files.

;;; Code:

;; Version control
(defvar emms-source-playlist-version "0.1 $Revision: 1.30 $"
  "emms-source-playlist.el version string")
;; $Id: emms-source-file.el,v 1.30 2005/08/11 06:16:15 yonirabkin Exp $

(require 'emms)
(require 'emms-source-file)

(defcustom emms-source-playlist-formats
  '((emms-source-playlist-native-p emms-source-playlist-parse-native)
    (emms-source-playlist-pls-p emms-source-playlist-parse-pls)
    (emms-source-playlist-m3u-p emms-source-playlist-parse-m3u))
  "*A list of playlist format functions.
Each entry is a list with two elements:
A function which returns non-nil if the current buffer is of this
type, and a function which parses such a buffer.
The former is called with no arguments, while the latter is
called with two buffers: The playlist buffer and the file buffer."
  :type 'sexpr
  :group 'emms)

;;; General playlist

;;;###autoload (autoload 'emms-play-playlist "emms-source-playlist" nil t)
;;;###autoload (autoload 'emms-add-playlist "emms-source-playlist" nil t)
(define-emms-source playlist (file)
  "An EMMS source for playlists.
See `emms-source-playlist-formats' for supported formats."
  (interactive (list (read-file-name "Playlist file: "
                                     emms-source-file-default-directory
                                     emms-source-file-default-directory
                                     t)))
  (mapc #'emms-playlist-insert-track
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (catch 'return
            (let ((formats emms-source-playlist-formats))
              (while formats
                (when (funcall (caar formats))
                  (throw 'return (funcall (cadr (car formats)))))
                (setq formats (cdr formats))))
            (error "Not a recognized playlist format")))))

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
OUT should be a buffer to get the native EMMS format."
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

(defun emms-playlist-save (file)
  "Store the current playlist in a native format."
  (interactive (list (read-file-name "Store as: "
                                     emms-source-file-default-directory
                                     emms-source-file-default-directory
                                     nil)))
  (with-temp-buffer
    (emms-source-playlist-unparse-native (with-current-emms-playlist
                                           (current-buffer))
                                         (current-buffer))
    (let ((backup-inhibited t))
      (write-file file))))

;;; m3u files

;; Format:
;; Either a list of filename-per-line, ignore lines beginning with #
;; or:
;; #EXTM3U
;; #EXTINF:<length in seconds>,<name>
;; <filename>

; emms-source-playlist-m3u-p
; emms-source-playlist-parse-m3u
; emms-source-playlist-unparse-m3u

(defun emms-source-playlist-m3u-p ()
  "Return non-nil if the current buffer contains a native EMMS playlist."
  t)

(defun emms-source-playlist-parse-m3u ()
  "Parse the native EMMS playlist in the current buffer."
  (mapcar (lambda (file)
            (if (string-match "\\`http://" file)
                (emms-track 'url file)
              (emms-track 'file (expand-file-name file))))
          (emms-source-playlist-parse-m3u-1)))

(defun emms-source-playlist-parse-m3u-1 ()
  "Extract a list of filenames from the given .m3u playlist.
Empty lines and lines starting with '#' are ignored."
  (let ((files nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[^# \n].*$" nil t)
        (setq files (cons (match-string 0) files))))
    (nreverse files)))

;;; pls files

;; Format:
;; A list of one filename per line.
;; File<position>=<filename>

; emms-source-playlist-pls-p
; emms-source-playlist-parse-pls
; emms-source-playlist-unparse-pls

(defun emms-source-playlist-pls-p ()
  "Return non-nil if the current buffer contains a native EMMS playlist."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^File[0-9]*=.+$" nil t)
        t
      nil)))

(defun emms-source-playlist-parse-pls ()
  "Parse the native EMMS playlist in the current buffer."
  (mapcar (lambda (file)
            (if (string-match "\\`http://" file)
                (emms-track 'url file)
              (emms-track 'file (expand-file-name file))))
          (emms-source-playlist-parse-pls-1)))

(defun emms-source-playlist-parse-pls-1 ()
  "Extract a list of filenames from the given .pls playlist.
Empty lines and lines starting with '#' are ignored."
  (let ((files nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^File[0-9]*=\\(.+\\)$" nil t)
        (setq files (cons (match-string 1) files))))
    (nreverse files)))

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

;;; Old stuff:
;; 
;; (defun emms-playlist-save (playlist filename)
;;   "Save a playlist in the native EMMS format."
;;   (interactive "bPlaylist buffer name: \nFFile to save playlist as: ")
;;   (let ((tracklist '()))
;;     (condition-case nil
;;         (with-current-buffer playlist
;;           (save-excursion
;;             (emms-playlist-first)
;;             (while (emms-playlist-track-at)
;;               (setq tracklist (cons (emms-playlist-track-at)
;;                                     tracklist))
;;               (emms-playlist-next))))
;;       (error nil))
;;     (setq tracklist (nreverse tracklist))
;;     ;; tracklist complete, let's write it !
;;     (with-current-buffer (find-file-noselect filename)
;;       (erase-buffer)
;;       (prin1 tracklist (current-buffer))
;;       (insert "\n")
;;       (save-buffer)
;;       (kill-buffer (current-buffer)))))
;; 
;; (defun emms-playlist-save-active (filename)
;;   "Save the active EMMS playlist in native format."
;;   (interactive "FFile to save playlist as: ")
;;   (emms-playlist-save emms-playlist-buffer filename))
;; 
;; (defun emms-playlist-save-as-m3u (playlist filename)
;;   "Save a playlist in .m3u format."
;;   (interactive "bPlaylist buffer name: \nFFile to save playlist as: ")
;;   (let ((tracklist '()))
;;     (condition-case nil
;;         (with-current-buffer playlist
;;           (save-excursion
;;             (emms-playlist-first)
;;             (while (emms-playlist-track-at)
;;               (setq tracklist (cons (emms-playlist-track-at)
;;                                     tracklist))
;;               (emms-playlist-next))))
;;       (error nil))
;;     (setq tracklist (nreverse tracklist))
;;     ;; tracklist complete, let's write it !
;;     (with-current-buffer (find-file-noselect filename)
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
;; 
;; (defun emms-playlist-save-current-as-m3u (filename)
;;   "Save the active EMMS playlist in m3u format."
;;   (interactive "FFile to save playlist as: ")
;;   (emms-playlist-save-as-m3u emms-playlist-buffer filename))


(provide 'emms-source-playlist)
;;; emms-source-playlist.el ends here
