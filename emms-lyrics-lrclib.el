;;; emms-lyrics-lrclib.el --- Fetch synchronized lyrics through LRCLIB -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Daniel Semyonov <daniel@dsemy.com>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; EMMS is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING. If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; This file provides a command/track initialization function which
;; automatically fetches synchronized lyrics for tracks (the current
;; track interactively) through an LRCLIB server.

;;; Code:

(eval-when-compile
  (declare-function json-parse-buffer "json.c"))

(require 'emms-lyrics)
(require 'emms-later-do)

(defgroup emms-lyrics-lrclib nil
  "EMMS module for fetching synchronized lyrics through LRCLIB servers."
  :group 'emms-lyrics
  :prefix "emms-lyrics-lrclib-")

(defcustom emms-lyrics-lrclib-url "https://lrclib.net/api/"
  "Base URL for LRCLIB API requests."
  :type 'string)

(defconst emms-lyrics-lrclib-max-requests 250
  "Maximum number of concurrent requests to LRCLIB.")

(defvar emms-lyrics-lrclib-requests 0
  "Current number of concurrent requests to LRCLIB.")

(defun emms-lyrics-lrclib-encode-name (name)
  "Encode (artist/album/track) NAME for an LRCLIB search."
  (and (stringp name) (string-replace " " "+" name)))

(defun emms-lyrics-lrclib-parse (_ file track interactive)
  "Parse and save synced lyrics in FILE.
If TRACK is the selected track in the current playlist, catch up.
When INTERACTIVE is non-nil, display messages and confirm overwrite."
  (unwind-protect
      (let (lyrics)
        (search-forward "\n\n")
        (if-let* (((functionp 'json-available-p))
                   ((json-available-p))
                   (p (json-parse-buffer :null-object nil)))
            (and (hash-table-p p) (setq lyrics (gethash "syncedLyrics" p)))
          (when-let* ((beg (search-forward "\"syncedLyrics\":\"" nil t))
                      (end (1- (search-forward-regexp "[^\\]\"" nil t))))
            (replace-string-in-region "\\n" "\n" beg end)
            (setq lyrics (buffer-substring-no-properties
                          beg (1- (point))))))
        (and lyrics interactive (file-exists-p file)
             (not (y-or-n-p (format "Overwrite existing file (\"%s\")?" file)))
             (setq lyrics nil))
        (when lyrics
          (with-temp-file file (insert lyrics))
          (when interactive (message "Saved synced lyrics at \"%s\"" file))
          (and (boundp 'emms-lyrics-display-p)
               emms-lyrics-display-p emms-player-playing-p
               (equal track (emms-playlist-current-selected-track))
               (emms-lyrics-catchup file))))
    (setq emms-lyrics-lrclib-requests (1- emms-lyrics-lrclib-requests))))

;;;###autoload
(defun emms-lyrics-lrclib-get (&optional track force interactive)
  "Search for synchronized lyrics for TRACK through LRCLIB's API.
If TRACK is omitted or nil, use the selected track in the current playlist.
The lyrics are saved in an \".lrc\" file alongside the track, unless the
file already exists (in which case the search isn't performed).
When called interactively (non-nil INTERACTIVE), display informative
messages, and with prefix argument FORCE, ask to overwrite existing
\".lrc\" files."
  (interactive (list nil current-prefix-arg t))
  (if (> emms-lyrics-lrclib-requests emms-lyrics-lrclib-max-requests)
      (emms-later-do #'emms-lyrics-lrclib-get track force interactive)
    (when-let* ((track (or track (emms-playlist-current-selected-track)))
                ((eq (emms-track-type track) 'file))
                (file (emms-track-name track))
                (lrc (replace-regexp-in-string "\\.[^.]+\\'" ".lrc" file))
                ((or force (not (file-exists-p lrc))))
                ((file-writable-p lrc))
                (title (emms-lyrics-lrclib-encode-name
                        (emms-track-get track 'info-title)))
                (artist (emms-lyrics-lrclib-encode-name
                         (emms-track-get track 'info-artist)))
                (album (emms-lyrics-lrclib-encode-name
                        (emms-track-get track 'info-album)))
                (time (emms-track-get track 'info-playing-time)))
      (setq emms-lyrics-lrclib-requests (1+ emms-lyrics-lrclib-requests))
      (when interactive (message "Searching for lyrics..."))
      (url-retrieve
       (url-encode-url
        (format "%sget?artist_name=%s&track_name=%s&album_name=%s&duration=%d"
                emms-lyrics-lrclib-url artist title album time))
       #'emms-lyrics-lrclib-parse (list lrc track interactive)))))

(provide 'emms-lyrics-lrclib)

;;; emms-lyrics-lrclib.el ends here
