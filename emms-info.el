;;; emms-info.el --- Retrieving track information

;; Copyright (C) 2005  Jorgen Schaefer

;; Author: Jorgen Schaefer <forcer@forcix.cx>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301  USA

;;; Commentary:

;; This EMMS module provides a way to add information for a track.
;; This can use an ID3 or OGG comment like syntax.

;; The code will add info symbols to the track. The following symbols
;; are defined:

;; info-artist - string naming the artist
;; info-title - string naming the title of the song
;; info-album - string naming the album
;; info-tracknum - string(?) naming the track number
;; info-year - string naming the year
;; info-note - string of free-form entry
;; info-genre - string naming the genre
;; info-playing-time - number giving the seconds of playtime

;;; Code:

(require 'emms)
(require 'later-do)

(defgroup emms-info nil
  "*Track information. ID3, OGG, etc."
  :group 'emms)

(defcustom emms-info-auto-update t
  "*Non-nil when EMMS should update track information if the file changes.
This will cause hard drive activity on track loading. If this is
too annoying for you, set this variable to nil."
  :type 'boolean
  :group 'emms-info)

(defcustom emms-info-asynchronously t
  "*Non-nil when track information should be loaded asynchronously.
This requires `later-do', which should come with EMMS."
  :type 'boolean
  :group 'emms-info)

(defcustom emms-info-functions nil
  "*Functions which add information to tracks.
Each is called with a track as argument."
  :type 'hook
  :group 'emms-info)

(defvar emms-info-asynchronous-tracks 0
  "Number of tracks we're waiting for to be done.")

(defun emms-info-initialize-track (track)
  "Initialize TRACK with emms-info information.
This is a suitable value for `emms-track-initialize-functions'."
  (if (not emms-info-asynchronously)
      (emms-info-really-initialize-track track)
    (setq emms-info-asynchronous-tracks (1+ emms-info-asynchronous-tracks))
    (later-do 'emms-info-really-initialize-track track)))

(defun emms-info-really-initialize-track (track)
  "Really initialize TRACK.
Return t when the track got changed."
  (let ((track-mtime (emms-track-get track 'info-mtime))
        (file-mtime (when emms-info-auto-update
                      (emms-info-track-file-mtime track))))
    (when (or (not track-mtime)
              (when emms-info-auto-update
                (time-less-p track-mtime
                             file-mtime)))
      (run-hook-with-args 'emms-info-functions
                          track)
      (emms-track-set track 'info-mtime file-mtime)
      (emms-track-updated track)
      (when emms-info-asynchronously
        (setq emms-info-asynchronous-tracks (1- emms-info-asynchronous-tracks))
        (when (zerop emms-info-asynchronous-tracks)
          (message "EMMS: All track information loaded.")))
      t)))

(defun emms-info-track-file-mtime (track)
  "Return the mtime of the file of TRACK, if any.
Return zero otherwise."
  (if (eq (emms-track-type track)
          'file)
      (nth 5 (file-attributes (emms-track-name track)))
    0))

(defun emms-info-track-description (track)
  "Return a description of the current track."
  (let ((artist (emms-track-get track 'info-artist))
        (title (emms-track-get track 'info-title)))
    (if (and artist title)
        (format "%s - %s" artist title)
      (emms-track-simple-description track))))

(provide 'emms-info)
;;; emms-info.el ends here
