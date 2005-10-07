;;; emms-setup.el --- Setup script for EMMS

;; Copyright (C) 2005 Yoni Rabkin

;; Author: Yoni Rabkin <yonirabkin@member.fsf.org>
;; Keywords: emms setup multimedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301 USA

;;; Commentary:
;;

;;; Code:

(defgroup emms-setup nil
  "*The Emacs Multimedia System setup utility."
  :prefix "emms-setup"
  :group 'multimedia)

(defcustom emms-setup-default-player-list
  '(emms-player-mpg321
    emms-player-ogg123
    emms-player-mplayer-playlist
    emms-player-mplayer)
  "*Default list of players for emms-setup."
  :group 'emms-setup
  :type 'list)

(defun emms-minimalistic ()
  "An Emms setup script.
Invisible playlists and all the basics for playing media."
  (require 'emms)
  (require 'emms-source-file)
  (require 'emms-player-simple)
  (require 'emms-player-mplayer))

(defun emms-standard ()
  "An Emms setup script.
Everything included in the `emms-minimalistic' setup, the Emms
interactive playlist mode and reading information from tagged
audio files."
  ;; include
  (emms-minimalistic)
  ;; define
  (require 'emms-playlist-mode)
  (require 'emms-info)
  (require 'emms-info-mp3info)
  (require 'emms-info-ogginfo)
  ;; setup
  (setq emms-playlist-default-major-mode 'emms-playlist-mode)
  (add-to-list 'emms-track-initialize-functions 'emms-info-initialize-track)
  (add-to-list 'emms-info-functions 'emms-info-mp3info)
  (add-to-list 'emms-info-functions 'emms-info-ogginfo)
  (setq emms-track-description-function 'emms-info-track-description))

(defun emms-all ()
  "An Emms setup script.
Everything included in the `emms-standard' setup and adds all the
stable features which come with the Emms distribution."
  ;; include
  (emms-standard)
  ;; define
  (require 'emms-metaplaylist-mode)
  (require 'emms-mode-line)
  (require 'emms-streams)
  (require 'emms-lyrics)
  (require 'emms-playing-time)
  ;; setup
  (emms-mode-line 1)
  (emms-mode-line-blank)
  (emms-lyrics-enable)
  (emms-playing-time-enable))

(defun emms-devel ()
  "An Emms setup script.
Everything included in the `emms-all' setup and adds all the
features which come with the Emms distribution regardless of if
they are considered stable or not.  Use this if you like living
on the edge."
  ;; include
  (emms-all)
  ;; define
  (require 'emms-stream-info)
  (require 'emms-playlist-sort))

(defun emms-default-players ()
  "Set `emms-player-list' to `emms-setup-default-player-list'."
  (setq emms-player-list
	emms-setup-default-player-list))

(provide 'emms-setup)
;;; emms-setup.el ends here
