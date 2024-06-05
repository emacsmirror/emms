;;; emms-setup.el --- Setup script for EMMS  -*- lexical-binding: t; -*-

;; Copyright (C) 2005-2022  Free Software Foundation, Inc.

;; Author: Yoni Rabkin <yrk@gnu.org>
;; Keywords: emms setup multimedia

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file provides the `emms-setup' feature. With `emms-setup' we
;; can setup Emms with different features enabled. The use of this
;; feature is documented in the Emms manual which is distributed with
;; Emms.
;;
;; The use this feature we can invoke (for example):
;;
;;         (require 'emms-setup)
;;         (emms-all)
;;
;; The first command loads the feature into Emacs and the second
;; chooses the `emms-all' level.

;;; Code:

(require 'emms)

(defgroup emms-setup nil
  "*The Emacs Multimedia System setup utility."
  :prefix "emms-setup"
  :group 'multimedia)

(defcustom emms-setup-default-player-list
  '(emms-player-mpg321
    emms-player-ogg123
    emms-player-mplayer-playlist
    emms-player-mplayer
    emms-player-mpv
    emms-player-vlc
    emms-player-vlc-playlist)
  "Default list of players for emms-setup."
  :type 'list)

(defvar emms-setup-discover-player-alist
  '((emms-player-mpg321  . "mpg123")
    (emms-player-ogg123  . "ogg123")
    (emms-player-mplayer . "mplayer")
    (emms-player-mpv     . "mpv")
    (emms-player-vlc     . "vlc"))
  "Association list of players and their binaries.")

(defvar emms-setup-discover-info-alist
  '((emms-info-exiftool . "exiftool")
    (emms-info-metaflac . "metaflac")
    (emms-info-mp3info  . "mp3info")
    (emms-info-ogginfo  . "ogginfo")
    (emms-info-opusinfo . "opusinfo"))
  "Association list of info-functions and their binaries.")

;;;###autoload
(defun emms-minimalistic ()
  "An Emms setup script.
Invisible playlists and all the basics for playing media."
  (require 'emms-source-file)
  (require 'emms-source-playlist)
  (require 'emms-player-simple)
  (require 'emms-player-mplayer)
  (require 'emms-player-mpv)
  (require 'emms-player-vlc))

;;;###autoload
(defun emms-all ()
  "An Emms setup script.
Everything included in the `emms-minimalistic' setup and adds all
the stable features which come with the Emms distribution."
  ;; include
  (emms-minimalistic)
  ;; define
  (eval-and-compile
    (require 'emms-playlist-mode)
    (require 'emms-info)
    (require 'emms-info-mp3info)
    (require 'emms-info-ogginfo)
    (require 'emms-info-opusinfo)
    (require 'emms-info-metaflac)
    (require 'emms-info-tinytag)
    (require 'emms-info-exiftool)
    (require 'emms-info-native)
    (require 'emms-cache)
    (require 'emms-mode-line)
    (require 'emms-mark)
    (require 'emms-tag-editor)
    (require 'emms-tag-tracktag)
    (require 'emms-show-all)
    (require 'emms-streams)
    (require 'emms-lyrics)
    (require 'emms-playing-time)
    (require 'emms-player-mpd)
    (require 'emms-player-xine)
    (require 'emms-playlist-sort)
    (require 'emms-browser)
    (require 'emms-mode-line-icon)
    (require 'emms-cue)
    (require 'emms-bookmarks)
    (require 'emms-last-played)
    (require 'emms-metaplaylist-mode)
    (require 'emms-stream-info)
    (require 'emms-score)
    (require 'emms-history)
    (require 'emms-i18n)
    (require 'emms-volume)
    (require 'emms-playlist-limit)
    (require 'emms-librefm-scrobbler)
    (require 'emms-librefm-stream)
    (require 'emms-mpris)
    (require 'emms-idapi-musicbrainz)
    (require 'emms-idapi-browser))
  ;; setup
  (setq emms-playlist-default-major-mode #'emms-playlist-mode)
  (add-to-list 'emms-track-initialize-functions #'emms-info-initialize-track)
  (setq emms-info-functions '(emms-info-native emms-info-cueinfo))
  (setq emms-track-description-function #'emms-info-track-description)
  (when (fboundp 'emms-cache)		; work around compiler warning
    (emms-cache 1))
  (emms-mode-line-mode 1)
  (emms-mode-line-blank)
  (emms-lyrics 1)
  (emms-playing-time-mode 1)
  (add-hook 'emms-player-started-hook #'emms-last-played-update-current)
  (emms-score 1))


;;;###autoload
(defun emms-default-players ()
  "Set `emms-player-list' to `emms-setup-default-player-list'."
  (setq emms-player-list
	emms-setup-default-player-list))



;; These are kept around in order not to break anyone's existing
;; setup.
;;;###autoload
(defun emms-devel ()
  (emms-all))
(make-obsolete 'emms-devel 'emms-all "4.1")

;;;###autoload
(defun emms-standard ()
  (emms-all))
(make-obsolete 'emms-standard 'emms-all "4.1")


;;; ------------------------------------------------------------------
;;; Player discovery
;;; ------------------------------------------------------------------
(defun emms-setup-discover-binary (bin-str)
  "Find if BIN-STR can be executed in the current environment."
  (when (not (eq system-type 'gnu/linux))
    (error "Player discovery only supported on GNU/Linux."))
  (let ((result (call-process "which" nil nil nil bin-str)))
    (cond ((eq 0 result) t)
          ((eq 1 result) nil)
          ((eq 2 result) (error "invalid arguments to `which'.")))))

(defun emms-setup-discover-player-has-binary-p (player)
  "Find if PLAYER has an excecutable in the current environment."
  (let ((bin-str (alist-get player emms-setup-discover-player-alist)))
    (if bin-str
	(emms-setup-discover-binary bin-str)
      nil)))

(defun emms-setup-discover-info-has-binary-p (info-function)
  "Find if INFO-FUNCTION has an excecutable in the current environment."
  (let ((bin-str (alist-get info-function emms-setup-discover-info-alist)))
    (if bin-str
	(emms-setup-discover-binary bin-str)
      nil)))

(defun emms-setup-discover-players (arg)
  "Interactively add players to `emms-player-list'.

With a prefix, also insert the configuration at point."
  (interactive "P")
  (when (and emms-player-list
             (y-or-n-p (format "`emms-player-list' is already set to %s, do you want to empty it
first?"
			       emms-player-list)))
    (setq emms-player-list nil))
  (let ((players (copy-tree emms-setup-default-player-list)))
    (while players
      (let ((player (car players)))
	(when (emms-setup-discover-player-has-binary-p player)
          (when (y-or-n-p
                 (format "Player \"%s\" is installed on your system, add it to the Emms player list?"
                         player))
            (add-to-list 'emms-player-list player))))
      (setq players (cdr players))))
  (when arg
    (insert
     (format "(setq emms-player-list '%s)" emms-player-list)))
  (message "emms-player-list is now set to: %s" emms-player-list))

(defun emms-setup-discover-info (arg)
  "Interactively add info-functions to `emms-info-functions'.

With prefix, also insert the configuration at point."
  (interactive "P")
  (let (native-p)
    (when (and emms-info-functions
               (y-or-n-p (format "`emms-info-functions' is already set to %s, do you want to empty it
first?"
				 emms-info-functions)))
      (setq emms-info-functions nil))
    (when (y-or-n-p
	   (format "Install the built-in `emms-info-native' info function (recommended)?"))
      (setq native-p t))
    (mapc
     #'(lambda (info-function)
	 (when (and (emms-setup-discover-info-has-binary-p info-function)
		    (y-or-n-p
		     (format "Meta-info reader \"%s\" is installed on your system, add it to the info-function list?"
			     (alist-get info-function emms-setup-discover-info-alist))))
	   (add-to-list 'emms-info-functions info-function)))
     (mapcar
      #'(lambda (e)
	  (car e))
      emms-setup-discover-info-alist))
    (when native-p
      (setq emms-info-functions
	    (cons 'emms-info-native emms-info-functions))))
  (when arg
    (insert
     (format "(setq emms-info-functions '%s)" emms-info-functions)))
  (message "emms-info-functions is now set to: %s"
	   emms-info-functions))

(defun emms-setup-discover ()
  "Discover and output players and info functions.

Scan for media players and meta-data readers on the user's
machine and print out a setup."
  (interactive)
  (emms-setup-discover-players t)
  (newline)
  (emms-setup-discover-info t))


(provide 'emms-setup)
;;; emms-setup.el ends here
