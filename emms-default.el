;;; emms-default.el --- Setup script for EMMS

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Ulrik Jensen <terryp@vernon>
;; Keywords:

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

;; This script can intiialise EMMS to different "levels" of usage.

;;; Code:

(eval-when-compile
  (require 'cl))

;;; FIXME! This is only backwards-compatibility stuff, remove
;;; `ignored' parameter.
(defun emms-setup (level &optional directory &rest ignored)
  "Sets up EMMS to a specific LEVEL of bells and whistles.

This also sets DIRECTORY as the default directory for finding
file-tracks.

\(emms-setup 'cvs\) -- Will setup EMMS to a testing environment, that
probably won't work, but utilizes all the available bells and whistles
of the version you have installed.

All possible values for the LEVEL, are:

`cvs' -- Everything and no guarantees
`advanced' -- info, playlist-mode, tageditor
`default' -- info and the playlist-buffer-interface.
`tiny' -- basic and playlist-mode
`minimalistic' -- No bells and whistles, no info, no interfaces. M-x
emms-next RET and such, as well as a single player. This should almost
always work, unless you get very unlucky with a CVS-build."
  ;; Always load the minimalistic setup
  (require 'emms)			; minimalistic
  (require 'emms-source-file)
  (require 'emms-player-simple)
  (require 'emms-player-mplayer)
  (setq emms-player-list
	'(emms-player-mpg321 emms-player-ogg123 emms-player-mplayer-playlist emms-player-mplayer)
	emms-source-file-default-directory directory)

  (when ignored
    (message "Interface for `emms-setup' has changed, please consult the docstring.")
    (ding))

  (unless (equal level 'minimalistic)	; tiny
    (require 'emms-playlist-mode)
    (setq emms-playlist-default-major-mode 'emms-playlist-mode)

    (unless (equal level 'tiny)		; default
      ;; must be default, advanced or cvs, include the playlist-mode and the info
      (require 'emms-info)
      (add-to-list 'emms-track-initialize-functions 'emms-info-initialize-track)
      (require 'emms-info-mp3info)
      (add-to-list 'emms-info-functions 'emms-info-mp3info)
      (require 'emms-info-ogginfo)
      (add-to-list 'emms-info-functions 'emms-info-ogginfo)

      ;; setup info
      (setq emms-track-description-function 'emms-info-track-description)

      (unless (equal level 'default)	; advanced
	;; + tageditor.
	;;(require 'emms-tageditor)
	;;(emms-tageditor-pbi-mode 1)

	(unless (equal level 'advanced)	; cvs
          ;; load the mode-line
          (require 'emms-mode-line)
          (emms-mode-line 1)
          (emms-mode-line-blank)

          ;; try using setnu
	  ;; (ignore-errors
	  ;; 	    (require 'setnu)
	  ;; 	    (add-hook 'emms-pbi-after-build-hook
	  ;; 		      (lambda ()
	  ;; 			(setnu-mode 1)))))))))

	  ;; streaming audio interface
	  (require 'emms-streams)

	  ;; streaming audio information
	  (require 'emms-stream-info)

	  ;; display lyrics
	  (require 'emms-lyrics)

	  ;; display playing-time
	  (require 'emms-playing-time))))))


(provide 'emms-default)

;;; emms-default.el ends here
