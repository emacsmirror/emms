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
`advanced' -- info, pbi, tageditor
`default' -- info and the playlist-buffer-interface.
`tiny' -- basic and pbi
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
    (require 'emms-pbi)

    (unless (equal level 'tiny)		; default
      ;; must be default, advanced or cvs, include the pbi and the info
      (require 'emms-info)
      (require 'emms-info-mp3info)
      (setq emms-info-methods-list '(emms-info-mp3info))

      ;; ogg-info might fail!
      (ignore-errors
	(require 'emms-info-ogg)
	(add-to-list 'emms-info-methods-list 'emms-info-ogg-comment))

      ;; setup info
      (setq emms-track-description-function 'emms-info-file-info-song-artist)

      (unless (equal level 'default)	; advanced
	;; + tageditor.
	(require 'emms-tageditor)
	(emms-tageditor-pbi-mode 1)

        ;; and pl-manip
        (require 'emms-pl-manip)

	(unless (equal level 'advanced)	; cvs
	  (require 'emms-pbi-mark)
	  (emms-pbi-mark 1)
	  (emms-tageditor-pbi-mark-mode 1)
	  (require 'emms-pbi-popup)

          ;; load the mode-line
          (require 'emms-mode-line)
          (emms-mode-line 1)
          (emms-mode-line-blank)

	  ;; load emms-info-later-do, but ignore problems (since
	  ;; later-do.el might not be available on this system)
	  (ignore-errors
	    (require 'emms-info-later-do)
	    (emms-info-later-do-mode 1)
	    (add-hook 'emms-info-later-do-read-info-functions
		      (lambda (track)
                        (when (get-buffer emms-pbi-playlist-buffer-name)
                          (emms-pbi-entry-update-track track)))))

	  ;; try using setnu
	  ;; (ignore-errors
	  ;; 	    (require 'setnu)
	  ;; 	    (add-hook 'emms-pbi-after-build-hook
	  ;; 		      (lambda ()
	  ;; 			(setnu-mode 1)))))))))

	  ;; display lyrics
	  (require 'emms-lyrics)

	  ;; display playing-time
	  (require 'emms-playing-time))))))


(provide 'emms-default)

;;; emms-default.el ends here
