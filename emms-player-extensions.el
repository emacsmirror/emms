;;; emms-player-extensions.el - Add more user control functions for EMMS

;; Copyright (C) 2005 William XWL

;; Author: William XWL <william.xwl@gmail.com>

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
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;; This package adds pause and seek support for EMMS. While pause and
;; seek are only available for special players. 

;; Specically, here we add pause and seek support for
;; `emms-player-mplayer' with mplayer's slave mode enabled. Put
;; something like this to your .emacs first:
;;
;; (setq emms-player-mplayer-command-name "mplayer"
;;       emms-player-mplayer-parameters '("-slave")
;;       emms-player-list
;;       '(emms-player-mplayer
;; 	   emms-player-mplayer-playlist
;; 	   emms-player-mpg321
;; 	   emms-player-ogg123))

;; To use, put this file to your load-path and the following to your
;; .emacs:
;;
;;     (require 'emms-player-extensions)

;;; Change Log

;; v 0.21 [2005/07/18 16:42:51] In function `emms-seek', add a call for
;;        `emms-lyric-seek' (defined in `emms-lyric.el').

;; v 0.2 [2005/07/17 20:22:53] And `emms-player-paused-p',
;;       `emms-player-player-paused-hook', and some modifications to
;;       cooperate with another package(`emms-lyric.el'). Rename this
;;       file from emms-patch.el to emms-player-extensions.el.

;; v 0.1 The initial version. Add `emms-pause', `emms-seek',
;;       `emms-repeat-curr', `emms-unrepeat-curr', `emms-repeat-all',
;;       `emms-unrepeat-all'.

;;; Codes:

;; Version control
(defvar emms-player-extensions-version "0.3 $Revision: 1.7 $"
  "EMMS player extensions version string.")
;; $Id: emms-player-extensions.el,v 1.7 2005/08/17 14:10:11 xwl Exp $

(require 'emms)

;;; Variables:

(defvar emms-player-paused-p nil
  "The EMMS player paused.")

(defvar emms-player-paused-hook nil
  "*Hook run when an EMMS player pauses playing.")

(defvar emms-player-seeked-hook nil
  "*Hook run when an EMMS player seeks forward or backward.")

;;; User Interfaces:

(defun emms-pause ()
  "Pause the current player."
  (interactive)
  (when emms-player-playing-p
    (funcall (emms-player-get emms-player-playing-p 'pause))
    (setq emms-player-paused-p (not emms-player-paused-p))
    (run-hooks 'emms-player-paused-hook)))

(defun emms-seek (&optional sec)
  "Seek forward/backward SEC(default is 10) seconds."
  (interactive)
  (unless sec (setq sec 10))
  (when emms-player-playing-p
    (funcall (emms-player-get emms-player-playing-p 'seek) sec)
    (run-hook-with-args 'emms-player-seeked-hook sec)))

;;; mplayer: pause, seek
(emms-player-set emms-player-mplayer
		 'pause
		 'emms-player-mplayer-pause)

(emms-player-set emms-player-mplayer
		 'seek
		 'emms-player-mplayer-seek)

(defun emms-player-mplayer-pause ()
  "Depends on mplayer's -slave mode."
  (process-send-string
   emms-player-simple-process-name "pause\n"))

(defun emms-player-mplayer-seek (sec)
  "Depends on mplayer's -slave mode."
  (process-send-string
   emms-player-simple-process-name
   (format "seek %d\n" sec)))


(provide 'emms-player-extensions)

;;; emms-player-extensions.el ends here
