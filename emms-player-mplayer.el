;;; emms-player-mplayer.el --- mplayer support for EMMS

;; Copyright (C) 2005  William XWL
;; Copyright (C) 2005  Jorgen Schaefer

;; Authors: William XWL <william.xwl@gmail.com>
;;          Jorgen Schaefer <forcer@forcix.cx>

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

;; This provides a player that uses mplayer. It supports pause and
;; seeking.

;;; Code:

(require 'emms-player-simple)

(define-emms-simple-player mplayer '(file url)
  (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
                ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://"
		".rm" ".rmvb" ".mp4"))
  "mplayer")

(define-emms-simple-player mplayer-playlist '(streamlist)
  "http://"
  "mplayer" "-playlist")

(emms-player-set emms-player-mplayer
		 'pause
		 'emms-player-mplayer-pause)

;;; Pause is also resume for mplayer
(emms-player-set emms-player-mplayer
                 'resume
                 nil)

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

(provide 'emms-player-mplayer)
;;; emms-player-mplayer.el ends here
