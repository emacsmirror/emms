;;; emms-player-xine.el --- xine support for EMMS

;; Copyright (C) 2007 Free Software Foundation, Inc.

;; Author: Tassilo Horn <tassilo@member.fsf.org>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; This provides a player that uses xine. It supports pause and
;; seeking.

;;; Code:

;; TODO: The video window cannot be disabled and I dunno how seeking works with
;; xine's stdin control. I asked on gmane.comp.video.xine.user
;; (<87y7ohqcbq.fsf@baldur.tsdh.de>)...

(require 'emms-player-simple)

(define-emms-simple-player xine '(file url)
  (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
                ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
                ".rm" ".rmvb" ".mp4" ".flac" ".vob"))
  "xine" "--no-gui" "--no-logo" "--no-splash" "--no-reload" "--stdctl")

(emms-player-set emms-player-xine
                 'pause
                 'emms-player-xine-pause)

;;; Pause is also resume for xine
(emms-player-set emms-player-xine
                 'resume
                 nil)

;; TODO: Find out how seeking works.
;; (emms-player-set emms-player-xine
;;                  'seek
;;                  'emms-player-xine-seek)

;; (emms-player-set emms-player-xine
;;                  'seek-to
;;                  'emms-player-xine-seek-to)

(defun emms-player-xine-pause ()
  "Depends on xine's --stdctl mode."
  (process-send-string
   emms-player-simple-process-name "pause\n"))

;; TODO: Find out how seeking works.
;; (defun emms-player-xine-seek (sec)
;;   "Depends on xine's --stdctl mode."
;;   (process-send-string
;;    emms-player-simple-process-name
;;    (format "seek %d\n" sec)))

;; (defun emms-player-xine-seek-to (sec)
;;   "Depends on xine's --stdctl mode."
;;   (process-send-string
;;    emms-player-simple-process-name
;;    (format "seek %d 2\n" sec)))

(provide 'emms-player-xine)
;;; emms-player-xine.el ends here
