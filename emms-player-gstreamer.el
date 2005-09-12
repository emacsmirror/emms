;; emms-gstreamer.el --- EMMS Gstreamer interaction

;; Copyright (C) 2005  Lucas Bonnet

;; Author: Lucas Bonnet <lucas@rincevent.net>
;; Keywords: emms, mp3, ogg, multimedia

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

;; Very basic support

;; The wrapper concept is easier to set up than a generic gstreamer
;; support, but in the long term, it's probably not a good idea.

;; Installation instructions :

;; 1. Put (require 'emms-player-gstreamer) in your ~/.emacs or
;; whatever you use to configure EMMS.

;; 2. Put the wrappers in your `exec-path' :
;;      (add-to-list 'exec-path "/path/to/wrapper") or the other way,
;;      by moving it to an already known directory.

(require 'emms-player-simple)

(defvar emms-player-gstreamer-sink "alsasink"
  "The audio output sink to use")

(define-emms-simple-player gstreamer '(file) 
  (regexp-opt '(".mp3" ".ogg" ".mod" ".flac" ".xm" ".it" ".ft"
		".MP3" ".OGG" ".MOD" ".FLAC" ".XM" ".IT" ".FT"))
  "gst-wrapper")

(setq emms-player-gstreamer-parameters (list emms-player-gstreamer-sink))

(emms-player-set emms-player-gstreamer 'pause  'emms-player-gstreamer-pause)
(emms-player-set emms-player-gstreamer 'resume 'emms-player-gstreamer-resume)

(defun emms-player-gstreamer-pause ()
  (interactive)
  (signal-process (get-process emms-player-simple-process-name) 'SIGSTOP))

(defun emms-player-gstreamer-resume ()
  (interactive)
  (signal-process (get-process emms-player-simple-process-name) 'SIGCONT))


(provide 'emms-player-gstreamer)
