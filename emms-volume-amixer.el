;;; emms-volume-amixer.el --- a mode for changing volume using amixer  -*- lexical-binding: t; -*-

;; Copyright (C) 2006, 2007, 2008, 2009, 2023  Free Software Foundation, Inc.

;; Author: Martin Schoenmakers <aiviru@diamond-age.net>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file defines a few simple functions to raise or lower the volume
;; using amixer. It can be used stand-alone, though it's meant for usage
;; with EMMS, particularly with emms-volume.el

;;; History:

;; May 30 2006: First cleanup and collation of amixer functions into a
;;              separate file for releasability.


;;; Code:
(defcustom emms-volume-amixer-control "Master"
  "The control to change the volume with.
Controls includes \"Master\", \"PCM\", etc. For a full list of available
controls, run `amixer controls' in a shell."
  :type '(choice (const :tag "Master" "Master")
                 (const :tag "PCM" "PCM")
                 (string :tag "Something else: "))
  :group 'emms-volume)

(defcustom emms-volume-amixer-card 0
  "The card number to change volume.
The card is identified by a number. For a full list run `cat
/proc/asound/cards' in a shell."
  :type 'integer
  :group 'emms-volume)

(defvar emms-volume-amixer-volume-regexp
  "\\[\\([0-9]+\\)%\\]"
  "Regexp to capture the volume from amixer output.")

;;;###autoload
(defun emms-volume-amixer-change (amount)
  "Change amixer master volume by AMOUNT."
  (message "Playback channels: %s"
           (with-temp-buffer
             (when (zerop
                    (call-process "amixer" nil (current-buffer) nil
				  "-c"
				  (format "%d" emms-volume-amixer-card)
                                  "sset" emms-volume-amixer-control
                                  (format "%d%%%s" (abs amount)
                                          (if (< amount 0) "-" "+"))))
               (if (re-search-backward emms-volume-amixer-volume-regexp nil t)
                   (match-string 1))))))

(defun emms-volume-amixer-get ()
  "Return the amixer volume.

Number is limited to the range [0-100]."
  (let ((v (with-temp-buffer
	     (when (zerop
		    (call-process "amixer" nil (current-buffer) nil
				  "-c"
				  (format "%d" emms-volume-amixer-card)
				  "sget" emms-volume-amixer-control))
	       (if (re-search-backward
		    emms-volume-amixer-volume-regexp nil t)
		   (match-string 1)
		 nil)))))
    (if v
	(max (min (string-to-number v) 100) 0)
      (error "could not get volume from amixer backend"))))



(provide 'emms-volume-amixer)

;;; emms-volume-amixer.el ends here
