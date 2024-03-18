;;; emms-volume-pulse.el --- a mode for changing volume using PulseAudio pactl  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2023 Free Software Foundation, Inc.

;; Author: Rasmus Pank Roulund <emacs@pank.eu>

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
;; using pactl. It can be used stand-alone, though it's meant for usage
;; with EMMS, particularly with emms-volume.el.
;;
;; To use add the following to your EMMS configuration
;;     (setq emms-volume-change-function 'emms-volume-pulse-change)

;;; History:

;; Marts 2015: First release.  Partly based on emms-volume-amixer.el

;;; Todo:

;; There probably needs to be more configurability, which may in turn
;; mean adding some more functions.
;; Some of this could benefit from adding customize interfaces.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; TODO: it would be great if custom could have
;; choices based on pactl list short sinks | cut -f1-2

(defcustom emms-volume-pulse-sink nil
  "The sink to use for volume adjustment.

If nil try to use the default sink.

See full list of devices on your system by running
    pactl list short sinks"
  :type '(choice (number :tag "Sink number")
                 (string :tag "Sink symbolic name")
                 (const :tag "Default sink" nil))
  :group 'emms-volume)

;; 'pactl get-sink-volume' was only added recently (version 14.1).
;; When that version is more widespread this function can be
;; simplified
(defun emms-volume--pulse-get-volume ()
  "Return `emms-volume-pulse-sink' volume."
  (let* ((emms-volume-pulse-sink
          (if emms-volume-pulse-sink
              emms-volume-pulse-sink
            (string-trim
             (shell-command-to-string
              "LC_ALL=C pactl info | grep 'Default Sink: ' | cut -d ' ' -f3-"))))
         (sink-number-p (numberp emms-volume-pulse-sink))
         (output
          (shell-command-to-string
           (concat "LC_ALL=C pactl list sinks" "|"
                   "grep -E -e 'Sink' -e 'Name' -e '^[^a-zA-Z]*Volume'")))
	 (volume-string
	  (car
	   (reverse
	    (funcall
             (if sink-number-p #'assq #'assoc)
             emms-volume-pulse-sink
             (mapcar (if sink-number-p 'identity 'cdr)
                     (cl-loop while
			      (string-match
			       (mapconcat #'identity
					  '(".*Sink[ \t]+\\#\\([0-9]+\\)"
					    ".*Name:[ \t]\\([^\n]+\\)"
					    ".*Volume:.*?\\([0-9]+\\)%.*\n?")
					  "\n")
			       output)
			      collect (list (string-to-number (match-string 1 output))
					    (match-string 2 output)
					    (match-string 3 output))
			      do (setq output (replace-match "" nil nil output)))))))))
    (if volume-string
	(string-to-number volume-string)
      (error "cannot get volume from sink, check `emms-volume-pulse-sink'"))))

(defun emms-volume-pulse-limit (v)
  "Limit V to the range [0-100]"
  (max (min v 100) 0))

(defun emms-volume-pulse-get ()
  "Return the pulse volume."
  (emms-volume-pulse-limit
   (emms-volume--pulse-get-volume)))

;;;###autoload
(defun emms-volume-pulse-change (amount)
  "Change PulseAudio volume by AMOUNT."
  (message "Volume is %s%%"
           (let ((pactl (or (executable-find "pactl")
                            (error "pactl is not in PATH")))
                 (next-vol (emms-volume-pulse-limit
			    (+ (emms-volume--pulse-get-volume) amount))))
             (when (zerop (shell-command
                           (format "%s set-sink-volume %s %s%%"
                                   pactl
                                   (or emms-volume-pulse-sink "@DEFAULT_SINK@")
                                   next-vol)))
               next-vol))))

(provide 'emms-volume-pulse)

;;; emms-volume-pulse.el ends here
