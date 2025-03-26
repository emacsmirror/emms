;;; emms-volume-mpv.el --- Volume function to adjust mpv volume easily  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Richard Sent <richard@freakingpenguin.com>

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
;; along with EMMS; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This file defines a function to raise or lower the volume of mpv.
;; It can be used stand-alone by passing a process object, though it
;; is meant for usage with Emms, particularly with emms-volume.el and
;; emms-player-mpv.el.
;;
;; To use add the following to your Emms configuration
;;     (setq emms-volume-change-function 'emms-volume-mpv-change)

;;; History:

;; January 2025: First release, partly based on emms-volume-pulse.el.

;;; Code:

(require 'emms-player-mpv)

(defcustom emms-volume-mpv-method 'native
  "How Emms should attempt to adjust mpv's volume.

If `native', Emms will adjust mpv's volume property. This
provides the same experience as adjusting the volume slider in
mpv.

If `system', Emms will adjust mpv's ao-volume property, which
adjusts the volume using the system audio service, such as
Pulseaudio. Depending on what audio service is being used this
may either change the mpv application's volume or global volume.

If `smart', Emms will adjust both mpv's volume and ao-volume
properties. When raising volume, the native volume will be raised
to 100. Emms will then switch to adjusting system volume to 100
before raising the native volume again. When lowering volume,
Emms will lower the software volume to 100, then lower system
volume to 0.

Both `system' and `smart' require mpv to expose the ao-volume
property. This property is only available while mpv audio output
is active. If audio output is not active, the volume will not be
changed.

Additionally, the percentage provided by and set for ao-volume
and thus this module may not match what is reported by the system
audio program."
  :type '(choice (const :tag "MPV Volume" native)
                 (const :tag "System Volume" system)
                 (const :tag "Smart" smart))
  :group 'emms-volume)

(defvar emms-volume-mpv--volume-sync (make-mutex "emms-volume-mpv--volume-sync")
  "Ensure only one volume-change function runs to completion at a
time.")

(defun emms-volume-mpv-synchronous-ipc (cmd &optional proc)
  "Run mpv command and get result synchronously for current thread.

This must not be run by the main thread. The handler for
emms-player-mpv-ipc-req-send runs in the main thread, potentially
causing a deadlock."
  (when (eq main-thread (current-thread))
    (error "This function cannot be invoked by the main thread"))
  (let* ((emms-volume-mpv--ipc-sync (make-mutex "emms-volume-mpv--ipc-sync"))
         (emms-volume-mpv--ipc-sync-check (make-condition-variable emms-volume-mpv--ipc-sync
                                                                   "emms-volume-mpv--ipc-sync-check"))
         (emms-volume-mpv--ipc-sync-reply nil))
    (with-mutex emms-volume-mpv--ipc-sync
      (emms-player-mpv-ipc-req-send
       cmd
       #'(lambda (data err)
           (with-mutex emms-volume-mpv--ipc-sync
             (setq emms-volume-mpv--ipc-sync-reply (list data err))
             (condition-notify emms-volume-mpv--ipc-sync-check)))
       proc)
      (while (not emms-volume-mpv--ipc-sync-reply)
        (condition-wait emms-volume-mpv--ipc-sync-check))
      (cl-multiple-value-bind (data err) emms-volume-mpv--ipc-sync-reply
        (if err (error "Failed to run %s, %s" cmd err) data)))))

(defun emms-volume-mpv-limit (vol volume-max &optional volume-min)
  "Limit VOL to the range [0 - volume-max]."
  (max (min vol volume-max) (or volume-min 0)))

(defun emms-volume-mpv--smart-increment (native-old system-old amount native-max)
  (cond
   ((< native-old 100)
    (list (emms-volume-mpv-limit (+ native-old amount) 100) system-old))
   ((< system-old 100)
    (list native-old (emms-volume-mpv-limit (+ system-old amount) 100)))
   (t (list (emms-volume-mpv-limit (+ native-old amount) native-max) system-old))))

(defun emms-volume-mpv--smart-decrement (native-old system-old amount native-max)
  (cond
   ((> native-old 100)
    (list (emms-volume-mpv-limit (+ native-old amount) native-max 100) system-old))
   (t (list native-old (emms-volume-mpv-limit (+ system-old amount) 100)))))

(defun emms-volume-mpv--smart-change (native-old system-old amount native-max)
  (if (>= amount 0)
      (emms-volume-mpv--smart-increment native-old system-old amount native-max)
    (emms-volume-mpv--smart-decrement native-old system-old amount native-max)))

;;;###autoload
(defun emms-volume-mpv-change (amount &optional proc)
  "Change volume by AMOUNT using mpv process PROC."
  (unless (or emms-player-mpv-ipc-proc proc)
    (error "mpv is not currently running"))
  ;; mpv does not protect against storing volumes > volume-max. We
  ;; must retrieve volume-max and verify the target volume.
  (make-thread
   (lambda ()
     (with-mutex emms-volume-mpv--volume-sync
       (with-demoted-errors "Failed to adjust the volume: %s"
         (let* ((native-max (emms-volume-mpv-synchronous-ipc '(get_property volume-max) proc))
                (native-old (emms-volume-mpv-synchronous-ipc '(get_property volume) proc))
                (system-old (emms-volume-mpv-synchronous-ipc '(get_property ao-volume) proc)))
           (pcase emms-volume-mpv-method
             ('native (let ((volume (emms-volume-mpv-limit (+ native-old amount) native-max)))
                        (emms-volume-mpv-synchronous-ipc
                         `(set_property volume ,volume) proc)
                        (message "Native volume is %d%%" volume)))
             ('system (let ((volume (emms-volume-mpv-limit (+ system-old amount) 100)))
                        (emms-volume-mpv-synchronous-ipc
                         `(set_property ao-volume ,volume) proc)
                        (message "System volume is %d%%" volume)))
             ('smart (cl-multiple-value-bind (native system)
                         (emms-volume-mpv--smart-change native-old system-old
                                                        amount native-max)
                       (emms-volume-mpv-synchronous-ipc
                        `(set_property volume ,native) proc)
                       (emms-volume-mpv-synchronous-ipc
                        `(set_property ao-volume ,system) proc)
                       (message "Native volume is %d%% and system volume is %d%%"
                                native system))))))))))

(provide 'emms-volume-mpv)

;;; emms-volume-mpv.el ends here
