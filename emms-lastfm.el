;;; emms-lastfm.el --- add your listened songs to your profile at last.fm

;; Copyright (C) 2006 Free Software Foundation, Inc.

;; Author: Tassilo Horn <tassilo@member.fsf.org>

;; Keywords: emms, mp3, mpeg, multimedia

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; EMMS is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; EMMS; see the file COPYING.  If not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; This code sends information about what music you are playing to last.fm.
;; See <URL:http://www.last.fm> and
;; <URL:http://www.audioscrobbler.net/wiki/Protocol1.1>.

;;; Sample configuration:

;; (setq emms-lastfm-username "my-user-name"
;;       emms-lastfm-password "very-secret!")

;;; Usage:

;; To activate the last.fm emms plugin, run:
;; `M-x emms-lastfm-activate'

;; To deactivate the last.fm emms plugin, run:
;; `C-u -1 M-x emms-lastfm-activate'

;; -----------------------------------------------------------------------

(require 'url)
(require 'emms)

(defvar emms-lastfm-username ""
  "Your last.fm username")
(defvar emms-lastfm-password ""
  "Your last.fm password")

(defconst emms-lastfm-server "http://post.audioscrobbler.com/"
  "The last.fm server responsible for the handshaking
procedure. Only for internal use.")
(defconst emms-lastfm-client-id "ems"
  "The client ID of EMMS. Don't change it!")
(defconst emms-lastfm-client-version 0.1
  "The version regitered at last.fm. Don't change it!")

;; used internally
(defvar emms-lastfm-buffer nil "-- only used internally --")
(defvar emms-lastfm-process nil "-- only used internally --")
(defvar emms-lastfm-md5-challenge nil "-- only used internally --")
(defvar emms-lastfm-submit-url nil "-- only used internally --")
(defvar emms-lastfm-current-track nil "-- only used internally --")
(defvar emms-lastfm-timer nil "-- only used internally --")

(defun emms-lastfm-new-track-function ()
  "This function should run whenever a new track starts (or a
paused track resumes) and sets the track submission timer."
  (setq emms-lastfm-current-track
        (emms-playlist-current-selected-track))
  ;; Tracks should be submitted, if they played 240 secs or half of their
  ;; length, whichever comes first.
  (let ((secs (/ (emms-track-get emms-lastfm-current-track
                                 'info-playing-time)
                    2)))
    (when (> secs 240)
      (setq secs 240))
    (unless (< secs 15) ;; Skip titles shorter than 30 seconds
      (setq secs (- secs emms-playing-time))
      (unless (< secs 0)
        (setq emms-lastfm-timer
              (run-with-timer secs nil 'emms-lastfm-submit-track))))))

(defun emms-lastfm-cancel-timer ()
  "Cancels `emms-lastfm-timer' if it is running."
  (when emms-lastfm-timer
    (cancel-timer emms-lastfm-timer)
    (setq emms-lastfm-timer nil)))

(defun emms-lastfm-pause ()
  "Handles things to be done when the player is paused or
resumed."
  (if emms-player-paused-p
      ;; the player paused
      (emms-lastfm-cancel-timer)
    ;; The player resumed
    (emms-lastfm-new-track-function)))

(defun emms-lastfm-activate (&optional ARG)
  "Start submitting the tracks you listened to to
http://www.last.fm, if ARG is positive. If ARG is negative or
zero submission of the tracks will be stopped. This applies to
the current track, too."
  (interactive "p")
  (if (not (and emms-lastfm-username emms-lastfm-password))
      (message "%s"
               (concat "EMMS: In order to activate the last.fm plugin you "
                       "first have to set both `emms-lastfm-username' and "
                       "`emms-lastfm-password'."))
    (if (> ARG 0)
        (progn
          (add-hook 'emms-player-started-hook
                    'emms-lastfm-handshake-if-needed)
          (add-hook 'emms-player-started-hook
                    'emms-lastfm-new-track-function)
          (add-hook 'emms-player-stopped-hook
                    'emms-lastfm-cancel-timer)
          (add-hook 'emms-player-paused-hook
                    'emms-lastfm-pause)
          (message "EMMS Last.fm plugin activated."))
      (remove-hook 'emms-player-started-hook
                   'emms-lastfm-handshake-if-needed)
      (remove-hook 'emms-player-started-hook
                   'emms-lastfm-new-track-function)
      (remove-hook 'emms-player-stopped-hook
                   'emms-lastfm-pause)
      (remove-hook 'emms-player-paused-hook
                   'emms-lastfm-cancel-timer)
      (cancel-timer emms-lastfm-timer)
      (setq emms-lastfm-md5-challenge nil
            emms-lastfm-submit-url    nil
            emms-lastfm-process       nil
            emms-lastfm-current-track nil)
      (message "EMMS Last.fm plugin deactivated."))))


(defun read-line ()
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun emms-lastfm-handshake-if-needed ()
  (when (not (and emms-lastfm-md5-challenge
                  emms-lastfm-submit-url))
    (emms-lastfm-handshake)))

(defun emms-lastfm-handshake ()
  "Handshakes with the last.fm server."
  (let ((url-request-method "GET"))
    (setq emms-lastfm-buffer
          (url-retrieve (concat emms-lastfm-server "?hs=true&p=1.1"
                                "&c=" emms-lastfm-client-id
                                "&v=" (number-to-string
                                       emms-lastfm-client-version)
                                "&u=" emms-lastfm-username)
                        'emms-lastfm-handshake-sentinel))))

(defun emms-lastfm-handshake-sentinel (&rest args)
  "Parses the server reponse and inform the user if all worked
well or if an error occured."
  (save-excursion
    (set-buffer emms-lastfm-buffer)
    (goto-char (point-min))
    (re-search-forward (rx (or "UPTODATE" "UPDATE" "FAILED" "BADUSER"))
                       nil t)
    (let ((response (read-line)))
      (if (not (string-match (rx (or "UPTODATE""UPDATE")) response))
          (progn
            (cond ((string-match "FAILED" response)
                   (message "EMMS: Handshake failed: %s." response))
                  ((string-match "BADUSER" response)
                   (message "EMMS: Wrong username."))))
        (when (string-match "UPDATE" response)
          (message "EMMS: There's a new last.fm plugin version."))
        (forward-line)
        (setq emms-lastfm-md5-challenge (read-line))
        (forward-line)
        (setq emms-lastfm-submit-url (read-line))
        (message "EMMS: Handshaking with server done.")))))

(defun emms-lastfm-submit-track ()
  "Submits the current track (`emms-lastfm-current-track') to
last.fm."
  (let* ((artist (emms-track-get emms-lastfm-current-track 'info-artist))
         (title  (emms-track-get emms-lastfm-current-track 'info-title))
         (album  (emms-track-get emms-lastfm-current-track 'info-album))
         (musicbrainz-id "")
         (track-length (number-to-string
                        (emms-track-get emms-lastfm-current-track
                                        'info-playing-time)))
         (date (format-time-string "%Y-%m-%d %H:%M:%S" (current-time) t))
         (url-http-attempt-keepalives nil)
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-type" .
             "application/x-www-form-urlencoded; charset=utf-8")))
         (url-request-data (encode-coding-string
                            (concat "u=" emms-lastfm-username
                                    "&s=" (md5 (concat
                                                (md5 emms-lastfm-password)
                                                emms-lastfm-md5-challenge))
                                    "&a[0]=" artist
                                    "&t[0]=" title
                                    "&b[0]=" album
                                    "&m[0]=" musicbrainz-id
                                    "&l[0]=" track-length
                                    "&i[0]=" date)
                            'utf-8)))
    (setq emms-lastfm-buffer
          (url-retrieve emms-lastfm-submit-url
                        'emms-lastfm-submission-sentinel))))

(defun emms-lastfm-submission-sentinel (&rest args)
  "Parses the server reponse and inform the user if all worked
well or if an error occured."
  (save-excursion
    (set-buffer emms-lastfm-buffer)
    (goto-char (point-min))
    (if (re-search-forward "^OK$" nil t)
        (progn
          (message "EMMS: \"%s\" submitted to last.fm."
                   (emms-track-description emms-lastfm-current-track))
          (kill-buffer emms-lastfm-buffer))
      (message "EMMS: Song couldn't be submitted to last.fm."))))


(provide 'emms-lastfm)
;;; emms-lastfm.el ends here

