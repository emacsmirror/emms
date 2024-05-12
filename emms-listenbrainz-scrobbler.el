;;; emms-listenbrainz-scrobbler.el --- Listenbrainz Scrobbling API  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Fran Burstall <fran.burstall@gmail.com>
;; Keywords: emms, listenbrainz

;; EMMS is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; EMMS is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; To use listenbrainz you need to add a user token to
;; ~/.authinfo.gpg or an equivalent file understood by auth-source
;; with a line like this:

;; machine api.listenbrainz.org password 0abc1de2-245-67fa-8906b-bc123d4dbdaa

;; To enable scrobbling call (emms-listenbrainz-scrobbler-enable).


;;; Code:

;;* Requires

(require 'emms-playing-time)
(require 'auth-source)
(require 'json)
(require 'url-vars)

;;* Vars

(defvar emms-listenbrainz-scrobbler-host
  "api.listenbrainz.org"
  "Listenbrainz server we target.")

(defvar emms-listenbrainz-scrobbler-token nil
  "Listenbrainz user token.

Note that the preferred way of authenticating is using authinfo.")

(defvar emms-listenbrainz-scrobbler-submission-url
  (concat "https://" emms-listenbrainz-scrobbler-host "/1/submit-listens")
  "URL for submissions.")

(defvar emms-listenbrainz-scrobbler-track-play-start-timestamp
  nil
  "Time when a track started playing.")

(defvar emms-listenbrainz-scrobbler-display-submissions
  t
  "Whether to display a user message on every submission.")

(defvar emms-listenbrainz-scrobbler-running
  nil
  "Non-nil if emms-listenbrainz-scrobbler is active.")


;;* User token

(defun emms-listenbrainz-scrobbler--get-auth-token ()
  "Return user token from auth-source."
  (plist-get (car (auth-source-search :host emms-listenbrainz-scrobbler-host
				      :max 1
				      :require '(:secret)))
	     :secret))

(declare-function emms-listenbrainz-scrobbler-get-token "emms-listenbrainz-scrobbler")
(let ((token))
  (defun emms-listenbrainz-scrobbler-get-token ()
    "Return user token."
    (unless token
      (setq token (emms-listenbrainz-scrobbler--get-auth-token)))
    (cond ((functionp token) (funcall token))
	  ((characterp token) token)
	  (t emms-listenbrainz-scrobbler-token))))


;;* Payload
;; there are two types of query: listen and now-playing.  The latter
;; is ephemeral and does not want the listened_at timestamp.

(defun emms-listenbrainz-scrobbler-make-query (track &optional playing-now)
  "Collect the data from TRACK.  No timestamp when PLAYING-NOW is non-nil."
  (let ((artist (emms-track-get track 'info-artist))
	(title (emms-track-get track 'info-title))
	(album (emms-track-get track 'info-album))
	(track-number (emms-track-get track 'info-tracknumber))
	;; (musicbrainz-id "")
	(track-length (emms-track-get track
				      'info-playing-time))
	payload metadata extra-data data)
    (unless (and title artist) (error "Track title and artist must be known"))
    ;; assemble metadata
    (push (cons "artist_name" (substring-no-properties artist)) metadata)
    (push (cons "track_name" (substring-no-properties title)) metadata)
    (when album (push (cons "release_name" (substring-no-properties album)) metadata))
    ;; additional data
    (when track-number (push (cons "tracknumber" (string-to-number track-number))
			     extra-data))
    (when track-number (push (cons "duration" track-length) extra-data))
    (when extra-data (push (cons "additional_info" extra-data) metadata))
    ;; payload
    (push (cons "track_metadata" metadata) payload)
    (unless playing-now
      (push (cons "listened_at"
		  (or emms-listenbrainz-scrobbler-track-play-start-timestamp
		      (time-convert nil 'integer)))
	    payload))
    ;; the whole package
    (push (cons "payload" (vector payload)) data)
    (push (cons "listen_type" (if playing-now "playing_now" "single")) data)
    data))


;;* Asynchronous submission

;; In memorium: seek how much shorter, neater and comprehensible this code is.

;; (defun emms-listenbrainz-scrobbler-make-async-submission-call (track &optional playing-now)
;;   "Submit listen, or playing-now if PLAYING-NOW non-nil, of TRACK to listenbrainz."
;;   (let ((data (emms-listenbrainz-scrobbler-make-query track playing-now))
;; 	(token (emms-listenbrainz-scrobbler-get-token))
;; 	(title (emms-track-get track 'info-title)))
;;     (request emms-listenbrainz-scrobbler-submission-url
;;       :type "POST"
;;       :parser 'json-read
;;       :headers `(("Content-type" . "application/json; charset=utf-8")
;; 		 ("Authorization" . ,(concat "Token " token)))
;;       :data (json-encode data)
;;       :success
;;       (lambda (&rest _)
;; 	(when (and (not playing-now) emms-listenbrainz-scrobbler-display-submissions)
;; 	  (message "Listenbrainz: submitted %s." title)))
;;       :error
;;       (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
;; 		     (message "Listenbrainz error: %S when submitting %s"
;; 			      error-thrown title))))))

;; we try to use the built-in url.  This was a struggle to get right
;; with respect to handling things like kanji.  The solution is to
;; encode absolutely anything beyond literal strings (so output of
;; concat, for example) that might end up in the request: not just
;; data but headers!

(defun emms-listenbrainz-scrobbler-make-async-submission-call (track &optional playing-now)
  "Submit listen, or playing-now if PLAYING-NOW non-nil, of TRACK to listenbrainz."
  (let* ((payload (emms-listenbrainz-scrobbler-make-query track playing-now))
	 (token (emms-listenbrainz-scrobbler-get-token))
	 (token-string (encode-coding-string (concat "Token " token) 'utf-8))
	 (url-request-method "POST")
	 (url-request-data (encode-coding-string (json-encode payload) 'utf-8))
	 (url-request-extra-headers `(("Content-type" . "application/json; charset=utf-8")
				      ("Authorization" . ,token-string))))
    (ignore url-request-method
	    url-request-data
	    url-request-extra-headers)
    (url-retrieve
     emms-listenbrainz-scrobbler-submission-url
     #'emms-listenbrainz-scrobbler-submission-callback
     (list (cons track playing-now)))))

(defun emms-listenbrainz-scrobbler-submission-callback (status &optional cbargs)
  "Callback to handle response from listenbrainz server.

Ignore STATUS argument and store submission data in CBARGS."
  (ignore status)
  (when (< (point-max) 1)
    (error "No response from submission server"))
  (goto-char (point-min ))
  (let* ((response (ignore-errors
		     (re-search-forward "\n\n")
		     (json-read)))
	 (track (car cbargs))
	 (playing-now (cdr cbargs))
	 (title (emms-track-get track 'info-title)))
    (cond ((string= "ok" (alist-get 'status response))
	   (when (and (not playing-now) emms-listenbrainz-scrobbler-display-submissions)
	     (message "Listenbrainz: submitted %s." title))
	   ;; tidy up
	   (kill-buffer))
	  ((assoc 'error response)
	   (message "Listenbrainz error: %s while submitting %s"
		    (alist-get 'error response)
		    title))
	  (t (error "Listenbrainz: unhandled error while submitting %s" title)))))


;;* Hooks

(defun emms-listenbrainz-scrobbler-start-hook ()
  "Record track start time."
  (setq emms-listenbrainz-scrobbler-track-play-start-timestamp
	(time-convert nil 'integer))
  (let* ((current-track (emms-playlist-current-selected-track))
	 (good-to-submit (eq (emms-track-type current-track) 'file)))
    (when good-to-submit
      (emms-listenbrainz-scrobbler-make-async-submission-call
       current-track t))))

(defun emms-listenbrainz-scrobbler-stop-hook ()
  "Submit the track to listenbrainz if we have listened long enough.

That is, if it has been played for 240 seconds or half the length of the track."
  (let ((current-track (emms-playlist-current-selected-track)))
    (let ((track-length (emms-track-get current-track 'info-playing-time)))
      (when (and track-length
		 ;; only submit files
		 (eq (emms-track-type current-track) 'file))
	(when (and
	       ;; track must be longer than 30 secs
	       (> track-length 30)
	       ;; track must be played for more than 240 secs or
	       ;;   half the tracks length, whichever comes first.
	       (> emms-playing-time (min 240 (/ track-length 2))))
	  (emms-listenbrainz-scrobbler-make-async-submission-call
	   current-track))))))


;;* Entry points
(defun emms-listenbrainz-scrobbler-enable ()
  "Enable the scrobbler and submit played tracks."
  (interactive)
  ;; check we have credentials
  (if (emms-listenbrainz-scrobbler-get-token)
      (unless emms-listenbrainz-scrobbler-running
	(add-hook 'emms-player-started-hook
		  'emms-listenbrainz-scrobbler-start-hook t)
	(add-hook 'emms-player-stopped-hook
		  'emms-listenbrainz-scrobbler-stop-hook)
	(add-hook 'emms-player-finished-hook
		  'emms-listenbrainz-scrobbler-stop-hook)
	(setq emms-listenbrainz-scrobbler-running t))
    (error "Listenbrainz scrobbler: no user token.  Please supply and try again")))

(defun emms-listenbrainz-scrobbler-disable ()
  "Disable the scrobbler and don't submit played tracks."
  (interactive)
  (when emms-listenbrainz-scrobbler-running
    (remove-hook 'emms-player-started-hook
		 'emms-listenbrainz-scrobbler-start-hook)
    (remove-hook 'emms-player-stopped-hook
		 'emms-listenbrainz-scrobbler-stop-hook)
    (remove-hook 'emms-player-finished-hook
		 'emms-listenbrainz-scrobbler-stop-hook)
    (setq emms-listenbrainz-scrobbler-running nil)))


(provide 'emms-listenbrainz-scrobbler)
;;; emms-listenbrainz-scrobbler.el ends here
