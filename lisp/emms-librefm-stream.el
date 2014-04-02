;;; emms-librefm-stream.el --- Libre.FM streaming

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Yoni Rabkin <yrk@gnu.org>

;; Keywords: emms, libre.fm, GNU FM

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


;;; Code:

(require 'emms-librefm-scrobbler)

(defvar emms-librefm-stream-host-url
  "alpha.libre.fm"
  "URL for the streaming host")

(defvar emms-librefm-stream-debug
  ""
  "Temporary debug information.")


;;; ------------------------------------------------------------------
;;; radio handshake
;;; ------------------------------------------------------------------

;; http://alpha.libre.fm/radio/handshake.php?version=1.3.0.58&platform=linux&username=USERNAME&passwordmd5=PASSWORDMD5&language=en

(defun emms-librefm-stream-tune-handshake-string ()
  (when (not emms-librefm-scrobbler-username)
    (error "null username"))
  (when (not emms-librefm-scrobbler-password)
    (error "null password"))
  (let ((url (concat "http://"
		     emms-librefm-stream-host-url
		     "/radio/handshake.php?"
		     "version=1.3.0.58" "&"
		     "platform=linux" "&"
		     "username=" (url-encode-url emms-librefm-scrobbler-username) "&"
		     "passwordmd5=" (md5 emms-librefm-scrobbler-password) "&"
		     "language=en")))
    url))

(defun emms-librefm-stream-tune-handshake-call ()
  ""
  (let ((url-request-method "POST"))
    (let ((response
	   (url-retrieve-synchronously
	    (emms-librefm-stream-tune-handshake-string))))
      (setq emms-librefm-stream-debug
	    (with-current-buffer response
	      (buffer-substring-no-properties (point-min)
					      (point-max))))
      response)))


;;; ------------------------------------------------------------------
;;; tuning
;;; ------------------------------------------------------------------

(defun emms-librefm-stream-tune-string (session-id station)
  ""
  (when (not session-id)
    (error "null session id"))
  (when (not station)
    (error "null station"))
  (let ((url (concat "http://"
		     emms-librefm-stream-host-url
		     "/radio/adjust.php?"
		     "session=" session-id "&"
		     "url=" (url-encode-url station))))
    url))

(defun emms-librefm-stream-tune-call (session-id station)
  ""
  (let ((url-request-method "POST"))
    (let ((response
	   (url-retrieve-synchronously
	    (emms-librefm-stream-tune-string
	     session-id station))))
      (setq emms-librefm-stream-debug
	    (with-current-buffer response
	      (buffer-substring-no-properties (point-min)
					      (point-max))))
      response)))

(defun emms-librefm-stream-handle-tune-response (resbuf)
  "Handle the tune server response."
  (when (not (bufferp resbuf))
    (error "response not a buffer"))
  (with-current-buffer resbuf
    (goto-char (point-min))
    (when (not (re-search-forward "^.*200 OK$" (point-at-eol) t))
      (error "bad HTTP server response"))
    ;; go to the start of the FM response
    (when (not (re-search-forward "\n\n" (point-max) t))
      (error "bad FM server response"))
    (let ((status (buffer-substring (point-at-bol)
				    (point-at-eol))))
      (cond ((string= status "OK")         'ok)
	    ((string= status "BADSESSION") 'badsession)
	    (t (error "unhandled response status: [%s]" status))))))


(provide 'emms-librefm-stream)

;;; emms-librefm-stream.el ends here
