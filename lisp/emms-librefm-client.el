;;; emms-librefm-client.el --- Last.FM Music API

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Yoni Rabkin <yrk@gnu.org>

;; Keywords: emms, libre.fm

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
;;
;; Definitive information on how to setup and use this package is
;; provided in the wonderful Emms manual, in the /doc directory of the
;; Emms distribution.

;;; Code:

(defvar emms-librefm-client-handshake-url
  "turtle.libre.fm")

(defvar emms-librefm-client-method
  "http")

(defvar emms-librefm-client-username
  "")

(defvar emms-librefm-client-password
  "")

(defvar emms-librefm-client-debug
  "")


(defun emms-librefm-client-handshake-string (url username password)
  (let ((timestamp (format-time-string "%s")))
    (concat emms-librefm-client-method
	    "://"
	    url "/?"
	    "hs=true" "&"
	    "p=1.2"   "&"
	    "c=emm"   "&"
	    "v=1.0"   "&"
	    "u=" (url-encode-url username) "&"
	    "t=" timestamp "&"
	    "a=" (md5 (concat (md5 password) timestamp)))))

(defun emms-librefm-client-handshake (url username password)
  (let ((url-request-method "POST"))
    (let ((response
	   (url-retrieve-synchronously
	    (emms-librefm-client-handshake-string
	     url username password))))
      (setq emms-librefm-client-debug
	    (with-current-buffer response
	      (buffer-substring-no-properties (point-min)
					      (point-max)))))))


(provide 'emms-librefm-client)


;;; emms-librefm-client.el ends here
