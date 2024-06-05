;;; emms-idapi.el --- EMMS Music ID API support  -*- lexical-binding: t; -*-
;;

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Yoni Rabkin <yrk@gnu.org>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; EMMS is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING. If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.


;;; Commentary:
;;

;;; Code:
(require 'emms-idapi-musicbrainz)


(defvar emms-idapi-services-alist
  '((musicbrainz . ((search-f . emms-idapi-musicbrainz-search)
		    (name     . "MusicBrainz")
		    (website  . "https://musicbrainz.org/"))))
  "Association list of services supported by IDAPI.")

(defvar emms-idapi-service nil
  "The music search service currently in use.")


(defun emms-idapi-search (service search-term-alist)
  "Search against SERVICE for SEARCH-TERM-ALIST."
  (let ((search-function (alist-get 'search-f
				    (alist-get service
					       emms-idapi-services-alist))))
    (if (not search-function)
	(error "`%s' is an unsupported service." service))
    (funcall search-function search-term-alist)))
	   

(provide 'emms-idapi)

;;; emms-idapi.el ends here
