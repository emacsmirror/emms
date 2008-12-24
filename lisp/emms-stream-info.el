;;; emms-stream-info.el --- Info from streaming audio

;; Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Yoni Rabkin <yonirabkin@member.fsf.org>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library was re-implemented from scratch around January of
;; 2009. If you are looking for the old code then grab source code
;; older than that.

;;; Code:

(defvar *emms-stream-info-debug-buffer* "*stream-info-debug*")

;; "http://di-fm-01.quintex.com:8888"

(defun emms-stream-info-call-backend (url)
  (with-temp-buffer
    (call-process "mplayer" nil *emms-stream-info-debug-buffer* nil
		  "-endpos" "0" "-vo" "null" "-ao" "null" url)
    (buffer-substring (point-min) (point-max))))

(setq foo (emms-stream-info-call-backend "http://di-fm-01.quintex.com:8888"))

;; point of entry
(defun emms-stream-info-message (url)
  'stub)

(provide 'emms-stream-info)

;;; emms-stream-info.el ends here
