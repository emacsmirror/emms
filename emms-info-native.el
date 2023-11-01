;;; emms-info-native.el --- Native Emacs Lisp info method for EMMS -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023 Free Software Foundation, Inc.

;; Author: Petteri Hintsanen <petterih@iki.fi>

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

;; This file provides a native emms-info-method for EMMS.  Here
;; "native" means a pure Emacs Lisp implementation instead of one
;; relying on external tools or libraries like
;; `emms-info-native-ogginfo' or `emms-info-libtag'.
;;
;; To use this method, add `emms-info-native' to
;; `emms-info-functions'.
;;
;; The following file formats are supported:
;;
;; - Vorbis: Ogg Vorbis I Profile, filename extension `.ogg',
;;   elementary streams only.
;;
;; - Opus: Ogg Opus profile, filename extension `.opus', elementary
;;   streams only.
;;
;; - FLAC streams in native encapsulation format, filename extension
;;   `.flac'.
;;
;; - MP3 files with extension `.mp3' and ID3v2 tags.  All ID3v2
;;   versions should work, but many features like compression and
;;   encryption are not supported.
;;
;; - SPC files with extension `.spc' and ID666 tags.  This is an audio
;;   file based on a memory dump from an SPC700, a special audio chip
;;   found within Super Nintendos.
;;
;; Format detection is based solely on filename extension, which is
;; matched case-insensitively.

;;; Code:

(require 'emms-info)
(require 'emms-info-native-flac)
(require 'emms-info-native-ogg)
(require 'emms-info-native-mp3)
(require 'emms-info-native-spc)

(defun emms-info-native (track)
  "Set info fields for TRACK.
Supports Ogg Vorbis/Opus, FLAC, MP3 and SPC files."
  (condition-case env
      (let* ((filename
              (emms-track-name track))
             (info-fields
              (emms-info-native--decode-info-fields filename)))
	(dolist (field info-fields)
	  (let ((name (intern (concat "info-" (car field))))
		(value (cdr field)))
            (when (stringp value)
              (setq value (string-trim-right value)))
            (emms-track-set track name value))))
    (error (message "emms-info-native error processing %s: %s"
		    (emms-track-name track) env))))

(defun emms-info-native--decode-info-fields (filename)
  "Decode info fields from FILENAME.
Return a list of (FIELD . VALUE) cons cells, where FIELD is an
info field and VALUE is the corresponding info value.  Both are
strings."
  (let ((stream-type (emms-info-native--find-stream-type filename)))
    (cond ((or (eq stream-type 'vorbis) (eq stream-type 'opus))
           (emms-info-native-ogg-decode-metadata filename stream-type))
          ((eq stream-type 'flac)
           (emms-info-native-flac-decode-metadata filename))
          ((eq stream-type 'mp3)
           (emms-info-native-mp3-decode-metadata filename))
	  ((eq stream-type 'spc)
	   (emms-info-native-spc-decode-id666 filename))
          (t nil))))

(defun emms-info-native--find-stream-type (filename)
  "Deduce the stream type from FILENAME.
This is a naive implementation that relies solely on filename
extension.

Return one of `vorbis', `opus', `flac', `mp3' or `spc', or nil if
the stream type cannot be deduced."
  (let ((case-fold-search t))
    (cond ((string-match ".ogg$" filename) 'vorbis)
          ((string-match ".opus$" filename) 'opus)
          ((string-match ".flac$" filename) 'flac)
          ((string-match ".mp3$" filename) 'mp3)
	  ((string-match ".spc$" filename) 'spc)
          (t nil))))

(provide 'emms-info-native)

;;; emms-info-native.el ends here
