;;; emms-info-native-spc.el --- EMMS info functions for SPC files  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Warren Wilkinson <warrenwilkinson@gmail.com>

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

;; This file provides a native emms-info-method for SPC files. (well,
;; actually the ID666 tag embedded inside them).  "Native" means a
;; pure Emacs Lisp implementation instead of one relying on external
;; tools or libraries.

;;; Code:

(require 'bindat)

(defconst emms-info-native-spc--id666-magic-pattern
  "SNES-SPC700 Sound File Data v0.30"
  "ID666 header magic pattern.")

(defconst emms-info-native-spc--id666-header-bindat-spec
  (if (eval-when-compile (fboundp 'bindat-type))
      (bindat-type
        (file-identifier str 33)
        (_ unit (unless (equal file-identifier
                               emms-info-native-spc--id666-magic-pattern)
                  (error "ID666 framing mismatch: expected `%s', got `%s'"
                         emms-info-native-spc--id666-magic-pattern
                         file-identifier)))
        (unused uint 16)
        (has-id666 u8)
        (revision u8)
        (pc-reg uint 16)
        (a-reg u8)
        (x-reg u8)
        (y-reg u8)
        (psw-reg u8)
        (sp-reg u8)
        (res-reg uint 16)
        (song-title strz 32)
        (game-title strz 32)
        (dumper strz 16)
        (comment strz 32)
        (date strz 11)
        (fadeout vec 3)
        (fadeout-length vec 5)
        (artist strz 32))
  '((file-identifier str 33)
    (eval (unless (equal last emms-info-native-spc--id666-magic-pattern)
            (error "ID666 framing mismatch: expected `%s', got `%s'"
                   emms-info-native-spc--id666-magic-pattern
                   last)))
    (unused u16)
    (has-id666 u8)
    (revision u8)
    (pc-reg u16)
    (a-reg u8)
    (x-reg u8)
    (y-reg u8)
    (psw-reg u8)
    (sp-reg u8)
    (res-reg u16)
    (song-title strz 32)
    (game-title strz 32)
    (dumper strz 16)
    (comment strz 32)
    (date strz 11)
    (fadeout vec 3)
    (fadeout-length vec 5)
    (artist strz 32)))
  "ID666 header specification.

Sources:

- URL `https://ocremix.org/info/SPC_Format_Specification'
- URL `https://picard-docs.musicbrainz.org/en/appendices/tag_mapping.html'")

(defun emms-info-native-spc--decode-id666-header (filename)
  "Read and decode ID666 header from FILENAME."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename nil 0 210)
    (bindat-unpack emms-info-native-spc--id666-header-bindat-spec
                   (buffer-string))))

(defun emms-info-native-spc-decode-id666 (filename)
  "Read and decode ID666 metadata from FILENAME.
Return metadata in a list of (FIELD . VALUE) cons cells, or nil
in case of errors or if there were no known fields in FILENAME."
  (condition-case nil
      (let ((header (emms-info-native-spc--decode-id666-header filename)))
	(when (= 26 (bindat-get-field header 'has-id666))
	  (list
	   (cons 'info-title (bindat-get-field header 'song-title))
	   (cons 'info-album (bindat-get-field header 'game-title))
	   (cons 'info-artist (bindat-get-field header 'artist))
	   (cons 'info-composer (bindat-get-field header 'artist))
	   (cons 'info-note (bindat-get-field header 'comment)))))
    (error nil)))

(provide 'emms-info-native-spc)

;;; emms-info-native-spc.el ends here
