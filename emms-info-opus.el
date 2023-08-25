;;; emms-info-opus.el --- EMMS Opus info support  -*- lexical-binding: t; -*-

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

;; This file contains Opus-specific code for `emms-info-ogg' feature.

(require 'emms-info-vorbis)

(defvar emms-info-opus--channel-count 0
  "Last decoded Opus channel count.
This is a kludge; it is needed because bindat spec cannot refer
outside itself.")

(defconst emms-info-opus--headers-bindat-spec
  '((identification-header struct emms-info-opus--id-header-bindat-spec)
    (comment-header struct emms-info-opus--comment-header-bindat-spec))
  "Specification for two first Opus header packets.
They are always an identification header followed by a comment
header.")

(defconst emms-info-opus--id-header-bindat-spec
  '((opus-head vec 8)
    (eval (unless (equal last emms-info-opus--id-magic-pattern)
            (error "Opus framing mismatch: expected `%s', got `%s'"
                   emms-info-opus--id-magic-pattern
                   last)))
    (opus-version u8)
    (eval (unless (< last 16)
            (error "Opus version mismatch: expected < 16, got %s"
                   last)))
    (channel-count u8)
    (eval (setq emms-info-opus--channel-count last))
    (pre-skip u16r)
    (sample-rate u32r)
    (output-gain u16r)
    (channel-mapping-family u8)
    (union (channel-mapping-family)
           (0 nil)
           (t (struct emms-info-opus--channel-mapping-bindat-spec))))
  "Opus identification header specification.")

(defconst emms-info-opus--id-magic-pattern
  (string-to-vector "OpusHead")
  "Opus identification header magic pattern.")

(defconst emms-info-opus--channel-mapping-bindat-spec
  '((stream-count u8)
    (coupled-count u8)
    (channel-mapping vec (eval emms-info-opus--channel-count)))
  "Opus channel mapping table specification.")

(defconst emms-info-opus--comment-header-bindat-spec
  '((opus-tags vec 8)
    (eval (unless (equal last emms-info-opus--tags-magic-pattern)
            (error "Opus framing mismatch: expected `%s', got `%s'"
                   emms-info-opus--tags-magic-pattern
                   last)))
    (vendor-length u32r)
    (eval (when (> last emms-info-vorbis--max-vendor-length)
            (error "Opus vendor length %s is too long" last)))
    (vendor-string vec (vendor-length))
    (user-comments-list-length u32r)
    (eval (when (> last emms-info-vorbis--max-comments)
            (error "Opus user comment list length %s is too long"
                   last)))
    (user-comments repeat
                   (user-comments-list-length)
                   (struct emms-info-vorbis--comment-field-bindat-spec)))
  "Opus comment header specification.")

(defconst emms-info-opus--tags-magic-pattern
  (string-to-vector "OpusTags")
  "Opus comment header magic pattern.")

(provide 'emms-info-opus)

;;; emms-info-opus.el ends here
