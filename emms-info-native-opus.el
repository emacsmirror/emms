;;; emms-info-native-opus.el --- EMMS Opus info support  -*- lexical-binding: t; -*-

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

;; This file contains Opus-specific code for `emms-info-native-ogg'
;; feature.

;;; Code:

(require 'emms-info-native-vorbis)
(require 'bindat)

(defvar bindat-raw)

(defvar emms-info-native-opus--channel-count 0
  "Last decoded Opus channel count.")

(defconst emms-info-native-opus--id-magic-pattern "OpusHead"
  "Opus identification header magic pattern.")

(defconst emms-info-native-opus--channel-mapping-bindat-spec
  (if (eval-when-compile (fboundp 'bindat-type))
      (bindat-type
        (stream-count u8)
        (coupled-count u8)
        (channel-mapping vec emms-info-native-opus--channel-count))
  '((stream-count u8)
    (coupled-count u8)
    (channel-mapping vec (eval emms-info-native-opus--channel-count))))
  "Opus channel mapping table specification.")

(defconst emms-info-native-opus--id-header-bindat-spec
  (if (eval-when-compile (fboundp 'bindat-type))
      (bindat-type
        (opus-head str 8)
        (_ unit (unless (equal opus-head emms-info-native-opus--id-magic-pattern)
                  (error "Opus framing mismatch: expected `%s', got `%s'"
                         emms-info-native-opus--id-magic-pattern
                         opus-head)))
        (opus-version u8)
        (_ unit (unless (< opus-version 16)
                  (error "Opus version mismatch: expected < 16, got %s"
                         opus-version)))
        (channel-count u8)
        (_ unit (progn (setq emms-info-native-opus--channel-count channel-count) nil))
        (pre-skip uintr 16)
        (sample-rate uintr 32)
        (output-gain uintr 16)
        (channel-mapping-family u8)
        (_ . (if (> channel-mapping-family 0)
                 (type emms-info-native-opus--channel-mapping-bindat-spec)
               (unit nil))))
    '((opus-head str 8)
      (eval (unless (equal last emms-info-native-opus--id-magic-pattern)
              (error "Opus framing mismatch: expected `%s', got `%s'"
                     emms-info-native-opus--id-magic-pattern
                     last)))
      (opus-version u8)
      (eval (unless (< last 16)
              (error "Opus version mismatch: expected < 16, got %s"
                     last)))
      (channel-count u8)
      (eval (setq emms-info-native-opus--channel-count last))
      (pre-skip u16r)
      (sample-rate u32r)
      (output-gain u16r)
      (channel-mapping-family u8)
      (union (channel-mapping-family)
             (0 nil)
             (t (struct emms-info-native-opus--channel-mapping-bindat-spec)))))
  "Opus identification header specification.")

(defconst emms-info-native-opus--tags-magic-pattern "OpusTags"
  "Opus comment header magic pattern.")

(defconst emms-info-native-opus--comment-header-bindat-spec
  (if (eval-when-compile (fboundp 'bindat-type))
      (bindat-type
        (opus-tags str 8)
        (_ unit (unless (equal opus-tags emms-info-native-opus--tags-magic-pattern)
                  (error "Opus framing mismatch: expected `%s', got `%s'"
                         emms-info-native-opus--tags-magic-pattern
                         opus-tags)))
        (vendor-length uintr 32)
        (_ unit (when (> vendor-length (length bindat-raw))
                  (error "Opus vendor length %s is too long"
                         vendor-length)))
        (vendor-string str vendor-length)
        (user-comments-list-length uintr 32)
        (_ unit (when (> user-comments-list-length (length bindat-raw))
                  (error "Opus user comment list length %s is too long"
                         user-comments-list-length)))
        (user-comments repeat user-comments-list-length
                       type emms-info-native-vorbis--comment-field-bindat-spec))
    '((opus-tags str 8)
      (eval (unless (equal last emms-info-native-opus--tags-magic-pattern)
              (error "Opus framing mismatch: expected `%s', got `%s'"
                     emms-info-native-opus--tags-magic-pattern
                     last)))
      (vendor-length u32r)
      (eval (when (> last (length bindat-raw))
              (error "Opus vendor length %s is too long" last)))
      (vendor-string str (vendor-length))
      (user-comments-list-length u32r)
      (eval (when (> last (length bindat-raw))
              (error "Opus user comment list length %s is too long"
                     last)))
      (user-comments repeat
                     (user-comments-list-length)
                     (struct emms-info-native-vorbis--comment-field-bindat-spec))))
  "Opus comment header specification.")

(defconst emms-info-native-opus--headers-bindat-spec
  (if (eval-when-compile (fboundp 'bindat-type))
      (bindat-type
        (identification-header type emms-info-native-opus--id-header-bindat-spec)
        (comment-header type emms-info-native-opus--comment-header-bindat-spec))
    '((identification-header struct emms-info-native-opus--id-header-bindat-spec)
      (comment-header struct emms-info-native-opus--comment-header-bindat-spec)))
  "Specification for two first Opus header packets.
They are always an identification header followed by a comment
header.")

(provide 'emms-info-native-opus)

;;; emms-info-native-opus.el ends here
