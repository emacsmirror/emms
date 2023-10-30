;;; emms-info-native-flac.el --- EMMS info functions for FLAC files  -*- lexical-binding: t; -*-

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

;; This file contains functions for extracting metadata from FLAC
;; files in their native encapsulation format.  The code is based on
;; xiph.org's FLAC format specification, available at
;; https://xiph.org/flac/format.html.

;;; Code:

(require 'emms)
(require 'emms-info-native-vorbis)
(require 'bindat)

(defvar bindat-raw)

(defconst emms-info-native-flac--max-peek-size (* 16 1024 1024)
  "Maximum buffer size for metadata decoding.
Functions in `emms-info-native-flac' read certain amounts of data
into a temporary buffer while decoding metadata.  This variable
controls the maximum size of that buffer: if more than
`emms-info-native-flac--max-peek-size' bytes are needed, an error
is signaled.

Technically metadata blocks can have almost arbitrary lengths,
but in practice processing must be constrained to prevent memory
exhaustion in case of garbled or malicious inputs.")

(defconst emms-info-native-flac--meta-header-bindat-spec
  (if (eval-when-compile (fboundp 'bindat-type))
      (bindat-type
        (flags u8)
        (length uint 24)
        (_ unit (when (or (> length emms-info-native-flac--max-peek-size)
                          (= length 0))
                  (error "FLAC block length %s is invalid" length))))
    '((flags u8)
      (length u24)
      (eval (when (or (> last emms-info-native-flac--max-peek-size)
                      (= last 0))
              (error "FLAC block length %s is invalid" last)))))
  "FLAC metadata block header specification.")

(defconst emms-info-native-flac--stream-info-block-bindat-spec
  (if (eval-when-compile (fboundp 'bindat-type))
      (bindat-type
        (min-block-size uint 16)
        (max-block-size uint 16)
        (min-frame-size uint 24)
        (max-frame-size uint 24)
        (sample-metadata vec 8)
        (md5 vec 16))
    '((min-block-size u16)
      (max-block-size u16)
      (min-frame-size u24)
      (max-frame-size u24)
      (sample-metadata vec 8)
      (md5 vec 16)))
  "FLAC stream info block specification.")

(defconst emms-info-native-flac--comment-block-bindat-spec
  (if (eval-when-compile (fboundp 'bindat-type))
      (bindat-type
        (vendor-length uintr 32)
        (_ unit (when (> vendor-length (length bindat-raw))
                  (error "FLAC vendor length %s is too long"
                         vendor-length)))
        (vendor-string str vendor-length)
        (user-comments-list-length uintr 32)
        (_ unit (when (> user-comments-list-length (length bindat-raw))
                  (error "FLAC user comment list length %s is too long"
                         user-comments-list-length)))
        (user-comments repeat user-comments-list-length
                       type emms-info-native-vorbis--comment-field-bindat-spec))
    '((vendor-length u32r)
      (eval (when (> last (length bindat-raw))
              (error "FLAC vendor length %s is too long" last)))
      (vendor-string str (vendor-length))
      (user-comments-list-length u32r)
      (eval (when (> last (length bindat-raw))
              (error "FLAC user comment list length %s is too long"
                     last)))
      (user-comments repeat
                     (user-comments-list-length)
                     (struct emms-info-native-vorbis--comment-field-bindat-spec))))
  "FLAC Vorbis comment block specification.")

(defun emms-info-native-flac-decode-metadata (filename)
  "Read and decode metadata from FLAC file FILENAME.
Return comments in a list of (FIELD . VALUE) cons cells.
Additionally return stream duration in `playing-time' field.

See `emms-info-native-vorbis-extract-comments' for details."
  (unless (emms-info-native-flac--has-signature filename)
    (error "Invalid FLAC stream"))
  (let* ((blocks
          (emms-info-native-flac--decode-meta-blocks
           (emms-info-native-flac--file-inserter filename)))
         (comment-block
          (and (car blocks)
               (bindat-unpack emms-info-native-flac--comment-block-bindat-spec
                              (car blocks))))
         (stream-info-block
          (and (cadr blocks)
               (bindat-unpack emms-info-native-flac--stream-info-block-bindat-spec
                              (cadr blocks))))
         (user-comments
          (and comment-block
               (bindat-get-field comment-block 'user-comments)))
         (comments
          (and user-comments
               (emms-info-native-vorbis-extract-comments user-comments)))
         (playing-time
          (and stream-info-block
               (emms-info-native-flac--decode-duration
                (emms-be-to-int
                 (bindat-get-field stream-info-block
                                   'sample-metadata))))))
    (nconc comments
           (when playing-time
             (list (cons "playing-time" playing-time))))))

(defun emms-info-native-flac--has-signature (filename)
  "Check for FLAC stream marker at the beginning of FILENAME.
Return t if there is a valid stream marker, nil otherwise."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename nil 0 4)
    (looking-at "fLaC")))

(defun emms-info-native-flac--file-inserter (filename)
  "Return a function for reading bytes from FILENAME.
This is meant for `emms-info-native-flac--decode-meta-blocks'."
  (lambda (offset end)
    (insert-file-contents-literally filename nil offset end t)))

(defun emms-info-native-flac--decode-meta-blocks (read-func)
  "Decode metadata blocks from data supplied by READ-FUNC.
Go through each metadata block looking for comment and stream
info blocks.  Extract and return them in a list, if found."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let (comment-block stream-info-block last-flag (offset 4))
      (while (not last-flag)
        (funcall read-func offset (setq offset (+ offset 4)))
        (let* ((header
                (bindat-unpack emms-info-native-flac--meta-header-bindat-spec
                               (buffer-string)))
               (end (+ offset (bindat-get-field header 'length)))
               (flags (bindat-get-field header 'flags))
               (block-type (logand flags #x7F)))
          (setq last-flag (> (logand flags #x80) 0))
          (when (> block-type 6)
            (error "FLAC block type error: expected <= 6, got %s"
                   block-type))
          (when (= block-type 0)
            ;; Stream info block found, extract it.
            (funcall read-func offset end)
            (setq stream-info-block (buffer-string)))
          (when (= block-type 4)
            ;; Comment block found, extract it.
            (funcall read-func offset end)
            (setq comment-block (buffer-string)))
          (setq offset end)))
      (list comment-block stream-info-block))))

(defun emms-info-native-flac--decode-duration (sample-meta)
  "Decode stream duration from SAMPLE-META.
SAMPLE-META should be a part of stream info metadata block.  See
`emms-info-native-flac--stream-info-block-bindat-spec'.

Return the duration in seconds, or nil if it is not available."
  (let ((sample-rate (emms-extract-bits sample-meta 44 63))
        (num-samples (emms-extract-bits sample-meta 0 35)))
    (when (and (> sample-rate 0)
               (> num-samples 0))
      (/ num-samples sample-rate))))

(provide 'emms-info-native-flac)

;;; emms-info-native-flac.el ends here
