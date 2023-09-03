;;; emms-info-flac.el --- EMMS info functions for FLAC files  -*- lexical-binding: t; -*-

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

(require 'bindat)
(require 'emms)
(require 'emms-info-vorbis)
(eval-when-compile
  (require 'cl-lib))

(defconst emms-info-flac--max-peek-size (* 2048 1024)
  "Maximum buffer size for metadata decoding.
Functions in `emms-info-flac' read certain amounts of data into a
temporary buffer while decoding metadata.  This variable controls
the maximum size of that buffer: if more than
`emms-info-flac--max-peek-size' bytes are needed, an error is
signaled.

Technically metadata blocks can have almost arbitrary lengths,
but in practice processing must be constrained to prevent memory
exhaustion in case of garbled or malicious inputs.")

(defconst emms-info-flac--meta-header-bindat-spec
  '((flags u8)
    (length u24)
    (eval (when (or (> last emms-info-flac--max-peek-size)
                    (= last 0))
            (error "FLAC block length %s is invalid" last))))
  "FLAC metadata block header specification.")

(defconst emms-info-flac--stream-info-block-bindat-spec
  '((min-block-size u16)
    (max-block-size u16)
    (min-frame-size u24)
    (max-frame-size u24)
    (sample-metadata vec 8)
    (md5 vec 16))
  "FLAC stream info block specification.")

(defconst emms-info-flac--comment-block-bindat-spec
  '((vendor-length u32r)
    (eval (when (> last emms-info-vorbis--max-vendor-length)
            (error "FLAC vendor length %s is too long" last)))
    (vendor-string vec (vendor-length))
    (user-comments-list-length u32r)
    (eval (when (> last emms-info-vorbis--max-comments)
            (error "FLAC user comment list length %s is too long"
                   last)))
    (user-comments repeat
                   (user-comments-list-length)
                   (struct emms-info-vorbis--comment-field-bindat-spec)))
  "FLAC Vorbis comment block specification.")

(defun emms-info-flac-decode-metadata (filename)
  "Read and decode metadata from FLAC file FILENAME.
Return comments in a list of (FIELD . VALUE) cons cells.
Additionally return stream duration in `playing-time' field.

See `emms-info-vorbis-extract-comments' for details."
  (unless (emms-info-flac--has-signature filename)
    (error "Invalid FLAC stream"))
  (let* ((blocks
          (emms-info-flac--decode-meta-blocks
           (emms-info-flac--file-inserter filename)))
         (comment-block
          (bindat-unpack emms-info-flac--comment-block-bindat-spec
                         (car blocks)))
         (stream-info-block
          (bindat-unpack emms-info-flac--stream-info-block-bindat-spec
                         (cadr blocks)))
         (user-comments
          (bindat-get-field comment-block 'user-comments))
         (comments
          (emms-info-vorbis-extract-comments user-comments))
         (playing-time
          (emms-info-flac--decode-duration
           (emms-be-to-int
            (bindat-get-field stream-info-block 'sample-metadata)))))
    (nconc comments
           (when playing-time
             (list (cons "playing-time" playing-time))))))

(defun emms-info-flac--has-signature (filename)
  "Check for FLAC stream marker at the beginning of FILENAME.
Return t if there is a valid stream marker, nil otherwise."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename nil 0 4)
    (looking-at "fLaC")))

(defun emms-info-flac--file-inserter (filename)
  "Return a function that reads and inserts bytes from FILENAME.
This is meant for `emms-info-flac--decode-meta-blocks'."
  (lambda (offset end)
    (insert-file-contents-literally filename nil offset end t)))

(defun emms-info-flac--decode-meta-blocks (read-func)
  "Decode metadata blocks from data supplied by READ-FUNC.
Go through each metadata block looking for comment and stream
info blocks.  Extract and return them in a list, if found."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let (comment-block stream-info-block last-flag (offset 4))
      (while (not last-flag)
        (funcall read-func offset (cl-incf offset 4))
        (let* ((header
                (bindat-unpack emms-info-flac--meta-header-bindat-spec
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

(defun emms-info-flac--decode-duration (sample-meta)
  "Decode stream duration from SAMPLE-META.
SAMPLE-META should be a part of stream info metadata block.  See
`emms-info-flac--stream-info-block-bindat-spec'.

Return the duration in seconds, or nil if it is not available."
  (let ((sample-rate (emms-extract-bits sample-meta 44 63))
        (num-samples (emms-extract-bits sample-meta 0 35)))
    (when (and (> sample-rate 0)
               (> num-samples 0))
      (/ num-samples sample-rate))))

(provide 'emms-info-flac)

;;; emms-info-flac.el ends here
