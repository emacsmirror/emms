;;; emms-info-native.el --- Native Emacs Lisp info method for EMMS

;; Copyright (C) 2020 Free Software Foundation, Inc.

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
;; relying on external tools or libraries like ‘emms-info-ogginfo’ or
;; ‘emms-info-libtag’.
;;
;; To use this method, add ‘emms-info-native’ to
;; ‘emms-info-functions’.
;;
;; The following file formats are supported:
;;
;; - Vorbis: Ogg Vorbis I Profile, filename extension ‘.ogg’,
;;   elemetary streams only.  Based on xiph.org’s Vorbis I
;;   specification, see URL
;;   ‘https://xiph.org/vorbis/doc/Vorbis_I_spec.html’.
;;
;; - Opus: Ogg Opus profile, filename extesion ‘.opus’, elementary
;;   streams only.  Based on RFC 7845, see URL
;;   ‘https://tools.ietf.org/html/rfc7845.html’.
;;
;; - FLAC: FLAC streams in native encapsulation format, filename
;;   extesion ‘.flac’.  Based on xiph.org’s FLAC format specification,
;;   see URL ‘https://xiph.org/flac/format.html’.
;;
;; Format detection is based solely on filename extension, which is
;; matched case-insensitively.

;;; Code:

(require 'bindat)

(defconst emms-info-native--max-peek-size (* 512 1024)
  "Maximum buffer size for metadata decoding.
Functions called by ‘emms-info-native’ read certain amounts of
data into a temporary buffer while attempting to read metadata
information.  This variable controls the maximum size of that
buffer: if more than ‘emms-info-native--max-peek-size’ bytes are
needed, an error is signaled.

Technically metadata blocks can have almost arbitrary lengths,
but in practice processing must be constrained to prevent memory
exhaustion in case of garbled or malicious inputs.")

;;;; Ogg code

(defconst emms-info-native--ogg-magic-array
  [79 103 103 83]
  "Ogg format magic capture pattern ‘OggS’.")

(defconst emms-info-native--ogg-page-size 65307
  "Maximum size for a single Ogg container page.
Ogg files are read in chunks of this size during decoding.")

(defconst emms-info-native--ogg-page-bindat-spec
  '((capture-pattern vec 4)
    (eval (when (not (equal last emms-info-native--ogg-magic-array))
            (error "Ogg framing mismatch: expected ‘%s’, got ‘%s’"
                   emms-info-native--ogg-magic-array
                   last)))
    (stream-structure-version u8)
    (eval (when (not (= last 0))
            (error ("Ogg stream structure version mismatch: expected 0, got %s")
                   last)))
    (header-type-flag u8)
    (granule-position vec 8)
    (stream-serial-number vec 4)
    (page-sequence-no vec 4)
    (page-checksum vec 4)
    (page-segments u8)
    (segment-table vec (page-segments))
    (payload vec (eval (seq-reduce #'+ last 0))))
  "Ogg page structure specification.
Framing and stream structure versions are verified, otherwise the
data is assumed to be valid.")

(defun emms-info-native--decode-ogg (filename packets)
  "Decode at least PACKETS number of packets from Ogg file FILENAME.
Read in data from the start of FILENAME, remove Ogg packet
frames, and concatenate payloads until at least PACKETS number of
packets have been decoded.  Return the decoded packets in a
vector, concatenated.

Data is read in ‘emms-info-native--ogg-page-size’ chunks.  If the
total length of concatenated packets becomes greater than
‘emms-info-native--max-peek-size’, an error is signaled.

Only elementary streams are supported, that is, FILENAME should
contain only a single logical stream.  Note that this assumption
is not verified: with non-elementary streams packets from
different streams will be mixed together without an error."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((npackets 0)
          (offset 0)
          (stream (vector))
          page)
      (while (< npackets packets)
        (insert-file-contents-literally filename
                                        nil
                                        offset
                                        (+ offset
                                           emms-info-native--ogg-page-size)
                                        t)
        (setq page
              (bindat-unpack emms-info-native--ogg-page-bindat-spec
                             (buffer-string)))
        (setq offset
              (+ offset
                 (bindat-length emms-info-native--ogg-page-bindat-spec page)))
        (setq stream (vconcat stream (bindat-get-field page 'payload)))
        (when (> (length stream) emms-info-native--max-peek-size)
          (error "Ogg payload is too large"))
        ;; Look for packet boundaries: every element that is less than 255
        ;; in the segment table represents a packet boundary.
        (setq npackets
              (+ (length (seq-filter (lambda (elt) (< elt 255))
                                     (bindat-get-field page 'segment-table)))
                 npackets)))
      stream)))

(defun emms-info-native--ogg-decode-comments (filename stream-type)
  "Decode comment header from Ogg file FILENAME.
The file is assumed to contain a single stream of type
STREAM-TYPE, which must either ‘vorbis’ or ‘opus’.

Return a list of comments.  Depending on STREAM-TYPE, its
elements are either of type
‘emms-info-native--vorbis-comment-header-bindat-spec’ or
‘emms-info-native--opus-comment-header-bindat-spec’."
  (let ((packets (emms-info-native--decode-ogg filename 2))
        stream)
    (setq stream
          (cond ((eq stream-type 'vorbis)
                 (bindat-unpack emms-info-native--vorbis-headers-bindat-spec
                                packets))
                ((eq stream-type 'opus)
                 (bindat-unpack emms-info-native--opus-headers-bindat-spec
                                packets))
                (t (error "Unknown stream type %s" stream-type))))
    (bindat-get-field stream 'comment-header 'user-comment)))

;;;; Vorbis code

(defconst emms-info-native--max-num-vorbis-comments 1024
  "Maximum number of Vorbis comment fields in a stream.
Technically a single Vorbis stream may have up to 2^32 comments,
but in practice processing must be constrained to prevent memory
exhaustion in case of garbled or malicious inputs.

This limit is used with Opus and FLAC streams as well, since
their comments have almost the same format as Vorbis.")

(defconst emms-info-native--max-vorbis-comment-size (* 64 1024)
  "Maximum length for a single Vorbis comment field.
Technically a single Vorbis comment may have a length up to 2^32
bytes, but in practice processing must be constrained to prevent
memory exhaustion in case of garbled or malicious inputs.

This limit is used with Opus and FLAC streams as well, since
their comments have almost the same format as Vorbis.")

(defconst emms-info-native--max-vorbis-vendor-length 1024
  "Maximum length of Vorbis vendor string.
Technically a vendor string can be up to 2^32 bytes long, but in
practice processing must be constrained to prevent memory
exhaustion in case of garbled or malicious inputs.

This limit is used with Opus and FLAC streams as well, since
their comments have almost the same format as Vorbis.")

(defconst emms-info-native--vorbis-magic-array
  [118 111 114 98 105 115]
  "Header packet magic pattern ‘vorbis’.")

(defconst emms-info-native--vorbis-identification-header-bindat-spec
  '((packet-type u8)
    (eval (when (not (= last 1))
            (error "Vorbis identification header type mismatch: expected 1, got %s"
                   last)))
    (vorbis vec 6)
    (eval (when (not (equal last emms-info-native--vorbis-magic-array))
            (error "Vorbis framing mismatch: expected ‘%s’, got ‘%s’"
                   emms-info-native--vorbis-magic-array
                   last)))
    (vorbis-version u32r)
    (eval (when (not (= last 0))
            (error "Vorbis version mismatch: expected 0, got %s" last)))
    (audio-channels u8)
    (audio-sample-rate u32r)
    (bitrate-maximum u32r)
    (bitrate-nominal u32r)
    (bitrate-minimum u32r)
    (blocksize u8)
    (framing-flag u8)
    (eval (unless (= last 1))
          (error "Vorbis framing bit mismatch: expected 1, got %s" last)))
  "Vorbis identification header specification.
Identification, framing and version data are verified, otherwise
the data is assumed to be valid.")

(defconst emms-info-native--vorbis-comment-header-bindat-spec
  '((packet-type u8)
    (eval (when (not (= last 3))
            (error "Vorbis comment header type mismatch: expected 3, got %s"
                   last)))
    (vorbis vec 6)
    (eval (when (not (equal last emms-info-native--vorbis-magic-array))
            (error "Vorbis framing mismatch: expected ‘%s’, got ‘%s’"
                   emms-info-native--vorbis-magic-array
                   last)))
    (vendor-length u32r)
    (eval (when (> last emms-info-native--max-vorbis-vendor-length)
            (error "Vorbis vendor length %s is too long" last)))
    (vendor-string vec (vendor-length))
    (user-comment-list-length u32r)
    (eval (when (> last emms-info-native--max-num-vorbis-comments)
            (error "Vorbis user comment list length %s is too long" last)))
    (user-comment repeat
                  (user-comment-list-length)
                  (struct emms-info-native--vorbis-comment-field-bindat-spec))
    (framing-bit u8)
    (eval (unless (= last 1))
          (error "Vorbis framing bit mismatch: expected 1, got %s" last)))
  "Vorbis comment header specification.
Header type and framing data are verified.  Too long vendor
string and comment list will also trigger an error.")

(defconst emms-info-native--vorbis-comment-field-bindat-spec
  '((length u32r)
    (eval (when (> last emms-info-native--max-vorbis-comment-size)
            (error "Vorbis comment is too long, length %s" last)))
    (user-comment vec (length)))
  "Vorbis comment field specification.
Too long comment will trigger an error.

This field is used in Opus and FLAC comment structures as well.")

(defconst emms-info-native--vorbis-headers-bindat-spec
  '((identification-header struct emms-info-native--vorbis-identification-header-bindat-spec)
    (comment-header struct emms-info-native--vorbis-comment-header-bindat-spec))
  "Specification for two first Vorbis header packets.
They are always an identification header followed by a comment
header.")

(defun emms-info-native--split-vorbis-comment (comment)
  "Split Vorbis comment to a field-value pair.
Vorbis comments are of form ‘FIELD=VALUE’.  FIELD is a
case-insensitive field name with a restricted set of ASCII
characters.  VALUE is an arbitrary UTF-8 encoded octet stream.

Return a cons cell (FIELD . VALUE), where FIELD is converted to
upper case and VALUE is the decoded value."
  (let ((comment-string (decode-coding-string (mapconcat
                                               #'byte-to-string
                                               comment
                                               "")
                                              'utf-8)))
    (when (string-match "^\\(.+?\\)=\\(.+?\\)$" comment-string)
      (cons (upcase (match-string 1 comment-string))
            (match-string 2 comment-string)))))

;;;; Opus code

(defconst emms-info-native--opus-head-magic-array
  [79 112 117 115 72 101 97 100]
  "Opus identification header magic pattern ‘OpusHead’.")

(defconst emms-info-native--opus-tags-magic-array
  [79 112 117 115 84 97 103 115]
  "Opus comment header magic pattern ‘OpusTags’.")

(defconst emms-info-native--opus-channel-mapping-table
  '((stream-count u8)
    (coupled-count u8)
    (channel-mapping vec (channel-count)))
  "Opus channel mapping table specification.")

(defconst emms-info-native--opus-identification-header-bindat-spec
  '((opus-head vec 8)
    (eval (when (not (equal last emms-info-native--opus-head-magic-array))
            (error "Opus framing mismatch: expected ‘%s’, got ‘%s’"
                   emms-info-native--opus-head-magic-array
                   last)))
    (opus-version u8)
    (eval (when (not (< last 16))
            (error "Opus version mismatch: expected less than 16, got %s"
                   last)))
    (channel-count u8)
    (pre-skip u16r)
    (sample-rate u32r)
    (output-gain u16r)
    (channel-mapping-family u8)
    (eval (> last 0) (struct opus-channel-mapping-table)))
  "Opus identification header specification.
Framing and version data are verified, otherwise the data is
assumed to be valid.")

(defconst emms-info-native--opus-comment-header-bindat-spec
  '((opus-tags vec 8)
    (eval (when (not (equal last emms-info-native--opus-tags-magic-array))
            (error "Opus framing mismatch: expected ‘%s’, got ‘%s’"
                   emms-info-native--opus-tags-magic-array
                   last)))
    (vendor-length u32r)
    (eval (when (> last emms-info-native--max-vorbis-vendor-length)
            (error "Opus vendor length %s is too long" last)))
    (vendor-string vec (vendor-length))
    (user-comment-list-length u32r)
    (eval (when (> last emms-info-native--max-num-vorbis-comments)
            (error "Opus user comment list length %s is too long" last)))
    (user-comment repeat
                  (user-comment-list-length)
                  (struct emms-info-native--vorbis-comment-field-bindat-spec)))
  "Opus comment header specification.
Framing is verified.  Too long vendor string and comment list
will also trigger an error.")

(defconst emms-info-native--opus-headers-bindat-spec
  '((identification-header struct emms-info-native--opus-identification-header-bindat-spec)
    (comment-header struct emms-info-native--opus-comment-header-bindat-spec))
  "Specification for two first Opus header packets.
They are always an identification header followed by a comment
header.")

;;;; FLAC code

(defconst emms-info-native--flac-metadata-block-header-bindat-spec
  '((block-type u8)
    (block-length u24))
  "FLAC metadata block header specification.")

(defconst emms-info-native--flac-comment-bindat-spec
  '((vendor-length u32r)
    (eval (when (> last emms-info-native--max-vorbis-vendor-length)
            (error "FLAC vendor length %s is too long" last)))
    (vendor-string vec (vendor-length))
    (user-comment-list-length u32r)
    (eval (when (> last emms-info-native--max-num-vorbis-comments)
            (error "FLAC user comment list length %s is too long" last)))
    (user-comment repeat
                  (user-comment-list-length)
                  (struct emms-info-native--vorbis-comment-field-bindat-spec)))
  "FLAC Vorbis comment block specification.
Too long vendor string and comment list will trigger an error.")

(defun emms-info-native--has-flac-signature (filename)
  "Check for FLAC stream marker at the beginning of FILENAME.
Return t if there is a valid stream marker, nil otherwise."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename nil 0 4)
    (looking-at "fLaC")))

(defun emms-info-native--flac-decode-block-header (filename offset)
  "Read and decode FLAC metadata block header from FILENAME starting at OFFSET.
Return a list (TYPE NEXT-OFFSET LAST).  Here, TYPE is the FLAC
metadata block type; NEXT-OFFSET is the starting offset of the
next block; and LAST is t if this was the last metadata block in
the stream, otherwise nil."
  (let (block-header
        block-type
        block-length
        end
        last-flag)
    (insert-file-contents-literally filename nil offset (+ offset 4) t)
    (setq offset (+ offset 4))
    (setq block-header
          (bindat-unpack emms-info-native--flac-metadata-block-header-bindat-spec
                         (buffer-string)))
    (setq block-type
          (logand (bindat-get-field block-header 'block-type)
                  #x7F))
    (setq block-length (bindat-get-field block-header 'block-length))
    (when (> block-type 6)
      (error "FLAC block type error: expected <= 6, got %s" block-type))
    (when (= block-length 0)
      (error "FLAC block length error: expected >0, got zero"))
    (setq last-flag (= (logand (bindat-get-field block-header 'block-type)
                               #x80)
                       1))
    (setq end (+ offset block-length))
    (list block-type end last-flag)))

(defun emms-info-native--flac-decode-comment-block (filename)
  "Find and decode a comment block from FLAC file FILENAME.
Return the comment block in a vector.  Trigger an error if any
metadata block larger than ‘emms-info-native--max-peek-size’ is
encountered."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (unless (emms-info-native--has-flac-signature filename)
      (error "Invalid FLAC stream"))
    (let ((offset 4)
          (comment-block (vector))
          block-type
          end
          last-flag)
      (while (not last-flag)
        (cl-multiple-value-setq (block-type
                                 end
                                 last-flag)
          (emms-info-native--flac-decode-block-header filename offset))
        (when (> (- end offset) emms-info-native--max-peek-size)
          (error "FLAC metadata block is too large: %s" (- end offset)))
        (when (= block-type 4)
          ;; Comment block found, extract it.
          (insert-file-contents-literally filename nil (+ offset 4) end t)
          (setq comment-block (vconcat (buffer-string))
                last-flag t))
        (setq offset end))
      comment-block)))

(defun emms-info-native--flac-decode-comments (filename)
  "Read and decode comments from FLAC file FILENAME.
Return a list of comments.  See
‘emms-info-native--vorbis-comment-field-bindat-spec’ for comment
structure."
  (bindat-get-field (bindat-unpack emms-info-native--flac-comment-bindat-spec
                                   (emms-info-native--flac-decode-comment-block filename))
                    'user-comment))

;;;; EMMS code

(defun emms-info-native--find-stream-type (filename)
  "Deduce the stream type from FILENAME.
This is a naive implementation that relies solely on filename
extension.

Return one of symbols ‘vorbis’, ‘opus’, or ‘flac’."
  (let ((case-fold-search t))
    (cond ((string-match ".ogg$" filename) 'vorbis)
          ((string-match ".opus$" filename) 'opus)
          ((string-match ".flac$" filename) 'flac)
          (t nil))))

(defun emms-info-native (track)
  "Set info fields for TRACK.
Supports Ogg Vorbis/Opus and FLAC files.

Return t if TRACK was updated, nil otherwise."
  (let* ((filename (emms-track-name track))
         (stream-type (emms-info-native--find-stream-type filename))
         (comments)
         update-flag)
    (setq comments
          (cond ((or (eq stream-type 'vorbis) (eq stream-type 'opus))
                 (emms-info-native--ogg-decode-comments filename stream-type))
                ((eq stream-type 'flac)
                 (emms-info-native--flac-decode-comments filename))
                (t nil)))
    (dolist (comment comments)
      (let ((pair (emms-info-native--split-vorbis-comment
                   (cdr (assoc 'user-comment comment)))))
        (when pair
          (let ((name (intern-soft (concat "info-" (downcase (car pair)))))
                (value (cdr pair)))
            (setq update-flag (or update-flag name))
            (emms-track-set track
                            name
                            (if (eq name 'info-playing-time)
                                (string-to-number value)
                              value))))))
    update-flag))

(provide 'emms-info-native)

;;; emms-info-native.el ends here
