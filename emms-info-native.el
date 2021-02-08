;;; emms-info-native.el --- Native Emacs Lisp info method for EMMS

;; Copyright (C) 2020-2021 Free Software Foundation, Inc.

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
;; - MP3: MP3 files with extension ‘.mp3’ and id3v2 tags.  All id3v2
;;   revisions should work, but many features like CRC and
;;   unsynchronization are not supported.  Based on id3v2 Informal
;;   Standards, see URL ‘https://id3.org’.
;;
;; Format detection is based solely on filename extension, which is
;; matched case-insensitively.

;;; Code:

(require 'bindat)
(require 'cl-lib)
(require 'emms-info)

(defconst emms-info-native--max-peek-size (* 512 1024)
  "Maximum buffer size for metadata decoding.
Functions called by ‘emms-info-native’ read certain amounts of
data into a temporary buffer while decoding metadata.  This
variable controls the maximum size of that buffer: if more than
‘emms-info-native--max-peek-size’ bytes are needed, an error is
signaled.

Technically metadata blocks can have almost arbitrary lengths,
but in practice processing must be constrained to prevent memory
exhaustion in case of garbled or malicious inputs.")

;;;; Ogg code

(defconst emms-info-native--ogg-magic-array
  [79 103 103 83]
  "Ogg format magic capture pattern ‘OggS’.")

(defconst emms-info-native--ogg-page-size 65307
  "Maximum size for a single Ogg container page.")

(defconst emms-info-native--ogg-page-bindat-spec
  '((capture-pattern vec 4)
    (eval (unless (equal last emms-info-native--ogg-magic-array)
            (error "Ogg framing mismatch: expected ‘%s’, got ‘%s’"
                   emms-info-native--ogg-magic-array
                   last)))
    (stream-structure-version u8)
    (eval (unless (= last 0)
            (error ("Ogg version mismatch: expected 0, got %s")
                   last)))
    (header-type-flag u8)
    (granule-position vec 8)
    (stream-serial-number vec 4)
    (page-sequence-no vec 4)
    (page-checksum vec 4)
    (page-segments u8)
    (segment-table vec (page-segments))
    (payload vec (eval (seq-reduce #'+ last 0))))
  "Ogg page structure specification.")

(defun emms-info-native--decode-ogg-comments (filename stream-type)
  "Read and decode comments from Ogg file FILENAME.
The file is assumed to contain a single stream of type
STREAM-TYPE, which must either ‘vorbis’ or ‘opus’.

Return comments in a list of (FIELD . VALUE) cons cells.  See
‘emms-info-native--split-vorbis-comment’ for details."
  (let* ((packets (emms-info-native--decode-ogg-packets filename 2))
         (headers (emms-info-native--decode-ogg-headers packets
                                                        stream-type))
         (comments (bindat-get-field headers
                                     'comment-header
                                     'user-comments)))
    (emms-info-native--extract-vorbis-comments comments)))

(defun emms-info-native--decode-ogg-packets (filename packets)
  "Read and decode packets from Ogg file FILENAME.
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
  (let ((num-packets 0)
        (offset 0)
        (stream (vector)))
    (while (< num-packets packets)
      (let ((page (emms-info-native--decode-ogg-page filename
                                                     offset)))
        (cl-incf num-packets (or (plist-get page :num-packets) 0))
        (cl-incf offset (plist-get page :num-bytes))
        (setq stream (vconcat stream (plist-get page :stream)))
        (when (> (length stream) emms-info-native--max-peek-size)
          (error "Ogg payload is too large"))))
    stream))

(defun emms-info-native--decode-ogg-page (filename offset)
  "Read and decode a single Ogg page from FILENAME.
Starting reading data from byte offset OFFSET.

Return a plist (:num-packets N :num-bytes B :stream S), where N
is the number of packets in the page, B is the size of the page
in bytes, and S is the unframed logical bitstream in a vector.
Note that N can be zero."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename
                                    nil
                                    offset
                                    (+ offset
                                       emms-info-native--ogg-page-size))
    (let* ((page (bindat-unpack emms-info-native--ogg-page-bindat-spec
                                (buffer-string)))
           (num-packets (emms-info-native--num-of-packets page))
           (num-bytes (bindat-length emms-info-native--ogg-page-bindat-spec
                                     page))
           (stream (bindat-get-field page 'payload)))
      (list :num-packets num-packets
            :num-bytes num-bytes
            :stream stream))))

(defun emms-info-native--num-of-packets (page)
  "Return the number of packets in Ogg page PAGE.
PAGE must correspond to
‘emms-info-native--ogg-page-bindat-spec’."
  ;; Every element that is less than 255 in the segment table
  ;; represents a packet boundary.
  (length (seq-filter (lambda (elt) (< elt 255))
                      (bindat-get-field page 'segment-table))))

(defun emms-info-native--decode-ogg-headers (packets stream-type)
  "Decode first two stream headers from PACKETS for STREAM-TYPE.
STREAM-TYPE must be either ‘vorbis’ or ‘opus’.

Return a structure that corresponds to either
‘emms-info-native--opus-headers-bindat-spec’ or
‘emms-info-native--vorbis-headers-bindat-spec’."
  (cond ((eq stream-type 'vorbis)
         (bindat-unpack emms-info-native--vorbis-headers-bindat-spec
                        packets))
        ((eq stream-type 'opus)
         (bindat-unpack emms-info-native--opus-headers-bindat-spec
                        packets))
        (t (error "Unknown stream type %s" stream-type))))

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
    (user-comments-list-length u32r)
    (eval (when (> last emms-info-native--max-num-vorbis-comments)
            (error "Vorbis user comment list length %s is too long" last)))
    (user-comments repeat
                   (user-comments-list-length)
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
            (error "Vorbis comment length %s is too long" last)))
    (user-comment vec (length)))
  "Vorbis comment field specification.
Too long comment will trigger an error.

This field is used in Opus and FLAC comment structures as well.")

(defconst emms-info-native--vorbis-headers-bindat-spec
  '((identification-header struct emms-info-native--vorbis-identification-header-bindat-spec)
    (comment-header struct emms-info-native--vorbis-comment-header-bindat-spec))
  "Specification for first two Vorbis header packets.
They are always an identification header followed by a comment
header.")

(defconst emms-info-native--accepted-vorbis-fields
  '("album"
    "albumartist"
    "albumartistsort"
    "albumsort"
    "artist"
    "artistsort"
    "composer"
    "composersort"
    "date"
    "discnumber"
    "genre"
    "label"
    "originaldate"
    "originalyear"
    "performer"
    "title"
    "titlesort"
    "tracknumber"
    "year")
  "Emms info fields that are extracted from Vorbis comments.")

(defun emms-info-native--extract-vorbis-comments (user-comments)
  "Return a decoded list of comments from USER-COMMENTS.
USER-COMMENTS should be a list of Vorbis comments according to
‘user-comments’ field in
‘emms-info-native--vorbis-comment-header-bindat-spec’,
‘emms-info-native--opus-comment-header-bindat-spec’ or
‘emms-info-native--flac-comment-block-bindat-spec’.

Return comments in a list of (FIELD . VALUE) cons cells.  Only
FIELDs that are listed in
‘emms-info-native--accepted-vorbis-fields’ are returned."
  (let (comments)
    (dolist (user-comment user-comments)
      (let* ((comment (cdr (assoc 'user-comment user-comment)))
             (pair (emms-info-native--split-vorbis-comment comment)))
        (push pair comments)))
    (seq-filter (lambda (elt)
                  (member (car elt)
                          emms-info-native--accepted-vorbis-fields))
                comments)))

(defun emms-info-native--split-vorbis-comment (comment)
  "Split Vorbis comment to a field-value pair.
Vorbis comments are of form ‘FIELD=VALUE’.  FIELD is a
case-insensitive field name with a restricted set of ASCII
characters.  VALUE is an arbitrary UTF-8 encoded octet stream.

Return a cons cell (FIELD . VALUE), where FIELD is converted to
lower case and VALUE is the decoded value."
  (let ((comment-string (decode-coding-string (mapconcat
                                               #'byte-to-string
                                               comment
                                               "")
                                              'utf-8)))
    (when (string-match "^\\(.+?\\)=\\(.+?\\)$" comment-string)
      (cons (downcase (match-string 1 comment-string))
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
    (user-comments-list-length u32r)
    (eval (when (> last emms-info-native--max-num-vorbis-comments)
            (error "Opus user comment list length %s is too long" last)))
    (user-comments repeat
                   (user-comments-list-length)
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

(defconst emms-info-native--flac-comment-block-bindat-spec
  '((vendor-length u32r)
    (eval (when (> last emms-info-native--max-vorbis-vendor-length)
            (error "FLAC vendor length %s is too long" last)))
    (vendor-string vec (vendor-length))
    (user-comments-list-length u32r)
    (eval (when (> last emms-info-native--max-num-vorbis-comments)
            (error "FLAC user comment list length %s is too long" last)))
    (user-comments repeat
                   (user-comments-list-length)
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
Return comments in a list of (FIELD . VALUE) cons cells.  Only
FIELDs that are listed in
‘emms-info-native--accepted-vorbis-fields’ are returned."
  (let* ((comment-block (bindat-unpack emms-info-native--flac-comment-block-bindat-spec
                                       (emms-info-native--flac-decode-comment-block filename)))
         (user-comments (bindat-get-field comment-block
                                          'user-comments)))
    (emms-info-native--extract-vorbis-comments user-comments)))

;;;; id3v2 (MP3) code

(defconst emms-info-native--id3v2-magic-array
  [#x49 #x44 #x33]
  "id3v2 header magic pattern ‘ID3’.")

(defconst emms-info-native--id3v2-header-bindat-spec
  '((file-identifier vec 3)
    (eval (unless (equal last emms-info-native--id3v2-magic-array)
            (error "id3v2 framing mismatch: expected ‘%s’, got ‘%s’"
                   emms-info-native--id3v2-magic-array
                   last)))
    (version u8)
    (revision u8)
    (flags bits 1)
    (size-bytes vec 4)
    (size eval (emms-info-native--checked-id3v2-size last)))
  "id3v2 header specification.")

(defconst emms-info-native--id3v2-frame-bindat-spec
  '((id str 4)
    (size-bytes vec 4)
    (size eval (emms-info-native--checked-id3v2-size last))
    (flags bits 2)
    (payload vec (size)))
  "id3v2 frame specification.")

(defconst emms-info-native--id3v2-frame-to-info
  '(("TP1"  . "artist")
    ("TPE1" . "artist")
    ("TCM"  . "composer")
    ("TCOM" . "composer")
    ("TIT2" . "title")
    ("TT2"  . "title")
    ("TALB" . "album")
    ("TAL"  . "album")
    ("TRCK" . "tracknumber")
    ("TRK"  . "tracknumber")
    ("TPOS" . "discnumber")
    ("TPA"  . "discnumber")
    ("TYER" . "year")
    ("TYE"  . "year")
    ("TORY" . "originalyear")
    ("TOR"  . "originalyear"))
  "Mapping from id3v2 frame identifiers to info fields.")

(defconst emms-info-native--id3v2-text-encodings
  '((0 . latin-1)
    (1 . utf-16)
    (2 . uft-16be)
    (3 . utf-8))
  "id3v2 text encodings.")

(defun emms-info-native--checked-id3v2-size (bytes)
  "Calculate id3v2 element size from BYTES and check its validity.
Return the size.  Signal an error if the size exceeds
‘emms-info-native--max-peek-size’."
  (let ((size (emms-info-native--decode-id3v2-size bytes)))
    (when (or (= size 0)
              (> size emms-info-native--max-peek-size))
      (error "id3v2 tag/header/frame size %s is invalid" bytes))
    size))

(defun emms-info-native--decode-id3v2-size (bytes)
  "Decode id3v2 element size from BYTES.
BYTES are interpreted as 7-bit bytes, MSB first.  Return the
size."
  (apply '+ (seq-map-indexed (lambda (elt idx)
                               (* (expt 2 (* 7 idx)) elt))
                             (reverse bytes))))

(defun emms-info-native--decode-id3v2 (filename)
  "Read and decode id3v2 metadata from FILENAME.
Return metadata in a list of (FIELD . VALUE) cons cells.  See
‘emms-info-native--decode-id3v2-text-frame’ for details."
  (let* ((header (emms-info-native--decode-id3v2-header filename))
         (tag-size (bindat-get-field header 'size))
         (offset 10))
    (when (> tag-size emms-info-native--max-peek-size)
      (error "id3v2 tag size %s is too large" size))
    (when (memq 7 (bindat-get-field header 'flags))
      (error "id3v2 unsynchronisation scheme is not supported"))
    (when (memq 6 (bindat-get-field header 'flags))
      ;; Skip the extended header.
      (cl-incf offset
               (emms-info-native--decode-id3v2-ext-header-size filename)))
    (emms-info-native--decode-id3v2-frames filename
                                           offset
                                           (+ tag-size 10))))

(defun emms-info-native--decode-id3v2-header (filename)
  "Read and decode id3v2 header from FILENAME."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename nil 0 10)
    (bindat-unpack emms-info-native--id3v2-header-bindat-spec
                   (buffer-string))))

(defun emms-info-native--decode-id3v2-ext-header-size (filename)
  "Read and decode id3v2 extended header size from FILENAME.
Return the size.  Signal an error if the size exceeds
‘emms-info-native--max-peek-size’."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename nil 10 14)
    (emms-info-native--checked-id3v2-size (buffer-string))))

(defun emms-info-native--decode-id3v2-frames (filename begin end)
  "Read and decode id3v2 text frames from FILENAME.
BEGIN should be the offset of first byte after id3v2 header and
extended header (if any), and END should be the offset after the
complete id3v2 tag.

Return metadata in a list of (FIELD . VALUE) cons cells.  See
‘emms-info-native--decode-id3v2-text-frame’ for details."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename nil begin end)
    (let (comments
          (offset 0))
      (condition-case nil
          (while (< offset end)
            (let* ((frame (bindat-unpack emms-info-native--id3v2-frame-bindat-spec
                                         (buffer-string)
                                         offset))
                   (comment (emms-info-native--decode-id3v2-text-frame
                             frame)))
              (when comment (push comment comments))
              (cl-incf offset (+ (bindat-get-field frame 'size)
                                 10))))
        (error nil))
      comments)))

(defun emms-info-native--decode-id3v2-text-frame (frame)
  "Identify and decode id3v2 text frame FRAME.
If FRAME’s identifier matches a key in
‘emms-info-native--id3v2-frame-to-info’, return a cons cell
(FIELD . VALUE), where FIELD is the corresponding info field
identifier and VALUE is the decoded text.  Otherwise return nil."
  (let ((info-id (emms-info-native--id3v2-frame-info-id frame))
        (payload (bindat-get-field frame 'payload)))
    (when info-id
      (cons info-id
            (emms-info-native--decode-id3v2-string payload)))))

(defun emms-info-native--id3v2-frame-info-id (frame)
  "Return the emms-info identifier for FRAME.
If there is no such identifier, return nil."
  (cdr (assoc (bindat-get-field frame 'id)
              emms-info-native--id3v2-frame-to-info)))

(defun emms-info-native--decode-id3v2-string (bytes)
  "Decode id3v2 text information.
Return the text in BYTES as string."
  (let ((encoding (emms-info-native--id3v2-text-encoding bytes))
        (string (mapconcat #'byte-to-string (seq-rest bytes) "")))
    ;; Discard the null terminator.
    (substring (decode-coding-string string encoding) 0 -1)))

(defun emms-info-native--id3v2-text-encoding (bytes)
  "Return the encoding for text information BYTES."
  (cdr (assoc (seq-first bytes)
              emms-info-native--id3v2-text-encodings)))

;;;; EMMS code

(defun emms-info-native (track)
  "Set info fields for TRACK.
Supports Ogg Vorbis/Opus, FLAC, and MP3 files.

Return t if TRACK was updated, nil otherwise."
  (let* ((filename (emms-track-name track))
         (info-fields (emms-info-native--decode-info-fields filename))
         update-flag)
    (dolist (field info-fields)
      (let ((name (intern (concat "info-" (car field))))
            (value (cdr field)))
        (setq update-flag (or update-flag name))
        (emms-track-set track
                        name
                        (if (eq name 'info-playing-time)
                            (string-to-number value)
                          value))))
    update-flag))

(defun emms-info-native--decode-info-fields (filename)
  "Decode info fields from FILENAME.
Return a list of (FIELD . VALUE) cons cells, where FIELD is an
info field and VALUE is the corresponding info value.  Both are
strings."
  (let ((stream-type (emms-info-native--find-stream-type filename)))
    (cond ((or (eq stream-type 'vorbis) (eq stream-type 'opus))
           (emms-info-native--decode-ogg-comments filename stream-type))
          ((eq stream-type 'flac)
           (emms-info-native--flac-decode-comments filename))
          ((eq stream-type 'mp3)
           (emms-info-native--decode-id3v2 filename))
          (t nil))))

(defun emms-info-native--find-stream-type (filename)
  "Deduce the stream type from FILENAME.
This is a naive implementation that relies solely on filename
extension.

Return one of symbols ‘vorbis’, ‘opus’, ‘flac’, or ‘mp3’."
  (let ((case-fold-search t))
    (cond ((string-match ".ogg$" filename) 'vorbis)
          ((string-match ".opus$" filename) 'opus)
          ((string-match ".flac$" filename) 'flac)
          ((string-match ".mp3$" filename) 'mp3)
          (t nil))))

(provide 'emms-info-native)

;;; emms-info-native.el ends here
