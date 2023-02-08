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
;; relying on external tools or libraries like `emms-info-ogginfo' or
;; `emms-info-libtag'.
;;
;; To use this method, add `emms-info-native' to
;; `emms-info-functions'.
;;
;; The following file formats are supported:
;;
;; - Vorbis: Ogg Vorbis I Profile, filename extension `.ogg',
;;   elementary streams only.  Based on xiph.org's Vorbis I
;;   specification, see URL
;;   `https://xiph.org/vorbis/doc/Vorbis_I_spec.html'.
;;
;; - Opus: Ogg Opus profile, filename extension `.opus', elementary
;;   streams only.  Based on RFC 7845, see URL
;;   `https://tools.ietf.org/html/rfc7845.html'.
;;
;; - FLAC streams in native encapsulation format, filename extension
;;   `.flac'.  Based on xiph.org's FLAC format specification, see URL
;;   `https://xiph.org/flac/format.html'.
;;
;; - MP3 files with extension `.mp3' and id3v2 tags.  All id3v2
;;   versions should work, but many features like CRC, compression and
;;   encryption are not supported.  Based on id3v2 Informal Standards,
;;   see URL `https://id3.org'.
;;
;; - SPC files with extension `.spc' and id666 tags.  This is an audio
;;   file based on a memory dump from an SPC700, a special audio chip
;;   found within Super Nintendos.
;;
;; Format detection is based solely on filename extension, which is
;; matched case-insensitively.

;;; Code:

(require 'bindat)
(require 'cl-lib)
(require 'emms-info)
(require 'emms-info-spc)
(require 'seq)
(require 'subr-x)

(defconst emms-info-native--max-peek-size (* 2048 1024)
  "Maximum buffer size for metadata decoding.
Functions called by `emms-info-native' read certain amounts of
data into a temporary buffer while decoding metadata.  This
variable controls the maximum size of that buffer: if more than
`emms-info-native--max-peek-size' bytes are needed, an error is
signaled.

Technically metadata blocks can have almost arbitrary lengths,
but in practice processing must be constrained to prevent memory
exhaustion in case of garbled or malicious inputs.")

(defvar emms-info-native--opus-channel-count 0
  "Last decoded Opus channel count.
This is a kludge; it is needed because bindat spec cannot refer
outside itself.")

(defvar emms-info-native--id3v2-version 0
  "Last decoded id3v2 version.
This is a kludge; it is needed because bindat spec cannot refer
outside itself.")


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
  "EMMS info fields that are extracted from Vorbis comments.")

(defconst emms-info-native--vorbis-headers-bindat-spec
  '((identification-header struct emms-info-native--vorbis-identification-header-bindat-spec)
    (comment-header struct emms-info-native--vorbis-comment-header-bindat-spec))
  "Specification for first two Vorbis header packets.
They are always an identification header followed by a comment
header.")

(defconst emms-info-native--vorbis-identification-header-bindat-spec
  '((packet-type u8)
    (eval (unless (= last 1)
            (error "Vorbis header type mismatch: expected 1, got %s"
                   last)))
    (vorbis vec 6)
    (eval (unless (equal last emms-info-native--vorbis-magic-pattern)
            (error "Vorbis framing mismatch: expected `%s', got `%s'"
                   emms-info-native--vorbis-magic-pattern
                   last)))
    (vorbis-version u32r)
    (eval (unless (= last 0)
            (error "Vorbis version mismatch: expected 0, got %s"
                   last)))
    (channel-count u8)
    (sample-rate u32r)
    (bitrate-maximum u32r)
    (bitrate-nominal u32r)
    (bitrate-minimum u32r)
    (blocksize u8)
    (framing-flag u8)
    (eval (unless (= last 1))
          (error "Vorbis framing bit mismatch: expected 1, got %s"
                 last)))
  "Vorbis identification header specification.")

(defconst emms-info-native--vorbis-magic-pattern
  (string-to-vector "vorbis")
  "Header packet magic pattern.")

(defconst emms-info-native--vorbis-comment-header-bindat-spec
  '((packet-type u8)
    (eval (unless (= last 3)
            (error "Vorbis header type mismatch: expected 3, got %s"
                   last)))
    (vorbis vec 6)
    (eval (unless (equal last emms-info-native--vorbis-magic-pattern)
            (error "Vorbis framing mismatch: expected `%s', got `%s'"
                   emms-info-native--vorbis-magic-pattern
                   last)))
    (vendor-length u32r)
    (eval (when (> last emms-info-native--max-vorbis-vendor-length)
            (error "Vorbis vendor length %s is too long" last)))
    (vendor-string vec (vendor-length))
    (user-comments-list-length u32r)
    (eval (when (> last emms-info-native--max-num-vorbis-comments)
            (error "Vorbis user comment list length %s is too long"
                   last)))
    (user-comments repeat
                   (user-comments-list-length)
                   (struct emms-info-native--vorbis-comment-field-bindat-spec))
    (framing-bit u8)
    (eval (unless (= last 1))
          (error "Vorbis framing bit mismatch: expected 1, got %s"
                 last)))
  "Vorbis comment header specification.")

(defconst emms-info-native--vorbis-comment-field-bindat-spec
  '((length u32r)
    (eval (when (> last emms-info-native--max-vorbis-comment-size)
            (error "Vorbis comment length %s is too long" last)))
    (user-comment vec (length)))
  "Vorbis comment field specification.")

(defun emms-info-native--extract-vorbis-comments (user-comments)
  "Return a decoded list of comments from USER-COMMENTS.
USER-COMMENTS should be a list of Vorbis comments according to
`user-comments' field in
`emms-info-native--vorbis-comment-header-bindat-spec',
`emms-info-native--opus-comment-header-bindat-spec' or
`emms-info-native--flac-comment-block-bindat-spec'.

Return comments in a list of (FIELD . VALUE) cons cells.  Only
FIELDs that are listed in
`emms-info-native--accepted-vorbis-fields' are returned."
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
Vorbis comments are of form `FIELD=VALUE'.  FIELD is a
case-insensitive field name with a restricted set of ASCII
characters.  VALUE is an arbitrary UTF-8 encoded octet stream.

Return a cons cell (FIELD . VALUE), where FIELD is converted to
lower case and VALUE is the decoded value."
  (let ((comment-string (decode-coding-string (mapconcat
                                               #'byte-to-string
                                               comment
                                               nil)
                                              'utf-8)))
    (when (string-match "^\\(.+?\\)=\\(.+?\\)$" comment-string)
      (cons (downcase (match-string 1 comment-string))
            (match-string 2 comment-string)))))


;;;; Opus code

(defconst emms-info-native--opus-headers-bindat-spec
  '((identification-header struct emms-info-native--opus-identification-header-bindat-spec)
    (comment-header struct emms-info-native--opus-comment-header-bindat-spec))
  "Specification for two first Opus header packets.
They are always an identification header followed by a comment
header.")

(defconst emms-info-native--opus-identification-header-bindat-spec
  '((opus-head vec 8)
    (eval (unless (equal last emms-info-native--opus-head-magic-pattern)
            (error "Opus framing mismatch: expected `%s', got `%s'"
                   emms-info-native--opus-head-magic-pattern
                   last)))
    (opus-version u8)
    (eval (unless (< last 16)
            (error "Opus version mismatch: expected < 16, got %s"
                   last)))
    (channel-count u8)
    (eval (setq emms-info-native--opus-channel-count last))
    (pre-skip u16r)
    (sample-rate u32r)
    (output-gain u16r)
    (channel-mapping-family u8)
    (union (channel-mapping-family)
           (0 nil)
           (t (struct emms-info-native--opus-channel-mapping-table))))
  "Opus identification header specification.")

(defconst emms-info-native--opus-head-magic-pattern
  (string-to-vector "OpusHead")
  "Opus identification header magic pattern.")

(defconst emms-info-native--opus-channel-mapping-table
  '((stream-count u8)
    (coupled-count u8)
    (channel-mapping vec (eval emms-info-native--opus-channel-count)))
  "Opus channel mapping table specification.")

(defconst emms-info-native--opus-comment-header-bindat-spec
  '((opus-tags vec 8)
    (eval (unless (equal last emms-info-native--opus-tags-magic-pattern)
            (error "Opus framing mismatch: expected `%s', got `%s'"
                   emms-info-native--opus-tags-magic-pattern
                   last)))
    (vendor-length u32r)
    (eval (when (> last emms-info-native--max-vorbis-vendor-length)
            (error "Opus vendor length %s is too long" last)))
    (vendor-string vec (vendor-length))
    (user-comments-list-length u32r)
    (eval (when (> last emms-info-native--max-num-vorbis-comments)
            (error "Opus user comment list length %s is too long"
                   last)))
    (user-comments repeat
                   (user-comments-list-length)
                   (struct emms-info-native--vorbis-comment-field-bindat-spec)))
  "Opus comment header specification.")

(defconst emms-info-native--opus-tags-magic-pattern
  (string-to-vector "OpusTags")
  "Opus comment header magic pattern.")


;;;; Ogg code

(defconst emms-info-native--ogg-page-size 65307
  "Maximum size for a single Ogg container page.")

(defconst emms-info-native--ogg-page-bindat-spec
  '((capture-pattern vec 4)
    (eval (unless (equal last emms-info-native--ogg-magic-pattern)
            (error "Ogg framing mismatch: expected `%s', got `%s'"
                   emms-info-native--ogg-magic-pattern
                   last)))
    (stream-structure-version u8)
    (eval (unless (= last 0)
            (error ("Ogg version mismatch: expected 0, got %s")
                   last)))
    (header-type-flag u8)
    (granule-position vec 8)
    (stream-serial-number u32r)
    (page-sequence-no u32r)
    (page-checksum u32r)
    (page-segments u8)
    (segment-table vec (page-segments))
    (payload vec (eval (seq-reduce #'+ last 0))))
  "Ogg page structure specification.")

(defconst emms-info-native--ogg-magic-pattern
  (string-to-vector "OggS")
  "Ogg format magic capture pattern.")

(defconst emms-info-native--ogg-crc-table
  [#x00000000 #x04C11DB7 #x09823B6E #x0D4326D9 #x130476DC
   #x17C56B6B #x1A864DB2 #x1E475005 #x2608EDB8 #x22C9F00F
   #x2F8AD6D6 #x2B4BCB61 #x350C9B64 #x31CD86D3 #x3C8EA00A
   #x384FBDBD #x4C11DB70 #x48D0C6C7 #x4593E01E #x4152FDA9
   #x5F15ADAC #x5BD4B01B #x569796C2 #x52568B75 #x6A1936C8
   #x6ED82B7F #x639B0DA6 #x675A1011 #x791D4014 #x7DDC5DA3
   #x709F7B7A #x745E66CD #x9823B6E0 #x9CE2AB57 #x91A18D8E
   #x95609039 #x8B27C03C #x8FE6DD8B #x82A5FB52 #x8664E6E5
   #xBE2B5B58 #xBAEA46EF #xB7A96036 #xB3687D81 #xAD2F2D84
   #xA9EE3033 #xA4AD16EA #xA06C0B5D #xD4326D90 #xD0F37027
   #xDDB056FE #xD9714B49 #xC7361B4C #xC3F706FB #xCEB42022
   #xCA753D95 #xF23A8028 #xF6FB9D9F #xFBB8BB46 #xFF79A6F1
   #xE13EF6F4 #xE5FFEB43 #xE8BCCD9A #xEC7DD02D #x34867077
   #x30476DC0 #x3D044B19 #x39C556AE #x278206AB #x23431B1C
   #x2E003DC5 #x2AC12072 #x128E9DCF #x164F8078 #x1B0CA6A1
   #x1FCDBB16 #x018AEB13 #x054BF6A4 #x0808D07D #x0CC9CDCA
   #x7897AB07 #x7C56B6B0 #x71159069 #x75D48DDE #x6B93DDDB
   #x6F52C06C #x6211E6B5 #x66D0FB02 #x5E9F46BF #x5A5E5B08
   #x571D7DD1 #x53DC6066 #x4D9B3063 #x495A2DD4 #x44190B0D
   #x40D816BA #xACA5C697 #xA864DB20 #xA527FDF9 #xA1E6E04E
   #xBFA1B04B #xBB60ADFC #xB6238B25 #xB2E29692 #x8AAD2B2F
   #x8E6C3698 #x832F1041 #x87EE0DF6 #x99A95DF3 #x9D684044
   #x902B669D #x94EA7B2A #xE0B41DE7 #xE4750050 #xE9362689
   #xEDF73B3E #xF3B06B3B #xF771768C #xFA325055 #xFEF34DE2
   #xC6BCF05F #xC27DEDE8 #xCF3ECB31 #xCBFFD686 #xD5B88683
   #xD1799B34 #xDC3ABDED #xD8FBA05A #x690CE0EE #x6DCDFD59
   #x608EDB80 #x644FC637 #x7A089632 #x7EC98B85 #x738AAD5C
   #x774BB0EB #x4F040D56 #x4BC510E1 #x46863638 #x42472B8F
   #x5C007B8A #x58C1663D #x558240E4 #x51435D53 #x251D3B9E
   #x21DC2629 #x2C9F00F0 #x285E1D47 #x36194D42 #x32D850F5
   #x3F9B762C #x3B5A6B9B #x0315D626 #x07D4CB91 #x0A97ED48
   #x0E56F0FF #x1011A0FA #x14D0BD4D #x19939B94 #x1D528623
   #xF12F560E #xF5EE4BB9 #xF8AD6D60 #xFC6C70D7 #xE22B20D2
   #xE6EA3D65 #xEBA91BBC #xEF68060B #xD727BBB6 #xD3E6A601
   #xDEA580D8 #xDA649D6F #xC423CD6A #xC0E2D0DD #xCDA1F604
   #xC960EBB3 #xBD3E8D7E #xB9FF90C9 #xB4BCB610 #xB07DABA7
   #xAE3AFBA2 #xAAFBE615 #xA7B8C0CC #xA379DD7B #x9B3660C6
   #x9FF77D71 #x92B45BA8 #x9675461F #x8832161A #x8CF30BAD
   #x81B02D74 #x857130C3 #x5D8A9099 #x594B8D2E #x5408ABF7
   #x50C9B640 #x4E8EE645 #x4A4FFBF2 #x470CDD2B #x43CDC09C
   #x7B827D21 #x7F436096 #x7200464F #x76C15BF8 #x68860BFD
   #x6C47164A #x61043093 #x65C52D24 #x119B4BE9 #x155A565E
   #x18197087 #x1CD86D30 #x029F3D35 #x065E2082 #x0B1D065B
   #x0FDC1BEC #x3793A651 #x3352BBE6 #x3E119D3F #x3AD08088
   #x2497D08D #x2056CD3A #x2D15EBE3 #x29D4F654 #xC5A92679
   #xC1683BCE #xCC2B1D17 #xC8EA00A0 #xD6AD50A5 #xD26C4D12
   #xDF2F6BCB #xDBEE767C #xE3A1CBC1 #xE760D676 #xEA23F0AF
   #xEEE2ED18 #xF0A5BD1D #xF464A0AA #xF9278673 #xFDE69BC4
   #x89B8FD09 #x8D79E0BE #x803AC667 #x84FBDBD0 #x9ABC8BD5
   #x9E7D9662 #x933EB0BB #x97FFAD0C #xAFB010B1 #xAB710D06
   #xA6322BDF #xA2F33668 #xBCB4666D #xB8757BDA #xB5365D03
   #xB1F740B4]
  "Lookup table for calculating Ogg checksums.")

(defun emms-info-native--decode-ogg-metadata (filename stream-type)
  "Read and decode metadata from Ogg file FILENAME.
The file is assumed to contain a single stream of type
STREAM-TYPE, which must either `vorbis' or `opus'.

Return comments in a list of (FIELD . VALUE) cons cells.  See
`emms-info-native--split-vorbis-comment' for details."
  (let* ((packets (emms-info-native--decode-ogg-packets filename 2))
         (headers (emms-info-native--decode-ogg-headers packets
                                                        stream-type))
         (rate (bindat-get-field headers 'identification-header 'sample-rate))
         (commdata (bindat-get-field headers 'comment-header 'user-comments))
         (lastpage (emms-info-native--read-and-decode-last-ogg-page filename))
         (granpos (cdr (assoc 'granule-position lastpage)))
         (playtime (emms-info-native--decode-ogg-granule-position granpos rate))
         (comments (emms-info-native--extract-vorbis-comments commdata)))
    (nconc comments (when playtime (list (cons "playing-time" playtime))))))

(defun emms-info-native--decode-ogg-packets (filename packets)
  "Read and decode packets from Ogg file FILENAME.
Read in data from the start of FILENAME, remove Ogg packet
frames, and concatenate payloads until at least PACKETS number of
packets have been decoded.  Return the decoded packets in a
vector, concatenated.

Data is read in `emms-info-native--ogg-page-size' chunks.  If the
total length of concatenated packets becomes greater than
`emms-info-native--max-peek-size', an error is signaled.

Only elementary streams are supported, that is, FILENAME should
contain only a single logical stream.  Note that this assumption
is not verified: with non-elementary streams packets from
different streams will be mixed together without an error."
  (let ((num-packets 0)
        (offset 0)
        (stream (vector)))
    (while (< num-packets packets)
      (let ((page (emms-info-native--read-and-decode-ogg-page filename
                                                              offset)))
        (cl-incf num-packets (or (plist-get page :num-packets) 0))
        (cl-incf offset (plist-get page :num-bytes))
        (setq stream (vconcat stream (plist-get page :stream)))
        (when (> (length stream) emms-info-native--max-peek-size)
          (error "Ogg payload is too large"))))
    stream))

(defun emms-info-native--read-and-decode-ogg-page (filename offset)
  "Read and decode a single Ogg page from FILENAME.
Starting reading data from byte offset OFFSET.

Return the plist from `emms-info-native--decode-ogg-page'."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename
                                    nil
                                    offset
                                    (+ offset
                                       emms-info-native--ogg-page-size)) ;FIXME: may go over file size
    (emms-info-native--decode-ogg-page (buffer-string))))

(defun emms-info-native--decode-ogg-page (bytes)
  "Decode a single Ogg page from a sequence of BYTES.
Return a plist (:num-packets N :num-bytes B :stream S), where N
is the number of packets in the page, B is the size of the page
in bytes, and S is the unframed logical bitstream in a vector.
Note that N can be zero."
  (let* ((page (bindat-unpack emms-info-native--ogg-page-bindat-spec bytes))
         (num-packets (emms-info-native--num-of-packets page))
         (num-bytes (bindat-length emms-info-native--ogg-page-bindat-spec page))
         (stream (bindat-get-field page 'payload)))
      (list :num-packets num-packets
            :num-bytes num-bytes
            :stream stream)))

(defun emms-info-native--num-of-packets (page)
  "Return the number of packets in Ogg page PAGE.
PAGE must correspond to
`emms-info-native--ogg-page-bindat-spec'."
  ;; Every element that is less than 255 in the segment table
  ;; represents a packet boundary.
  (length (seq-filter (lambda (elt) (< elt 255))
                      (bindat-get-field page 'segment-table))))

(defun emms-info-native--decode-ogg-headers (packets stream-type)
  "Decode first two stream headers from PACKETS for STREAM-TYPE.
STREAM-TYPE must be either `vorbis' or `opus'.

Return a structure that corresponds to either
`emms-info-native--opus-headers-bindat-spec' or
`emms-info-native--vorbis-headers-bindat-spec'."
  (cond ((eq stream-type 'vorbis)
         (bindat-unpack emms-info-native--vorbis-headers-bindat-spec
                        packets))
        ((eq stream-type 'opus)
         (let (emms-info-native--opus-channel-count)
           (bindat-unpack emms-info-native--opus-headers-bindat-spec
                          packets)))
        (t (error "Unknown stream type %s" stream-type))))

(defun emms-info-native--read-and-decode-last-ogg-page (filename)
  "Read and decode the last Ogg page from FILENAME.
Return the page in bindat type structure."
  (with-temp-buffer
    (let* ((length (file-attribute-size (file-attributes filename)))
           (begin (max 0 (- length emms-info-native--ogg-page-size))))
      (set-buffer-multibyte nil)
      (insert-file-contents-literally filename nil begin length)
      (emms-info-native--decode-last-ogg-page))))

(defun emms-info-native--decode-last-ogg-page ()
  "Find and return the last valid Ogg page from the current buffer.
Ensure page synchronization by verifying Ogg page checksum.
Return the page in bindat type structure.  If there is no valid
Ogg page in the buffer, return nil."
  (let (page)
    (goto-char (point-max))
    (while (and (not page)
                (search-backward (concat emms-info-native--ogg-magic-pattern) nil t))
      (setq page (condition-case nil
                     (emms-info-native--verify-ogg-page)
                   (error nil))))
    (when (and page
               (> (logand (cdr (assoc 'header-type-flag page)) #x04) 0))
      page)))

(defun emms-info-native--verify-ogg-page ()
  "Verify Ogg page starting from point.
Unpack page into `emms-info-native--ogg-page-bindat-spec'
structure and calculate its checksum.  Return the page if the
checksum is correct, or nil if the checksum does not match."
  (let* ((offset (point))
         (page (bindat-unpack emms-info-native--ogg-page-bindat-spec
                              (buffer-string)
                              (1- offset)))
         (num-bytes (bindat-length emms-info-native--ogg-page-bindat-spec page))
         (buf (buffer-substring-no-properties offset (+ offset num-bytes)))
         (checksum (emms-info-native--calculate-ogg-checksum (concat (substring buf 0 22)
                                                             [0 0 0 0]
                                                             (substring buf 26)))))
    (when (= (cdr (assoc 'page-checksum page)) checksum) page)))

(defun emms-info-native--calculate-ogg-checksum (bytes)
  "Calculate and return Ogg checksum for BYTES.
See URL `https://xiph.org/vorbis/doc/framing.html' for details on
checksum."
  (let ((crc 0))
    (dotimes (n (length bytes))
      (setq crc (logxor (logand (ash crc 8) #xffffffff)
                        (aref emms-info-native--ogg-crc-table
                              (logxor (ash crc -24)
                                      (aref bytes n))))))
    crc))

(defun emms-info-native--decode-ogg-granule-position (vec rate)
  "Decode Ogg granule position vector VEC for sampling rate RATE.
Granule position is 64-bit little-endian signed integer counting
the number of PCM samples per channel.  If granule position is
-1, it was for a partial packet and hence invalid.  In that case
return nil."
  (let* ((int (emms-info-native--vector-to-integer vec))
         (pos (emms-info-native--unsigned-to-signed int 64)))
    (unless (= pos -1) (/ pos rate))))

(defun emms-info-native--vector-to-integer (vec)
  (apply '+ (seq-map-indexed (lambda (elt idx)
                               (* (expt 2 (* 8 idx)) elt))
                             vec)))

(defun emms-info-native--unsigned-to-signed (num bits)
  (let ((bitmask (1- (expt 2 bits)))
        (max (1- (expt 2 (1- bits)))))
    (if (> num max)
        (* -1 (1+ (logand (lognot num) bitmask)))
      num)))


;;;; FLAC code

(defconst emms-info-native--flac-metadata-block-header-bindat-spec
  '((flags u8)
    (length u24)
    (eval (when (or (> last emms-info-native--max-peek-size)
                    (= last 0))
            (error "FLAC block length %s is invalid" last))))
  "FLAC metadata block header specification.")

(defconst emms-info-native--flac-comment-block-bindat-spec
  '((vendor-length u32r)
    (eval (when (> last emms-info-native--max-vorbis-vendor-length)
            (error "FLAC vendor length %s is too long" last)))
    (vendor-string vec (vendor-length))
    (user-comments-list-length u32r)
    (eval (when (> last emms-info-native--max-num-vorbis-comments)
            (error "FLAC user comment list length %s is too long"
                   last)))
    (user-comments repeat
                   (user-comments-list-length)
                   (struct emms-info-native--vorbis-comment-field-bindat-spec)))
  "FLAC Vorbis comment block specification.")

(defun emms-info-native--decode-flac-comments (filename)
  "Read and decode comments from FLAC file FILENAME.
Return comments in a list of (FIELD . VALUE) cons cells.  Only
FIELDs that are listed in
`emms-info-native--accepted-vorbis-fields' are returned."
  (unless (emms-info-native--has-flac-signature filename)
    (error "Invalid FLAC stream"))
  (let* ((block (emms-info-native--decode-flac-comment-block (emms-info-native--file-inserter filename)))
         (unpacked (bindat-unpack emms-info-native--flac-comment-block-bindat-spec block))
         (user-comments (bindat-get-field unpacked 'user-comments)))
    (emms-info-native--extract-vorbis-comments user-comments)))

(defun emms-info-native--has-flac-signature (filename)
  "Check for FLAC stream marker at the beginning of FILENAME.
Return t if there is a valid stream marker, nil otherwise."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename nil 0 4)
    (looking-at "fLaC")))

(defun emms-info-native--file-inserter (filename)
  "Return a function that reads and inserts bytes from FILENAME.
This is meant for `emms-info-native--decode-flac-comment-block'."
  (lambda (offset end replace)
    (insert-file-contents-literally filename nil offset end replace)))

(defun emms-info-native--decode-flac-comment-block (read-func)
  "Read and decode a comment block from FLAC file FILENAME.
Return the comment block data in a vector."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let (comment-block
          last-flag
          (offset 4))
      (while (and (not comment-block) (not last-flag))
        (funcall read-func offset (cl-incf offset 4) t)
        (let* ((header (bindat-unpack emms-info-native--flac-metadata-block-header-bindat-spec
                                      (buffer-string)))
               (end (+ offset (bindat-get-field header 'length)))
               (flags (bindat-get-field header 'flags))
               (block-type (logand flags #x7F)))
          (setq last-flag (> (logand flags #x80) 0))
          (when (> block-type 6)
            (error "FLAC block type error: expected <= 6, got %s"
                   block-type))
          (when (= block-type 4)
            ;; Comment block found, extract it.
            (funcall read-func offset end t)
            (setq comment-block (vconcat (buffer-string))))
          (setq offset end)))
      comment-block)))


;;;; id3v2 (MP3) code

(defconst emms-info-native--id3v2-header-bindat-spec
  '((file-identifier vec 3)
    (eval (unless (equal last emms-info-native--id3v2-magic-pattern)
            (error "id3v2 framing mismatch: expected `%s', got `%s'"
                   emms-info-native--id3v2-magic-pattern
                   last)))
    (version u8)
    (eval (setq emms-info-native--id3v2-version last))
    (revision u8)
    (flags bits 1)
    (size-bytes vec 4)
    (size eval (emms-info-native--checked-id3v2-size 'tag last)))
  "id3v2 header specification.")

(defconst emms-info-native--id3v2-magic-pattern
  (string-to-vector "ID3")
  "id3v2 header magic pattern.")

(defconst emms-info-native--id3v2-frame-header-bindat-spec
  '((id str (eval (if (= emms-info-native--id3v2-version 2) 3 4)))
    (eval (unless (emms-info-native--valid-id3v2-frame-id-p last)
            (error "id3v2 frame id `%s' is invalid" last)))
    (size-bytes vec (eval (if (= emms-info-native--id3v2-version 2) 3 4)))
    (size eval (emms-info-native--checked-id3v2-size 'frame last))
    (flags bits (eval (if (= emms-info-native--id3v2-version 2) 0 2))))
  "id3v2 frame header specification.")

(defconst emms-info-native--id3v2-frame-to-info
  '(("TAL"  . "album")
    ("TALB" . "album")
    ("TPE2" . "albumartist")
    ("TSO2" . "albumartistsort")
    ("TSOA" . "albumsort")
    ("TP1"  . "artist")
    ("TPE1" . "artist")
    ("TSOP" . "artistsort")
    ("TCM"  . "composer")
    ("TCOM" . "composer")
    ("TSOC" . "composersort")
    ("TDRC" . "date")
    ("TPA"  . "discnumber")
    ("TPOS" . "discnumber")
    ("TCON" . genre)
    ("TPUB" . "label")
    ("TDOR" . "originaldate")
    ("TOR"  . "originalyear")
    ("TORY" . "originalyear")
    ("TIT2" . "title")
    ("TT2"  . "title")
    ("TSOT" . "titlesort")
    ("TRK"  . "tracknumber")
    ("TRCK" . "tracknumber")
    ("TYE"  . "year")
    ("TYER" . "year")
    ("TXXX" . user-defined))
  "Mapping from id3v2 frame identifiers to EMMS info fields.

Sources:

- URL `https://picard-docs.musicbrainz.org/en/appendices/tag_mapping.html'
- URL `http://wiki.hydrogenaud.io/index.php?title=Foobar2000:ID3_Tag_Mapping'")

(defconst emms-info-native--id3v1-genres
  '((0 . "Blues")
    (1 . "Classic Rock")
    (2 . "Country")
    (3 . "Dance")
    (4 . "Disco")
    (5 . "Funk")
    (6 . "Grunge")
    (7 . "Hip-Hop")
    (8 . "Jazz")
    (9 . "Metal")
    (10 . "New Age")
    (11 . "Oldies")
    (12 . "Other")
    (13 . "Pop")
    (14 . "R&B")
    (15 . "Rap")
    (16 . "Reggae")
    (17 . "Rock")
    (18 . "Techno")
    (19 . "Industrial")
    (20 . "Alternative")
    (21 . "Ska")
    (22 . "Death Metal")
    (23 . "Pranks")
    (24 . "Soundtrack")
    (25 . "Euro-Techno")
    (26 . "Ambient")
    (27 . "Trip-Hop")
    (28 . "Vocal")
    (29 . "Jazz+Funk")
    (30 . "Fusion")
    (31 . "Trance")
    (32 . "Classical")
    (33 . "Instrumental")
    (34 . "Acid")
    (35 . "House")
    (36 . "Game")
    (37 . "Sound Clip")
    (38 . "Gospel")
    (39 . "Noise")
    (40 . "AlternRock")
    (41 . "Bass")
    (42 . "Soul")
    (43 . "Punk")
    (44 . "Space")
    (45 . "Meditative")
    (46 . "Instrumental Pop")
    (47 . "Instrumental Rock")
    (48 . "Ethnic")
    (49 . "Gothic")
    (50 . "Darkwave")
    (51 . "Techno-Industrial")
    (52 . "Electronic")
    (53 . "Pop-Folk")
    (54 . "Eurodance")
    (55 . "Dream")
    (56 . "Southern Rock")
    (57 . "Comedy")
    (58 . "Cult")
    (59 . "Gangsta")
    (60 . "Top 40")
    (61 . "Christian Rap")
    (62 . "Pop/Funk")
    (63 . "Jungle")
    (64 . "Native American")
    (65 . "Cabaret")
    (66 . "New Wave")
    (67 . "Psychadelic")
    (68 . "Rave")
    (69 . "Showtunes")
    (70 . "Trailer")
    (71 . "Lo-Fi")
    (72 . "Tribal")
    (73 . "Acid Punk")
    (74 . "Acid Jazz")
    (75 . "Polka")
    (76 . "Retro")
    (77 . "Musical")
    (78 . "Rock & Roll")
    (79 . "Hard Rock")
    (80 . "Folk")
    (81 . "Folk-Rock")
    (82 . "National Folk")
    (83 . "Swing")
    (84 . "Fast Fusion")
    (85 . "Bebob")
    (86 . "Latin")
    (87 . "Revival")
    (88 . "Celtic")
    (89 . "Bluegrass")
    (90 . "Avantgarde")
    (91 . "Gothic Rock")
    (92 . "Progressive Rock")
    (93 . "Psychedelic Rock")
    (94 . "Symphonic Rock")
    (95 . "Slow Rock")
    (96 . "Big Band")
    (97 . "Chorus")
    (98 . "Easy Listening")
    (99 . "Acoustic")
    (100 . "Humour")
    (101 . "Speech")
    (102 . "Chanson")
    (103 . "Opera")
    (104 . "Chamber Music")
    (105 . "Sonata")
    (106 . "Symphony")
    (107 . "Booty Bass")
    (108 . "Primus")
    (109 . "Porn Groove")
    (110 . "Satire")
    (111 . "Slow Jam")
    (112 . "Club")
    (113 . "Tango")
    (114 . "Samba")
    (115 . "Folklore")
    (116 . "Ballad")
    (117 . "Power Ballad")
    (118 . "Rhythmic Soul")
    (119 . "Freestyle")
    (120 . "Duet")
    (121 . "Punk Rock")
    (122 . "Drum Solo")
    (123 . "A cappella")
    (124 . "Euro-House")
    (125 . "Dance Hall"))
  "id3v1 genres.")

(defconst emms-info-native--id3v2-text-encodings
  '((0 . latin-1)
    (1 . utf-16)
    (2 . uft-16be)
    (3 . utf-8))
  "id3v2 text encodings.")

(defun emms-info-native--valid-id3v2-frame-id-p (id)
  "Return t if ID is a proper id3v2 frame identifier, nil otherwise."
  (if (= emms-info-native--id3v2-version 2)
      (string-match "[A-Z0-9]\\{3\\}" id)
    (string-match "[A-Z0-9]\\{4\\}" id)))

(defun emms-info-native--checked-id3v2-size (elt bytes)
  "Calculate id3v2 element ELT size from BYTES.
ELT must be either \\='tag or \\='frame.

Return the size.  Signal an error if the size is zero."
  (let ((size (cond ((eq elt 'tag)
                     (emms-info-native--decode-id3v2-size bytes t))
                    ((eq elt 'frame)
                     (if (= emms-info-native--id3v2-version 4)
                         (emms-info-native--decode-id3v2-size bytes t)
                       (emms-info-native--decode-id3v2-size bytes nil))))))
    (if (zerop size)
        (error "id3v2 tag/frame size is zero")
      size)))

(defun emms-info-native--decode-id3v2-size (bytes syncsafe)
  "Decode id3v2 element size from BYTES.
Depending on SYNCSAFE, BYTES are interpreted as 7- or 8-bit
bytes, MSB first.

Return the decoded size."
  (let ((num-bits (if syncsafe 7 8)))
    (apply '+ (seq-map-indexed (lambda (elt idx)
                                 (* (expt 2 (* num-bits idx)) elt))
                               (reverse bytes)))))

(defun emms-info-native--decode-id3v2 (filename)
  "Read and decode id3v2 metadata from FILENAME.
Return metadata in a list of (FIELD . VALUE) cons cells, or nil
in case of errors or if there were no known fields in FILENAME.

See `emms-info-native--id3v2-frame-to-info' for recognized
fields."
  (condition-case nil
      (let* (emms-info-native--id3v2-version
             (header (emms-info-native--decode-id3v2-header filename))
             (tag-size (bindat-get-field header 'size))
             (unsync (memq 7 (bindat-get-field header 'flags)))
             (offset 10))
        (when (memq 6 (bindat-get-field header 'flags))
          ;; Skip the extended header.
          (cl-incf offset
                   (emms-info-native--checked-id3v2-ext-header-size filename)))
        (emms-info-native--decode-id3v2-frames filename
                                               offset
                                               (+ tag-size 10)
                                               unsync))
    (error nil)))

(defun emms-info-native--decode-id3v2-header (filename)
  "Read and decode id3v2 header from FILENAME."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename nil 0 10)
    (bindat-unpack emms-info-native--id3v2-header-bindat-spec
                   (buffer-string))))

(defun emms-info-native--checked-id3v2-ext-header-size (filename)
  "Read and decode id3v2 extended header size from FILENAME.
Return the size.  Signal an error if the size is zero."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename nil 10 14)
    (emms-info-native--checked-id3v2-size 'frame (buffer-string))))

(defun emms-info-native--decode-id3v2-frames (filename begin end unsync)
  "Read and decode id3v2 text frames from FILENAME.
BEGIN should be the offset of first byte of the first frame, and
END should be the offset after the complete id3v2 tag.

If UNSYNC is t, the frames are assumed to have gone through
unsynchronization and decoded as such.

Return metadata in a list of (FIELD . VALUE) cons cells."
  (let ((offset begin)
        (limit (- end (emms-info-native--id3v2-frame-header-size)))
        comments)
    (condition-case nil
        (while (< offset limit)
          (let* ((frame-data (emms-info-native--decode-id3v2-frame filename
                                                                   offset
                                                                   unsync))
                 (next-frame-offset (car frame-data))
                 (comment (cdr frame-data)))
            (when comment (push comment comments))
            (setq offset next-frame-offset)))
      (error nil))
    comments))

(defun emms-info-native--id3v2-frame-header-size ()
  "Return the last decoded header size in bytes."
  (if (= emms-info-native--id3v2-version 2) 6 10))

(defun emms-info-native--decode-id3v2-frame (filename offset unsync)
  (let* ((header (emms-info-native--decode-id3v2-frame-header filename
                                                              offset))
         (info-id (emms-info-native--id3v2-frame-info-id header))
         (data-offset (car header))
         (size (bindat-get-field (cdr header) 'size)))
    (if (or info-id unsync)
        ;; Note that if unsync is t, we have to always read the frame
        ;; to determine next-frame-offset.
        (let* ((data (emms-info-native--read-id3v2-frame-data filename
                                                              data-offset
                                                              size
                                                              unsync))
               (next-frame-offset (car data))
               (value (emms-info-native--decode-id3v2-frame-data (cdr data)
                                                                 info-id)))
          (cons next-frame-offset value))
      ;; Skip the frame.
      (cons (+ data-offset size) nil))))

(defun emms-info-native--decode-id3v2-frame-header (filename begin)
  "Read and decode id3v2 frame header from FILENAME.
Start reading from offset BEGIN.

Return a cons cell (OFFSET . FRAME), where OFFSET is the byte
offset after the frame header, and FRAME is the decoded frame."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((end (+ begin (emms-info-native--id3v2-frame-header-size))))
      (insert-file-contents-literally filename nil begin end)
      (cons end (bindat-unpack emms-info-native--id3v2-frame-header-bindat-spec
                               (buffer-string))))))

(defun emms-info-native--id3v2-frame-info-id (frame)
  "Return the emms-info identifier for FRAME.
If there is no such identifier, return nil."
  (cdr (assoc (bindat-get-field frame 'id)
              emms-info-native--id3v2-frame-to-info)))

(defun emms-info-native--read-id3v2-frame-data (filename
                                                begin
                                                num-bytes
                                                unsync)
  "Read NUM-BYTES of raw id3v2 frame data from FILENAME.
Start reading from offset BEGIN.  If UNSYNC is t, all 'FF 00'
byte combinations are replaced by 'FF'.  Replaced byte pairs are
counted as one, instead of two, towards NUM-BYTES.

Return a cons cell (OFFSET . DATA), where OFFSET is the byte
offset after NUM-BYTES bytes have been read, and DATA is the raw
data."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (if unsync
        ;; Reverse unsynchronization.
        (let ((peek-end (+ begin (* 2 num-bytes)))
              (end num-bytes))
          (insert-file-contents-literally filename nil begin peek-end)
          (goto-char (point-min))
          (while (and (re-search-forward (string 255 0) nil t)
                      (< (point) end))
            (replace-match (string 255))
            (cl-incf end 1))
          (delete-region (1+ num-bytes) (point-max))
          (cons (+ begin end) (buffer-string)))
      ;; No unsynchronization: read the data as-is.
      (let ((end (+ begin num-bytes)))
        (insert-file-contents-literally filename nil begin end)
        (cons end (buffer-string))))))

(defun emms-info-native--decode-id3v2-frame-data (data info-id)
  "Decode id3v2 text frame data DATA.
If INFO-ID is `user-defined', assume that DATA is a TXXX frame
with key/value-pair.  Extract the key and, if it is a mapped
element in `emms-info-native--id3v2-frame-to-info', use it as
INFO-ID.

If INFO-ID is `genre', assume that DATA is either an integral
id3v1 genre reference or a plain genre string.  In the former
case map the reference to a string via
`emms-info-native--id3v1-genres'; in the latter case use the
genre string verbatim.

Return a cons cell (INFO-ID . VALUE) where VALUE is the decoded
string."
  (when info-id
    (let ((str (emms-info-native--decode-id3v2-string data)))
      (cond ((stringp info-id) (cons info-id str))
            ((eq info-id 'genre)
             (if (string-match "^(?\\([0-9]+\\))?" str)
                 (let ((v1-genre (assoc (string-to-number (match-string 1 str))
                                        emms-info-native--id3v1-genres)))
                   (when v1-genre (cons "genre" (cdr v1-genre))))
               (cons "genre" str)))
            ((eq info-id 'user-defined)
             (let* ((key-val (split-string str (string 0)))
                    (key (downcase (car key-val)))
                    (val (cadr key-val)))
               (when (rassoc key emms-info-native--id3v2-frame-to-info)
                 (cons key val))))))))

(defun emms-info-native--decode-id3v2-string (bytes)
  "Decode id3v2 text information from BYTES.
Remove the terminating null byte, if any.

Return the text as string."
  (let* ((encoding (emms-info-native--id3v2-text-encoding bytes))
         (string (mapconcat #'byte-to-string (seq-rest bytes) ""))
         (decoded (decode-coding-string string encoding)))
    (when (> (length decoded) 0)
      (if (equal (substring decoded -1) "\0")
          (substring decoded 0 -1)
        decoded))))

(defun emms-info-native--id3v2-text-encoding (bytes)
  "Return the encoding for text information BYTES."
  (cdr (assoc (seq-first bytes)
              emms-info-native--id3v2-text-encodings)))


;;;; EMMS code

(defun emms-info-native (track)
  "Set info fields for TRACK.
Supports Ogg Vorbis/Opus, FLAC, and MP3 files."
  (condition-case env
      (let* ((filename (emms-track-name track))
             (info-fields (emms-info-native--decode-info-fields filename)))
	(dolist (field info-fields)
	  (let ((name (intern (concat "info-" (car field))))
		(value (cdr field)))
            (unless (zerop (length value))
              (emms-track-set track
                              name
                              (if (eq name 'info-playing-time)
				  (string-to-number value)
				(string-trim-right value)))))))
    (error (message "emms-info-native error processing %s: %s"
		    (emms-track-name track) env))))

(defun emms-info-native--decode-info-fields (filename)
  "Decode info fields from FILENAME.
Return a list of (FIELD . VALUE) cons cells, where FIELD is an
info field and VALUE is the corresponding info value.  Both are
strings."
  (let ((stream-type (emms-info-native--find-stream-type filename)))
    (cond ((or (eq stream-type 'vorbis) (eq stream-type 'opus))
           (emms-info-native--decode-ogg-metadata filename stream-type))
          ((eq stream-type 'flac)
           (emms-info-native--decode-flac-comments filename))
          ((eq stream-type 'mp3)
           (emms-info-native--decode-id3v2 filename))
	  ((eq stream-type 'spc)
	   (emms-info-spc--decode-id666 filename))
          (t nil))))

(defun emms-info-native--find-stream-type (filename)
  "Deduce the stream type from FILENAME.
This is a naive implementation that relies solely on filename
extension.

Return one of symbols `vorbis', `opus', `flac', or `mp3'."
  (let ((case-fold-search t))
    (cond ((string-match ".ogg$" filename) 'vorbis)
          ((string-match ".opus$" filename) 'opus)
          ((string-match ".flac$" filename) 'flac)
          ((string-match ".mp3$" filename) 'mp3)
	  ((string-match ".spc$" filename) 'spc)
          (t nil))))

(provide 'emms-info-native)

;;; emms-info-native.el ends here
