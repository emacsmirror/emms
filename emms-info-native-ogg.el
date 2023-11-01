;;; emms-info-native-ogg.el --- EMMS info functions for Ogg files  -*- lexical-binding: t; -*-

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

;; This file contains functions for extracting metadata from Ogg
;; files, specifically from encapsulated Vorbis and Opus streams.
;; Only elementary streams are supported.
;;
;; Ogg code is based on its programming documentation available at
;; https://xiph.org/ogg/doc/.
;;
;; Vorbis code is based on xiph.org's Vorbis I specification,
;; available at https://xiph.org/vorbis/doc/Vorbis_I_spec.html.  See
;; also emms-info-native-vorbis.el.
;;
;; Opus code is based on RFC 7845; see
;; https://tools.ietf.org/html/rfc7845.html and
;; emms-info-native-opus.el.

;;; Code:

(require 'emms-info-native-opus)
(require 'emms-info-native-vorbis)
(require 'bindat)

(defconst emms-info-native-ogg--page-size 65307
  "Maximum size for a single Ogg container page.")

(defconst emms-info-native-ogg--max-peek-size (* 16 1024 1024)
  "Maximum buffer size for metadata decoding.
Functions in `emms-info-native-ogg' read certain amounts of data
into a temporary buffer while decoding metadata.  This variable
controls the maximum size of that buffer: if more than
`emms-info-native-ogg--max-peek-size' bytes are needed, an error
is signaled.

Technically metadata blocks can have almost arbitrary lengths,
but in practice processing must be constrained to prevent memory
exhaustion in case of garbled or malicious inputs.")

(defconst emms-info-native-ogg--magic-pattern "OggS"
  "Ogg format magic capture pattern.")

(defconst emms-info-native-ogg--page-bindat-spec
  (if (eval-when-compile (fboundp 'bindat-type))
      (bindat-type
        (capture-pattern str 4)
        (_ unit (unless (equal capture-pattern emms-info-native-ogg--magic-pattern)
                  (error "Ogg framing mismatch: expected `%s', got `%s'"
                         emms-info-native-ogg--magic-pattern
                         capture-pattern)))
        (stream-structure-version u8)
        (_ unit (unless (= stream-structure-version 0)
                  (error "Ogg version mismatch: expected 0, got %d"
                         stream-structure-version)))
        (header-type-flag u8)
        (granule-position sint 64 'le)
        (stream-serial-number uintr 32)
        (page-sequence-no uintr 32)
        (page-checksum uintr 32)
        (page-segments u8)
        (segment-table vec page-segments)
        (payload str (seq-reduce #'+ segment-table 0)))
    ;; For Emacsen older than 28
    '((capture-pattern str 4)
      (eval (unless (equal last emms-info-native-ogg--magic-pattern)
              (error "Ogg framing mismatch: expected `%s', got `%s'"
                     emms-info-native-ogg--magic-pattern
                     last)))
      (stream-structure-version u8)
      (eval (unless (= last 0)
              (error "Ogg version mismatch: expected 0, got %s" last)))
      (header-type-flag u8)
      (granule-position-bytes vec 8)
      (granule-position eval (emms-from-twos-complement
                              (emms-le-to-int last) 64))
      (stream-serial-number u32r)
      (page-sequence-no u32r)
      (page-checksum u32r)
      (page-segments u8)
      (segment-table vec (page-segments))
      (payload str (eval (seq-reduce #'+ last 0)))))
  "Ogg page structure specification.")

(defconst emms-info-native-ogg--crc-table
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

(defun emms-info-native-ogg-decode-metadata (filename stream-type)
  "Read and decode metadata from Ogg file FILENAME.
The file is assumed to contain a single stream of type
STREAM-TYPE, which must either `vorbis' or `opus'.

Return comments in a list of (FIELD . VALUE) cons cells.
Additionally return stream duration in `playing-time' field.

See `emms-info-native-vorbis--split-comment' for details."
  (let* ((packets
          (emms-info-native-ogg--read-and-decode-packets filename 2))
         (headers
          (emms-info-native-ogg--decode-headers packets stream-type))
         (user-comments
          (bindat-get-field headers 'comment-header 'user-comments))
         (comments
          (emms-info-native-vorbis-extract-comments user-comments))
         (last-page
          (emms-info-native-ogg--read-and-decode-last-page filename))
         (granule-pos
          (alist-get 'granule-position last-page))
         (sample-rate
          (if (eq stream-type 'vorbis)
              (bindat-get-field headers
                                'identification-header
                                'sample-rate)
            ;; Opus assumes a fixed sample rate of 48 kHz for granule
            ;; position.
            48000))
         (playing-time
          (when (and granule-pos (> granule-pos 0))
            (/ granule-pos sample-rate))))
    (nconc comments
           (when playing-time
             (list (cons "playing-time" playing-time))))))

(defun emms-info-native-ogg--read-and-decode-packets (filename packets)
  "Read and decode PACKETS packets from Ogg file FILENAME.
Read in data from the start of FILENAME, remove Ogg packet
frames, and concatenate payloads until at least PACKETS number of
packets have been decoded.  Return the decoded packets in a
string, concatenated.

Read data in `emms-info-native-ogg--page-size' chunks.  If more
than `emms-info-native-ogg--max-peek-size' bytes of data would be
read, signal an error.

Only elementary streams are supported, that is, FILENAME should
contain only a single logical stream.  Note that this assumption
is not verified: with non-elementary streams packets from
different streams will be mixed together without an error."
  (let ((num-packets 0) (offset 0) (stream (list)))
    (while (< num-packets packets)
      (when (> offset emms-info-native-ogg--max-peek-size)
        (error "Ogg payload is too large"))
      (let ((page (emms-info-native-ogg--read-and-decode-page filename offset)))
        (setq num-packets (+ num-packets
                             (emms-info-native-ogg--num-packets page)))
        (setq offset (+ offset
                        (bindat-length
                         emms-info-native-ogg--page-bindat-spec page)))
        (push (bindat-get-field page 'payload) stream)))
    (reverse (mapconcat #'nreverse stream nil))))

(defun emms-info-native-ogg--read-and-decode-page (filename offset)
  "Read and decode a single Ogg page from FILENAME.
Starting reading data from byte offset OFFSET.

Return the plist from `emms-info-native-ogg--decode-page'."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally
     filename nil offset (+ offset emms-info-native-ogg--page-size))
    (bindat-unpack emms-info-native-ogg--page-bindat-spec
                   (buffer-string))))

(defun emms-info-native-ogg--num-packets (page)
  "Return the number of packets in Ogg page PAGE.
PAGE must correspond to `emms-info-native-ogg--page-bindat-spec'."
  ;; Every element that is less than 255 in the segment table
  ;; represents a packet boundary.
  (length (seq-filter (lambda (elt) (< elt 255))
                      (bindat-get-field page 'segment-table))))

(defun emms-info-native-ogg--decode-headers (packets stream-type)
  "Decode first two stream headers from PACKETS for STREAM-TYPE.
STREAM-TYPE must be either `vorbis' or `opus'.

Return a structure that corresponds to either
`emms-info-native-opus--headers-bindat-spec' or
`emms-info-native-vorbis--headers-bindat-spec'."
  (cond ((eq stream-type 'vorbis)
         (bindat-unpack emms-info-native-vorbis--headers-bindat-spec
                        packets))
        ((eq stream-type 'opus)
         (bindat-unpack emms-info-native-opus--headers-bindat-spec
                        packets))
        (t (error "Unknown stream type %s" stream-type))))

(defun emms-info-native-ogg--read-and-decode-last-page (filename)
  "Read and decode the last Ogg page from FILENAME.
Return the page in bindat type structure."
  (with-temp-buffer
    (let* ((length (file-attribute-size
                    (file-attributes
                     (file-truename filename))))
           (begin (max 0 (- length emms-info-native-ogg--page-size))))
      (set-buffer-multibyte nil)
      (insert-file-contents-literally filename nil begin length)
      (emms-info-native-ogg--decode-last-page))))

(defun emms-info-native-ogg--decode-last-page ()
  "Find and return the last valid Ogg page from the current buffer.
Ensure page synchronization by verifying page checksum.

Return the page in bindat type structure.  If there is no valid
Ogg page in the buffer, return nil."
  (let (page)
    (goto-char (point-max))
    (while (and (not page)
                (search-backward emms-info-native-ogg--magic-pattern nil t))
      (setq page (emms-info-native-ogg--verify-page)))
    (when (and page
               (> (logand (alist-get 'header-type-flag page) #x04) 0))
      page)))

(defun emms-info-native-ogg--verify-page ()
  "Verify Ogg page starting from point.
Unpack page into `emms-info-native-ogg--page-bindat-spec'
structure and calculate its checksum.  Return the page if the
checksum is correct, or nil if the checksum does not match or the
page is otherwise invalid."
  (ignore-errors
    (let* ((offset (point))
           (page
            (bindat-unpack emms-info-native-ogg--page-bindat-spec
                           (buffer-string)
                           (1- offset)))
           (num-bytes
            (bindat-length emms-info-native-ogg--page-bindat-spec page))
           (buf
            (buffer-substring-no-properties offset
                                            (+ offset num-bytes)))
           (checksum
            (emms-info-native-ogg--checksum (concat (substring buf 0 22)
                                             [0 0 0 0]
                                             (substring buf 26)))))
      (when (= (alist-get 'page-checksum page) checksum) page))))

(defun emms-info-native-ogg--checksum (bytes)
  "Calculate and return Ogg checksum for BYTES.
See URL `https://xiph.org/vorbis/doc/framing.html' for details on
checksum."
  (let ((crc 0))
    (dotimes (n (length bytes))
      (setq crc (logxor (logand (ash crc 8) #xffffffff)
                        (aref emms-info-native-ogg--crc-table
                              (logxor (ash crc -24)
                                      (aref bytes n))))))
    crc))

(provide 'emms-info-native-ogg)

;;; emms-info-native-ogg.el ends here
