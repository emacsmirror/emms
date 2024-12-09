;;; emms-info-native-mp3.el --- EMMS info functions for MP3 files  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

;; This file contains functions for extracting metadata from MP3 files
;; with ID3v2 tags.  The code is based on ID3v2 Informal Standards,
;; see https://id3lib.sourceforge.net/id3/

;; All ID3v2 versions should be recognized, but many features like
;; CRC, compression and encryption are not supported.  Since MP3 does
;; not have a generally agreed-upon format for specifying the stream
;; length, the reported playing time is only an estimate.  For
;; constant bit rate streams the estimate is usually accurate, but for
;; variable bit rate streams it may be completely wrong, especially if
;; there are no Xing/VBRI headers embedded in the file.

;; For technical details on MP3 duration estimation, see
;; https://www.codeproject.com/Articles/8295/MPEG-Audio-Frame-Header.

;;; Code:

(require 'emms)
(require 'bindat)
(eval-when-compile (require 'subr-x))


;;; ID3 code

(defvar emms-info-native-id3v2--version 0
  "Last decoded ID3v2 version.")

(defconst emms-info-native-id3v2--magic-pattern "ID3"
  "ID3v2 header magic pattern.")

(defconst emms-info-native-id3v2--header-bindat-spec
  (if (eval-when-compile (fboundp 'bindat-type))
      (bindat-type
        (file-identifier str 3)
        (_ unit (unless (equal file-identifier emms-info-native-id3v2--magic-pattern)
                  (error "ID3v2 framing mismatch: expected `%s', got `%s'"
                         emms-info-native-id3v2--magic-pattern
                         file-identifier)))
        (version u8)
        (_ unit (progn (setq emms-info-native-id3v2--version version) nil))
        (revision u8)
        (flags bits 1)
        (size-bytes vec 4)
        (size unit (emms-info-native-id3v2--checked-size 'tag size-bytes)))
    '((file-identifier str 3)
      (eval (unless (equal last emms-info-native-id3v2--magic-pattern)
              (error "ID3v2 framing mismatch: expected `%s', got `%s'"
                     emms-info-native-id3v2--magic-pattern
                     last)))
      (version u8)
      (eval (setq emms-info-native-id3v2--version last))
      (revision u8)
      (flags bits 1)
      (size-bytes vec 4)
      (size eval (emms-info-native-id3v2--checked-size 'tag last))))
  "ID3v2 header specification.")

(defconst emms-info-native-id3v2--frame-header-bindat-spec
  (if (eval-when-compile (fboundp 'bindat-type))
      (bindat-type
        (id str (if (= emms-info-native-id3v2--version 2) 3 4))
        (_ unit (unless (emms-info-native-id3v2--valid-frame-id-p id)
                  (error "ID3v2 frame id `%s' is invalid" id)))
        (size-bytes vec (if (= emms-info-native-id3v2--version 2) 3 4))
        (size unit (emms-info-native-id3v2--checked-size 'frame size-bytes))
        (flags bits (if (= emms-info-native-id3v2--version 2) 0 2)))
    '((id str (eval (if (= emms-info-native-id3v2--version 2) 3 4)))
      (eval (unless (emms-info-native-id3v2--valid-frame-id-p last)
              (error "ID3v2 frame id `%s' is invalid" last)))
      (size-bytes vec (eval (if (= emms-info-native-id3v2--version 2) 3 4)))
      (size eval (emms-info-native-id3v2--checked-size 'frame last))
      (flags bits (eval (if (= emms-info-native-id3v2--version 2) 0 2)))))
  "ID3v2 frame header specification.")

(defconst emms-info-native-id3v2--frame-to-info
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
  "Mapping from ID3v2 frame identifiers to EMMS info fields.

Sources:

- URL `https://picard-docs.musicbrainz.org/en/appendices/tag_mapping.html'
- URL `http://wiki.hydrogenaud.io/index.php?title=Foobar2000:ID3_Tag_Mapping'")

(defconst emms-info-id3v1--genres
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
  "ID3v1 genres.")

(defconst emms-info-native-id3v2--text-encodings
  '((0 . latin-1)
    (1 . utf-16)
    (2 . uft-16be)
    (3 . utf-8))
  "ID3v2 text encodings.")

(defun emms-info-native-id3v2--valid-frame-id-p (id)
  "Return t if ID is a proper ID3v2 frame identifier, nil otherwise."
  (if (= emms-info-native-id3v2--version 2)
      (string-match "^[A-Z0-9]\\{3\\}$" id)
    (string-match "^[A-Z0-9]\\{4\\}$" id)))

(defun emms-info-native-id3v2--checked-size (elt bytes)
  "Calculate ID3v2 element ELT size from BYTES.
ELT must be either `tag' or `frame'.

Return the size.  Signal an error if the size is zero."
  (let ((size (cond ((eq elt 'tag)
                     (emms-info-native-id3v2--decode-size bytes t))
                    ((eq elt 'frame)
                     (if (= emms-info-native-id3v2--version 4)
                         (emms-info-native-id3v2--decode-size bytes t)
                       (emms-info-native-id3v2--decode-size bytes nil))))))
    (if (zerop size)
        (error "ID3v2 tag/frame size is zero")
      size)))

(defun emms-info-native-id3v2--decode-size (bytes syncsafe)
  "Decode ID3v2 element size from BYTES.
Depending on SYNCSAFE, BYTES are interpreted as 7- or 8-bit
bytes, MSB first.

Return the decoded size."
  (let ((num-bits (if syncsafe 7 8))
        (mask (if syncsafe #x7f #xff)))
    (apply '+ (seq-map-indexed (lambda (elt idx)
                                 (* (expt 2 (* num-bits idx))
                                    (logand elt mask)))
                               (reverse bytes)))))

(defun emms-info-native-id3v2--decode-header (filename)
  "Read and decode ID3v2 header from FILENAME."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename nil 0 10)
    (bindat-unpack emms-info-native-id3v2--header-bindat-spec
                   (buffer-string))))

(defun emms-info-native-id3v2--checked-ext-header-size (filename)
  "Read and decode ID3v2 extended header size from FILENAME.
Return the size.  Signal an error if the size is zero."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename nil 10 14)
    (emms-info-native-id3v2--checked-size 'frame (buffer-string))))

(defun emms-info-native-id3v2--decode-frames (filename begin end unsync)
  "Read and decode ID3v2 text frames from FILENAME.
BEGIN should be the offset of first byte of the first frame, and
END should be the offset after the complete ID3v2 tag.

If UNSYNC is non-nil, the frames are assumed to have gone through
unsynchronization and decoded as such.

Return metadata in a list of (FIELD . VALUE) cons cells."
  (let ((offset begin)
        (limit (- end (emms-info-native-id3v2--frame-header-size)))
        comments)
    (ignore-errors
      (while (< offset limit)
        (let* ((frame-data (emms-info-native-id3v2--decode-frame
                            filename offset unsync))
               (next-frame-offset (car frame-data))
               (comment (cdr frame-data)))
          (when comment (push comment comments))
          (setq offset next-frame-offset))))
    comments))

(defun emms-info-native-id3v2--frame-header-size ()
  "Return the last decoded header size in bytes."
  (if (= emms-info-native-id3v2--version 2) 6 10))

(defun emms-info-native-id3v2--decode-frame (filename offset unsync)
  "Read and decode a single ID3v2 frame from FILENAME.
Start reading the frame from byte offset OFFSET.  See
`emms-info-native-id3v2--read-frame-data' for details on UNSYNC.

Skip frames that do not map to any info-id in
`emms-info-native-id3v2--frame-to-info'.

Return cons cell (NEXT . FRAME), where NEXT is the offset of the
next frame (if any) and FRAME is the decoded frame.  See
`emms-info-native-id3v2--decode-frame-data'."
  (let* ((decoded (emms-info-native-id3v2--decode-frame-header
                   filename offset))
         (data-offset (car decoded))
         (header (cdr decoded))
         (frame-id (bindat-get-field header 'id))
         (info-id (cdr (assoc frame-id emms-info-native-id3v2--frame-to-info)))
         (size (bindat-get-field header 'size)))
    (if (or info-id unsync)
        ;; Note that if unsync is non-nil, we have to always read the
        ;; frame to determine next-frame-offset.
        (let* ((data (emms-info-native-id3v2--read-frame-data
                      filename data-offset size unsync))
               (next-frame-offset (car data))
               (value (emms-info-native-id3v2--decode-frame-data
                       (cdr data) info-id)))
          (cons next-frame-offset value))
      ;; Skip the frame.
      (cons (+ data-offset size) nil))))

(defun emms-info-native-id3v2--decode-frame-header (filename begin)
  "Read and decode ID3v2 frame header from FILENAME.
Start reading from byte offset BEGIN.

Return a cons cell (OFFSET . HEADER), where OFFSET is the byte
offset after the frame header, and HEADER is the decoded frame
header."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((end (+ begin (emms-info-native-id3v2--frame-header-size))))
      (insert-file-contents-literally filename nil begin end)
      (cons end
            (bindat-unpack emms-info-native-id3v2--frame-header-bindat-spec
                           (buffer-string))))))

(defun emms-info-native-id3v2--read-frame-data (filename begin num-bytes unsync)
  "Read NUM-BYTES of raw ID3v2 frame data from FILENAME.
Start reading from offset BEGIN.  If UNSYNC is non-nil, all \"FF
00\" byte combinations are replaced by \"FF\".  Replaced byte
pairs are counted as one, instead of two, towards NUM-BYTES.

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
            (setq end (1+ end)))
          (delete-region (1+ num-bytes) (point-max))
          (cons (+ begin end) (buffer-string)))
      ;; No unsynchronization: read the data as-is.
      (let ((end (+ begin num-bytes)))
        (insert-file-contents-literally filename nil begin end)
        (cons end (buffer-string))))))

(defun emms-info-native-id3v2--decode-frame-data (data info-id)
  "Decode ID3v2 text frame data DATA.
If INFO-ID is `user-defined', assume that DATA is a TXXX frame
with key/value-pair.  Extract the key and, if it is a mapped
element in `emms-info-native-id3v2--frame-to-info', use it as INFO-ID.

If INFO-ID is `genre', assume that DATA is either an integral
ID3v1 genre reference or a plain genre string.  In the former
case map the reference to a string via `emms-info-id3v1--genres';
in the latter case use the genre string verbatim.

Return a cons cell (INFO-ID . VALUE) where VALUE is the decoded
string, or nil if the decoding failed."
  (when info-id
    (let ((str (emms-info-native-id3v2--decode-string data)))
      (cond ((string-empty-p str) nil)
            ((stringp info-id) (cons info-id str))
            ((eq info-id 'genre)
             (if (string-match "^(?\\([0-9]+\\))?" str)
                 (let ((v1-genre
                        (assoc (string-to-number (match-string 1 str))
                               emms-info-id3v1--genres)))
                   (when v1-genre (cons "genre" (cdr v1-genre))))
               (cons "genre" str)))
            ((eq info-id 'user-defined)
             (let* ((key-val (split-string str (string 0)))
                    (key (downcase (car key-val)))
                    (val (cadr key-val)))
               (when (and (rassoc key emms-info-native-id3v2--frame-to-info)
                          (not (string-empty-p val)))
                 (cons key val))))))))

(defun emms-info-native-id3v2--decode-string (bytes)
  "Decode ID3v2 text information from BYTES.
Remove the terminating null byte, if any.

Return the text as string."
  (let* ((encoding (emms-info-native-id3v2--text-encoding bytes))
         (decoded (decode-coding-string (seq-rest bytes) encoding)))
    (if (and (> (length decoded) 0)
             (equal (substring decoded -1) "\0"))
        (substring decoded 0 -1)
      decoded)))

(defun emms-info-native-id3v2--text-encoding (bytes)
  "Return the encoding for text information BYTES."
  (alist-get (seq-first bytes)
             emms-info-native-id3v2--text-encodings))


;;; MP3 code

(defconst emms-info-native-mp3--versions
  '((0 . mpeg25)
    (1 . reserved)
    (2 . mpeg2)
    (3 . mpeg1))
  "MPEG versions.")

(defconst emms-info-native-mp3--layers
  '((0 . reserved)
    (1 . layerIII)
    (2 . layerII)
    (3 . layerI))
  "MPEG Audio Layers.")

(defconst emms-info-native-mp3--channel-modes
  '((0 . stereo)
    (1 . joint-stereo)
    (2 . dual-channel)
    (3 . single-channel))
  "MPEG channel modes.")

(defconst emms-info-native-mp3--bit-rates
  '((mpeg1-layerI        free  32  64  96  128  160  192  224  256  288  320  352  384  416  448  reserved)
    (mpeg1-layerII       free  32  48  56  64   80   96   112  128  160  192  224  256  320  384  reserved)
    (mpeg1-layerIII      free  32  40  48  56   64   80   96   112  128  160  192  224  256  320  reserved)
    (mpeg2x-layerI       free  32  48  56  64   80   96   112  128  144  160  176  192  224  256  reserved)
    (mpeg2x-layerII-III  free  8   16  24  32   40   48   56   64   80   96   112  128  144  160  reserved))
  "Bit rates for each MPEG version/layer combination.")

(defconst emms-info-native-mp3--samples-per-frame
  '((layerI          . 384)
    (layerII         . 1152)
    (layerIII-mpeg1  . 1152)
    (layerIII-mpeg2x . 576))
  "Samples per frame for each MPEG version/layer combination.")

(defconst emms-info-native-mp3--sample-rates
  '((mpeg1   44100  48000  32000  reserved)
    (mpeg2   22050  24000  16000  reserved)
    (mpeg25  11025  12000  8000   reserved))
  "Sample rate for each MPEG version/layer combination.")

(defconst emms-info-native-mp3--vbri-header-bindat-spec
  (if (eval-when-compile (fboundp 'bindat-type))
      (bindat-type
        (id str 4)
        (version uint 16)
        (delay uint 16)
        (quality uint 16)
        (bytes uint 32)
        (frames uint 32))
    '((id str 4)
      (version u16)
      (delay u16)
      (quality u16)
      (bytes u32)
      (frames u32)))
  "VBR header, VBRI format.
This specification is purposefully incomplete, as we are
interested only in the frame count.")

(defconst emms-info-native-mp3--xing-header-bindat-spec
    (if (eval-when-compile (fboundp 'bindat-type))
        (bindat-type
          (id vec 4)
          (flags bits 4)
          (frames uint 32))
      '((id vec 4)
        (flags bits 4)
        (frames u32)))
  "VBR header, Xing/Info format.
This specification is purposefully incomplete, as we are
interested only in the frame count.")

(defun emms-info-native-mp3-decode-metadata (filename)
  "Read and decode metadata from MP3 file FILENAME.
Return metadata in a list of (FIELD . VALUE) cons cells, or nil
in case of errors or if there were no known fields in FILENAME.
Also try to estimate the stream duration, and return it in
`playing-time' field if successful.

See `emms-info-native-id3v2--frame-to-info' for recognized fields."
  (let* (emms-info-native-id3v2--version
         (header (emms-info-native-id3v2--decode-header filename))
         (tag-size (bindat-get-field header 'size))
         (unsync (memq 7 (bindat-get-field header 'flags)))
         (offset 10))
    (when (memq 6 (bindat-get-field header 'flags))
      ;; Skip the extended header.
      (setq offset (+ offset
                      (emms-info-native-id3v2--checked-ext-header-size
                       filename))))
    (let ((tags
           (emms-info-native-id3v2--decode-frames
            filename offset (+ tag-size 10) unsync))
          (playtime
           (emms-info-native-mp3--decode-duration filename (+ tag-size 10))))
      (nconc tags (when playtime
                    (list (cons "playing-time" playtime)))))))

(defun emms-info-native-mp3--decode-duration (filename offset)
  "Decode or estimate stream duration for MP3 file FILENAME.
Start looking for necessary headers from byte offset OFFSET.

Return the duration in seconds, or nil in case of errors."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename nil offset (+ offset 1024))
    (let* ((header
            (emms-info-native-mp3--find-and-decode-frame-header))
           (samples-per-frame
            (alist-get 'samples-per-frame header))
           (sample-rate
            (alist-get 'sample-rate header))
           (bit-rate
            (alist-get 'bit-rate header))
           (frames
            (or (emms-info-native-mp3--find-and-decode-xing-header)
                (emms-info-native-mp3--find-and-decode-vbri-header))))
      (cond ((and frames samples-per-frame (numberp sample-rate))
             ;; The file has a usable VBR (Xing, Info or VBRI) header.
             (/ (* frames samples-per-frame) sample-rate))
            (bit-rate
             ;; The file does not have a usable VBR header, therefore
             ;; estimate the duration.
             (emms-info-native-mp3--estimate-duration filename bit-rate))))))

(defun emms-info-native-mp3--find-and-decode-frame-header ()
  "Find and decode MP3 audio frame header from the current buffer.
Return the decoded header in an alist, or nil if a header cannot
be found or decoded.

See `emms-info-native-mp3--decode-frame-header' for details."
  (let (header)
    (goto-char (point-min))
    (ignore-errors
      (while (and (not header)
                  (search-forward (string 255) nil t))
        (let ((bytes
               (emms-be-to-int
                (buffer-substring-no-properties (- (point) 1)
                                                (+ (point) 3)))))
          (setq header
                (emms-info-native-mp3--decode-frame-header bytes)))))
    header))

(defun emms-info-native-mp3--decode-frame-header (header)
  "Decode 32-bit numeric HEADER data.
Pack its elements to an alist and return the list.  Return nil if
HEADER does not have MP3 sync bits set."
  (when (= (logand header #xffe00000) #xffe00000)
    (let* ((version-bits
            (emms-extract-bits header 19 20))
           (layer-bits
            (emms-extract-bits header 17 18))
           (crc-bit
            (emms-extract-bits header 16))
           (bit-rate-bits
            (emms-extract-bits header 12 15))
           (sample-rate-bits
            (emms-extract-bits header 10 11))
           (padding-bit
            (emms-extract-bits header 9))
           (private-bit
            (emms-extract-bits header 8))
           (channel-mode-bits
            (emms-extract-bits header 6 7))
           (mode-extension-bits
            (emms-extract-bits header 4 5))
           (copyright-bit
            (emms-extract-bits header 3))
           (original-bit
            (emms-extract-bits header 2))
           (emphasis-bits
            (emms-extract-bits header 0 1))
           (version
            (alist-get version-bits
                       emms-info-native-mp3--versions))
           (layer
            (alist-get layer-bits
                       emms-info-native-mp3--layers))
           (channel-mode
            (alist-get channel-mode-bits
                       emms-info-native-mp3--channel-modes))
           (sample-rate
            (nth sample-rate-bits
                 (alist-get version
                            emms-info-native-mp3--sample-rates)))
           (samples-per-frame
            (emms-info-native-mp3--samples-per-frame
             version layer))
           (bit-rate
            (emms-info-native-mp3--decode-bit-rate
             version layer bit-rate-bits)))
      (list (cons 'version version)
            (cons 'layer layer)
            (cons 'crc crc-bit)
            (cons 'bit-rate bit-rate)
            (cons 'sample-rate sample-rate)
            (cons 'samples-per-frame samples-per-frame)
            (cons 'padding padding-bit)
            (cons 'private private-bit)
            (cons 'channel-mode channel-mode)
            (cons 'mode-extension mode-extension-bits)
            (cons 'copyright copyright-bit)
            (cons 'emphasis emphasis-bits)
            (cons 'original original-bit)))))

(defun emms-info-native-mp3--find-and-decode-xing-header ()
  "Find and decode Xing VBR header from the current buffer.
Return the number of frames in the stream, or nil if a header
cannot be found or decoded."
  (goto-char (point-min))
  (when (re-search-forward "Xing\\|Info" (point-max) t)
    (let ((header
           (bindat-unpack emms-info-native-mp3--xing-header-bindat-spec
                          (buffer-string)
                          (1- (match-beginning 0)))))
      (when (memq 0 (bindat-get-field header 'flags))
        (bindat-get-field header 'frames)))))

(defun emms-info-native-mp3--find-and-decode-vbri-header ()
  "Find and decode VBRI header from the current buffer.
Return the number of frames in the stream, or nil if a header
cannot be found or decoded."
  (goto-char (point-min))
  (when (re-search-forward "VBRI" (point-max) t)
    (let ((header
           (bindat-unpack emms-info-native-mp3--vbri-header-bindat-spec
                          (buffer-string)
                          (1- (match-beginning 0)))))
      (bindat-get-field header 'frames))))

(defun emms-info-native-mp3--estimate-duration (filename bitrate)
  "Estimate stream duration for FILENAME.
Assume constant encoding bit rate of BITRATE kilobits per second.
Return the estimated stream duration in seconds, or nil in case
of errors."
  (let ((size
         (file-attribute-size
          (file-attributes (file-chase-links filename)))))
    (when (and size (numberp bitrate))
      (/ (* 8 size) (* 1000 bitrate)))))

(defun emms-info-native-mp3--decode-bit-rate (version layer bits)
  "Return the bit rate for MPEG VERSION/LAYER combination.
BITS is the bitrate index from MP3 header."
  (cond ((eq version 'mpeg1)
         (cond ((eq layer 'layerI)
                (nth bits
                     (alist-get 'mpeg1-layerI
                                emms-info-native-mp3--bit-rates)))
               ((eq layer 'layerII)
                (nth bits
                     (alist-get 'mpeg1-layerII
                                emms-info-native-mp3--bit-rates)))
               ((eq layer 'layerIII)
                (nth bits
                     (alist-get 'mpeg1-layerIII
                                emms-info-native-mp3--bit-rates)))))
        (t (cond ((eq layer 'layerI)
                  (nth bits
                       (alist-get 'mpeg2x-layerI
                                  emms-info-native-mp3--bit-rates)))
                 (t (nth bits
                         (alist-get 'mpeg2x-layerII-III
                                    emms-info-native-mp3--bit-rates)))))))

(defun emms-info-native-mp3--samples-per-frame (version layer)
  "Return the samples per frame for MPEG VERSION/LAYER combination."
  (cond ((eq layer 'layerIII)
         (cond ((eq version 'mpeg1)
                (alist-get 'layerIII-mpeg1
                           emms-info-native-mp3--samples-per-frame))
               (t (alist-get 'layerIII-mpeg2x
                             emms-info-native-mp3--samples-per-frame))))
        ((eq layer 'layerII)
         (alist-get 'layerII
                    emms-info-native-mp3--samples-per-frame))
        ((eq layer 'layerI)
         (alist-get 'layerI
                    emms-info-native-mp3--samples-per-frame))))

(provide 'emms-info-native-mp3)

;;; emms-info-native-mp3.el ends here
