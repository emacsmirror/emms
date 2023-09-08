;;; emms-info-ogg-tests.el --- Test suite for emms-info-ogg  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

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

;;; Code:

(require 'emms-info-ogg)
(require 'ert)

(ert-deftest emms-ogg-test-decode-page ()
  (let* ((bytes (unibyte-string 79 103 103 83 0 2 0 0 0 0 0 0 0 0 134 209 158 23 0 0 0 0 53 82 251 136 1 30 1 118 111 114 98 105 115 0 0 0 0 1 68 172 0 0 0 0 0 0 128 56 1 0 0 0 0 0 184 1))
         (page (bindat-unpack emms-info-ogg--page-bindat-spec bytes)))
    (should (= (emms-info-ogg--num-packets page) 1))
    (should (= (bindat-length emms-info-ogg--page-bindat-spec page) 58))
    (should (equal (bindat-get-field page 'payload)
                   (unibyte-string 1 118 111 114 98 105 115 0 0 0 0 1 68 172 0 0 0 0 0 0 128 56 1 0 0 0 0 0 184 1)))))

(ert-deftest emms-ogg-test-decode-vorbis-headers ()
  "Test `emms-info-ogg--decode-headers' with Vorbis data."
  (let ((bytes (unibyte-string 1 118 111 114 98 105 115 0 0 0 0 1 68 172 0 0 0 0 0 0 128 56 1 0 0 0 0 0 184 1 3 118 111 114 98 105 115 52 0 0 0 88 105 112 104 46 79 114 103 32 108 105 98 86 111 114 98 105 115 32 73 32 50 48 50 48 48 55 48 52 32 40 82 101 100 117 99 105 110 103 32 69 110 118 105 114 111 110 109 101 110 116 41 2 0 0 0 7 0 0 0 102 111 111 61 98 97 114 27 0 0 0 75 101 121 61 206 159 225 189 144 207 135 225 189 182 32 206 164 206 177 225 189 144 207 132 225 189 176 1)))
    (should
     (emms-equal-lists
      (emms-info-ogg--decode-headers bytes 'vorbis)
      '((identification-header
         (packet-type . 1)
         (vorbis . "vorbis")
         (vorbis-version . 0)
         (channel-count . 1)
         (sample-rate . 44100)
         (bitrate-maximum . 0)
         (bitrate-nominal . 80000)
         (bitrate-minimum . 0)
         (blocksize . 184)
         (framing-flag . 1))
        (comment-header
         (packet-type . 3)
         (vorbis . "vorbis")
         (vendor-length . 52)
         (vendor-string . "Xiph.Org libVorbis I 20200704 (Reducing Environment)")
         (user-comments-list-length . 2)
         (user-comments
          ((length . 7)
           (user-comment . "foo=bar"))
          ((length . 27)
           (user-comment . "Key=\316\237\341\275\220\317\207\341\275\266 \316\244\316\261\341\275\220\317\204\341\275\260")))
         (framing-bit . 1)))))))

(ert-deftest emms-ogg-test-decode-opus-headers ()
  "Test `emms-info-ogg--decode-headers' with Opus data."
  (let ((bytes (unibyte-string 79 112 117 115 72 101 97 100 1 1 56 1 68 172 0 0 0 0 0 79 112 117 115 84 97 103 115 13 0 0 0 108 105 98 111 112 117 115 32 49 46 51 46 49 3 0 0 0 38 0 0 0 69 78 67 79 68 69 82 61 111 112 117 115 101 110 99 32 102 114 111 109 32 111 112 117 115 45 116 111 111 108 115 32 48 46 49 46 49 48 7 0 0 0 102 111 111 61 98 97 114 27 0 0 0 75 101 121 61 206 159 225 189 144 207 135 225 189 182 32 206 164 206 177 225 189 144 207 132 225 189 176)))
    (emms-equal-lists
     (emms-info-ogg--decode-headers bytes 'opus)
     '((identification-header
        (opus-head . "OpusHead")
        (opus-version . 1)
        (channel-count . 1)
        (pre-skip . 312)
        (sample-rate . 44100)
        (output-gain . 0)
        (channel-mapping-family . 0))
       (comment-header
        (opus-tags . "OpusTags")
        (vendor-length . 13)
        (vendor-string . "libopus 1.3.1")
        (user-comments-list-length . 3)
        (user-comments
         ((length . 38)
          (user-comment . "ENCODER=opusenc from opus-tools 0.1.10"))
         ((length . 7)
          (user-comment . "foo=bar"))
         ((length . 27)
          (user-comment . "Key=\316\237\341\275\220\317\207\341\275\266 \316\244\316\261\341\275\220\317\204\341\275\260"))))))))

(defun emms-ogg-test--decode-last-page (bytes)
  "Call `emms-info-ogg--decode-last-page' with BYTES input.

This is a helper function for `emms-ogg-test-decode-last-page'."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (concat bytes))
    (emms-info-ogg--decode-last-page)))

(ert-deftest emms-ogg-test-decode-last-page()
  (let ((valid [#x01 #x02 #x03 #x04 #x4f #x67 #x67 #x53 #x00 #x04 #x00 #x24 #x08 #x01 #x00 #x00 #x00 #x00 #x9c #x39 #x6e #x47 #x40 #x08 #x00 #x00 #x19 #x4e #xac #xa3 #x01 #x0a #x4f #x67 #x67 #x53 #x31 #x32 #x33 #x34 #x35 #x36])
        (notlast [#x01 #x02 #x03 #x04 #x4f #x67 #x67 #x53 #x00 #x00 #x00 #x24 #x08 #x01 #x00 #x00 #x00 #x00 #x9c #x39 #x6e #x47 #x40 #x08 #x00 #x00 #x19 #x4e #xac #xa3 #x01 #x0a #x4f #x67 #x67 #x53 #x31 #x32 #x33 #x34 #x35 #x36])
        (invalid [#x01 #x02 #x03 #x04 #x4f #x67 #x67 #x53 #x00 #x04 #x00 #x24 #x08 #x01 #x00 #x00 #x00 #x00 #x9c #x39 #x6e #x47 #x40 #x08 #x00 #x00 #x01 #x02 #x03 #x04 #x01 #x0a #x4f #x67 #x67 #x53 #x31 #x32 #x33 #x34 #x35 #x36])
        (valid-result
         (quote
          ((capture-pattern . "OggS")
           (stream-structure-version . 0)
           (header-type-flag . 4)
           (granule-position . 17310720)
           (stream-serial-number . 1198406044)
           (page-sequence-no . 2112)
           (page-checksum . 2745978393)
           (page-segments . 1)
           (segment-table . [10])
           (payload . "OggS123456")))))
    (unless emms--use-bindat-type
      (push (cons 'granule-position-bytes [0 36 8 1 0 0 0 0]) valid-result))
    (should (emms-equal-lists (emms-ogg-test--decode-last-page valid)
                              valid-result))
    (should (equal (emms-ogg-test--decode-last-page notlast) nil))
    (should (equal (emms-ogg-test--decode-last-page invalid) nil))))

(ert-deftest emms-ogg-test-calculate-checksum ()
  (let ((bytes [#x01 #x02 #x03 #x04 #x4f #x67 #x67 #x53 #x00 #x04 #x00 #x24 #x08 #x01 #x00 #x00 #x00 #x00 #x9c #x39 #x6e #x47 #x40 #x08 #x00 #x00 #x19 #x4e #xac #xa3 #x01 #x0a #x4f #x67 #x67 #x53 #x31 #x32 #x33 #x34 #x35 #x36]))
    (should (= (emms-info-ogg--checksum bytes) 445885580))))

(provide 'emms-info-ogg-tests)

;;; emms-info-ogg-tests.el ends here
