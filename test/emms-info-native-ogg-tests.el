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

(require 'emms)
(require 'emms-info-native-ogg)
(require 'ert)

(ert-deftest emms-test-ogg-decode-page ()
  (let* ((bytes "\x4f\x67\x67\x53\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x86\xd1\x9e\x17\x00\x00\x00\x00\x35\x52\xfb\x88\x01\x1e\x01\x76\x6f\x72\x62\x69\x73\x00\x00\x00\x00\x01\x44\xac\x00\x00\x00\x00\x00\x00\x80\x38\x01\x00\x00\x00\x00\x00\xb8\x01")
         (page (bindat-unpack emms-info-native-ogg--page-bindat-spec bytes)))
    (should (= (emms-info-native-ogg--num-packets page) 1))
    (should (= (bindat-length emms-info-native-ogg--page-bindat-spec page) 58))
    (should (equal (bindat-get-field page 'payload)
                   "\x01\x76\x6f\x72\x62\x69\x73\x00\x00\x00\x00\x01\x44\xac\x00\x00\x00\x00\x00\x00\x80\x38\x01\x00\x00\x00\x00\x00\xb8\x01"))))

(ert-deftest emms-test-ogg-decode-vorbis-headers ()
  "Test `emms-info-ogg--decode-headers' with Vorbis data."
  (let ((bytes "\x01\x76\x6f\x72\x62\x69\x73\x00\x00\x00\x00\x01\x44\xac\x00\x00\x00\x00\x00\x00\x80\x38\x01\x00\x00\x00\x00\x00\xb8\x01\x03\x76\x6f\x72\x62\x69\x73\x34\x00\x00\x00\x58\x69\x70\x68\x2e\x4f\x72\x67\x20\x6c\x69\x62\x56\x6f\x72\x62\x69\x73\x20\x49\x20\x32\x30\x32\x30\x30\x37\x30\x34\x20\x28\x52\x65\x64\x75\x63\x69\x6e\x67\x20\x45\x6e\x76\x69\x72\x6f\x6e\x6d\x65\x6e\x74\x29\x02\x00\x00\x00\x07\x00\x00\x00\x66\x6f\x6f\x3d\x62\x61\x72\x1b\x00\x00\x00\x4b\x65\x79\x3d\xce\x9f\xe1\xbd\x90\xcf\x87\xe1\xbd\xb6\x20\xce\xa4\xce\xb1\xe1\xbd\x90\xcf\x84\xe1\xbd\xb0\x01"))
    (should
     (emms-equal-lists
      (emms-info-native-ogg--decode-headers bytes 'vorbis)
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

(ert-deftest emms-test-ogg-decode-opus-headers ()
  "Test `emms-info-ogg--decode-headers' with Opus data."
  (let ((bytes "\x4f\x70\x75\x73\x48\x65\x61\x64\x01\x01\x38\x01\x44\xac\x00\x00\x00\x00\x00\x4f\x70\x75\x73\x54\x61\x67\x73\x0d\x00\x00\x00\x6c\x69\x62\x6f\x70\x75\x73\x20\x31\x2e\x33\x2e\x31\x03\x00\x00\x00\x26\x00\x00\x00\x45\x4e\x43\x4f\x44\x45\x52\x3d\x6f\x70\x75\x73\x65\x6e\x63\x20\x66\x72\x6f\x6d\x20\x6f\x70\x75\x73\x2d\x74\x6f\x6f\x6c\x73\x20\x30\x2e\x31\x2e\x31\x30\x07\x00\x00\x00\x66\x6f\x6f\x3d\x62\x61\x72\x1b\x00\x00\x00\x4b\x65\x79\x3d\xce\x9f\xe1\xbd\x90\xcf\x87\xe1\xbd\xb6\x20\xce\xa4\xce\xb1\xe1\xbd\x90\xcf\x84\xe1\xbd\xb0"))
    (emms-equal-lists
     (emms-info-native-ogg--decode-headers bytes 'opus)
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

(defun emms-test-ogg--decode-last-page (bytes)
  "Call `emms-info-ogg--decode-last-page' with BYTES input.

This is a helper function for `emms-test-ogg-decode-last-page'."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (concat bytes))
    (emms-info-native-ogg--decode-last-page)))

(ert-deftest emms-test-ogg-decode-last-page()
  (let ((valid "\x01\x02\x03\x04\x4f\x67\x67\x53\x00\x04\x00\x24\x08\x01\x00\x00\x00\x00\x9c\x39\x6e\x47\x40\x08\x00\x00\x19\x4e\xac\xa3\x01\x0a\x4f\x67\x67\x53\x31\x32\x33\x34\x35\x36")
        (notlast "\x01\x02\x03\x04\x4f\x67\x67\x53\x00\x00\x00\x24\x08\x01\x00\x00\x00\x00\x9c\x39\x6e\x47\x40\x08\x00\x00\x19\x4e\xac\xa3\x01\x0a\x4f\x67\x67\x53\x31\x32\x33\x34\x35\x36")
        (invalid "\x01\x02\x03\x04\x4f\x67\x67\x53\x00\x04\x00\x24\x08\x01\x00\x00\x00\x00\x9c\x39\x6e\x47\x40\x08\x00\x00\x01\x02\x03\x04\x01\x0a\x4f\x67\x67\x53\x31\x32\x33\x34\x35\x36")
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
    (unless (eval-when-compile (fboundp 'bindat-type))
      (push (cons 'granule-position-bytes [0 36 8 1 0 0 0 0]) valid-result))
    (should (emms-equal-lists (emms-test-ogg--decode-last-page valid)
                              valid-result))
    (should (equal (emms-test-ogg--decode-last-page notlast) nil))
    (should (equal (emms-test-ogg--decode-last-page invalid) nil))))

(ert-deftest emms-test-ogg-calculate-checksum ()
  (let ((bytes "\x01\x02\x03\x04\x4f\x67\x67\x53\x00\x04\x00\x24\x08\x01\x00\x00\x00\x00\x9c\x39\x6e\x47\x40\x08\x00\x00\x19\x4e\xac\xa3\x01\x0a\x4f\x67\x67\x53\x31\x32\x33\x34\x35\x36"))
    (should (= (emms-info-native-ogg--checksum bytes) 445885580))))

;;; emms-info-native-ogg-tests.el ends here
