;;; emms-info-native-mp3-tests.el --- Test suite for emms-info-native-mp3  -*- lexical-binding: t; -*-

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

(require 'emms-info-native-mp3)
(require 'ert)

(ert-deftest emms-test-id3v2-valid-frame-id-p ()
  (let ((emms-info-native-id3v2--version 2))
    (should (emms-info-native-id3v2--valid-frame-id-p "A1B"))
    (should (not (emms-info-native-id3v2--valid-frame-id-p "~B1")))
    (should (not (emms-info-native-id3v2--valid-frame-id-p "XX")))
    (should (not (emms-info-native-id3v2--valid-frame-id-p "XXXX"))))
  (let ((emms-info-native-id3v2--version 3))
    (should (emms-info-native-id3v2--valid-frame-id-p "ABC9"))
    (should (not (emms-info-native-id3v2--valid-frame-id-p "~BCD")))
    (should (not (emms-info-native-id3v2--valid-frame-id-p "XXX")))
    (should (not (emms-info-native-id3v2--valid-frame-id-p "XXXXX")))))

(ert-deftest emms-test-id3v2-checked-size ()
  (should (= (emms-info-native-id3v2--checked-size 'tag [0 0 2 1]) 257))
  (should (= (emms-info-native-id3v2--checked-size 'tag [1 1 1 1]) 2113665))
  (should (= (emms-info-native-id3v2--checked-size 'tag [#xff #xff #xff #xff])
             (1- (* 256 1024 1024))))
  (should (= (emms-info-native-id3v2--checked-size 'tag [#x7f #x7f #x7f #x7f])
             (1- (* 256 1024 1024))))
  (should (= (emms-info-native-id3v2--checked-size 'tag [#x12 #x34 #x56 #x78])
             38611832))
  (let ((emms-info-native-id3v2--version 4))
    (should (= (emms-info-native-id3v2--checked-size 'frame [#xff #xff #xff #xff])
               (1- (* 256 1024 1024)))))
  (let ((emms-info-native-id3v2--version 3))
    (should (= (emms-info-native-id3v2--checked-size 'frame [#xff #xff #xff #xff])
               (1- (* 4 1024 1024 1024))))))

(ert-deftest emms-test-id3v2-decode-size ()
  (should (= (emms-info-native-id3v2--decode-size [01 01 01 01] nil)
             16843009))
  (should (= (emms-info-native-id3v2--decode-size [01 01 01 01] t)
             2113665))
  (should (= (emms-info-native-id3v2--decode-size [00 00 02 01] nil)
             513))
  (should (= (emms-info-native-id3v2--decode-size [00 00 02 01] t)
             257)))

(ert-deftest emms-test-mp3-find-and-decode-frame-header ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert "\x00\x00\x00\x00\x00\x00\x00\xff\xfb\xb0\x04\x00\x00\x00\x00\x00\x69\x06\x00\x00\x00\x00\x00\x0d\x20\xc0\x00\x00\x00\x00\x01\xa4\x1c\x00\x00\x00\x00\x00\x34\x83\x80\x00\x00\x4c\x41\x4d\x45\x33\x2e\x39\x31\x55\x55")
    (should (equal (emms-info-native-mp3--find-and-decode-frame-header)
                   '((version . mpeg1)
                     (layer . layerIII)
                     (crc . 1)
                     (bit-rate . 192)
                     (sample-rate . 44100)
                     (samples-per-frame . 1152)
                     (padding . 0)
                     (private . 0)
                     (channel-mode . stereo)
                     (mode-extension . 0)
                     (copyright . 0)
                     (emphasis . 0)
                     (original . 1))))))

(ert-deftest emms-test-mp3-find-and-decode-xing-header ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert "\xff\xea\x90\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x58\x69\x6e\x67\x00\x00\x00\x0f\x00\x00\x21\x59\x00\x50\x1d\x79\x00\x03\x06\x08\x0b\x0e\x0f\x12\x15\x17\x1a\x1d\x1f\x22\x25\x27\x2a\x2d\x2f\x32\x35\x37\x39\x3c\x3e\x41\x44\x46\x49\x4c\x4e\x51\x54\x56\x59\x5c\x5e\x61\x64\x65\x68\x6b\x6d\x70\x73\x75\x78\x7a\x7b\x7d\x7f\x82\x85\x87\x8a\x8d\x8f\x92\x95\x97\x9a\x9d\x9f\xa2\xa5\xa7\xaa\xad\xae\xb1\xb4\xb6\xb9\xbc\xbe\xc1\xc4\xc6\xc9\xcc\xce\xd1\xd4\xd6\xd9\xdc\xde\xe1\xe4\xe6\xe9\xec\xee\xf1\xf4\xf6\xf9\xfb\xfd\xff\x00\x00\x00\x58\x4c\x41\x4d\x45\x33\x2e\x38\x38\x20\x28\x61\x6c\x70\x68\x61\x29\x00\x00")
    (should (= (emms-info-native-mp3--find-and-decode-xing-header) 8537))))

(ert-deftest emms-test-mp3-find-decode-xing-header-2 ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert "\xff\xfb\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x49\x6e\x66\x6f\x00\x00\x00\x0f\x00\x00\x23\xd8\x00\x3a\x86\x09\x00\x02\x05\x08\x0a\x0d\x10\x12\x14\x17\x19\x1c\x1f\x21\x24\x26\x29\x2b\x2e\x31\x33\x36\x38\x3a\x3d\x40\x42\x45\x48\x4a\x4c\x4f\x52\x54\x57\x5a\x5b\x5e\x61\x63\x66\x69\x6c\x6d\x70\x73\x75\x78\x7b\x7d\x80\x82\x85\x87\x8a\x8d\x8f\x92\x94\x96\x99\x9c\x9e\xa1\xa4\xa6\xa8\xab\xae\xb0\xb3\xb6\xb7\xba\xbd\xbf\xc2\xc5\xc7\xc9\xcc\xcf\xd1\xd4\xd7\xd9\xdb\xde\xe0\xe3\xe6\xe9\xeb\xed\xf0\xf2\xf5\xf8\xfa\xfd\x00\x00\x00\x00\x4c\x61\x76\x63\x35\x39\x2e\x33\x37\x00\x00")
    (should (= (emms-info-native-mp3--find-and-decode-xing-header) 9176))))

(ert-deftest emms-test-mp3-find-and-decode-vbri-header ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert "\xff\xfb\xa1\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x56\x42\x52\x49\x00\x01\x0d\xb1\x00\x64\x00\x62\xdb\x91\x00\x00\x21\x3a\x00\x84\x00\x01\x00\x02\x00\x40\x98\xb1\xbd\xa8\xbb\x36\xba\xce\xbb\x37\xba\xcf\xba\x67\xbb\x37\xbc\xd7\xbb\x9f\xba\xcf\xb9\x2c\xbb\x35\xbb\x38\xbc\x08\xbb\x9f\xb9\x95\xbe\xe0\xbc\x08\xb9\xfa\xba\x63\xb8\x5a\xb6")
    (should (= (emms-info-native-mp3--find-and-decode-vbri-header) 8506))))

;;; emms-info-native-mp3-tests.el ends here
