;;; emms-info-mp3-tests.el --- Test suite for emms-info-mp3  -*- lexical-binding: t; -*-

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

(require 'emms-info-mp3)
(require 'ert)

(ert-deftest emms-id3v2-test-valid-frame-id-p ()
  (let ((emms-info-id3v2--version 2))
    (should (emms-info-id3v2--valid-frame-id-p "A1B"))
    (should (not (emms-info-id3v2--valid-frame-id-p "~B1")))
    (should (not (emms-info-id3v2--valid-frame-id-p "XX")))
    (should (not (emms-info-id3v2--valid-frame-id-p "XXXX"))))
  (let ((emms-info-id3v2--version 3))
    (should (emms-info-id3v2--valid-frame-id-p "ABC9"))
    (should (not (emms-info-id3v2--valid-frame-id-p "~BCD")))
    (should (not (emms-info-id3v2--valid-frame-id-p "XXX")))
    (should (not (emms-info-id3v2--valid-frame-id-p "XXXXX")))))

(ert-deftest emms-id3v2-test-checked-size ()
  (should (= (emms-info-id3v2--checked-size 'tag [0 0 2 1]) 257))
  (should (= (emms-info-id3v2--checked-size 'tag [1 1 1 1]) 2113665))
  (should (= (emms-info-id3v2--checked-size 'tag [#xff #xff #xff #xff])
             (1- (* 256 1024 1024))))
  (should (= (emms-info-id3v2--checked-size 'tag [#x7f #x7f #x7f #x7f])
             (1- (* 256 1024 1024))))
  (should (= (emms-info-id3v2--checked-size 'tag [#x12 #x34 #x56 #x78])
             38611832))
  (let ((emms-info-id3v2--version 4))
    (should (= (emms-info-id3v2--checked-size 'frame [#xff #xff #xff #xff])
               (1- (* 256 1024 1024)))))
  (let ((emms-info-id3v2--version 3))
    (should (= (emms-info-id3v2--checked-size 'frame [#xff #xff #xff #xff])
               (1- (* 4 1024 1024 1024))))))

(ert-deftest emms-id3v2-test-decode-size ()
  (should (= (emms-info-id3v2--decode-size [01 01 01 01] nil)
             16843009))
  (should (= (emms-info-id3v2--decode-size [01 01 01 01] t)
             2113665))
  (should (= (emms-info-id3v2--decode-size [00 00 02 01] nil)
             513))
  (should (= (emms-info-id3v2--decode-size [00 00 02 01] t)
             257)))

(ert-deftest emms-mp3-test-find-and-decode-frame-header ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (concat [#x00 #x00 #x00 #x00 #x00 #x00 #x00 #xff #xfb #xb0 #x04 #x00 #x00 #x00 #x00 #x00 #x69 #x06 #x00 #x00 #x00 #x00 #x00 #x0d #x20 #xc0 #x00 #x00 #x00 #x00 #x01 #xa4 #x1c #x00 #x00 #x00 #x00 #x00 #x34 #x83 #x80 #x00 #x00 #x4c #x41 #x4d #x45 #x33 #x2e #x39 #x31 #x55 #x55]))
    (should (equal (emms-info-mp3--find-and-decode-frame-header)
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

(ert-deftest emms-mp3-test-find-and-decode-xing-header ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (concat [255 234 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 88 105 110 103 0 0 0 15 0 0 33 89 0 80 29 121 0 3 6 8 11 14 15 18 21 23 26 29 31 34 37 39 42 45 47 50 53 55 57 60 62 65 68 70 73 76 78 81 84 86 89 92 94 97 100 101 104 107 109 112 115 117 120 122 123 125 127 130 133 135 138 141 143 146 149 151 154 157 159 162 165 167 170 173 174 177 180 182 185 188 190 193 196 198 201 204 206 209 212 214 217 220 222 225 228 230 233 236 238 241 244 246 249 251 253 255 0 0 0 88 76 65 77 69 51 46 56 56 32 40 97 108 112 104 97 41 0 0 ]))
    (should (= (emms-info-mp3--find-and-decode-xing-header) 8537))))

(ert-deftest emms-mp3-test-find-decode-xing-header-2 ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (concat [255 251 80 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 73 110 102 111 0 0 0 15 0 0 35 216 0 58 134 9 0 2 5 8 10 13 16 18 20 23 25 28 31 33 36 38 41 43 46 49 51 54 56 58 61 64 66 69 72 74 76 79 82 84 87 90 91 94 97 99 102 105 108 109 112 115 117 120 123 125 128 130 133 135 138 141 143 146 148 150 153 156 158 161 164 166 168 171 174 176 179 182 183 186 189 191 194 197 199 201 204 207 209 212 215 217 219 222 224 227 230 233 235 237 240 242 245 248 250 253 0 0 0 0 76 97 118 99 53 57 46 51 55 0 0]))
    (should (= (emms-info-mp3--find-and-decode-xing-header) 9176))))

(ert-deftest emms-mp3-test-find-and-decode-vbri-header ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (concat [255 251 161 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 86 66 82 73 0 1 13 177 0 100 0 98 219 145 0 0 33 58 0 132 0 1 0 2 0 64 152 177 189 168 187 54 186 206 187 55 186 207 186 103 187 55 188 215 187 159 186 207 185 44 187 53 187 56 188 8 187 159 185 149 190 224 188 8 185 250 186 99 184 90 182]))
    (should (= (emms-info-mp3--find-and-decode-vbri-header) 8506))))

;;; emms-info-mp3-tests.el ends here
