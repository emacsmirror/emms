;;; emms-info-native-vorbis-tests.el --- Test suite for emms-info-native-vorbis  -*- lexical-binding: t; -*-

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

(require 'emms-info-native-vorbis)
(require 'ert)

(ert-deftest emms-test-vorbis-extract-comments ()
  (let ((comments
         (quote (((user-comment . "MUSICBRAINZ_RELEASEGROUPID=9b307293-d2e6-34a9-a289-161c5baf187f")
                  (length . 63))
                 ((user-comment . "ORIGINALDATE=1997-03-31")
                  (length . 23))
                 ((user-comment . "ORIGINALYEAR=1997")
                  (length . 17))
                 ((user-comment . "RELEASETYPE=album")
                  (length . 17))
                 ((user-comment . "BARCODE=769233004727")
                  (length . 20))
                 ((user-comment . "ALBUM=A toda Cuba le gusta")
                  (length . 26))))))
    (should (equal (emms-info-native-vorbis-extract-comments comments)
                   (quote (("album" . "A toda Cuba le gusta")
                           ("originalyear" . "1997")
                           ("originaldate" . "1997-03-31")))))))

(ert-deftest emms-test-vorbis-split-comment ()
  (should (equal (emms-info-native-vorbis--split-comment "") nil))
  (should (equal (emms-info-native-vorbis--split-comment "x") nil))
  (should (equal (emms-info-native-vorbis--split-comment "x=") nil))
  (should (equal (emms-info-native-vorbis--split-comment "=x") nil))
  (should (equal (emms-info-native-vorbis--split-comment "a=B")
                 (cons "a" "B")))
  (should (equal (emms-info-native-vorbis--split-comment "abc=ABC=123")
                 (cons "abc" "ABC=123")))
  (let ((comment "Key=\316\237\341\275\220\317\207\341\275\266 \316\244\316\261\341\275\220\317\204\341\275\260"))
    (should (equal (emms-info-native-vorbis--split-comment comment)
                   (cons "key" "Οὐχὶ Ταὐτὰ")))))

;;; emms-info-native-vorbis-tests.el ends here
