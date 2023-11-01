;;; emms-info-native-tests.el --- Test suite for emms-info-native  -*- lexical-binding: t; -*-

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

;;; Commentary:

;; This test suite exercises `emms-info-native' with various input
;; files.

;;; Code:

(require 'emms-info-native)
(require 'ert)

(ert-deftest emms-test-info-native-mp3 ()
  (should (equal (emms-info-native--decode-info-fields
                  "resources/sine.mp3")
                 '(("year" . "2023")
                   ("album" . "Test Data ☺")
                   ("artist" . "EMMS project")
                   ("title" . "440 Hz sine wave")
                   ("playing-time" . 5)))))

(ert-deftest emms-test-info-native-ogg ()
  (should (equal (emms-info-native--decode-info-fields
                  "resources/sine.ogg")
                 '(("artist" . "EMMS project")
                   ("date" . "2023-09-02")
                   ("title" . "440 Hz sine wave")
                   ("album" . "Test Data ☺")
                   ("playing-time" . 5)))))

(ert-deftest emms-test-info-native-flac ()
  (should (equal (emms-info-native--decode-info-fields
                  "resources/sine.flac")
                 '(("artist" . "EMMS project")
                   ("date" . "2023-09-02")
                   ("title" . "440 Hz sine wave")
                   ("album" . "Test Data ☺")
                   ("playing-time" . 5)))))

(ert-deftest emms-test-info-native-opus ()
  (should (equal (emms-info-native--decode-info-fields
                  "resources/sine.opus")
                 '(("artist" . "EMMS project")
                   ("date" . "2023-09-02")
                   ("title" . "440 Hz sine wave")
                   ("album" . "Test Data ☺")
                   ("playing-time" . 5)))))

;;; emms-info-native-tests.el ends here
