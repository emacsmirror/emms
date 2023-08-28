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
(require 'ert-x)

(ert-deftest emms-info-native-test-mp3 ()
  (should (equal (emms-info-native--decode-info-fields
                  (ert-resource-file "id3v22-test.mp3"))
                 '(("year" . "2004")
                   ("tracknumber" . "3/11")
                   ("album" . "Hymns for the Exiled")
                   ("artist" . "Anais Mitchell")
                   ("title" . "cosmic american")
                   ("playing-time" . 0))))
  
  (should (equal (emms-info-native--decode-info-fields
                  (ert-resource-file "silence-44-s.mp3"))
                 '(("tracknumber" . "02/10")
                   ("title" . "Silence")
                   ("artist" . "jzig")
                   ("artist" . "piman")
                   ("album" . "Quod Libet Test Data")
                   ("genre" . "Silence")
                   ("year" . "2004")
                   ("playing-time" . 4))))

  (should (equal (emms-info-native--decode-info-fields
                  (ert-resource-file "vbri.mp3"))
                 '(("title" . "I Can Walk On Water I Can Fly")
                   ("artist" . "Basshunter")
                   ("album" . "I Can Walk On Water I Can Fly")
                   ("year" . "2007")
                   ("genre" . "Dance")
                   ("composer")
                   ("tracknumber" . "01")
                   ("playing-time" . 222)))))

(ert-deftest emms-info-native-test-ogg ()
  (should (equal (emms-info-native--decode-info-fields
                  (ert-resource-file "multipage-setup.ogg"))
                 '(("artist" . "UVERworld")
                   ("genre" . "JRock")
                   ("title" . "Burst")
                   ("album" . "Timeless")
                   ("tracknumber" . "7")
                   ("date" . "2006")
                   ("playing-time" . 4)))))

  (ert-deftest emms-info-native-test-flac ()
    (should (equal (emms-info-native--decode-info-fields
                    (ert-resource-file "52-overwritten-metadata.flac"))
                   '(("tracknumber" . "01")
                     ("date" . "1990")
                     ("genre" . "Klezmer")
                     ("album" . "The Magic of the Klezmer")
                     ("artist" . "Giora Feidman")
                     ("title" . "Songs of Rejoicing")
                     ("playing-time" . 236))))
    (should (equal (emms-info-native--decode-info-fields
                    (ert-resource-file "flac_application.flac"))
                   '(("artistsort" . "Belle and Sebastian")
                     ("artist" . "Belle and Sebastian")
                     ("title" . "I Want the World to Stop")
                     ("album" . "Belle and Sebastian Write About Love")
                     ("tracknumber" . "4/11")
                     ("date" . "2010-10-11")
                     ("playing-time" . 273))))
    (should (equal (emms-info-native--decode-info-fields
                    (ert-resource-file "silence-44-s.flac"))
                   '(("title" . "Silence")
                     ("date" . "2004")
                     ("tracknumber" . "02/10")
                     ("genre" . "Silence")
                     ("artist" . "jzig")
                     ("artist" . "piman")
                     ("album" . "Quod Libet Test Data")
                     ("playing-time" . 3))))
    (should (equal (emms-info-native--decode-info-fields
                    (ert-resource-file "variable-block.flac"))
                   '(("tracknumber" . "01")
                     ("title" . "DIVE FOR YOU")
                     ("genre" . "Anime Soundtrack")
                     ("discnumber" . "1")
                     ("date" . "2004")
                     ("composer" . "Boom Boom Satellites (Lyrics)")
                     ("artist" . "Boom Boom Satellites")
                     ("album" . "Appleseed Original Soundtrack")
                     ("playing-time" . 261)))))
