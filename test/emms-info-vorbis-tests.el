;;; emms-info-vorbis-tests.el --- Test suite for emms-info-vorbis  -*- lexical-binding: t; -*-

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

(require 'emms-info-vorbis)
(require 'ert)

(ert-deftest emms-vorbis-test-extract-comments ()
  (let ((comments '(((user-comment . [77 85 83 73 67 66 82 65 73 78 90 95 82 69 76 69 65 83 69 71 82 79 85 80 73 68 61 57 98 51 48 55 50 57 51 45 100 50 101 54 45 51 52 97 57 45 97 50 56 57 45 49 54 49 99 53 98 97 102 49 56 55 102]) (length . 63)) ;musicbrainz_releasegroupid
                    ((user-comment . [79 82 73 71 73 78 65 76 68 65 84 69 61 49 57 57 55 45 48 51 45 51 49]) (length . 23)) ;originaldate
                    ((user-comment . [79 82 73 71 73 78 65 76 89 69 65 82 61 49 57 57 55]) (length . 17)) ;originalyear
                    ((user-comment . [82 69 76 69 65 83 69 84 89 80 69 61 97 108 98 117 109]) (length . 17)) ;releasetype
                    ((user-comment . [66 65 82 67 79 68 69 61 55 54 57 50 51 51 48 48 52 55 50 55]) (length . 20)) ;barcode
                    ((user-comment . [65 76 66 85 77 61 65 32 116 111 100 97 32 67 117 98 97 32 108 101 32 103 117 115 116 97]) (length . 26))))) ;album
    (should (equal (emms-info-vorbis-extract-comments comments)
                   (quote (("album" . "A toda Cuba le gusta")
                           ("originalyear" . "1997")
                           ("originaldate" . "1997-03-31")))))))

(ert-deftest emms-vorbis-test-split-comment ()
  (should (equal (emms-info-vorbis--split-comment "") nil))
  (should (equal (emms-info-vorbis--split-comment "x") nil))
  (should (equal (emms-info-vorbis--split-comment "x=") nil))
  (should (equal (emms-info-vorbis--split-comment "=x") nil))
  (should (equal (emms-info-vorbis--split-comment "a=B")
                 (cons "a" "B")))
  (should (equal (emms-info-vorbis--split-comment "abc=ABC=123")
                 (cons "abc" "ABC=123")))
  (let ((comment [75 101 121 61 206 159 225 189 144 207 135 225 189 182 32 206 164 206 177 225 189 144 207 132 225 189 176 10 ])) ;Key=Οὐχὶ Ταὐτὰ
    (should (equal (emms-info-vorbis--split-comment comment)
                   (cons "key" "Οὐχὶ Ταὐτὰ")))))

(provide 'emms-info-vorbis-tests)

;;; emms-info-vorbis-tests.el ends here
