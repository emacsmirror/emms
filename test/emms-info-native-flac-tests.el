;;; emms-info-native-flac-tests.el --- Test suite for emms-info-native-flac  -*- lexical-binding: t; -*-

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

(require 'emms-info-native-flac)
(require 'ert)

(defmacro emms-test-flac-make-data-func (name bytes)
  "Macro for defining test data generator.
This macro defines a suitable function with NAME that outputs
BYTES after FLAC signature.  The function NAME can then be passed
for `emms-info-native-flac--decode-meta-blocks'."
  `(defun ,name (offset end)
     (let ((bytes (concat "fLaC" ,bytes)))
       (erase-buffer)
       (insert (substring bytes offset end)))))

(emms-test-flac-make-data-func emms-test-invalid-flac-block-length "\x01\xff\xff\xff\x00\x01\x02\x03")
(emms-test-flac-make-data-func emms-test-invalid-flac-block-type "\x09\x00\x00\x00\x00\x01\x02\x03")
(emms-test-flac-make-data-func emms-test-valid-flac-block "\x00\x00\x00\x08\x10\x11\x12\x13\x14\x15\x16\x17\x84\x00\x00\x04\x01\x02\x03\x04")

(ert-deftest emms-test-flac-meta-blocks ()
  (should-error (emms-info-native-flac--decode-meta-blocks
                 #'emms-test-invalid-flac-block-length))
  (should-error (emms-info-native-flac--decode-meta-blocks
                 #'emms-test-invalid-flac-block-type))
  (should (equal (emms-info-native-flac--decode-meta-blocks
                  #'emms-test-valid-flac-block)
                 (list "\x01\x02\x03\x04"
                       "\x10\x11\x12\x13\x14\x15\x16\x17"))))

(ert-deftest emms-test-flac-decode-duration ()
  ;; The corresponding sample metadata bytes are [10 196 66 240 1 8 36 0].
  (should (= (emms-info-native-flac--decode-duration 775818634391462912) 392)))

;;; emms-info-native-flac-tests.el ends here
