;;; emms-info-flac-tests.el --- Test suite for emms-info-flac  -*- lexical-binding: t; -*-

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

(require 'emms-info-flac)
(require 'ert)

(defmacro emms-flac-test-make-data-func (name bytes)
  "Macro for defining test data generator.
This macro defines a suitable function with NAME that outputs
BYTES after FLAC signature.  The function NAME can then be passed
for `emms-info-flac--decode-meta-blocks'."
  `(defun ,name (offset end)
     (let ((bytes (seq-concatenate 'vector [102 76 97 67] ,bytes)))
       (erase-buffer)
       (mapcar #'insert (seq-subseq bytes offset end)))))

(emms-flac-test-make-data-func emms-test-invalid-flac-block-length [1 200 200 200 0 1 2 3])
(emms-flac-test-make-data-func emms-test-invalid-flac-block-type [9 0 0 0 0 1 2 3])
(emms-flac-test-make-data-func emms-test-valid-flac-block [0 0 0 8 10 11 12 13 14 15 16 17 132 0 0 4 1 2 3 4])

(ert-deftest emms-flac-test-meta-blocks ()
  (should-error (emms-info-flac--decode-meta-blocks
                 #'emms-test-invalid-flac-block-length))
  (should-error (emms-info-flac--decode-meta-blocks
                 #'emms-test-invalid-flac-block-type))
  (should (equal (emms-info-flac--decode-meta-blocks
                  #'emms-test-valid-flac-block)
                 (list (unibyte-string 1 2 3 4)
                       (unibyte-string 10 11 12 13 14 15 16 17)))))

(ert-deftest emms-flac-test-decode-duration ()
  ;; The corresponding sample metadata bytes are [10 196 66 240 1 8 36 0].
  (should (= (emms-info-flac--decode-duration 775818634391462912) 392)))

(provide 'emms-info-flac-tests)

;;; emms-info-flac-tests.el ends here
