;; -*- lexical-binding: t; -*-

(require 'emms)
(require 'ert)

(ert-deftest emms-test-le-to-int ()
  (should (= (emms-le-to-int nil) 0))
  (should (= (emms-le-to-int [0]) 0))
  (should (= (emms-le-to-int [127]) 127))
  (should (= (emms-le-to-int [255]) 255))
  (should (= (emms-le-to-int [0 1]) 256))
  (should (= (emms-le-to-int [1 0]) 1))
  (should (= (emms-le-to-int [0 128]) 32768))
  (should (= (emms-le-to-int [1 2 4 8]) 134480385)))

(ert-deftest emms-test-from-twos-complement ()
  (should (= (emms-from-twos-complement 0 8) 0))
  (should (= (emms-from-twos-complement 1 8) 1))
  (should (= (emms-from-twos-complement 127 8) 127))
  (should (= (emms-from-twos-complement 128 8) -128))
  (should (= (emms-from-twos-complement 129 8) -127))
  (should (= (emms-from-twos-complement 254 8) -2))
  (should (= (emms-from-twos-complement 255 8) -1))
  (should (= (emms-from-twos-complement 0 10) 0))
  (should (= (emms-from-twos-complement 511 10) 511))
  (should (= (emms-from-twos-complement 512 10) -512))
  (should (= (emms-from-twos-complement 1023 10) -1)))

(ert-deftest emms-test-extract-bits ()
  (should (= (emms-extract-bits 128 7) 1))
  (should (= (emms-extract-bits 64 6 7) 1))
  (should (= (emms-extract-bits 128 6 7) 2))
  (should (= (emms-extract-bits 192 6 7) 3))
  (should (eq (emms-extract-bits 192 7 6) nil))
  (should (= (emms-extract-bits 128 32) 0))
  (should (= (emms-extract-bits 4294688772 21 31) 2047)))

(provide 'emms-tests)

;;; emms-tests.el ends here
