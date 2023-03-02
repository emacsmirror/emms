;; -*- lexical-binding: t; -*-

(require 'emms-info-native)
(require 'ert)

(ert-deftest emms-test-extract-vorbis-comments ()
  (let ((comments '(((user-comment . [77 85 83 73 67 66 82 65 73 78 90 95 82 69 76 69 65 83 69 71 82 79 85 80 73 68 61 57 98 51 48 55 50 57 51 45 100 50 101 54 45 51 52 97 57 45 97 50 56 57 45 49 54 49 99 53 98 97 102 49 56 55 102]) (length . 63)) ;musicbrainz_releasegroupid
                    ((user-comment . [79 82 73 71 73 78 65 76 68 65 84 69 61 49 57 57 55 45 48 51 45 51 49]) (length . 23)) ;originaldate
                    ((user-comment . [79 82 73 71 73 78 65 76 89 69 65 82 61 49 57 57 55]) (length . 17)) ;originalyear
                    ((user-comment . [82 69 76 69 65 83 69 84 89 80 69 61 97 108 98 117 109]) (length . 17)) ;releasetype
                    ((user-comment . [66 65 82 67 79 68 69 61 55 54 57 50 51 51 48 48 52 55 50 55]) (length . 20)) ;barcode
                    ((user-comment . [65 76 66 85 77 61 65 32 116 111 100 97 32 67 117 98 97 32 108 101 32 103 117 115 116 97]) (length . 26))))) ;album
    (should (equal (emms-info-native--extract-vorbis-comments comments)
                   (quote (("album" . "A toda Cuba le gusta")
                           ("originalyear" . "1997")
                           ("originaldate" . "1997-03-31")))))))

(ert-deftest emms-test-split-vorbis-comment ()
  (let ((utf-8-comment [75 101 121 61 206 159 225 189 144 207 135 225 189 182 32 206 164 206 177 225 189 144 207 132 225 189 176 10 ])) ;Key=Οὐχὶ Ταὐτὰ
    (should (equal (emms-info-native--split-vorbis-comment "") nil))
    (should (equal (emms-info-native--split-vorbis-comment "x") nil))
    (should (equal (emms-info-native--split-vorbis-comment "x=") nil))
    (should (equal (emms-info-native--split-vorbis-comment "=x") nil))
    (should (equal (emms-info-native--split-vorbis-comment "a=B")
                   (cons "a" "B")))
    (should (equal (emms-info-native--split-vorbis-comment "abc=ABC=123")
                   (cons "abc" "ABC=123")))
    (should (equal (emms-info-native--split-vorbis-comment utf-8-comment)
                   (cons "key" "Οὐχὶ Ταὐτὰ")))))

(ert-deftest emms-test-decode-ogg-page ()
  (let ((bytes [79 103 103 83 0 2 0 0 0 0 0 0 0 0 134 209 158 23 0 0 0 0 53 82 251 136 1 30 1 118 111 114 98 105 115 0 0 0 0 1 68 172 0 0 0 0 0 0 128 56 1 0 0 0 0 0 184 1]))
    (should (equal (emms-info-native--decode-ogg-page bytes)
                   (list :num-packets 1
                         :num-bytes 58
                         :stream [1 118 111 114 98 105 115 0 0 0 0 1 68 172 0 0 0 0 0 0 128 56 1 0 0 0 0 0 184 1])))))

(ert-deftest emms-test-decode-ogg-vorbis-headers ()
  (let ((bytes [1 118 111 114 98 105 115 0 0 0 0 1 68 172 0 0 0 0 0 0 128 56 1 0 0 0 0 0 184 1 3 118 111 114 98 105 115 52 0 0 0 88 105 112 104 46 79 114 103 32 108 105 98 86 111 114 98 105 115 32 73 32 50 48 50 48 48 55 48 52 32 40 82 101 100 117 99 105 110 103 32 69 110 118 105 114 111 110 109 101 110 116 41 2 0 0 0 7 0 0 0 102 111 111 61 98 97 114 27 0 0 0 75 101 121 61 206 159 225 189 144 207 135 225 189 182 32 206 164 206 177 225 189 144 207 132 225 189 176 1]))
    (should (equal (emms-info-native--decode-ogg-headers bytes 'vorbis)
                   '((comment-header
                      (framing-bit . 1)
                      (user-comments
                       ((user-comment . [102 111 111 61 98 97 114])
                        (length . 7))
                       ((user-comment . [75 101 121 61 206 159 225 189 144 207 135 225 189 182 32 206 164 206 177 225 189 144 207 132 225 189 176])
                        (length . 27)))
                      (user-comments-list-length . 2)
                      (vendor-string . [88 105 112 104 46 79 114 103 32 108 105 98 86 111 114 98 105 115 32 73 32 50 48 50 48 48 55 48 52 32 40 82 101 100 117 99 105 110 103 32 69 110 118 105 114 111 110 109 101 110 116 41])
                      (vendor-length . 52)
                      (vorbis . [118 111 114 98 105 115])
                      (packet-type . 3))
                     (identification-header
                      (framing-flag . 1)
                      (blocksize . 184)
                      (bitrate-minimum . 0)
                      (bitrate-nominal . 80000)
                      (bitrate-maximum . 0)
                      (sample-rate . 44100)
                      (channel-count . 1)
                      (vorbis-version . 0)
                      (vorbis . [118 111 114 98 105 115])
                      (packet-type . 1)))))))

(ert-deftest emms-test-decode-ogg-opus-headers ()
  (let ((bytes [79 112 117 115 72 101 97 100 1 1 56 1 68 172 0 0 0 0 0 79 112 117 115 84 97 103 115 13 0 0 0 108 105 98 111 112 117 115 32 49 46 51 46 49 3 0 0 0 38 0 0 0 69 78 67 79 68 69 82 61 111 112 117 115 101 110 99 32 102 114 111 109 32 111 112 117 115 45 116 111 111 108 115 32 48 46 49 46 49 48 7 0 0 0 102 111 111 61 98 97 114 27 0 0 0 75 101 121 61 206 159 225 189 144 207 135 225 189 182 32 206 164 206 177 225 189 144 207 132 225 189 176]))
    (should (equal (emms-info-native--decode-ogg-headers bytes 'opus)
                   '((comment-header
                      (user-comments
                       ((user-comment . [69 78 67 79 68 69 82 61 111 112 117 115 101 110 99 32 102 114 111 109 32 111 112 117 115 45 116 111 111 108 115 32 48 46 49 46 49 48])
                        (length . 38))
                       ((user-comment . [102 111 111 61 98 97 114])
                        (length . 7))
                       ((user-comment . [75 101 121 61 206 159 225 189 144 207 135 225 189 182 32 206 164 206 177 225 189 144 207 132 225 189 176])
                        (length . 27)))
                      (user-comments-list-length . 3)
                      (vendor-string . [108 105 98 111 112 117 115 32 49 46 51 46 49])
                      (vendor-length . 13)
                      (opus-tags . [79 112 117 115 84 97 103 115]))
                     (identification-header
                      (channel-mapping-family . 0)
                      (output-gain . 0)
                      (sample-rate . 44100)
                      (pre-skip . 312)
                      (channel-count . 1)
                      (opus-version . 1)
                      (opus-head . [79 112 117 115 72 101 97 100])))))))

(defun emms-test--decode-last-ogg-page (bytes)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (concat bytes))
    (emms-info-native--decode-last-ogg-page)))

(ert-deftest emms-test-decode-last-ogg-page()
  (let ((valid [#x01 #x02 #x03 #x04 #x4f #x67 #x67 #x53 #x00 #x04 #x00 #x24 #x08 #x01 #x00 #x00 #x00 #x00 #x9c #x39 #x6e #x47 #x40 #x08 #x00 #x00 #x19 #x4e #xac #xa3 #x01 #x0a #x4f #x67 #x67 #x53 #x31 #x32 #x33 #x34 #x35 #x36])
        (notlast [#x01 #x02 #x03 #x04 #x4f #x67 #x67 #x53 #x00 #x00 #x00 #x24 #x08 #x01 #x00 #x00 #x00 #x00 #x9c #x39 #x6e #x47 #x40 #x08 #x00 #x00 #x19 #x4e #xac #xa3 #x01 #x0a #x4f #x67 #x67 #x53 #x31 #x32 #x33 #x34 #x35 #x36])
        (invalid [#x01 #x02 #x03 #x04 #x4f #x67 #x67 #x53 #x00 #x04 #x00 #x24 #x08 #x01 #x00 #x00 #x00 #x00 #x9c #x39 #x6e #x47 #x40 #x08 #x00 #x00 #x01 #x02 #x03 #x04 #x01 #x0a #x4f #x67 #x67 #x53 #x31 #x32 #x33 #x34 #x35 #x36]))
    (should (equal (emms-test--decode-last-ogg-page valid)
                   '((payload . [79 103 103 83 49 50 51 52 53 54])
                     (segment-table . [10])
                     (page-segments . 1)
                     (page-checksum . 2745978393)
                     (page-sequence-no . 2112)
                     (stream-serial-number . 1198406044)
                     (granule-position . [0 36 8 1 0 0 0 0])
                     (header-type-flag . 4)
                     (stream-structure-version . 0)
                     (capture-pattern . [79 103 103 83]))))
    (should (equal (emms-test--decode-last-ogg-page notlast) nil))
    (should (equal (emms-test--decode-last-ogg-page invalid) nil))))

(ert-deftest emms-test-calculate-ogg-checksum ()
  (let ((bytes [#x01 #x02 #x03 #x04 #x4f #x67 #x67 #x53 #x00 #x04 #x00 #x24 #x08 #x01 #x00 #x00 #x00 #x00 #x9c #x39 #x6e #x47 #x40 #x08 #x00 #x00 #x19 #x4e #xac #xa3 #x01 #x0a #x4f #x67 #x67 #x53 #x31 #x32 #x33 #x34 #x35 #x36]))
    (should (equal (emms-info-native--calculate-ogg-checksum bytes) 445885580))))

(ert-deftest emms-test-decode-ogg-granule-position ()
  (should (equal (emms-info-native--decode-ogg-granule-position [0 36 8 1 0 0 0 0] 44100)
                 392))
  (should (equal (emms-info-native--decode-ogg-granule-position [40 236 178 11 0 0 0 0] 48000)
                 4089))
  (should (equal (emms-info-native--decode-ogg-granule-position [255 255 255 255 255 255 255 255] nil)
                 nil)))

(ert-deftest emms-test-vector-to-integer ()
  (should (equal (emms-info-native--vector-to-integer [0]) 0))
  (should (equal (emms-info-native--vector-to-integer [127]) 127))
  (should (equal (emms-info-native--vector-to-integer [255]) 255))
  (should (equal (emms-info-native--vector-to-integer [0 1]) 256))
  (should (equal (emms-info-native--vector-to-integer [1 0]) 1))
  (should (equal (emms-info-native--vector-to-integer [0 128]) 32768)))

(ert-deftest emms-test-unsigned-to-signed ()
  (should (equal (emms-info-native--unsigned-to-signed 0 8) 0))
  (should (equal (emms-info-native--unsigned-to-signed 1 8) 1))
  (should (equal (emms-info-native--unsigned-to-signed 127 8) 127))
  (should (equal (emms-info-native--unsigned-to-signed 128 8) -128))
  (should (equal (emms-info-native--unsigned-to-signed 129 8) -127))
  (should (equal (emms-info-native--unsigned-to-signed 254 8) -2))
  (should (equal (emms-info-native--unsigned-to-signed 255 8) -1)))

(defmacro emms-test-make-flac-data-func (name bytes)
  `(defun ,name (offset end replace)
     (let ((bytes (seq-concatenate 'vector [102 76 97 67] ,bytes)))
       (when replace (erase-buffer))
       (mapcar #'insert (seq-subseq bytes offset end)))))

(emms-test-make-flac-data-func emms-test-invalid-flac-block-length [1 200 200 200 0 1 2 3])
(emms-test-make-flac-data-func emms-test-invalid-flac-block-type [9 0 0 0 0 1 2 3])
(emms-test-make-flac-data-func emms-test-valid-flac-block [1 0 0 1 0 4 0 0 4 1 2 3 4])

(ert-deftest emms-test-decode-flac-comment-block ()
  (should-error (emms-info-native--decode-flac-comment-block #'emms-test-invalid-flac-block-length))
  (should-error (emms-info-native--decode-flac-comment-block #'emms-test-invalid-flac-block-type))
  (should (equal (emms-info-native--decode-flac-comment-block #'emms-test-valid-flac-block)
                 [1 2 3 4])))

