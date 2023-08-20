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

(ert-deftest emms-test-lsb-to-integer ()
  (should (equal (emms-info-native--lsb-to-integer [0]) 0))
  (should (equal (emms-info-native--lsb-to-integer [127]) 127))
  (should (equal (emms-info-native--lsb-to-integer [255]) 255))
  (should (equal (emms-info-native--lsb-to-integer [0 1]) 256))
  (should (equal (emms-info-native--lsb-to-integer [1 0]) 1))
  (should (equal (emms-info-native--lsb-to-integer [0 128]) 32768)))

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
(emms-test-make-flac-data-func emms-test-valid-flac-block [0 0 0 8 10 11 12 13 14 15 16 17 132 0 0 4 1 2 3 4])

(ert-deftest emms-test-decode-flac-meta-blocks ()
  (should-error (emms-info-native--decode-flac-meta-blocks
                 #'emms-test-invalid-flac-block-length))
  (should-error (emms-info-native--decode-flac-meta-blocks
                 #'emms-test-invalid-flac-block-type))
  (should (equal (emms-info-native--decode-flac-meta-blocks
                  #'emms-test-valid-flac-block)
                 '([1 2 3 4] [10 11 12 13 14 15 16 17]))))

(ert-deftest emms-test-decode-flac-playtime ()
  ;; The corresponding sample metadata bytes are [10 196 66 240 1 8 36 0].
  (should (= (emms-info-native--decode-flac-playtime 775818634391462912) 392)))

(ert-deftest emms-test-valid-id3v2-frame-id-p ()
  (let ((emms-info-native--id3v2-version 2))
    (should (emms-info-native--valid-id3v2-frame-id-p "A1B"))
    (should (not (emms-info-native--valid-id3v2-frame-id-p "~B1")))
    (should (not (emms-info-native--valid-id3v2-frame-id-p "XX")))
    (should (not (emms-info-native--valid-id3v2-frame-id-p "XXXX"))))
  (let ((emms-info-native--id3v2-version 3))
    (should (emms-info-native--valid-id3v2-frame-id-p "ABC9"))
    (should (not (emms-info-native--valid-id3v2-frame-id-p "~BCD")))
    (should (not (emms-info-native--valid-id3v2-frame-id-p "XXX")))
    (should (not (emms-info-native--valid-id3v2-frame-id-p "XXXXX")))))

(ert-deftest emms-test-checked-id3v2-size ()
  (should (= (emms-info-native--checked-id3v2-size 'tag [0 0 2 1]) 257))
  (should (= (emms-info-native--checked-id3v2-size 'tag [1 1 1 1]) 2113665))
  (should (= (emms-info-native--checked-id3v2-size 'tag [#xff #xff #xff #xff])
             (1- (* 256 1024 1024))))
  (should (= (emms-info-native--checked-id3v2-size 'tag [#x7f #x7f #x7f #x7f])
             (1- (* 256 1024 1024))))
  (should (= (emms-info-native--checked-id3v2-size 'tag [#x12 #x34 #x56 #x78])
             38611832))
  (let ((emms-info-native--id3v2-version 4))
    (should (= (emms-info-native--checked-id3v2-size 'frame [#xff #xff #xff #xff])
               (1- (* 256 1024 1024)))))
  (let ((emms-info-native--id3v2-version 3))
    (should (= (emms-info-native--checked-id3v2-size 'frame [#xff #xff #xff #xff])
               (1- (* 4 1024 1024 1024))))))

(ert-deftest emms-test-decode-id3v2-size ()
  (should (= (emms-info-native--decode-id3v2-size [01 01 01 01] nil)
             16843009))
  (should (= (emms-info-native--decode-id3v2-size [01 01 01 01] t)
             2113665))
  (should (= (emms-info-native--decode-id3v2-size [00 00 02 01] nil)
             513))
  (should (= (emms-info-native--decode-id3v2-size [00 00 02 01] t)
             257)))

(ert-deftest emms-test-extract-bits ()
  (should (= (emms-info-native--extract-bits 128 7) 1))
  (should (= (emms-info-native--extract-bits 64 6 7) 1))
  (should (= (emms-info-native--extract-bits 128 6 7) 2))
  (should (= (emms-info-native--extract-bits 192 6 7) 3))
  (should (eq (emms-info-native--extract-bits 192 7 6) nil))
  (should (= (emms-info-native--extract-bits 128 32) 0))
  (should (= (emms-info-native--extract-bits 4294688772 21 31) 2047)))

(ert-deftest emms-test-decode-mp3-frame-header ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (concat [#x00 #x00 #x00 #x00 #x00 #x00 #x00 #xff #xfb
                          #xb0 #x04 #x00 #x00 #x00 #x00 #x00 #x69 #x06 #x00 #x00 #x00 #x00
                          #x00 #x0d #x20 #xc0 #x00 #x00 #x00 #x00 #x01 #xa4 #x1c #x00 #x00
                          #x00 #x00 #x00 #x34 #x83 #x80 #x00 #x00 #x4c #x41 #x4d #x45 #x33
                          #x2e #x39 #x31 #x55 #x55]))
    (should (equal (emms-info-native--find-and-decode-mp3-frame-header)
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

(ert-deftest emms-test-decode-xing-header ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (concat [255 234 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 88 105 110 103 0 0 0 15 0 0 33
                         89 0 80 29 121 0 3 6 8 11 14 15 18 21 23 26 29 31 34 37 39 42 45
                         47 50 53 55 57 60 62 65 68 70 73 76 78 81 84 86 89 92 94 97 100
                         101 104 107 109 112 115 117 120 122 123 125 127 130 133 135 138
                         141 143 146 149 151 154 157 159 162 165 167 170 173 174 177 180
                         182 185 188 190 193 196 198 201 204 206 209 212 214 217 220 222
                         225 228 230 233 236 238 241 244 246 249 251 253 255 0 0 0 88 76
                         65 77 69 51 46 56 56 32 40 97 108 112 104 97 41 0 0 ]))
    (should (= (emms-info-native--find-and-decode-xing-header) 8537))))

(ert-deftest emms-test-decode-xing-header-2 ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (concat [255 251 80 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 73 110 102 111 0 0 0 15 0 0 35
                         216 0 58 134 9 0 2 5 8 10 13 16 18 20 23 25 28 31 33 36 38 41 43
                         46 49 51 54 56 58 61 64 66 69 72 74 76 79 82 84 87 90 91 94 97
                         99 102 105 108 109 112 115 117 120 123 125 128 130 133 135 138
                         141 143 146 148 150 153 156 158 161 164 166 168 171 174 176 179
                         182 183 186 189 191 194 197 199 201 204 207 209 212 215 217 219
                         222 224 227 230 233 235 237 240 242 245 248 250 253 0 0 0 0 76
                         97 118 99 53 57 46 51 55 0 0]))
    (should (= (emms-info-native--find-and-decode-xing-header) 9176))))

(ert-deftest emms-test-decode-vbri-header ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (concat [255 251 161 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 86 66 82 73 0 1 13 177 0 100 0
 98 219 145 0 0 33 58 0 132 0 1 0 2 0 64 152 177 189 168 187 54
 186 206 187 55 186 207 186 103 187 55 188 215 187 159 186 207
 185 44 187 53 187 56 188 8 187 159 185 149 190 224 188 8 185 250
 186 99 184 90 182]))
    (should (= (emms-info-native--find-and-decode-vbri-header) 8506))))
