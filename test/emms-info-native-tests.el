;; -*- lexical-binding: t; -*-

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
    (should (equal (emms-info-native--split-vorbis-comment "a=B") (cons "a" "B")))
    (should (equal (emms-info-native--split-vorbis-comment utf-8-comment)
                   (cons "key" "Οὐχὶ Ταὐτὰ")))))

(ert-deftest emms-test-decode-ogg-page ()
  (let ((bytes [79 103 103 83 0 2 0 0 0 0 0 0 0 0 134 209 158 23 0 0 0 0 53 82 251 136 1 30 1 118 111 114 98 105 115 0 0 0 0 1 68 172 0 0 0 0 0 0 128 56 1 0 0 0 0 0 184 1]))
    (should (equal (emms-info-native--decode-ogg-page bytes)
                   (list :num-packets 1
                         :num-bytes 58
                         :stream [1 118 111 114 98 105 115 0 0 0 0 1 68 172 0 0 0 0 0 0 128 56 1 0 0 0 0 0 184 1 ])))))

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
                      (audio-sample-rate . 44100)
                      (audio-channels . 1)
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
