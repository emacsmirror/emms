;; maybe not needed, but ensures tests run against the latest changes
(byte-recompile-directory ".")

(mapc #'load
      (mapcar (lambda (f) (file-truename (concat "test/" f)))
              (seq-filter (lambda (f) (string-match "[^.#]+\\test.el$" f)) (directory-files (file-truename "./test")))))
