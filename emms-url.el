;;; emms-url.el --- Make URL and EMMS work together well

;; Copyright (C) 2006, 2007 Free Software Foundation, Inc.

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; These routines sanify input to URL and parse data returned by URL.

;;; Code:

(require 'url)

(defvar emms-url-specials
  '((?\  . "%20")
    (?\n . "%0D%0A")
    (?&  . "%26")
    (??  . "%3F"))
  "*An alist of characters which must be represented specially in URLs.
The transformation is the key of the pair.")

(defun emms-escape-url (url)
  "Escape specials in URL.

The specials to escape are specified by the `emms-url-specials'
variable."
  (apply (function concat)
         (mapcar
          (lambda (ch)
            (let ((repl (assoc ch emms-url-specials)))
              (if (null repl)
                  (char-to-string ch)
                (cdr repl))))
          (append url nil))))

(defun emms-http-content-coding ()
  (save-match-data
    (and (boundp 'url-http-content-type)
         (stringp url-http-content-type)
         (string-match ";\\s-*charset=\\([^;[:space:]]+\\)"
                       url-http-content-type)
         (intern-soft (downcase (match-string 1 url-http-content-type))))))

(defun emms-http-decode-buffer (&optional buffer)
  "Recode the buffer with `url-retrieve's contents. Else the
buffer would contain multibyte chars like \\123\\456."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((default (or (car default-process-coding-system) 'utf-8))
           (coding  (or (emms-http-content-coding) default)))
      (when coding
        ;; (pop-to-buffer (current-buffer))
        ;; (message "content-type: %s" url-http-content-type)
        ;; (message "coding: %S [default: %S]" coding default)
        (set-buffer-multibyte t)
        (decode-coding-region (point-min) (point-max) coding)))))

(provide 'emms-url)
;;; emms-url.el ends here
