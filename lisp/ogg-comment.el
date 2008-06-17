;;;  ogg-comment.el --- Read Ogg-Vorbis file headers.

;; Copyright (C) 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.

;; Filename: ogg-comment.el
;; Version: $Revision: 1.5 $
;; Author: lawrence mitchell <wence@gmx.li>
;; Maintainer: lawrence mitchell <wence@gmx.li>
;; Created: 2003-09-26
;; Keywords: music

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more
;; details. http://www.gnu.org/copyleft/gpl.html
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If you did not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:
;; This file provides a minimal interface to reading the "comment"
;; section from an Ogg-Vorbis stream as defined in <URL:
;; http://www.xiph.org/ogg/vorbis/doc/Vorbis_I_spec.html>
;; It relies on all the comments being in the first 28kilobytes of
;; the file, thus removing the need to read the whole ogg file into
;; an Emacs buffer.

;; The implementation is rather "byte-oriented", due to the way the
;; Ogg-Vorbis file headers are specified.  Any improvements in making
;; the implementation more emacsy would be welcomed.

;;; Installation:
;; To use, put this file somewhere in your `load-path' and do
;; (require 'ogg-comment).
;; You can then read ogg comments from a file by doing:
;; M-x oggc-show-header RET.

;;; History:
;;

;;; TODO:
;; o Read setup header, to get bitrate and such like.
;; o Make writing comments possible.

;;; Code:
(eval-when-compile
 (defvar it)
 (require 'cl))

(defconst oggc-ogg-header "OggS"
  "The string indicating the start of an Ogg stream.")

(defconst oggc-identification-header "\001vorbis"
  "The string indicating the start of the Ogg identification header.")

(defconst oggc-comment-header "\003vorbis"
  "The string indicating the start of the Ogg comment header.")

(defconst oggc-setup-header "\005vorbis"
  "The string indicating the start of the Ogg setup header.")

(defconst oggc-code-book-pattern "BCV"
  "The string indicating the start of an Ogg code book.")

(defconst oggc-version "$Revision: 1.5 $"
  "Ogg-comment's version number.")

(defmacro with-part-of-file (file-spec &rest body)
  "Execute BODY in a buffer containing part of FILE.

BEG and END are as `insert-file-contents' (q.v.).

\(fn (FILE &optional BEG END) &rest BODY)"
  (let (file beg end)
    (setq file (pop file-spec))
    (and file-spec (setq beg (pop file-spec)))
    (and file-spec (setq end (pop file-spec)))
    `(with-temp-buffer
       (insert-file-contents-literally ,file nil ,beg ,end)
       (goto-char (point-min))
       ,@body)))

(defmacro aif (test-form then &rest else)
  "Like `if', but with `it' bound to the result of TEST-FORM.
`it' is accessible in the THEN and ELSE clauses.

Warning, non-hygienic by design.

\(fn TEST-FORM THEN &rest ELSE)"
  `(let ((it ,test-form))
     (if it
         ,then
       ,@else)))

(defun oggc-split-comment (comment)
  "Split Ogg COMMENT into a (name, value) pair.

If possible (`ccl-execute-on-string' and `ccl-decode-mule-utf-8'
available), COMMENT is decoded into utf-8.

The name-part is converted to lowercase, to make sure case-differences
are ignored."
  (setq comment (split-string comment "="))
  (list (downcase (car comment))
        (oggc-decode-utf-8 (or (cadr comment)
                               ""))))

(defun oggc-encode-utf-8 (string)
  "Encode STRING into utf-8."
  (if (and (fboundp 'ccl-execute-on-string)
           (boundp 'ccl-encode-mule-utf-8))
      (ccl-execute-on-string ccl-encode-mule-utf-8
                             (make-vector 9 nil)
                             string)
    string))

(defun oggc-decode-utf-8 (string)
  "Decode STRING from utf-8."
  (if (and (fboundp 'ccl-execute-on-string)
           (boundp 'ccl-decode-mule-utf-8))
      (ccl-execute-on-string ccl-decode-mule-utf-8
                             (make-vector 9 nil)
                             string)
    string))
      
(defun oggc-read-string (length)
  "Read a string from `point' of LENGTH characters.

Advances to (+ LENGTH (point))."
  (buffer-substring-no-properties
   (point) (goto-char (+ length (point)))))

(defun oggc-valid-ogg-stream-p ()
  "Return non-nil if the current buffer contains a valid Ogg-Vorbis stream."
  (or (search-forward oggc-ogg-header (min 100 (point-max)) t)
      (error "File does not appear to be a valid ogg stream"))
  (or (search-forward oggc-identification-header (min 300 (point-max)) t)
      (error "Not a valid ogg stream")))

(defun oggc-comment-exists-p ()
  "Return the value of `point' where comments are found in the current buffer."
  (let ((max (save-excursion
               (search-forward oggc-setup-header nil t)
               (point))))
    (and (search-forward oggc-comment-header max t)
         (point))))

(defun oggc-bytes-to-lsb-int (n)
  "Read N bytes as a LSB integer."
  (loop for i from 0 below n
       sum (* (expt 256 i)
              (prog1 (char-after)
                (forward-char 1)))))

(defun oggc-int-to-lsb-bytes (int n)
  "Return a list of N bytes encoding INT as a LSB integer."
  (nreverse (loop for i downfrom (1- n) to 0
               for exp = (expt 256 i)
               collect (floor int exp)
               when (<= exp int)
               do (setq int (/ int exp)))))

(defun oggc-construct-comment-field (comment-list)
  "Construct an Ogg-Vorbis comment header from COMMENT-LIST.

COMMENT-LIST should be of the form (TITLE VALUE).
VALUE is encoded into UTF-8 if possible (`ccl-execute-on-string' and
`ccl-decode-mule-utf-8' available).  The length of the thus ensuing
comment header is prepended to the string as a 4-byte lsb int."
  (let* ((title (pop comment-list))
         (value (pop comment-list)))
    (setq title (concat title "="
                        (oggc-encode-utf-8 value)))
    (concat (oggc-int-to-lsb-bytes (length title) 4)
            title)))

(defun oggc-construct-vendor (vendor)
  "Construct a vendor string from VENDOR."
  (concat (oggc-int-to-lsb-bytes (length vendor) 4)
          vendor))

;;; FIXME: This doesn't work!!
;;; Somehow, we need to modify one of the code-book headers to make
;;; note of the fact that the comment has changed.  I can't see in
;;; the spec what needs to be done.
;;; This doesn't work even for the case where we don't change the
;;; length of the comment, just one character, e.g. tracknumber=1 to
;;; tracknumber=2.
(defun oggc-write-comments (file comments)
  "Write COMMENTS to FILE.

COMMENTS should be as for `oggc-construct-comment-string' (q.v.)."
  (with-temp-buffer
    ;; dog slow for large files.
    ;; an alternative would be to use head/tail/cut as needed to
    ;; split the file up and put it back together again.
    (insert-file-contents-literally file)
    (when (oggc-valid-ogg-stream-p)
      (when (oggc-comment-exists-p)
        (let ((vendor (save-excursion (oggc-read-vendor))))
          (delete-region (point) (progn (oggc-read-comments (point))
                                        (point)))
          (insert (oggc-construct-vendor vendor)
                  (oggc-construct-comment-string comments))))
      (write-region nil nil file))))
  
(defun oggc-construct-comment-string (comments)
  "Construct a string off Ogg-Vorbis comment headers from COMMENTS.

COMMENTS should be an alist of the form:
 ((TITLE-1 VALUE-1)
  (TITLE-2 VALUE-2))"
  (concat (oggc-int-to-lsb-bytes (length comments) 4)
          (mapconcat #'oggc-construct-comment-field comments "")))

(defun oggc-read-vendor ()
  "Read an Ogg-Vorbis vendor string from the current buffer."
  (let ((length (oggc-bytes-to-lsb-int 4)))
    (oggc-read-string length)))

(defun oggc-read-comments (pos)
  "Read Ogg-Vorbis comments, starting POS bytes from `point-min'."
  (goto-char pos)
  (let ((vendor (oggc-read-vendor))
        (length (oggc-bytes-to-lsb-int 4))
        comments)
    (loop repeat length
       for this-length = (oggc-bytes-to-lsb-int 4)
       for c = (oggc-read-string this-length) do
         (push (oggc-split-comment c) comments))
    (list vendor (nreverse comments))))

(defun oggc-read-header (file)
  "Read an Ogg-Vorbis header from FILE."
  (with-part-of-file (file 0
                           ;; Lets hope that the comments
                           ;; aren't more than 28KB long.
                           (* 1024 28))
    (when (oggc-valid-ogg-stream-p)
      (aif (oggc-comment-exists-p)
           (oggc-read-comments it)))))

(defun oggc-pretty-print-header (header)
  "Print Ogg HEADER readably in a temporary buffer."
  (let ((vendor (car header))
        (comments (cadr header)))
    (switch-to-buffer (get-buffer-create "*comments*"))
    (erase-buffer)
    (insert "Vendor: "vendor "\n")
    (mapc #'(lambda (s)
              (insert (car s) ": " (cadr s) "\n"))
          comments)))

;;;###autoload
(defun oggc-show-header (file)
  "Show a pretty printed representation of the Ogg Comments in FILE."
  (interactive "FFile: ")
  (oggc-pretty-print-header (oggc-read-header file)))

(provide 'ogg-comment)

;;; ogg-comment.el ends here
