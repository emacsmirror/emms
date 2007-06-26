;;; emms-i18n.el --- Function for handling coding system

;; Copyright (C) 2006, 2007 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@163.com>

;; This file is part of EMMS.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; When read from process, first check the CAR part of
;; `emms-i18n-default-coding-system', if non-nil, use this for decode, and
;; nerver detect coding system, if nil, first call
;; `emms-i18n-coding-dectect-functions' to get coding system, if success,
;; decode the result, otherwise, use `emms-i18n-detect-coding-function',
;; the emacs detect coding function, if the coding detected is not in
;; `emms-i18n-nerver-used-coding-system', decode it, otherwise use
;; locale-coding-system.
;;
;; When write send data to process, first check the CDR part of
;; `emms-i18n-default-coding-system', if non-nil, use this to encode data,
;; otherwise do nothing, that means use `default-process-coding-system' or
;; `process-coding-system-alist' to encode data.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'emms-i18n-i18n)

;;; Code:

(provide 'emms-i18n)
(eval-when-compile
  (require 'cl))

;; TODO: Change these to use defcustom

(defvar emms-i18n-nerver-used-coding-system
  '(raw-text undecided)
  "If the `emms-i18n-coding-dectect-functions' return coding system in
this list, use `emms-i18n-default-coding-system' instead.")

(defvar emms-i18n-coding-system-for-read 'utf-8
  "If coding detect failed, use this for decode")

(defvar emms-i18n-default-coding-system nil
  "If non-nil, used for decode and encode")

(defvar emms-i18n-coding-dectect-functions nil
  "A list of function to call to detect codings")

(defvar emms-i18n-detect-max-size 10000
  "Max bytes to detect coding system. Nil mean scan whole buffer.")

(defun emms-i18n-iconv (from to str)
  "Convert STR from FROM coding to TO coding."
  (if (and from to)
      (decode-coding-string
       (encode-coding-string str to)
       from)
    str))

(defun emms-i18n-iconv-region (beg end from to)
  (when (and from to)
    (save-restriction
      (narrow-to-region beg end)
      (encode-coding-region (point-min) (point-max) to)
      (decode-coding-region (point-min) (point-max) from))))

(defun emms-i18n-iconv-buffer (from to &optional buf)
  (save-excursion
    (and buf (set-buffer buf))
    (emms-i18n-iconv-region (point-min) (point-max) from to)))

(defun emms-i18n-set-default-coding-system (read-coding write-coding)
  "Set `emms-i18n-default-coding-system'"
  (interactive "zSet coding system for read: \nzSet coding system for write: ")
  (setq emms-i18n-default-coding-system
        (cons
         (and (coding-system-p read-coding) read-coding)
         (and (coding-system-p write-coding) write-coding)))
  (message (concat
            (if (car emms-i18n-default-coding-system)
                (format "The coding system for read is %S." (car emms-i18n-default-coding-system))
              "Good, you want detect coding system by me!")
            (format " The coding system for write is %S."
                    (or (cdr emms-i18n-default-coding-system)
                        (cdr default-process-coding-system))))))

(defun emms-i18n-call-process-simple (&rest args)
  "This function run program and return the program result. If the CAR
part of `emms-i18n-default-coding-system' is non-nil, the program result will
be decode use the CAR part of emms-i18n-default-coding-system. Otherwise,
use `emms-i18n-coding-dectect-functions' to detect the coding system of the
result. If the emms-i18n-coding-dectect-functions failed, use
`emms-i18n-detect-coding-function' to detect coding system. If all the
coding system is nil or in `emms-i18n-nerver-used-coding-system', decode
the result using `emms-i18n-coding-system-for-read'.

The rest arguments ARGS is as the same as `call-process', except the
BUFFER should always have value t. Otherwise the coding detection will
not perform."
  (let ((default-process-coding-system (copy-tree default-process-coding-system))
        (process-coding-system-alist nil) exit pos)
    (when (eq (nth 2 args) 't)
      (setcar default-process-coding-system (car emms-i18n-default-coding-system))
      (setq pos (point)))
    (setq exit (apply 'call-process args))
    (when (and (eq (nth 2 args) 't)
               (null (car emms-i18n-default-coding-system)))
      (save-restriction
        (narrow-to-region pos (point))
        (decode-coding-region (point-min) (point-max) (emms-i18n-detect-buffer-coding-system))))
    exit))

;; Is this function useful?
(defun emms-i18n-call-process (&rest args)
  "Run the program like `call-process'. If
the cdr part `emms-i18n-default-coding-system' is non-nil, the string in
ARGS will be encode by the CDR part of `emms-i18n-default-coding-system',
otherwise, it is pass all parameter to `call-process'."
  (with-temp-buffer
    (if (cdr emms-i18n-default-coding-system)
        (let ((default-process-coding-system emms-i18n-default-coding-system)
              (process-coding-system-alist nil))
          (apply 'call-process args))
      (apply 'call-process args))))

(defun emms-i18n-detect-coding-function (size)
  (detect-coding-region (point)
                        (+ (if (null emms-i18n-detect-max-size)
                               size
                             (min size emms-i18n-detect-max-size))
                           (point)) t))

(defun emms-i18n-detect-buffer-coding-system (&optional buf)
  "Before call this function, make sure the buffer is literal"
  (let ((size (- (point-max) (point-min)))
        (func (append emms-i18n-coding-dectect-functions 'emms-i18n-detect-coding-function))
        coding)
    (save-excursion
      (and buf (set-buffer buf))
      (goto-char (point-min))
      (when (> size 0)
        (setq coding (run-hook-with-args-until-success 'func size))
        (if (member (coding-system-base coding) emms-i18n-nerver-used-coding-system)
            (setq coding (emms-i18n-detect-coding-function size))))
      (if (or (null coding) (member (coding-system-base coding) emms-i18n-nerver-used-coding-system))
          emms-i18n-coding-system-for-read
        coding))))

;;; emms-i18n.el ends here
