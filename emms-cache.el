;;; emms-cache.el --- persistence for emms-track

;; Copyright (C) 2006  Damien Elmes <resolve@repose.cx>

;; Author: Damien Elmes <emacs@repose.cx>
;; Keywords: emms, mp3, mpeg, multimedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The cache is a mapping of a full path name to information, and so
;; it is invalidated when you rename or move files about. It also does
;; not differentiate between file or uri tracks.

;; Because cache lookups are much faster than disk access, this works
;; much better with a later-do-interval of something like 0.001. Also
;; consider using synchronous mode, as it's quite fast now.

;; This code is activated by (emms-standard) and above.

;; To activate it by hand, use:

;; (add-hook 'after-init-hook 'emms-cache-restore)
;; (add-hook 'kill-emacs-hook 'emms-cache-save)
;; (setq emms-cache-get-function 'emms-cache-get)
;; (setq emms-cache-set-function 'emms-cache-set)

;;; Code:

(define-hash-table-test 'string-hash 'string= 'sxhash)
(defvar emms-cache-db (make-hash-table :test 'string-hash)
  "A mapping of paths to file info.
This is used to cache over emacs sessions.")

(defvar emms-cache-file "~/.emms-cache"
  "A file used to store cached file info information over sessions")

(defvar emms-cache-dirty nil
  "True if the cache has been updated since init.")

(defun emms-cache-get (type path)
  "Return a cache element for PATH, or nil."
  (gethash path emms-cache-db))

(defun emms-cache-set (type path track)
  "Set PATH to TRACK in the cache."
  (puthash path track emms-cache-db)
  (setq emms-cache-dirty t))

(defun emms-cache-save ()
  "Save the track cache to a file."
  (when emms-cache-dirty
    (message "Saving emms track cache...")
    (set-buffer (get-buffer-create " emms-cache "))
    (erase-buffer)
    (maphash (lambda (k v)
               (insert (format
                        "(puthash %S '%S emms-cache-db)\n" k v)))
             emms-cache-db)
    (set-buffer-file-coding-system 'mule-utf-8)
    (write-region (point-min) (point-max) emms-cache-file)
    (kill-buffer (current-buffer))
    (message "Saving emms track cache...done")
    (setq emms-cache-dirty nil)))

(defun emms-cache-restore ()
  "Restore the track cache from a file."
  (load emms-cache-file t nil t)
  (setq emms-cache-dirty nil))

(provide 'emms-cache)
;;; emms-cache.el ends here
