;;; emms-source-beets.el --- EMMS source utilizing a beets library database -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Daniel Semyonov <daniel@dsemy.com>

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

;;; Commentary:

;; This file provides an EMMS source which utilizes Emacs' SQLite
;; support to read a beets library database.

;;; Code:

(eval-when-compile
  (require 'subr-x)

  (declare-function sqlite-next "sqlite.c")
  (declare-function sqlite-open "sqlite.c")
  (declare-function sqlite-select "sqlite.c"))

(require 'emms)

(defgroup emms-source-beets nil
  "EMMS source for beets library databases."
  :group 'emms-source
  :prefix "emms-source-beets-")

(defcustom emms-source-beets-database
  (expand-file-name "beets/library.db"
                    (or (getenv "XDG_CONFIG_HOME") "~/.config"))
  "Database containing beets library information."
  :type '(file :must-match t))

(defconst emms-source-beets--items-columns
  '("title" "artist" "artist_sort" "album" "albumartist"
    "albumartist_sort" "genre" "composer" "composer_sort" "year" "track"
    "tracktotal" "disc" "disctotal" "label" "original_year" "length")
  "Columns to process in the \"items\" table.")

(defcustom emms-source-beets-sort-columns
  '(("albumartist_sort") ("album") ("track"))
  "List of columns to sort by when adding tracks from a beets database.
Each \"column\" should be cons cell whose car is the column name (a
string), and the cdr, if non-nil, indicates a descending sort order
for the column."
  :type `( repeat
           (cons :tag "Sort"
                 (radio :tag "Column"
                        ,@(mapcar (lambda (col) `(const ,col))
                                  emms-source-beets--items-columns))
                 (boolean :tag "Descending"))))

(defun emms-source-beets--ensure-sqlite ()
  "Emit a user error if SQLite support is not available."
  (or (and (fboundp 'sqlite-available-p)
           (sqlite-available-p))
      (user-error
       "SQLite support is not available (required to read beets databases)")))

;;;###autoload (autoload 'emms-play-beets "emms-source-beets" nil t)
;;;###autoload (autoload 'emms-add-beets "emms-source-beets" nil t)
(define-emms-source beets (&optional database filter sort)
  "An EMMS source for beets library databases.

DATABASE should be a path to a beets library database
\(`emms-source-beets-database' is used by default).
When called interactively, prefix argument FILTER will cause added
tracks to be filtered according to unique values from columns in the
\"items\" table of the database; with a double prefix argument (or
more), SORT will also be set interactively, defaulting to the value of
`emms-source-beets-sort-columns' otherwise.

Filtering is done in two steps:
- Choose column(s) (with completion).
- For each chosen column (in order), choose from its unique values
  (with completion) which match any row which hasn't been filtered by
  a previous choice.

For example, if you have 5 albums:
Nice Band - Nice Album (2001)
Nice Band - Good Album (2002)
Cool Band - Cool Album (2001)
Cool Band - Cold Album (2002)
Cool Band - Warm Album (2003)

Then:

\\[universal-argument] \\[emms-play-beets] year \\`RET' 2001 \\`RET'

will play \"Nice Album\" and \"Cool Album\".  The year chosen must be
one of 2001, 2002 or 2003 (or any combination of them).

\\[universal-argument] \\[emms-add-beets] albumartist,year \
\\`RET' Nice Band \\`RET' 2002 \\`RET'

will add only \"Good Album\".  Since the first choice was \"Nice
Band\", the choice of year is restricted to 2001 to 2002 (or both).

Sorting occurs after filtering, and allows selecting multiple columns
to sort by."
  (interactive
   (when-let (((emms-source-beets--ensure-sqlite))
              (filter (and current-prefix-arg '(nil . "")))
              (sort (prog1 (or current-prefix-arg t)
                        ;; Unset after use unconditionally to prevent
                        ;; EMMS's default behavior when source
                        ;; commands are called with a prefix argument.
                        (setq current-prefix-arg nil prefix-arg nil)))
              (db (sqlite-open emms-source-beets-database))
              (dec " (descending)"))
     (dolist ( col (completing-read-multiple
                    "Filter by: "
                    emms-source-beets--items-columns nil t))
       ;; For each column chosen to filter by, only allow
       ;; choosing between distinct values which correspond
       ;; to items which matched distinct values chosen for
       ;; previously processed columns.
       (when-let ((where (cdr filter))
                  (dist (sqlite-select
                         db (format "select distinct %s from items%s"
                                    col (if (string-empty-p where)
                                            "" (concat " where" where)))
                         (car filter)))
                  (dist (if (stringp (caar dist)) dist
                          (mapcar (lambda (val) (number-to-string (car val)))
                                  dist))))
         (setcdr filter (format " %s in (%s)%s" col
                                (mapconcat
                                 (lambda (_) "?")
                                 (mapcar (lambda (val) (push val (car filter)))
                                         (completing-read-multiple
                                          (concat col ": ") dist nil t))
                                 ", ")
                                (if (string-empty-p where) ""
                                  (concat " and " where))))))
     (list db filter
           (and (> (prefix-numeric-value sort) 4) ; more than one C-u
                (mapcan
                 (lambda (c)
                   (list (if (string-suffix-p dec c)
                             (cons (string-remove-suffix dec c) t)
                           (list c))))
                 (completing-read-multiple
                  "Sort added tracks by: "
                  `(,@emms-source-beets--items-columns
                    ,@(mapcar (lambda (c) (concat c dec))
                              emms-source-beets--items-columns))
                  nil t))))))
  (when-let (((emms-source-beets--ensure-sqlite))
             (db (or database (sqlite-open emms-source-beets-database)))
             (filter (or filter '(nil . "")))
             (where (cdr filter))
             (sort (or sort emms-source-beets-sort-columns))
             (db (sqlite-select
                  db (format "select path, %s from items%s order by %s"
                             (mapconcat #'identity
                                        emms-source-beets--items-columns
                                        ", ")
                             (if (string-empty-p where) ""
                               (concat " where" where))
                             (mapconcat
                              (lambda (col)
                                (if (cdr col) (concat (car col) " desc")
                                  (car col)))
                              sort ", "))
                  (car filter) 'set))
             (init (gensym)))
    (set init (remq 'emms-info-initialize-track
                    emms-track-initialize-functions))
    (let (item track path)
      (while (and (setq item (sqlite-next db))
                  (setq track (emms-dictionary '*track)))
        (when (setq path (decode-coding-string (car item) 'utf-8 t))
          (emms-dictionary-set track 'type 'file)
          (emms-dictionary-set track 'name path)
          (mapc (lambda (type)
                  (when-let ((val (car (setq item (cdr item)))))
                    (setq type (cond ((string-match "_" type)
                                      (replace-match "" nil nil type))
                                     ((member type '("track" "disc"))
                                      (concat type "number"))
                                     (t type)))
                    (if (and (string= type "length") (numberp val))
                        (progn (setq val (ceiling val))
                               (emms-dictionary-set
                                track 'info-playing-time-min (/ val 60))
                               (emms-dictionary-set
                                track 'info-playing-time-sec (% val 60))
                               (emms-dictionary-set
                                track 'info-playing-time val))
                      (when (numberp val) (setq val (number-to-string val)))
                      (unless (string-blank-p val)
                        (emms-dictionary-set
                         track (intern (concat "info-" type)) val)))))
                emms-source-beets--items-columns)
          (run-hook-with-args init track)
          (emms-playlist-insert-track track)
          (when (fboundp emms-cache-modified-function)
            (funcall emms-cache-modified-function track))
          (when (fboundp emms-cache-set-function)
            (funcall emms-cache-set-function 'file path track)))))))

(provide 'emms-source-beets)

;;; emms-source-beets.el ends here
