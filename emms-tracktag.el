;;; emms-tracktag.el --- EMMS interface for audiotools tracktag  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Grant Shoshin Shangreaux

;; Author: Grant Shoshin Shangreaux <grant@churls.world>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a wrapper for audiotools tracktag executable
;; http://audiotools.sourceforge.net/tracktag.html
;; Given an EMMS TRACK structure, it will map the emms-info fields onto
;; arguments for tracktag. Then it calls the tracktag process to write the
;; info as metadata tags on the track's associated file.

;;; Code:

(require 'emms)

(defvar emms-info-tracktag--info-fields
  '((info-album . album)
    (info-artist . artist)
    (info-composer . composer)
    (info-performer . performer)
    (info-year . year)
    (info-date . year)
    (info-tracknumber . number)
    (info-discnumber . album-number)
    (info-note . comment)
    (info-title . name))
  "An alist mapping info-* fields to tracktag fields.")

(defun emms-tracktag--map-track-info (track)
  (seq-filter (lambda (cell) (cdr cell))
              (mapcar (lambda (pair)
                        (cons (cdr pair) (emms-track-get track (car pair))))
                      emms-info-tracktag--info-fields)))

(defun emms-tracktag--build-args (track)
  (flatten-list
   (append (mapcar (lambda (pair)
                     (let ((tag (car pair)) (value (cdr pair)))
                       (when value
                         (if (string-equal value "") (concat "--remove-" (format "%s" tag))
                           (concat "--" (format "%s" tag) "=" value)))))
                   (emms-tracktag--map-track-info track))
           (list (emms-track-name track)))))

(defun emms-tracktag-file (track)
  (apply #'call-process
   "tracktag" nil
   (get-buffer-create emms-tag-editor-log-buffer)
   nil
   "-Vdebug"
   (emms-tracktag--build-args track)))

(provide 'emms-tracktag)
;;; emms-tracktag.el ends here
