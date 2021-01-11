;; emms-tracktag-test.el --- Unit tests for emms-tracktag module  -*- lexical-binding: t; -*-

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

;; Tests for emms-tracktag.el

;;; Code:

(require 'emms)
(require 'emms-tracktag)

(ert-deftest emms-tracktag--map-track-info-test ()
  "Ensure mapping of emms info to tracktag fields is correct."
  (let ((track '(*track*
                 (type . file) (name . "foo")
                 (info-album . "The Sounds of the Sounds of Science")
                 (info-artist . "Yo La Tengo")
                 (info-title . "Sea Urchins")
                 (info-date . "2020")
                 (info-tracknumber . "1")))
        (track2 '(*track*
                  (info-composer . "Ira Kaplan, Georgia Hubley, James Mcnew")
                  (info-performer . "Yo La Tengo")
                  (info-year . "2002")
                  (info-discnumber . "1")
                  (info-note . "new soundtrack to an old film"))))
    (should (seq-set-equal-p
             '((album . "The Sounds of the Sounds of Science")
               (artist . "Yo La Tengo")
               (name . "Sea Urchins")
               (year . "2020")
               (number . "1"))
             (emms-tracktag--map-track-info track)))
    (should (seq-set-equal-p
             '((composer . "Ira Kaplan, Georgia Hubley, James Mcnew")
               (performer . "Yo La Tengo")
               (year . "2002")
               (album-number . "1")
               (comment . "new soundtrack to an old film"))
             (emms-tracktag--map-track-info track2)))))

(ert-deftest emms-tracktag--build-args-test ()
  "Ensure args for tracktag are properly formed."
  (let ((track '(*track*
                 (type . file) (name . "foo.flac")
                 (info-album . "The Sounds of the Sounds of Science")
                 (info-artist . "Yo La Tengo")
                 (info-title . "Sea Urchins")
                 (info-date . "2020")
                 (info-tracknumber . "1")
                 (info-composer . "Ira Kaplan, Georgia Hubley, James Mcnew")
                 (info-performer . "Yo La Tengo")
                 (info-discnumber . "1")
                 (info-note . "new soundtrack to an old film"))))
    (should (seq-set-equal-p
             '("--album=The Sounds of the Sounds of Science"
               "--artist=Yo La Tengo"
               "--name=Sea Urchins"
               "--year=2020"
               "--number=1"
               "--composer=Ira Kaplan, Georgia Hubley, James Mcnew"
               "--performer=Yo La Tengo"
               "--album-number=1"
               "--comment=new soundtrack to an old film"
               "foo.flac")
             (emms-tracktag--build-args track)))
    (let ((track-with-empty-strings (copy-alist track)))
      (setcdr (assq 'info-title track-with-empty-strings) "")
      (setcdr (assq 'info-note track-with-empty-strings) "")
      (should (seq-set-equal-p
               '("--album=The Sounds of the Sounds of Science"
                 "--artist=Yo La Tengo"
                 "--year=2020"
                 "--number=1"
                 "--composer=Ira Kaplan, Georgia Hubley, James Mcnew"
                 "--performer=Yo La Tengo"
                 "--album-number=1"
                 "--remove-comment"
                 "--remove-name"
                 "foo.flac")
               (emms-tracktag--build-args track-with-empty-strings))))))

;;; emms-tracktag-test.el ends here
