;;; emms-idapi-browser.el --- EMMS Music ID API support  -*- lexical-binding: t; -*-
;;

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Yoni Rabkin <yrk@gnu.org>

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
;;

(defvar emms-idapi-browser-debug-name
  " *Emms Search Debug Browser*"
  "Name of the search browser debug buffer")

(defvar emms-idapi-browser-debug-buffer
  nil
  "Search browser debug buffer")

(defvar emms-idapi-browser-name
  "Emms Search Browser"
  "Name of the search browser buffer")

(defvar emms-idapi-browser-buffer
  nil
  "Search browser buffer")

(defvar emms-idapi-browser-mode-hook nil
  "Emms search browser mode hook.")

(defvar emms-idapi-browser-field-alist
  '(("artist" . info-artist)
    ("album"  . info-album)
    ("track"  . info-title))
  "Association list of readable fields and track properties.")

;;; Code:
(require 'emms-idapi)


;;; ------------------------------------------------------------------
;;; Search Mode
;;; ------------------------------------------------------------------
(defun emms-idapi-browser-get-buffer ()
  "Get/create and return `emms-idapi-browser-mode' buffer."
  (when (or (not emms-idapi-browser-buffer)
	    (not (buffer-live-p emms-idapi-browser-buffer)))
    (with-current-buffer (get-buffer-create emms-idapi-browser-name)
      (when (not (equal major-mode 'emms-idapi-browser-mode))
	(emms-idapi-browser-mode))))
  emms-idapi-browser-buffer)

(defvar emms-idapi-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'bury-buffer)
    map)
  "Keymap for `emms-idapi-browser-mode'.")

(defun emms-idapi-browser-mode ()
  "A major mode for the Emms search browser.
\\{emms-idapi-browser-mode-map}"
  (interactive)

  (use-local-map emms-idapi-browser-mode-map)

  (setq major-mode 'emms-idapi-browser-mode
        mode-name "Emms-Search-Browser"
	buffer-read-only t
	emms-idapi-browser-buffer (current-buffer))

  (run-hooks 'emms-idapi-browser-mode-hook))


;;; ------------------------------------------------------------------
;;; Call
;;; ------------------------------------------------------------------
(defun emms-idapi-browser-track-at ()
  "Return a copy of the track at point."
  (let* ((originial-track (emms-playlist-track-at (point)))
	 (track (copy-sequence originial-track)))
    (when (not (emms-track-p track))
      (error "could not read Emms track at point"))
    track))

(defun emms-idapi-browser-search-recording-artist (track)
  "Search for the recording and artist of TRACK."
  (let ((recording (alist-get 'info-title track))
	(artist (or (alist-get 'info-artist track)
		    (alist-get 'info-albumartist track))))
    (list
     (cons 'info-title (read-string "search for recording (track): " recording))
     (cons 'info-artist (read-string "search for artist: " artist)))))

(defun emms-idapi-browser-search-recording (track)
  "Search for the recording of TRACK."
  (let ((recording (alist-get 'info-title track)))
    (list
     (cons 'info-title (read-string "search for recording (track): " recording)))))

(defun emms-idapi-browser-search-artist (track)
  "Search for the artist of TRACK."
  (let ((artist (or (alist-get 'info-artist track)
		    (alist-get 'info-albumartist track))))
    (list
     (cons 'info-artist (read-string "search for artist: " artist)))))

(defun emms-idapi-browser-search-album (track)
  "Search for the album of TRACK."
  (let ((album (alist-get 'info-album track)))
    (list
     (cons 'info-album (read-string "search for album: " album)))))

(defun emms-idapi-browser-search-album-artist (track)
  "Search for both artist and album of TRACK."
  (let ((artist (or (alist-get 'info-artist track)
		    (alist-get 'info-albumartist track)))
	(album (alist-get 'info-album track))
	search-album)
    (setq search-album (read-string "search for album: " album))
    (list
     (cons 'info-album search-album)
     (cons 'info-artist (read-string
			 (format "search for album \"%s\" by artist: " search-album)
			 artist)))))

(defun emms-idapi-browser-search-recording-artist-at ()
  "Search for the recording and artist of the track at point."
  (interactive)
  (emms-idapi-browser-show
   (emms-idapi-search emms-idapi-service
		      (emms-idapi-browser-search-recording-artist
		       (emms-playlist-track-at (point))))))

(defun emms-idapi-browser-search-recording-at ()
  "Search for the recording of the track at point."
  (interactive)
  (emms-idapi-browser-show
   (emms-idapi-search emms-idapi-service
		      (emms-idapi-browser-search-recording
		       (emms-playlist-track-at (point))))))

(defun emms-idapi-browser-search-artist-at ()
  "Search for the artist of the track at point."
  (interactive)
  (emms-idapi-browser-show
   (emms-idapi-search emms-idapi-service
		      (emms-idapi-browser-search-artist
		       (emms-playlist-track-at (point))))))

(defun emms-idapi-browser-search-album-at ()
  "Search for the album of the track at point."
  (interactive)
  (emms-idapi-browser-show
   (emms-idapi-search emms-idapi-service
		      (emms-idapi-browser-search-album
		       (emms-playlist-track-at (point))))))

(defun emms-idapi-browser-search-album-artist-at ()
  "Search for the album and artist of the track at point."
  (interactive)
  (emms-idapi-browser-show
   (emms-idapi-search emms-idapi-service
		      (emms-idapi-browser-search-album-artist
		       (emms-playlist-track-at (point))))))

;;; ------------------------------------------------------------------
;;; Response
;;; ------------------------------------------------------------------
(defun emms-idapi-browser-write-debug (response)
  "Write RESPONSE to the browser debug buffer."
  (let ((buffer (get-buffer-create emms-idapi-browser-debug-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "%s" response))
      (setq emms-idapi-browser-debug-buffer buffer))))

(defun emms-idapi-browser-print-header (header)
  "Print the material for the search HEADER."
  (let ((artist (alist-get 'info-artist header))
	(album  (alist-get 'info-album header))
	(title  (alist-get 'info-title header))
	(service (alist-get emms-idapi-service emms-idapi-services-alist)))
    (when (not (or artist album title))
      (error "could not read header: %s" header))
    (insert (format "service: %s (%s)\n"
		    (alist-get 'name service)
		    (alist-get 'website service)))
    (when artist
      (insert (format "artist:  %s\n" artist)))
    (when album
      (insert (format "album:   %s\n" album)))
    (when title
      (insert (format "title:   %s\n" title)))
    (insert "\n")))

(defun emms-idapi-browser-entry-packaging (entry)
  "Print packaging information for ENTRY."
  (let ((packaging (alist-get 'info-packaging entry)))
    (if (and packaging
	     (not (string= "None" packaging)))
	(format ", %s" packaging)
      "")))

(defun emms-idapi-browser-print-entry-artist (entry)
  "Return artist ENTRY."
  (format "%s%s%s\n\n"
	  (alist-get 'info-artist entry)
	  (if (alist-get 'info-country entry)
	      (format " (%s) " (alist-get 'info-country entry))
	    "")
	  (let ((begin (alist-get 'begin (alist-get 'info-time entry)))
		(end (alist-get 'end (alist-get 'info-time entry))))
	    (format "%s%s"
		    (if begin begin "")
		    (if end (format " - %s, " end) "")))))

(defun emms-idapi-browser-print-entry (entry)
  "Print ENTRY."
  (cond ((equal 'idapi-release (alist-get 'type entry))
	 (insert (format "\"%s\" by %s%s\n"
			 (alist-get 'info-album entry)
			 (alist-get 'info-artist entry)
			 (if (alist-get 'info-date entry)
			     (format ", released on %s" (alist-get 'info-date entry))
			   "")))

	 (insert (format "%s tracks%s%s\n\n"
			 (alist-get 'info-track-count entry)
			 (emms-idapi-browser-entry-packaging entry)
			 (if (alist-get 'info-country entry)
			     (format ", (%s)" (alist-get 'info-country entry))
			   ""))))

	((equal 'idapi-artist (alist-get 'type entry))
	 (insert (emms-idapi-browser-print-entry-artist entry)))

	(t (insert (format  "unhandled entry:\n\n%s\n" entry)))))

(defun emms-idapi-browser-show (response)
  "Display RESPONSE in a search buffer."
  (emms-idapi-browser-write-debug response)
  (let ((buffer (emms-idapi-browser-get-buffer)))
    (pop-to-buffer buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Emms Music Search Query\n\n")
      (emms-idapi-browser-print-header (car response))
      (insert (format "%d results\n\n" (length (cdr response))))
      (dolist (e (cdr response))
	(emms-idapi-browser-print-entry e)))))


(provide 'emms-idapi-browser)

;;; emms-idapi-browser.el ends here
