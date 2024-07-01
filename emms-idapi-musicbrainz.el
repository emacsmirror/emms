;;; emms-idapi-musicbrainz.el --- EMMS MusicBrainz API support  -*- lexical-binding: t; truncate-lines: t; -*-
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

;;; Code:
(require 'mm-bodies)
(require 'url-vars)
(require 'json)
(require 'emms)


(defvar emms-idapi-musicbrainz-url-timeout-seconds 7
  "Number of seconds to wait before an API call times out.")

(defvar emms-idapi-musicbrainz-url-buffer nil
  "Buffer to store `url' response.")

(defvar emms-idapi-musicbrainz-response-limit 100
  "Maximum number of responses to ask for. Maximum is 100.")

(defconst emms-idapi-musicbrainz-root-url "https://musicbrainz.org/ws/2/"
  "The API root URL for the MusicBrainz service.")

(defvar emms-idapi-query-local nil
  "Cross-call storage for search query.")
(make-variable-buffer-local 'emms-idapi-query-local)

(defvar emms-idapi-musicbrainz-debug-buffer-name
  " *Emms MusicBrainz Debug Buffer*"
  "Name of debug buffer for MusicBrainz url responses.")

(defvar emms-idapi-musicbrainz-debug-buffer nil
  "Debug buffer for MusicBrainz url responses.")


(defconst emms-idapi-musicbrainz-search-string-map
  '((info-artist      . "artist")
    (info-albumartist . "artist")
    (info-title       . "recording")
    (info-album       . "release"))
  "Mapping between MusicBrainz API TYPES and Emms fields.")


;;; ------------------------------------------------------------------
;;; Response
;;; ------------------------------------------------------------------
(defun emms-idapi-musicbrainz-decode (response)
  "Decode the strings in response."
  (mapc
   (lambda (elt)
     (when (stringp (cdr elt))
       (setf (cdr elt) (mm-decode-string (cdr elt) 'utf-8))))
   response))

(defun emms-idapi-musicbrainz-read-artist (artist)
  "Return a track from the MusicBrainz ARTIST."
  (when (not (alist-get 'id artist))
    (error "could not parse from: %s" artist))
  `(*track* (search-backend    . musicbrainz)
            (type              . idapi-artist)
	    (name              . nil)
	    (idapi-artist-id   . ,(list (cons 'musicbrainz (alist-get 'id artist))))
	    (info-artist       . ,(alist-get 'name artist))
	    (info-gender       . ,(alist-get 'gender artist))
	    (info-type         . ,(alist-get 'type artist))
	    (info-country      . ,(alist-get 'country artist))
	    (info-area-type    . ,(alist-get 'type (cddr (assoc 'area artist))))
	    (info-area-country . ,(alist-get 'name (cddr (assoc 'area artist))))
	    (info-aliases      . ,(list
				   (seq-map
				    (lambda (elt)
				      (mm-decode-string
				       (alist-get 'sort-name elt) 'utf-8))
				    (alist-get 'aliases artist))))
	    (info-tags         . ,(list
				   (seq-map
				    (lambda (elt)
				      (mm-decode-string
				       (alist-get 'name elt) 'utf-8))
				    (alist-get 'tags artist))))
	    (info-time         . ,(alist-get 'life-span artist))))

(defun emms-idapi-musicbrainz-read-release (release)
  "Return a track from the MusicBrainz RELEASE."
  (when (not (alist-get 'id release))
    (error "could not parse from: %s" release))
  `(*track* (search-backend . musicbrainz)
	    (type                . idapi-release)
	    (name                . nil)
	    (idapi-release-id    . ,(list (cons 'musicbrainz (alist-get 'id release))))
	    (info-artist         . ,(alist-get 'name (elt (alist-get 'artist-credit release) 0)))
	    (info-album          . ,(alist-get 'title release))
	    (info-status         . ,(alist-get 'status release))
	    (info-disambiguation . ,(alist-get 'disambiguation release))
	    (info-packaging      . ,(alist-get 'packaging release))
	    (info-date           . ,(alist-get 'date release))
	    (info-country        . ,(alist-get 'country release))
	    (info-track-count    . ,(alist-get 'track-count release))))

(defun emms-idapi-musicbrainz-read-recording (recording)
  "Return a track from the MusicBrainz RECORDING."
  (when (not (alist-get 'id recording))
    (error "could not parse from: %s" recording))
  (let ((length-ms (or (alist-get 'length recording) 0)))
    `(*track* (search-backend . musicbrainz)
	      (type                  . idapi-recording)
	      (name                  . ,(alist-get 'title recording))
	      (info-playing-time     . ,(floor (/ length-ms 1000)))
	      (info-playing-time-min . ,(floor (/ (/ length-ms 1000) 60)))
	      (info-playing-time-sec . ,(% (floor (/ length-ms 1000)) 60))
	      (info-recording-id     . ,(alist-get 'id recording))
	      (idapi-releases 	     . ,(seq-map
					 (lambda (elt)
					   (emms-idapi-musicbrainz-read-release elt))
					 (alist-get 'releases recording)))
	      (info-length-ms        . ,length-ms))))

(defun emms-idapi-musicbrainz-process-type-dispatch (response)
  "Call the appropriate processing function for RESPONSE."
  (let ((process-f (cond ((alist-get 'artists response) #'emms-idapi-musicbrainz-read-artist)
			 ((alist-get 'releases response) #'emms-idapi-musicbrainz-read-release)
			 ((alist-get 'recordings response) #'emms-idapi-musicbrainz-read-recording)
			 (t (error "unhandled response type %s" response))))
	;; the actual items without header data
	(elements (cdr (nth 3 response)))
	(debug-buffer (get-buffer-create emms-idapi-musicbrainz-debug-buffer-name)))
    (setq emms-idapi-musicbrainz-debug-buffer debug-buffer)
    (with-current-buffer debug-buffer
      (erase-buffer)
      (insert (format "%s" response)))
    (append (alist-get 'query response)
	    (mapcar
	     #'(lambda (e)
		 (emms-idapi-musicbrainz-decode
		  (cdr (funcall process-f e))))
	     elements))))

(defun emms-idapi-musicbrainz-process-json (buffer)
  "Return the sexp form of the json in BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((header-max (if (not (re-search-forward "\n\n" (point-max)))
			  (error "cannot find header")
			(point))))
      (append
       (json-read-from-string
	(mm-decode-string
	 (buffer-substring header-max (point-max))
	 'utf-8))
       `((query . ,(list emms-idapi-query-local)))))))

(defun emms-idapi-musicbrainz-process (buffer)
  "Process response stored in BUFFER. Return BUFFER."
  (when (or (not buffer)
	    (not (bufferp buffer)))
    (error "cannot access response buffer"))
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (not (re-search-forward "HTTP/1.1 200 OK" (line-end-position) t))
      (error "not a valid HTTP response from server"))
    buffer))


;;; ------------------------------------------------------------------
;;; Call
;;; ------------------------------------------------------------------
(defun emms-idapi-musicbrainz-call (search-term-alist)
  "Make a call into search service based on SEARCH-TERM-ALIST."
  (let (buffer
	;; Robert Kaye from MusicBrainz said on the 21st of February,
	;; 2024, that this format of user agent string is good.
	(url-user-agent (concat "Emacs_Emms/"
				emms-version
				" (https://www.gnu.org/software/emms/)"))
	(url-string (emms-idapi-musicbrainz-make-search-string
		     search-term-alist)))
    (setq buffer (url-retrieve-synchronously
		  url-string
		  t t
		  emms-idapi-musicbrainz-url-timeout-seconds))
    (if (not buffer)
	(error "call to musicbrainz API timeout or returned nothing")
      (with-current-buffer buffer
	(setq emms-idapi-query-local search-term-alist))
      (setq emms-idapi-musicbrainz-url-buffer buffer))))

(defun emms-idapi-musicbrainz-get-search-string (field)
  "Return the search string associated with FIELD."
  (let ((string (alist-get field emms-idapi-musicbrainz-search-string-map)))
    (when (not string)
      (error "no associated string for \"%s\" field" field))
    string))

(defun emms-idapi-musicbrainz-make-search-string (term-alist)
  (let ((artist  (or (alist-get 'info-artist term-alist)
		     (alist-get 'info-albumartist term-alist)))
	(release (alist-get 'info-album  term-alist))
	(title   (alist-get 'info-title  term-alist))
	;; (reid    (alist-get 'reid        term-alist))
	;; (arid    (alist-get 'arid        term-alist))
	)
    (concat emms-idapi-musicbrainz-root-url

	    (cond ((and title
			artist)
		   (format "recording/?query=recording:%s%sartist:%s"
			   (url-encode-url (concat "\"" title "\""))
			   (url-encode-url " AND ")
			   (url-encode-url (concat "\"" artist "\""))))

		  ((and artist
			(not release)
			(not title))
		   (format "artist/?query=%s" (url-encode-url (concat "\"" artist "\""))))

		  ;; Will only work if the browser supplies a meaningful musicbrainz ARID
		  ;;
		  ;; (release
		  ;;  (format "release/?query=release:%s%s%s"
		  ;; 	   (url-encode-url (concat "\"" release "\""))
		  ;; 	   (if artist (url-encode-url (concat " AND artist:\"" artist "\"")) "")
		  ;; 	   (if arid (concat (url-encode-url " AND ") "arid:" arid) "")))

		  (t (error "unhandled field %s" term-alist)))
	    (format "&limit=%d&fmt=json" emms-idapi-musicbrainz-response-limit))))


;;; ------------------------------------------------------------------
;;; Interface
;;; ------------------------------------------------------------------
(defun emms-idapi-musicbrainz-search (search-term-alist)
  "IDAPI interface function for searching MusicBrainz service.

ARID limits the search to a MusicBrainz artist id."
  (when (not (listp search-term-alist))
    (error "%s is not a list" search-term-alist))
  (emms-idapi-musicbrainz-process-type-dispatch
   (emms-idapi-musicbrainz-process-json
    (emms-idapi-musicbrainz-process
     (emms-idapi-musicbrainz-call search-term-alist)))))


(provide 'emms-idapi-musicbrainz)

;;; emms-idapi-musicbrainz.el ends here
