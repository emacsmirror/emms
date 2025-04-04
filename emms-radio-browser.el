;;; emms-radio-browser.el --- EMMS client for radio-brower API  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Fran Burstall <fran.burstall@gmail.com>
;; Keywords: emms, multimedia

;; EMMS is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; EMMS is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;;  This package enables searches for internet radio streams against
;;  the radio-browser API (https://www.radio-browser.info).
;;  Successful searches return an EMMS playlist of hits.

;; Entry points:
;; emms-radio-browser-search-by-name
;; emms-radio-browser-search-by-url
;; emms-radio-browser-full-search

;; `emms-radio-browser-full-search' needs the `transient' package
;; (built in to Emacs since v28.1).

;;; Code:

;;* Requires
(require 'dns)
(require 'url)
(require 'json)
(require 'emms-playlist-mode)
(require 'seq)
(require 'transient)

;;* Constants

(defconst emms-radio-browser-server-server
  "all.api.radio-browser.info"
  "Server to query for list of radio-browser servers.")

(defconst emms-radio-browser-search-endpoint
  "/json/stations/search"
  "Endpoint for station searches against the radio-browser API.")

(defconst emms-radio-browser-url-endpoint
  "/json/stations/byurl"
  "Endpoint for station URL searches against the radio-browser API.")

(defvar emms-radio-browser-user-agent
  "EMMS radio-browser"
  "The user-agent we declare to the server.")

(defvar emms-radio-browser-search-limit 30
  "Maximum number of hits to pull from the server.")

(defvar emms-radio-browser-search-order "votes"
  "Default field to order results by.")

(defvar emms-radio-browser-search-descending t
  "Non-nil if results should be sorted in descending order.")

(defconst emms-radio-browser-order-fields
  '("name"
    "url"
    "homepage"
    "favicon"
    "tags"
    "country"
    "state"
    "language"
    "votes"
    "codec"
    "bitrate"
    "lastcheckok"
    "lastchecktime"
    "clicktimestamp"
    "clickcount"
    "clicktrend"
    "changetimestamp"
    "random")
  "Search fields we can order the results by.")


;;* Query the server

;;** Target url
;; The API asks us to get a list of servers from a DNS lookup on
;; all.api.radio-browser.info, do reverse DNS on the IP
;; addresses so found and then choose one at random.  In fact, there
;; are only three servers but we want play nice and so do as we are
;; asked.
(defun emms-radio-browser-get-server-list ()
  "Get the list of radio-browser servers.

Error out if the list is empty as this suggests we have network problems
and so are doomed."
  (let ((server-list
	 (mapcar (lambda (ip) (dns-query ip nil nil 'reverse))
		 (mapcar (lambda (it) (car (alist-get 'data it)))
			 (car (alist-get 'answers
					 (dns-query emms-radio-browser-server-server nil 'full)))))))
    (if server-list server-list (error "Network problem: DNS lookup failed"))))

(defun emms-radio-browser-base-url ()
  "Return a (randomised) radio-browser URL."
  (concat "http://" (seq-random-elt (emms-radio-browser-get-server-list))))

;;** Payload
(defun emms-radio-browser-query-template ()
  "Return basic search template.

This is an alist suitable for `json-encode'."
  (list (cons 'limit emms-radio-browser-search-limit)
	(cons 'order emms-radio-browser-search-order)
	(cons 'reverse emms-radio-browser-search-descending)
	(cons 'hidebroken t)))

(defun emms-radio-browser-search-by-name-payload (name)
  "Return payload to search by name NAME."
  (let ((payload (emms-radio-browser-query-template)))
    (push (cons 'name name) payload)
    payload))

;;** Full search
;; We use a transient for this which will need a little scene-setting.
;; Accessible applications of the transient library are a little thin
;; on the ground so let us explain what we are doing in a bit more
;; detail than usual.
;;
;; The entry point is `emms-radio-browser-full-search' which is a kind
;; of dispatcher (in transient terminimology it is a "prefix").  It is
;; populated with data fields, called "infixes", with which the user
;; interacts and commands, called "suffixes", which can read the data
;; collected in the infixes and do something with it.
;;
;; All of these things are EIEIO classes.
;;
;; Our implementation was heavily inspired by the project:
;; https://codeberg.org/martianh/tp.el

;; The idea is to equip each infix with an alist-key slot which stores
;; a symbol.  We arrange that each infix reports its value as a cons
;; cell whose car is this symbol and whose cdr the contents of the
;; value slot.  The prefix reports the list of all these cons cells to
;; a suffix so what the suffix receives is an alist---in this way we
;; construct a query of exactly the kind we need to feed to the
;; radio-browser server!

;; We subclass a suitable infix class to add the alist-key slot.
(defclass emms-radio-browser-field (transient-option)
  ((format :initarg :format :initform " %k %-13d %v")
   (alist-key :initarg :alist-key))
  "An infix class for string fields.")

;; We subclass this to get something suitable for boolean fields.
;; Why? Because we display their values differently in the transient
;; UI and also because our alist will be fed to `json-encode' so we
;; treat nil specially.
(defclass emms-radio-browser-bool (emms-radio-browser-field)
  ()
  "An infix class for boolean fields.")

;; `transient-format-value' determines how the infix value is shown in
;; the transient UI

(cl-defmethod transient-format-value ((obj emms-radio-browser-field))
  "Format the value of OBJ.

Nil is formatted as the empty string."
  (or (oref obj value) ""))

(cl-defmethod transient-format-value ((obj emms-radio-browser-bool))
  "Format the value of boolean OBJ.

Returns either \"True\" or \"False\"."
  (if (oref obj value) "True" "False"))

;; `transient-infix-value' returns the infix value to the calling
;; suffix: as discussed above, we wrap the value into a cons cell.
(cl-defmethod transient-infix-value ((obj emms-radio-browser-field))
  "Return the infix value of OBJ as a cons cell if non-nil."
  (when-let ((val (oref obj value)))
    (cons (oref obj alist-key) val)))

(cl-defmethod transient-infix-value ((obj emms-radio-browser-bool))
  "Return the infix value of OBJ as a cons cell."
  (let ((val (oref obj value)))
    (cons (oref obj alist-key) (if val val :json-false))))

;; `transient-init-value' is called to initialise each infix when the
;; prefix starts up.  We set some default values by reading them from
;; `emms-radio-browser-query-template'.
(cl-defmethod transient-init-value ((obj emms-radio-browser-field))
  "Initialise OBJ, an option."
  (let ((key (oref obj alist-key)))
    (oset obj value
          (alist-get key (emms-radio-browser-query-template)))))

;; `transient-infix-read' sets the value of the infix from the user.
;; Usually, the method of the parent class `transient-option' is
;; perfect for this but, for booleans, it suffices to toggle the
;; existing value.
(cl-defmethod transient-infix-read ((obj emms-radio-browser-bool))
  "Toggle the (boolean) value of OBJ."
  (not (oref obj value)))

;; Now for the suffices that acts on the data we have gathered.

;; This is the main suffix that slurps the query alist and passes it to the server.
(transient-define-suffix emms-radio-browser-execute-full-search (args)
  "Extract query from `emms-radio-browser-full-search' and execute it.

Switches to an EMMS playlist containing the results."
  :transient 'transient--do-return
  (interactive (list (transient-args transient-current-command)))
  (emms-radio-browser-query-api args emms-radio-browser-search-endpoint))

;; Here is another which just shows the query in the message buffer
;; for debugging purposes
(transient-define-suffix emms-radio-browser-show-full-search (args)
  "Extract query from `emms-radio-browser-full-search' and show it."
  :transient 'transient--do-return
  (interactive (list (transient-args transient-current-command)))
  (message "%S" args))

;; Finally, we define the prefix.  Sadly emacs-29, ships with a
;; prehistoric version of transient which misses both a level-toggling
;; command and the transient-information class.  So we use a macro to
;; give different defintions of the prefix accordinding to emacs version.

(defmacro emms-radio-browser--make-full-search ()
  "Define a transient with features conditional on Emacs version."
  (if (and (< emacs-major-version 30) (not (boundp 'transient-version)))
      '(transient-define-prefix emms-radio-browser-full-search-prefix ()
	 "Construct a search query by filling in a form.

Optionally dispatch it to the radio-browser server and switch to an
EMMS playlist of results."
	 ["EMMS radio browser full search: hit coloured letters to set/unset fields\n"
	  ["Search terms:"
	   ("n" "Name" "Station name" :alist-key name :class emms-radio-browser-field)
	   ("t" "Tags" "Tags (comma separated)" :alist-key tagList :class emms-radio-browser-field)
	   ("c" "Country" "Country" :alist-key country :class emms-radio-browser-field)
	   ("l" "Language" "Language" :alist-key language :class emms-radio-browser-field)]
	  ["Exact matches for:"
	   ("xn" "Name" "Exact names" :alist-key nameExact :class emms-radio-browser-bool)
	   ("xt" "Tags" "Exact tags" :alist-key tagExact :class emms-radio-browser-bool)
	   ("xc" "Country" "Exact country" :alist-key countryExact :class emms-radio-browser-bool)
	   ("xl" "Language" "Exact language" :alist-key languageExact :class emms-radio-browser-bool)]
	  ["Advanced search terms:" :pad-keys t
	   ("C" "Codec" "Codec" :alist-key codec :class emms-radio-browser-field)
	   ("bn" "Minimum bitrate" "Minimum bitrate (kb/s)" :alist-key bitrateMin :class emms-radio-browser-field
	    :reader transient-read-number-N0)
	   ("bz" "Maximum bitrate" "Maximum bitrate (kb/s)" :alist-key bitrateMin :class emms-radio-browser-field
	    :reader transient-read-number-N0)
	   ("k" "Country code" "Country code" :alist-key countrycode :class emms-radio-browser-field)]]
	 ["Search parameters:"
	  ("m" "Maximum hits" "Maximum Hits" :alist-key limit :class emms-radio-browser-field
	   :reader transient-read-number-N+ :always-read t)
	  ("o" "Order by" "Order by" :alist-key order :class emms-radio-browser-field
	   :choices (lambda () emms-radio-browser-order-fields) :always-read t)
	  ("d" "Descending" "Descending order" :alist-key reverse :class emms-radio-browser-bool)]
	 [:class transient-row "Actions:"
		 ("C-c C-c" "Execute search" emms-radio-browser-execute-full-search)
		 ("C-c C-k" "Abandon search" ignore)
		 ])
    '(transient-define-prefix emms-radio-browser-full-search-prefix ()
       "Construct a search query by filling in a form.

Optionally dispatch it to the radio-browser server and switch to an
EMMS playlist of results."
       :column-widths '(30 20 30)
       [:description "EMMS radio browser full search"
		     (:info "Hit coloured letters to set/unset fields")
		     (:info '(lambda () (concat (propertize "C-x a" 'face 'help-key-binding)
						" to toggle advanced search")))
		     (:info '(lambda () (concat (propertize "C-c C-c" 'face 'help-key-binding)
						" to execute the search")))
		     (:info '(lambda () (concat (propertize "C-c C-k" 'face 'help-key-binding)
						" to abandon the search")))]
       [["Search terms:"
	 ("n" "Name" "Station name" :alist-key name :class emms-radio-browser-field)
	 ("t" "Tags" "Tags (comma separated)" :alist-key tagList :class emms-radio-browser-field)
	 ("c" "Country" "Country" :alist-key country :class emms-radio-browser-field)
	 ("l" "Language" "Language" :alist-key language :class emms-radio-browser-field)]
	[5 "Exact matches for:"
	   ("xn" "Name" "Exact names" :alist-key nameExact :class emms-radio-browser-bool)
	   ("xt" "Tags" "Exact tags" :alist-key tagExact :class emms-radio-browser-bool)
	   ("xc" "Country" "Exact country" :alist-key countryExact :class emms-radio-browser-bool)
	   ("xl" "Language" "Exact language" :alist-key languageExact :class emms-radio-browser-bool)]
	[5 "Advanced search terms:" :pad-keys t
	   ("C" "Codec" "Codec" :alist-key codec :class emms-radio-browser-field)
	   ("bn" "Minimum bitrate" "Minimum bitrate (kb/s)" :alist-key bitrateMin :class emms-radio-browser-field
	    :reader transient-read-number-N0)
	   ("bz" "Maximum bitrate" "Maximum bitrate (kb/s)" :alist-key bitrateMin :class emms-radio-browser-field
	    :reader transient-read-number-N0)
	   ("k" "Country code" "Country code" :alist-key countrycode :class emms-radio-browser-field)]]
       ["Search parameters:"
	("m" "Maximum hits" "Maximum Hits" :alist-key limit :class emms-radio-browser-field
	 :reader transient-read-number-N+ :always-read t)
	("o" "Order by" "Order by" :alist-key order :class emms-radio-browser-field
	 :choices (lambda () emms-radio-browser-order-fields) :always-read t)
	("d" "Descending" "Descending order" :alist-key reverse :class emms-radio-browser-bool)]
       [:class transient-row "Actions:"
	       ("C-c C-c" "Execute search" emms-radio-browser-execute-full-search)
	       ("C-c C-k" "Abandon search" ignore)
	       (6 "s" "Show search" emms-radio-browser-show-full-search)])))

(emms-radio-browser--make-full-search)

;;** Query the server

(defun emms-radio-browser-query-api (query endpoint)
  "Send QUERY to radio-browser ENDPOINT.

QUERY is an alist suitable for `json-encode'."
  (let* ((target-url (concat (emms-radio-browser-base-url) endpoint))
	 ;; we encode EVERYTHING to stop url-retrieve throwing a wobbly
	 ;; if it encounters non-ascii data, sigh.
	 (user-agent-encoded (encode-coding-string emms-radio-browser-user-agent 'utf-8))
	 (url-request-method "POST")
	 (url-request-data (encode-coding-string (json-encode query) 'utf-8))
	 (url-request-extra-headers `(("Content-type" . "application/json; charset=utf-8")
				      ("User-Agent" . ,user-agent-encoded))))
    (ignore url-request-method
	    url-request-data
	    url-request-extra-headers)
    (url-retrieve
     target-url
     #'emms-radio-browser-query-callback
     (list query))))



;;* Handle the reply
(defun emms-radio-browser-check-response ()
  "Error out if server response headers look bad."
  (let ((ok200 "HTTP/1.1 200 OK"))
    (if (< (point-max) 1)
	(error "No response from server"))
    (if (not (string= ok200 (buffer-substring-no-properties (point-min) 16)))
	(error "Server not responding correctly"))))

(defun emms-radio-browser-json-to-track (data)
  "Convert DATA to EMMS stream-list.

Tries not to cache the result."
  (let ((emms-cache-modified-function nil)
	(emms-cache-set-function nil))
    (let-alist data
      (let ((track (emms-track 'streamlist .url))
	    (metadata (list .name .url 1 'streamlist)))
	(emms-track-set track 'metadata metadata)
	track))))

(defun emms-radio-browser-display-tracks (tracks)
  "Load TRACKS into new playlist buffer and display same."
  (let ((buf (emms-playlist-new "*EMMS radio-browser search results*")))
    (with-current-buffer buf
      (mapc #'emms-playlist-insert-track tracks)
      (emms-playlist-select (point-min))
      (emms-playlist-mode-center-current)
      ;; (emms-playlist-set-playlist-buffer)
      (switch-to-buffer buf))))


(defun emms-radio-browser-query-callback (status &optional cbargs)
  "Process server response and display playlist of results.

Mandatory callback arguments STATUS and CBARGS are ignored."
  ;; Check response OK.
  (ignore status cbargs)
  (set-buffer-multibyte t)
  (emms-radio-browser-check-response)
  ;; Slurp json
  (goto-char (point-min))
  (let ((response (ignore-errors
		    (re-search-forward "\n\n")
		    (json-read))))
    (kill-buffer)
    (if (seq-empty-p response)
	(message "emms-radio-browser: No matches found!")
      (emms-radio-browser-display-tracks
       (mapcar #'emms-radio-browser-json-to-track response)))))


;;* Entry points
;;;###autoload
(defun emms-radio-browser-search-by-name (name)
  "Search radio-browser for stations matching NAME.

Switches to an EMMS playlist containing the results."
  (interactive "sSearch for station name: ")
  (emms-radio-browser-query-api (emms-radio-browser-search-by-name-payload name)
				emms-radio-browser-search-endpoint))

;;;###autoload
(defun emms-radio-browser-search-by-url (url)
  "Search radio-browser for stations matching URL.

Switches to an EMMS playlist containing the results."
  (interactive "sSearch for URL: ")
  (emms-radio-browser-query-api (list (cons 'url url))
				emms-radio-browser-url-endpoint))

;; Finally load the transient for making a full search.  This was
;;conditionally defined above.  We wrap in in a function to get the autoload.
;;;###autoload
(defun emms-radio-browser-full-search ()
  "Construct a search query by filling in a form.

Optionally dispatch it to the radio-browser server and switch to an
EMMS playlist of results."
  (interactive)
  (call-interactively #'emms-radio-browser-full-search-prefix t))


(provide 'emms-radio-browser)
;;; emms-radio-browser.el ends here
