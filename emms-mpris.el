;;; emms-mpris.el --- Mpris interface for EMMS        -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Free Software Foundation, Inc.

;; Author: Fran Burstall <fran.burstall@gmail.com>
;; Keywords: multimedia

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
;; This package provides a dbus interface to EMMS.

;; Usage:
;;  (require 'emms-mpris)
;;  (emms-mpris-enable)

;; Switch off with
;;  (emms-mpris-disable)

;; Caveats: this is not quite a complete implementation of the
;; org.mpris.MediaPlayer2 and org.mpris.MediaPlayer2.Player interfaces
;; (see
;; https://specifications.freedesktop.org/mpris-spec/latest/index.html).
;; What is missing:
;;  - Volume: this should be easy but there seems to be no way to get a
;; simple percentage to report the volume---every emms-volume
;; controller returns a string in a different format, sigh.



;;; Code:

;;* What we need
(require 'dbus)
(require 'url-parse)
(require 'emms)
(require 'emms-browser)
(require 'emms-playing-time)
(require 'cl-lib)
(require 'pcase)
(require 'subr-x)
(require 'seq)

;;* Dbus components
(defconst emms-mpris-service "org.mpris.MediaPlayer2.emms"
  "The service we expose.")

(defconst emms-mpris-path "/org/mpris/MediaPlayer2"
  "Our object path.")

;;* Register and update
(defun emms-mpris-register-method (iface method handler)
  "Register METHOD with HANDLER on interface IFACE."
  (dbus-register-method :session
			emms-mpris-service
			emms-mpris-path
			iface
			method
			handler
			t))

(defun emms-mpris-register-property (iface property access value)
  "Register PROPERTY on interface IFACE.

VALUE is the initial value, ACCESS the access mode."
  (let ((val (cond ((functionp value) (funcall value))
		   ((and (symbolp value) (boundp value)) (symbol-value value))
		   (t value))))
    (dbus-register-property :session
			    emms-mpris-service
			    emms-mpris-path
			    iface
			    property
			    access
			    val
			    ;; emit signal when readwrite properties change
			    (equal access :readwrite)
			    t)))

(defun emms-mpris-register-iface (spec)
  "Register an interface with spec SPEC on the EMMS service.

The spec is a list of the form (IFACE METHODS PROPS).

IFACE is a string naming the interface being registered.

METHODS is a list of methods to register on the interface.
Each method is a list (NAME FN) with NAME a string and FN the
function the method calls.

PROPS is a list of properties to register on the interface.
Each property is a list of the form (NAME ACCESS VAL) with
NAME a string, ACCESS a keyword and VAL either a function
that returns the default value of the property, a variable
which evaluates to that value or the value itself."
  (cl-destructuring-bind (iface methods props) spec
    (dolist (method methods)
      (apply #'emms-mpris-register-method iface method))
    (dolist (prop props)
      (apply #'emms-mpris-register-property iface prop))))


;;* Interfaces

;;** MediaPlayer2 interface

(defvar emms-mpris-mediaplayer-iface-spec
  '("org.mpris.MediaPlayer2"
    (("Raise" ignore)
     ("Quit" ignore))
    (("CanQuit" :read nil)
     ("CanRaise" :read nil)
     ("HasTrackList" :read nil)
     ("Identity" :read "EMMS media player")
     ("SupportedUriSchemes" :read (:array "file"))
     ("SupportedMimeTypes" :read (:array "audio/mpeg" "application/ogg"))))
  "Interface spec for MediaPlayer2.")

;;** MediaPlayer2.Player interface

(defvar emms-mpris-player-iface-spec
  '("org.mpris.MediaPlayer2.Player"
   ;; Methods:
    (("OpenUri" emms-mpris-open-uri)
     ("Next" (lambda () (ignore-errors (emms-next)) :ignore))
     ("Previous" (lambda () (ignore-errors (emms-previous)) :ignore))
     ("Pause" (lambda () (emms-pause) :ignore))
     ("PlayPause" (lambda () (emms-pause) :ignore))
     ("Stop" (lambda () (emms-stop) :ignore))
     ("Play" (lambda () (emms-pause) :ignore))
     ("Seek" emms-mpris-seek)
     ("SetPosition" emms-mpris-set-position))
   ;; Properties: Shuffle, LoopStatus, Volume not supported (yet)
    (("LoopStatus" :readwrite emms-mpris-loop-status)
     ("Shuffle" :readwrite emms-random-playlist)
     ("PlaybackStatus" :read emms-mpris-status)
     ("Rate" :readwrite 1.0)
     ("MinimumRate" :read 1.0)
     ("MaximumRate" :read 1.0)
     ("Position" :read (:int64 0))	;think more about this
     ("CanGoNext" :read t)
     ("CanGoPrevious" :read t)
     ("CanPlay" :read t)
     ("CanPause" :read t)
     ("CanPause" :read t)
     ("CanControl" :read t)
     ("CanSeek" :read t)
     ("Metadata" :read emms-mpris-current-metadata)))
  "Interface spec for MediaPlayer2.Player.")
;;** Introspection interface

(defvar emms-mpris-xml
  "<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\"
                      \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">
<!-- GDBus 2.66.8 -->
<node>
  <interface name=\"org.freedesktop.DBus.Properties\">
    <method name=\"Get\">
      <arg type=\"s\" name=\"interface_name\" direction=\"in\"/>
      <arg type=\"s\" name=\"property_name\" direction=\"in\"/>
      <arg type=\"v\" name=\"value\" direction=\"out\"/>
    </method>
    <method name=\"GetAll\">
      <arg type=\"s\" name=\"interface_name\" direction=\"in\"/>
      <arg type=\"a{sv}\" name=\"properties\" direction=\"out\"/>
    </method>
    <method name=\"Set\">
      <arg type=\"s\" name=\"interface_name\" direction=\"in\"/>
      <arg type=\"s\" name=\"property_name\" direction=\"in\"/>
      <arg type=\"v\" name=\"value\" direction=\"in\"/>
    </method>
    <signal name=\"PropertiesChanged\">
      <arg type=\"s\" name=\"interface_name\"/>
      <arg type=\"a{sv}\" name=\"changed_properties\"/>
      <arg type=\"as\" name=\"invalidated_properties\"/>
    </signal>
  </interface>
  <interface name=\"org.freedesktop.DBus.Introspectable\">
    <method name=\"Introspect\">
      <arg type=\"s\" name=\"xml_data\" direction=\"out\"/>
    </method>
  </interface>
  <interface name=\"org.freedesktop.DBus.Peer\">
    <method name=\"Ping\"/>
    <method name=\"GetMachineId\">
      <arg type=\"s\" name=\"machine_uuid\" direction=\"out\"/>
    </method>
  </interface>
  <interface name=\"org.mpris.MediaPlayer2\">
    <method name=\"Raise\"/>
    <method name=\"Quit\"/>
    <property type=\"b\" name=\"CanQuit\" access=\"read\"/>
    <property type=\"b\" name=\"CanRaise\" access=\"read\"/>
    <property type=\"b\" name=\"HasTrackList\" access=\"read\"/>
    <property type=\"s\" name=\"Identity\" access=\"read\"/>
    <property type=\"s\" name=\"DesktopEntry\" access=\"read\"/>
    <property type=\"as\" name=\"SupportedUriSchemes\" access=\"read\"/>
    <property type=\"as\" name=\"SupportedMimeTypes\" access=\"read\"/>
  </interface>
  <interface name=\"org.mpris.MediaPlayer2.Player\">
    <method name=\"Next\"/>
    <method name=\"Previous\"/>
    <method name=\"Pause\"/>
    <method name=\"PlayPause\"/>
    <method name=\"Stop\"/>
    <method name=\"Play\"/>
    <method name=\"Seek\">
      <arg type=\"x\" name=\"Offset\" direction=\"in\"/>
    </method>
    <method name=\"SetPosition\">
      <arg type=\"o\" name=\"TrackId\" direction=\"in\"/>
      <arg type=\"x\" name=\"Position\" direction=\"in\"/>
    </method>
    <method name=\"OpenUri\">
      <arg type=\"s\" name=\"Uri\" direction=\"in\"/>
    </method>
    <signal name=\"Seeked\">
      <arg type=\"x\" name=\"Position\"/>
    </signal>
    <property type=\"s\" name=\"PlaybackStatus\" access=\"read\"/>
    <property type=\"s\" name=\"LoopStatus\" access=\"readwrite\"/>
    <property type=\"d\" name=\"Rate\" access=\"readwrite\"/>
    <property type=\"b\" name=\"Shuffle\" access=\"readwrite\"/>
    <property type=\"a{sv}\" name=\"Metadata\" access=\"read\"/>
    <property type=\"d\" name=\"Volume\" access=\"readwrite\"/>
    <property type=\"x\" name=\"Position\" access=\"read\"/>
    <property type=\"d\" name=\"MinimumRate\" access=\"read\"/>
    <property type=\"d\" name=\"MaximumRate\" access=\"read\"/>
    <property type=\"b\" name=\"CanGoNext\" access=\"read\"/>
    <property type=\"b\" name=\"CanGoPrevious\" access=\"read\"/>
    <property type=\"b\" name=\"CanPlay\" access=\"read\"/>
    <property type=\"b\" name=\"CanPause\" access=\"read\"/>
    <property type=\"b\" name=\"CanSeek\" access=\"read\"/>
    <property type=\"b\" name=\"CanControl\" access=\"read\"/>
  </interface>
</node>
"
  "Mpris introspection data for emms.")

(defun emms-mpris-introspect ()
  "Return dbus introspection data."
  emms-mpris-xml)

(defvar emms-mpris-introspectable-iface-spec
  '("org.freedesktop.DBus.Introspectable"
    (("Introspect" emms-mpris-introspect))
    nil)
  "Introspectable interface spec for dbus.")

;;** Properties interface

;; We re-implement the "Get" and "GetAll" methods of the
;; dbus.properties interface.  For why?  Well, the default handler
;; looks up the value of a property in a hash table which works fine
;; unless we want the "Position" property of the Player interface
;; which changes all the time (and we don't want to update the table
;; every second!).  So we wrap the default handler to update the
;; Position entry in the table before delegating to the default
;; handler.  This is a bit of a hack in that we go rather beyond the
;; API of dbus.el and hope that the internals do not change.

(defun emms-mpris-update-position-hash-value ()
  "Update the D-Bus hash-table.

Refresh the value in the hash-table corresponding to the Position
property of the org.mpris.MediaPlayer2.Player interface."
  (puthash (list :property :session "org.mpris.MediaPlayer2.Player" "Position")
	   (list (list nil
		       emms-mpris-service
		       emms-mpris-path
		       (list :read nil (list :variant :int64 (emms-mpris-sec-to-musec emms-playing-time)))))
	   dbus-registered-objects-table))

(defun emms-mpris-get-property-handler (&rest args)
  "Handle Get and GetAll event for property in ARGS.

The Position property gets refreshed before delegating
to `dbus-property-handler'."
  (let* ((last-input-event last-input-event))
    (emms-mpris-update-position-hash-value)
    (apply #'dbus-property-handler args)))

(defvar emms-mpris-properties-iface-spec
  '("org.freedesktop.DBus.Properties"
    (("Get" emms-mpris-get-property-handler)
     ("GetAll" emms-mpris-get-property-handler))
    nil)
  "Partial Properties interface spec for dbus.")


;;* Implementation

;;** Utilities
;; Emms thinks in seconds but mpris in microseconds
(defun emms-mpris-musec-to-sec (ms)
  "Convert MS microseconds to seconds."
  (* ms .000001))

(defun emms-mpris-sec-to-musec (s)
  "Convert S seconds to microseconds."
  (truncate (* s 1000000)))

;; Track-id is a d-bus object id and these have rules...
(defun emms-mpris-track-id (track)
  "Return track-id of TRACK as D-Bus object id."
  ;; FIX ME: this won't work if we implement the tracklist interface
  ;; and the tracklist has repeated tracks.
  (concat "/" (mapconcat #'dbus-escape-as-identifier
			 (split-string (emms-track-get track 'name) "/" t)
			 "/")))

;;** Update properties
(defun emms-mpris-update-property (iface property access value)
  "Update PROPERTY on interface IFACE to VALUE."
  (dbus-register-property :session
			  emms-mpris-service
			  emms-mpris-path
			  iface
			  property
			  access
			  value
			  t nil))

(defvar emms-mpris-ignore-signal-p nil
  "Non-nil if we should ignore a PropertiesChanged signal.

We do this when we have already taken action via the EMMS UI.")

(defun emms-mpris-property-change-handler (_service changes _invalidated)
  "Respond to PropertiesChanged signal by updating emms state to reflect CHANGES."
  (if emms-mpris-ignore-signal-p
      (setq emms-mpris-ignore-signal-p nil)
    (when-let ((payload (assoc "LoopStatus" changes)))
      (pcase (caadr payload)
	("Track" (setq emms-repeat-playlist nil
		       emms-repeat-track t))
	("Playlist" (setq emms-repeat-playlist t
			  emms-repeat-track nil))
	(_ (setq emms-repeat-playlist nil
		 emms-repeat-track nil))))
    (when-let ((payload (assoc "Shuffle" changes)))
      (setq emms-random-playlist (caadr payload))
      (if emms-random-playlist
	  (setq emms-player-next-function #'emms-random)
	(setq emms-player-next-function #'emms-next-noerror)))))

;;*** Playback status
(defun emms-mpris-status ()
  "Return the playback status of EMMS as string: Playing, Paused or Stopped."
  (if emms-player-playing-p
      (if emms-player-paused-p
          "Paused" "Playing")
    "Stopped"))

;;*** Loop status
(defun emms-mpris-loop-status ()
  "Return the loop status of EMMS as a string: Track, Playlist or None."
  (cond (emms-repeat-track "Track")
	(emms-repeat-playlist "Playlist")
	(t "None")))

(defun emms-mpris-advise-loop-status ()
  "Update dbus value of LoopStatus.

Intended to advise emms-toggle-repeat-*."
  (setq emms-mpris-ignore-signal-p t)
  (dbus-set-property :session
		     emms-mpris-service
		     emms-mpris-path
		     "org.mpris.MediaPlayer2.Player"
		     "LoopStatus"
		     (emms-mpris-loop-status)))

;;*** Shuffle
(defun emms-mpris-advise-shuffle ()
  "Update dbus value of Shuffle.

Intended to advise `emms-toggle-random-playlist'."
  (setq emms-mpris-ignore-signal-p t)
  (dbus-set-property :session
		     emms-mpris-service
		     emms-mpris-path
		     "org.mpris.MediaPlayer2.Player"
		     "Shuffle"
		     emms-random-playlist))

;;*** Metadata

(defvar emms-mpris-metadata-dict
  '((info-album "xesam:album" :s)
    (info-albumartist "xesam:albumArtist" :as)
    (info-artist "xesam:artist" :as)
    (info-composer "xesam:composer" :as)
    (info-discnumber "xesam:discNumber" :int)
    (info-tracknumber "xesam:trackNumber" :int)
    (info-title "xesam:title" :s)
    (play-count "xesam:useCount" :int))
  "Dictionary between emms metadata and mpris metadata.

Each entry of the form (info-field mpris-field dbus-type).")

(defun emms-mpris-dict (k v &optional type)
  "Return a dbus dict-entry with key K and value V, optionally of type TYPE."
  (if type
      (list :dict-entry k (list :variant type v))
    (list :dict-entry k (list :variant v))))

(defun emms-mpris-convert-field (track info key type)
  "Convert field INFO of TRACK into dbus dict-entry with key KEY and type TYPE."
  (let ((data (emms-track-get track info))
	value)
    (when data
      (setq value (pcase type
		    (:as (list :array data))
		    (:int (if (stringp data) (string-to-number data) data))
		    (:s data)))
      (emms-mpris-dict key value))))

(defun emms-mpris-metadata (track)
  "Return mpris metadata for TRACK."
  (let ((track-name (emms-track-get track 'name))
	metadata)
    ;; standard fields
    (dolist (field emms-mpris-metadata-dict)
      (when-let ((entry (apply #'emms-mpris-convert-field track field)))
	(push entry metadata)))
    ;; url
    (push (emms-mpris-dict "xesam:url" (url-encode-url (concat "file:" track-name))) metadata)
    ;; artUrl
    ;; Shockingly, emms-browser-get-cover-from-path needs a graphical display to
    ;; function (it eventually calls image-size) so we check there is one...
    (when (seq-some #'display-graphic-p (frame-list))
      (when-let ((art-file (emms-browser-get-cover-from-path track-name 'medium)))
	(push (emms-mpris-dict "mpris:artUrl" (url-encode-url (concat "file://" art-file))) metadata)))
    ;; length
    (push
     (emms-mpris-dict "mpris:length"
		      (emms-mpris-sec-to-musec (emms-track-get track 'info-playing-time 0))
		      :int64)
     metadata)
    ;; trackid
    (push
     (emms-mpris-dict "mpris:trackid"
		      (emms-mpris-track-id track)
		      :object-path)
     metadata)
    (cons :array metadata)))

(defun emms-mpris-current-metadata ()
  "Return metadata of current track if it exists, else return a placeholder."
  (if-let ((track (emms-playlist-current-selected-track)))
      (emms-mpris-metadata track)
    '(:array (:dict-entry "mpris:trackid" (:variant :object-path "/no/track/here")))))

;;*** update them!
(defun emms-mpris-change-status ()
  "Notify emms status to dbus."
  (let ((iface "org.mpris.MediaPlayer2.Player"))
    (emms-mpris-update-property iface
				"PlaybackStatus"
				:read
				(emms-mpris-status))
    (emms-mpris-update-property iface
				"Metadata"
				:read
				(emms-mpris-current-metadata))))


;;** Seek and SetPosition

;;*** Signal position change (after Seek or SetPosition)
(defun emms-mpris-signal-position (pos)
  "Send \"Seeked\" signal with new position POS (in seconds)."
  (dbus-send-signal :session
		    nil
		    emms-mpris-path
		    "org.mpris.MediaPlayer2.Player"
		    "Seeked"
		    :int64
		    (emms-mpris-sec-to-musec pos)))

;;*** Seek method
(defun emms-mpris-seek (ms)
  "Method to seek by MS microseconds."
  (emms-seek (number-to-string (emms-mpris-musec-to-sec ms)))
  (emms-mpris-signal-position emms-playing-time)
  :ignore)

;;*** SetPosition method
(defun emms-mpris-set-position (track-id pos)
  "Method to seek to POS (in microseconds) if current track has id TRACK-ID."
  (let* ((track (emms-playlist-current-selected-track))
	 (duration (emms-track-get track 'info-playing-time 0))
	 (current-track-id (emms-mpris-track-id track))
	 (pos-in-secs (emms-mpris-musec-to-sec pos)))
    (when (and (string-equal track-id current-track-id)
	       (<= 0.0 pos-in-secs duration))
      (emms-seek-to (number-to-string pos-in-secs))
      (emms-mpris-signal-position emms-playing-time))
    :ignore))

;;** OpenURI

(defun emms-mpris-open-uri (uri)
  "Method for opening file URI and playing it."
  (let* ((parsed-uri (url-generic-parse-url uri))
	 (file (url-unhex-string (url-filename parsed-uri)))
	 (type (url-type parsed-uri)))
    (when (and (string-equal type "file") (file-exists-p file))
      (cond ((file-regular-p file) (emms-play-file file))
	    ((file-directory-p file) (emms-play-directory file)))))
  :ignore)


;;* Entry point

(defvar emms-mpris-enabled-p nil
  "Non-nil if the EMMS mpris service is enabled.")

(defun emms-mpris-enable ()
  "Activate EMMS dbus service."
  (interactive)
  (unless emms-mpris-enabled-p
    (emms-mpris-register-iface emms-mpris-mediaplayer-iface-spec)
    (emms-mpris-register-iface emms-mpris-player-iface-spec)
    (emms-mpris-register-iface emms-mpris-introspectable-iface-spec)
    (emms-mpris-register-iface emms-mpris-properties-iface-spec)
    (dbus-register-service :session emms-mpris-service :allow-replacement)
    (dbus-register-signal :session
			  emms-mpris-service
			  emms-mpris-path
			  dbus-interface-properties
			  "PropertiesChanged"
			  #'emms-mpris-property-change-handler
			  :eavesdrop)
    (advice-add 'emms-toggle-repeat-track :after #'emms-mpris-advise-loop-status)
    (advice-add 'emms-toggle-repeat-playlist :after #'emms-mpris-advise-loop-status)
    (advice-add 'emms-toggle-random-playlist :after #'emms-mpris-advise-shuffle)
    (add-hook 'emms-player-started-hook #'emms-mpris-change-status)
    (add-hook 'emms-player-paused-hook #'emms-mpris-change-status)
    (add-hook 'emms-player-stopped-hook #'emms-mpris-change-status)
    (add-hook 'emms-player-finished-hook #'emms-mpris-change-status)
    (setq emms-mpris-enabled-p t)))

(defun emms-mpris-disable ()
  "Turn off EMMS dbus service."
  (interactive)
  (when emms-mpris-enabled-p
    (remove-hook 'emms-player-started-hook #'emms-mpris-change-status)
    (remove-hook 'emms-player-paused-hook #'emms-mpris-change-status)
    (remove-hook 'emms-player-stopped-hook #'emms-mpris-change-status)
    (remove-hook 'emms-player-finished-hook #'emms-mpris-change-status)
    (advice-remove 'emms-toggle-repeat-track  #'emms-mpris-advise-loop-status)
    (advice-remove 'emms-toggle-repeat-playlist  #'emms-mpris-advise-loop-status)
    (advice-remove 'emms-toggle-random-playlist #'emms-mpris-advise-shuffle)
    ;; Call this twice: we have two methods for "Get" on the Properties
    ;; interface (there /must/ be a better way to do this!):
    (dbus-unregister-service :session emms-mpris-service)
    (dbus-unregister-service :session emms-mpris-service)
    (setq emms-mpris-enabled-p nil)))


(provide 'emms-mpris)
;;; emms-mpris.el ends here
