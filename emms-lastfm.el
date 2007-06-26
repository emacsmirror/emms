;;; emms-lastfm.el --- add your listened songs to your profile at last.fm

;; Copyright (C) 2006, 2007 Free Software Foundation, Inc.

;; Author: Tassilo Horn <tassilo@member.fsf.org>

;; Keywords: emms, mp3, mpeg, multimedia

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.

;; EMMS is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; EMMS; see the file COPYING.  If not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; This code sends information about what music you are playing to last.fm.
;; See <URL:http://www.last.fm> and
;; <URL:http://www.audioscrobbler.net/wiki/Protocol1.1>.

;;; Sample configuration:

;; (setq emms-lastfm-username "my-user-name"
;;       emms-lastfm-password "very-secret!")

;;; Usage:

;; To activate the last.fm emms plugin, run:
;;   `M-x emms-lastfm-enable'

;; Now all music you listen to will be submitted to Last.fm to enhance your
;; profile.

;; To deactivate the last.fm emms plugin, run:
;;   `M-x emms-lastfm-disable'

;; Beside submitting the tracks you listen to, you can also listen to Last.fm
;; radio. Simply copy the lastfm:// URL and run & paste:
;;   `M-x emms-lastfm-radio RET lastfm://artist/Britney Spears/fans'
;; (Of course you don't need to use _this_ URL. :-))

;; You can also insert Last.fm streams into playlists (or use
;; emms-streams.el to listen to them) by activating the player as
;; follows.
;;   (add-to-list 'emms-player-list 'emms-player-lastfm-radio)
;; To insert a Last.fm stream into a playlist, do
;;   (emms-insert-lastfm "lastfm://rest-of-url")

;; There are some functions for conveniently playing the Similar Artists and
;; the Global Tag Radio. Here you only need to enter the band's name or the tag
;; respectively.
;;   `M-x emms-lastfm-radio-similar-artists RET Britney Spears'
;;   `M-x emms-lastfm-radio-global-tag RET pop'

;; When you're listening to a Last.fm radio station you have the possibility to
;; give feedback to them. If you like the current song, type
;;   `M-x emms-lastfm-radio-love'.
;; If it's not that good, or it just happens to not fit to your actual mood,
;; type
;;   `M-x emms-lastfm-radio-skip'
;; and this song will be skipped.
;; If you really hate that song and you never want to hear it again, ban it by
;; typing
;;   `M-x emms-lastfm-radio-ban'.

;; -----------------------------------------------------------------------

(require 'url)
(require 'emms)
(require 'emms-mode-line)
(require 'emms-playing-time)
(require 'emms-source-file)
(require 'emms-url)

(defgroup emms-lastfm nil
  "Interaction with the services offered by http://www.last.fm."
  :prefix "emms-lastfm-"
  :group 'emms)

(defcustom emms-lastfm-username ""
  "Your last.fm username"
  :type 'string
  :group 'emms-lastfm)

(defcustom emms-lastfm-password ""
  "Your last.fm password"
  :type 'string
  :group 'emms-lastfm)

(defconst emms-lastfm-server "http://post.audioscrobbler.com/"
  "The last.fm server responsible for the handshaking
procedure. Only for internal use.")
(defconst emms-lastfm-client-id "ems"
  "The client ID of EMMS. Don't change it!")
(defconst emms-lastfm-client-version 0.1
  "The version registered at last.fm. Don't change it!")

;; used internally
(defvar emms-lastfm-process nil "-- only used internally --")
(defvar emms-lastfm-md5-challenge nil "-- only used internally --")
(defvar emms-lastfm-submit-url nil "-- only used internally --")
(defvar emms-lastfm-current-track nil "-- only used internally --")
(defvar emms-lastfm-timer nil "-- only used internally --")

(defun emms-lastfm-new-track-function ()
  "This function should run whenever a new track starts (or a
paused track resumes) and sets the track submission timer."
  (setq emms-lastfm-current-track
        (emms-playlist-current-selected-track))
  ;; Tracks should be submitted, if they played 240 secs or half of their
  ;; length, whichever comes first.
  (let ((secs (/ (emms-track-get emms-lastfm-current-track
                                 'info-playing-time)
                    2)))
    (when (> secs 240)
      (setq secs 240))
    (unless (< secs 15) ;; Skip titles shorter than 30 seconds
      (setq secs (- secs emms-playing-time))
      (unless (< secs 0)
        (setq emms-lastfm-timer
              (run-with-timer secs nil 'emms-lastfm-submit-track))))))

(defun emms-lastfm-cancel-timer ()
  "Cancels `emms-lastfm-timer' if it is running."
  (emms-cancel-timer emms-lastfm-timer)
  (setq emms-lastfm-timer nil))

(defun emms-lastfm-pause ()
  "Handles things to be done when the player is paused or
resumed."
  (if emms-player-paused-p
      ;; the player paused
      (emms-lastfm-cancel-timer)
    ;; The player resumed
    (emms-lastfm-new-track-function)))

(defun emms-lastfm (&optional ARG)
  "Start submitting the tracks you listened to to
http://www.last.fm, if ARG is positive. If ARG is negative or
zero submission of the tracks will be stopped. This applies to
the current track, too."
  (interactive "p")
  (cond
   ((not (and emms-lastfm-username emms-lastfm-password))
    (message "%s"
             (concat "EMMS: In order to activate the last.fm plugin you "
                     "first have to set both `emms-lastfm-username' and "
                     "`emms-lastfm-password'")))
   ((not emms-playing-time-p)
    (message "%s"
             (concat "EMMS: The last.fm plugin needs the functionality "
                     "provided by `emms-playing-time'. It seems that you "
                     "disabled it explicitly in your init file using code "
                     "like this: `(emms-playing-time -1)'. Delete that "
                     "line and have a look at `emms-playing-time's doc "
                     "string")))
   (t
    (if (and ARG (> ARG 0))
        (progn
          ;; Append it. Else the playing time could be started a bit too late.
          (add-hook 'emms-player-started-hook
                    'emms-lastfm-handshake-if-needed t)
          ;; Has to be appended, because it has to run after
          ;; `emms-playing-time-start'
          (add-hook 'emms-player-started-hook
                    'emms-lastfm-new-track-function t)
          (add-hook 'emms-player-stopped-hook
                    'emms-lastfm-cancel-timer)
          (add-hook 'emms-player-paused-hook
                    'emms-lastfm-pause)
          (message "EMMS Last.fm plugin activated"))
      (remove-hook 'emms-player-started-hook
                   'emms-lastfm-handshake-if-needed)
      (remove-hook 'emms-player-started-hook
                   'emms-lastfm-new-track-function)
      (remove-hook 'emms-player-stopped-hook
                   'emms-lastfm-cancel-timer)
      (remove-hook 'emms-player-paused-hook
                   'emms-lastfm-pause)
      (when emms-lastfm-timer (emms-cancel-timer emms-lastfm-timer))
      (setq emms-lastfm-md5-challenge nil
            emms-lastfm-submit-url    nil
            emms-lastfm-process       nil
            emms-lastfm-current-track nil)
      (message "EMMS Last.fm plugin deactivated")))))

(defalias 'emms-lastfm-activate 'emms-lastfm)
(emms-make-obsolete 'emms-lastfm-activate 'emms-lastfm "EMMS 2.2")

(defun emms-lastfm-enable ()
  "Enable the emms last.fm plugin."
  (interactive)
  (emms-lastfm 1))

(defun emms-lastfm-disable ()
  "Disable the emms last.fm plugin."
  (interactive)
  (emms-lastfm -1))

(defun emms-lastfm-restart ()
  "Disable and reenable the last.fm plugin. This will cause a new
handshake."
  (emms-lastfm-disable)
  (emms-lastfm-enable))

(defun emms-lastfm-handshake-if-needed ()
  (when (not (and emms-lastfm-md5-challenge
                  emms-lastfm-submit-url))
    (emms-lastfm-handshake)))

(defun emms-lastfm-handshake ()
  "Handshakes with the last.fm server."
  (let ((url-request-method "GET"))
    (url-retrieve
     (concat emms-lastfm-server
             "?hs=true&p=1.1"
             "&c=" emms-lastfm-client-id
             "&v=" (number-to-string emms-lastfm-client-version)
             "&u=" (emms-escape-url emms-lastfm-username))
     'emms-lastfm-handshake-sentinel)))

(defun emms-lastfm-handshake-sentinel (&rest args)
  "Parses the server reponse and inform the user if all worked
well or if an error occured."
  (let ((buffer (current-buffer)))
    (emms-http-decode-buffer buffer)
    (goto-char (point-min))
    (re-search-forward (rx (or "UPTODATE" "UPDATE" "FAILED" "BADUSER"))
                       nil t)
    (let ((response (emms-read-line)))
      (if (not (string-match (rx (or "UPTODATE""UPDATE")) response))
          (progn
            (cond ((string-match "FAILED" response)
                   (message "EMMS: Handshake failed: %s" response))
                  ((string-match "BADUSER" response)
                   (message "EMMS: Wrong username"))))
        (when (string-match "UPDATE" response)
          (message "EMMS: There's a new last.fm plugin version"))
        (forward-line)
        (setq emms-lastfm-md5-challenge (emms-read-line))
        (forward-line)
        (setq emms-lastfm-submit-url (emms-read-line))
        (message "EMMS: Handshaking with server done")))
    (kill-buffer buffer)))

(defun emms-lastfm-submit-track ()
  "Submits the current track (`emms-lastfm-current-track') to
last.fm."
  (let* ((artist (emms-track-get emms-lastfm-current-track 'info-artist))
         (title  (emms-track-get emms-lastfm-current-track 'info-title))
         (album  (emms-track-get emms-lastfm-current-track 'info-album))
         (musicbrainz-id "")
         (track-length (number-to-string
                        (emms-track-get emms-lastfm-current-track
                                        'info-playing-time)))
         (date (format-time-string "%Y-%m-%d %H:%M:%S" (current-time) t))
         (url-http-attempt-keepalives nil)
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-type" .
             "application/x-www-form-urlencoded; charset=utf-8")))
         (url-request-data
          (encode-coding-string
           (concat "u=" (emms-escape-url emms-lastfm-username)
                   "&s=" (md5 (concat (md5 emms-lastfm-password)
                                      emms-lastfm-md5-challenge))
                   "&a[0]=" (emms-escape-url artist)
                   "&t[0]=" (emms-escape-url title)
                   "&b[0]=" (emms-escape-url album)
                   "&m[0]=" musicbrainz-id
                   "&l[0]=" track-length
                   "&i[0]=" date)
           'utf-8)))
    (url-retrieve emms-lastfm-submit-url
                  'emms-lastfm-submission-sentinel)))

(defun emms-lastfm-submission-sentinel (&rest args)
  "Parses the server reponse and inform the user if all worked
well or if an error occured."
  (let ((buffer (current-buffer)))
    (emms-http-decode-buffer buffer)
    (goto-char (point-min))
    (if (re-search-forward "^OK$" nil t)
        (progn
          (message "EMMS: \"%s\" submitted to last.fm"
                   (emms-track-description emms-lastfm-current-track))
          (kill-buffer buffer))
      (message "EMMS: Song couldn't be submitted to last.fm")
      (goto-char (point-min))
      (if (re-search-forward "^BADAUTH$" nil t)
          ;; Somehow our md5-challenge expired...
          (progn
            (kill-buffer buffer)
            (message "EMMS: Restarting last.fm plugin")
            (emms-lastfm-restart))
        (kill-buffer buffer)))))


;;; Playback of lastfm:// streams

(defgroup emms-player-lastfm-radio nil
  "EMMS player for Last.fm streams."
  :group 'emms-player
  :prefix "emms-player-lastfm-")

(defcustom emms-player-lastfm-radio (emms-player 'emms-lastfm-radio-start
                                                 'ignore ; no need to stop
                                                 'emms-lastfm-radio-playable-p)
  "*Parameters for the Last.fm radio player."
  :type '(cons symbol alist)
  :group 'emms-player-lastfm-radio)

(defconst emms-lastfm-radio-base-url "http://ws.audioscrobbler.com/radio/"
  "The base URL for playing lastfm:// stream.
-- only used internally --")

(defvar emms-lastfm-radio-session nil "-- only used internally --")
(defvar emms-lastfm-radio-stream-url nil "-- only used internally --")

(defun emms-lastfm-radio-get-handshake-url ()
  (concat emms-lastfm-radio-base-url
          "handshake.php?version=" (number-to-string 
                                    emms-lastfm-client-version)
          "&platform="              emms-lastfm-client-id
          "&username="              (emms-escape-url emms-lastfm-username)
          "&passwordmd5="           (md5 emms-lastfm-password)
          "&debug="                 (number-to-string 9)))

(defun emms-lastfm-radio-handshake (fn radio-url)
  "Handshakes with the last.fm server.
Calls FN when done with RADIO-URL as its only argument."
  (let ((url-request-method "GET"))
    (url-retrieve (emms-lastfm-radio-get-handshake-url)
                  'emms-lastfm-radio-handshake-sentinel
                  (list fn radio-url))))

(defun emms-lastfm-radio-handshake-sentinel (status fn radio-url)
  (let ((buffer (current-buffer)))
    (emms-http-decode-buffer buffer)
    (setq emms-lastfm-radio-session    (emms-key-value "session"))
    (setq emms-lastfm-radio-stream-url (emms-key-value "stream_url"))
    (kill-buffer buffer)
    (if (and emms-lastfm-radio-session emms-lastfm-radio-stream-url)
        (progn
          (message "EMMS: Handshaking for Last.fm playback successful")
          (funcall fn radio-url))
      (message "EMMS: Failed handshaking for Last.fm playback"))))

(defun emms-lastfm-radio-1 (lastfm-url)
  "Internal function used by `emms-lastfm-radio'."
  (if (and emms-lastfm-radio-session
           emms-lastfm-radio-stream-url)
      (let ((url-request-method "GET"))
        (url-retrieve
         (concat emms-lastfm-radio-base-url
                 "adjust.php?"
                 "session=" emms-lastfm-radio-session
                 "&url="    (emms-escape-url lastfm-url)
                 "&debug="  (number-to-string 0))
         'emms-lastfm-radio-sentinel))
    (message "EMMS: Cannot play Last.fm stream")))

(defun emms-lastfm-radio (lastfm-url)
  "Plays the stream associated with the given Last.fm URL. (A
Last.fm URL has the form lastfm://foo/bar/baz, e.g.

  lastfm://artist/Manowar/similarartists

or

  lastfm://globaltags/metal."
  (interactive "sLast.fm URL: ")
  ;; Streamed songs must not be added to the lastfm profile
  (emms-lastfm-disable)
  (if (not (and emms-lastfm-radio-session
                emms-lastfm-radio-stream-url))
      (emms-lastfm-radio-handshake #'emms-lastfm-radio-1 lastfm-url)
    (emms-lastfm-radio-1 lastfm-url)))

(defun emms-lastfm-radio-playable-p (track)
  "Determine whether the Last.fm player can play this track."
  (let ((name (emms-track-get track 'name))
        (type (emms-track-get track 'type)))
    (and (eq type 'lastfm)
         (string-match "^lastfm://" name))))

(defun emms-lastfm-radio-start (track)
  "Start playing TRACK."
  (when (emms-lastfm-radio-playable-p track)
    (let ((name (emms-track-get track 'name)))
      (emms-lastfm-radio name))))

(defcustom emms-lastfm-radio-metadata-period 15
  "When listening to Last.fm Radio every how many seconds should
emms-lastfm poll for metadata? If set to nil, there won't be any
polling at all.

The default is 15: That means that the mode line will display the
wrong (last) track's data for a maximum of 15 seconds. If your
network connection has a big latency this value may be too
high. (But then streaming a 128KHz mp3 won't be fun anyway.)"
  :type '(choice integer
                 (const :tag "Disable" nil))
  :group 'emms-lastfm)

(defun emms-lastfm-radio-sentinel (&rest args)
  (let ((buffer (current-buffer)))
    (emms-http-decode-buffer buffer)
    (if (string= (emms-key-value "response") "OK")
        (progn
          (kill-buffer buffer)
          (emms-play-url emms-lastfm-radio-stream-url)
          (when emms-lastfm-radio-metadata-period
            (setq emms-lastfm-timer
                  (run-with-timer 0 emms-lastfm-radio-metadata-period
                                  'emms-lastfm-radio-request-metadata))
            (add-hook 'emms-player-stopped-hook
                      'emms-lastfm-cancel-timer))
          (message "EMMS: Playing Last.fm stream"))
      (kill-buffer buffer)
      (message "EMMS: Bad response from Last.fm"))))

(defun emms-lastfm-np (&optional insertp callback)
  "Show the currently-playing lastfm radio tune.

If INSERTP is non-nil, insert the description into the current
buffer instead.

If CALLBACK is a function, call it with the current buffer and
description as arguments instead of displaying the description or
inserting it."
  (interactive "P")
  (emms-lastfm-radio-request-metadata
   (lambda (status insertp buffer callback)
     (let ((response-buf (current-buffer))
           artist title)
       (emms-http-decode-buffer response-buf)
       (setq artist (emms-key-value "artist")
             title  (emms-key-value "track"))
       (kill-buffer response-buf)
       (let ((msg (if (and title artist)
                      (format emms-show-format
                              (format "%s - %s" artist title))
                    "Nothing playing right now")))
         (cond ((functionp callback)
                (when (and title artist)
                  (funcall callback buffer msg)))
               ((and insertp title artist)
                (with-current-buffer buffer
                  (insert msg)))
               (t (message msg))))))
   (list insertp (current-buffer) callback)))

(defun emms-lastfm-radio-similar-artists (artist)
  "Plays the similar artist radio of ARTIST."
  (interactive "sArtist: ")
  (emms-lastfm-radio (concat "lastfm://artist/"
                             artist
                             "/similarartists")))

(defun emms-lastfm-radio-global-tag (tag)
  "Plays the global tag radio of TAG."
  (interactive "sGlobal Tag: ")
  (emms-lastfm-radio (concat "lastfm://globaltags/" tag)))

(defun emms-lastfm-radio-artist-fan (artist)
  "Plays the artist fan radio of ARTIST."
  (interactive "sArtist: ")
  (emms-lastfm-radio (concat "lastfm://artist/" artist "/fans")))

(defun emms-lastfm-radio-love ()
  "Inform Last.fm that you love the currently playing song."
  (interactive)
  (emms-lastfm-radio-rating "love"))

(defun emms-lastfm-radio-skip ()
  "Inform Last.fm that you want to skip the currently playing
song."
  (interactive)
  (emms-lastfm-radio-rating "skip"))

(defun emms-lastfm-radio-ban ()
  "Inform Last.fm that you want to ban the currently playing
song."
  (interactive)
  (emms-lastfm-radio-rating "ban"))

(defun emms-lastfm-radio-rating (command)
  (let ((url-request-method "GET"))
    (url-retrieve
     (concat emms-lastfm-radio-base-url
             "control.php?"
             "session="  emms-lastfm-radio-session
             "&command=" command
             "&debug="   (number-to-string 0))
     'emms-lastfm-radio-rating-sentinel)))

(defun emms-lastfm-radio-rating-sentinel (&rest args)
  (let ((buffer (current-buffer)))
    (emms-http-decode-buffer buffer)
    (if (string= (emms-key-value "response") "OK")
        (message "EMMS: Rated current track")
      (message "EMMS: Rating failed"))
    (kill-buffer buffer)))

(defun emms-lastfm-radio-request-metadata (&optional fn data)
  "Request the metadata of the current song and display it.

If FN is given, call it instead of
`emms-lastfm-radio-request-metadata-sentinel', with DATA as its
first parameter.

If DATA is given, it should be a list."
  (interactive)
  (let ((url-request-method "GET")
        (url-show-status nil))
    (url-retrieve
     (concat emms-lastfm-radio-base-url
             "np.php?"
             "session=" emms-lastfm-radio-session
             "&debug="  (number-to-string 0))
     (or fn 'emms-lastfm-radio-request-metadata-sentinel)
     data)))

(defun emms-lastfm-radio-request-metadata-sentinel (&rest args)
  (let ((buffer (current-buffer)))
    (emms-http-decode-buffer buffer)
    (let ((artist (emms-key-value "artist"))
          (title  (emms-key-value "track")))
      (kill-buffer buffer)
      (setq emms-mode-line-string (format emms-mode-line-format
                                          (concat artist " - " title)))
      (force-mode-line-update))))


;;; Utility functions

(defun emms-read-line ()
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun emms-key-value (key)
  "Returns the value of KEY. The buffer has to contain a
key-value list like:

foo=bar
x=17"
  (goto-char (point-min))
  (when (re-search-forward (concat "^" key "=") nil t)
    (buffer-substring-no-properties (point) (line-end-position))))

(provide 'emms-lastfm)
;;; emms-lastfm.el ends here
