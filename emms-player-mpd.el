;; emms-player-mpd.el --- MusicPD support for EMMS

;; Copyright (C) 2005  Free Software Foundation, Inc.

;; Author: Michael Olson (mwolson AT gnu DOT org)

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Benefits

;; MusicPD features crossfade, very little skipping, minor CPU usage,
;; many clients, many supported output formats, fast manipulation via
;; network processes, and good abstraction of client and server.

;;; MusicPD setup

;; If you want to set up a local MusicPD server, you'll need to have
;; mpd installed.  If you want to use a remote server instance, no
;; installation is needed.

;; The website is at http://musicpd.org/.  Debian packages are
;; available.  I recommend getting the latest development version; see
;; http://mpd.wikicities.com/wiki/Subversion for nightly Debian
;; packages and the svn repo.
;;
;; Copy the example configuration for mpd into ~/.mpdconf and edit it
;; to your needs.  Use your top level music directory for
;; music_directory.  If your playlists use absolute file names, be
;; certain that music_directory has the leading directory part.
;;
;; Before you try to play anything, but after setting up the above,
;; run `mkdir ~/.mpd && mpd --create-db' to create MusicPD's track
;; database.
;;
;; Check to see if mpd is running.  It must be running as a daemon for
;; you to be able to play anything.  Launch it by executing "mpd".  It
;; can be killed later with "mpd --kill" (or just "killall mpd" if
;; you're not using the latest development version).

;;; EMMS setup

;; Add the following to your config.
;;
;; (require 'emms-player-mpd)

;; Adjust `emms-player-mpd-server-name' and
;; `emms-player-mpd-server-port' to match the location and port of
;; your MusicPD server.
;;
;; (setq emms-player-mpd-server-name "localhost")
;; (setq emms-player-mpd-server-port "6600")

;; To get track info from MusicPD, do the following.
;;
;; (add-to-list 'emms-info-functions 'emms-info-mpd)

;; Add 'emms-player-mpd to the top of `emms-player-list'.
;;
;; (add-to-list 'emms-player-list 'emms-player-mpd)

;; If you use absolute file names in your m3u playlists (which is most
;; likely), make sure you set `emms-player-mpd-music-directory' to the
;; value of "music_directory" from your MusicPD config.  There are
;; additional options available as well, but the defaults should be
;; sufficient for most uses.

;; You will have to set `emms-player-mpd-sync-playlist' to non-nil if
;; you want to use MusicPD in a similar way as most other EMMS
;; backends.  If your EMMS playlist contains music files rather than
;; playlists, set this to non-nil, otherwise if your EMMS playlist
;; contains stored playlists, leave this set to nil.

;;; TODO

;; Write a function that imports the current MusicPD playlist into
;; EMMS.

(require 'emms-player-simple)

(defun emms-player-mpd-get-supported-regexp ()
  "Returns a regexp of file extensions that MusicPD supports,
or nil if we cannot figure it out."
  (let ((out (split-string (shell-command-to-string "mpd --version")
                           "\n"))
        supported)
    ;; Get last non-empty line
    (while (car out)
      (when (not (string= (car out) ""))
        (setq supported (car out)))
      (setq out (cdr out)))
    ;; Create regexp
    (when (and (stringp supported)
               (not (string= supported "")))
      (concat "\\`http://\\|\\.\\(m3u\\|pls\\|"
              (mapconcat 'identity (delq nil (split-string supported))
                         "\\|")
              "\\)\\'"))))

(defvar emms-player-mpd-supported-regexp
  ;; Use a sane default, just in case
  (or (emms-player-mpd-get-supported-regexp)
      "\\.\\(m3u\\|ogg\\|flac\\|mp3\\|wav\\|mod\\|aac\\)\\'")
  "Formats supported by MusicPD Client.")

(defcustom emms-player-mpd-music-directory nil
  "The value of 'music_directory' in your MusicPD configuration file.
You need this if your playlists use absolute file names, otherwise
leave it set to nil."
  ;; The :format part ensures that entering directories happens on the
  ;; next line, where there is more space to work with
  :type '(choice :format "%{%t%}:\n   %[Value Menu%] %v"
                 (const nil)
                 directory)
  :group 'emms-player-mpd)

(defcustom emms-player-mpd-connect-function
  (if (and (fboundp 'open-network-stream-nowait)
           ;; CVS Emacs claims to define open-network-stream-nowait on
           ;; windows, however, it does, in fact, not work.
           (not (memq system-type '(windows-nt cygwin ms-dos darwin))))
      'open-network-stream-nowait
    'open-network-stream)
  "Function used to initiate the connection to MusicPD.
It should take same arguments as `open-network-stream' does.

This will usually be auto-detected correctly."
  :type 'function
  :group 'emms-player-mpd)

(defcustom emms-player-mpd-server-name "localhost"
  "The MusicPD server that we should connect to."
  :type 'string
  :group 'emms-player-mpd)

(defcustom emms-player-mpd-server-port "6600"
  "The port of the MusicPD server that we should connect to."
  :type '(choice number string)
  :group 'emms-player-mpd)

(defcustom emms-player-mpd-timeout 10
  "The maximum acceptable delay (in seconds) while waiting for a
response from the MusicPD server."
  :type 'integer
  :group 'emms-player-mpd)

(defcustom emms-player-mpd-verbose nil
  "Whether to provide notifications for server connection events
and errors."
  :type 'boolean
  :group 'emms-player-mpd)

(defcustom emms-player-mpd-sync-playlist nil
  "Whether to syncronize the EMMS playlist with the MusicPD playlist.

If your EMMS playlist contains stored playlists, leave this set
to nil.

If your EMMS playlist contains music files rather than playlists,
set this to non-nil."
  :type 'boolean
  :group 'emms-player-mpd)

(defcustom emms-player-mpd-check-interval 2
  "The number of seconds to wait before checking to see whether
MusicPD has moved on to the next song.

This is used only if `emms-player-mpd-sync-playlist' is non-nil"
  :type 'integer
  :group 'emms-player-mpd)

(define-emms-simple-player mpd '(file url playlist)
  emms-player-mpd-supported-regexp "mpd")

(emms-player-set emms-player-mpd
                 'pause
                 'emms-player-mpd-pause)

(emms-player-set emms-player-mpd
                 'resume
                 'emms-player-mpd-pause)

(emms-player-set emms-player-mpd
                 'seek
                 'emms-player-mpd-seek)

;;; Dealing with the MusicPD network process

(defvar emms-player-mpd-process nil)
(defvar emms-player-mpd-returned-data nil)

(defvar emms-player-mpd-playlist-id nil)
(make-variable-buffer-local 'emms-player-mpd-playlist-id)

(defvar emms-player-mpd-current-song nil)
(defvar emms-player-mpd-status-timer nil)

(defun emms-player-mpd-sentinel (proc str)
  "The process sentinel for MusicPD."
  (let ((status (process-status proc)))
    (cond ((memq status '(exit signal closed))
           (when emms-player-mpd-verbose
             (message "Closed MusicPD process"))
           (setq emms-player-mpd-process nil))
          ((memq status '(run open))
           (when emms-player-mpd-verbose
             (message "MusicPD process started successfully")))
          (t
           (when emms-player-mpd-verbose
             (message "Other MusicPD status change: %s" status))))))

(defun emms-player-mpd-filter (proc string)
  "The process filter for MusicPD."
  (setq emms-player-mpd-returned-data string))

(defun emms-player-mpd-ensure-process ()
  "Make sure that a MusicPD process is currently active."
  (unless (and emms-player-mpd-process
               (processp emms-player-mpd-process)
               (memq (process-status emms-player-mpd-process) '(run open)))
    (setq emms-player-mpd-process
          (funcall emms-player-mpd-connect-function "mpd"
                   nil
                   emms-player-mpd-server-name
                   emms-player-mpd-server-port))
    (set-process-sentinel emms-player-mpd-process
                          'emms-player-mpd-sentinel)
    (set-process-filter emms-player-mpd-process
                        'emms-player-mpd-filter)
    (if (fboundp 'set-process-query-on-exit-flag)
        (set-process-query-on-exit-flag emms-player-mpd-process nil)
      (process-kill-without-query emms-player-mpd-process))
    ;; wait a bit for the process to finish starting, as it likes to
    ;; send us an "OK" message initially
    (accept-process-output emms-player-mpd-process 0 200)))

(defun emms-player-mpd-send (command)
  "Send the given COMMAND to the MusicPD server."
  (emms-player-mpd-ensure-process)
  (unless (string= (substring command -1) "\n")
    (setq command (concat command "\n")))
  (process-send-string emms-player-mpd-process command)
  nil)

(defun emms-player-mpd-send-and-wait (command)
  "Send the given COMMAND to the MusicPD server and await a response,
which is returned."
  (setq emms-player-mpd-returned-data nil)
  (emms-player-mpd-send command)
  (accept-process-output emms-player-mpd-process emms-player-mpd-timeout)
  emms-player-mpd-returned-data)

;;; Helper functions

(defun emms-player-mpd-parse-response (response)
  "Convert the given MusicPD response into a list.
The car of the list is special:
If an error has occurred, it will contain a cons cell whose car is
an error number and whose cdr is the corresponding message.
Otherwise, it will be nil."
  (when (stringp response)
    (save-match-data
      (let* ((data (split-string response "\n"))
             (cruft (last data 3))
             (status (if (string= (cadr cruft) "")
                         (car cruft)
                       (cadr cruft))))
        (setcdr cruft nil)
        (when (string-match "^OK\\( MPD \\)?" (car data))
          (setq data (cdr data)))
        (if (string-match "^ACK \\[\\([0-9]+\\)@[0-9]+\\] \\(.+\\)" status)
            (cons (cons (match-string 1 status)
                        (match-string 2 status))
                  data)
          (cons nil data))))))

(defun emms-player-mpd-get-alist (info)
  "Turn the given parsed INFO from MusicPD into an alist.
The format of the alist is (name . value)."
  (when (and info
             (null (car info))          ; no error has occurred
             (cdr info))                ; data exists
    (let (alist)
      (dolist (line (cdr info))
        (when (string-match "\\`\\([^:]+\\):\\s-*\\(.+\\)" line)
          (let ((name (match-string 1 line))
                (value (match-string 2 line)))
            (when (and name value)
              (setq name (downcase name))
              (add-to-list 'alist (cons name value) t)))))
      alist)))

(defun emms-player-mpd-get-playlist-id ()
  "Get the current playlist ID from MusicPD."
  (let ((info (emms-player-mpd-get-alist
               (emms-player-mpd-parse-response
                (emms-player-mpd-send-and-wait "status")))))
    (cdr (assoc "playlist" info))))

(defun emms-player-mpd-get-current-song ()
  "Get the current song from MusicPD.
This is in the form of a number that indicates the position of
the song on the current playlist."
  (let ((info (emms-player-mpd-get-alist
               (emms-player-mpd-parse-response
                (emms-player-mpd-send-and-wait "status")))))
    (cdr (assoc "song" info))))

(defun emms-player-mpd-sync-from-emms ()
  "Synchronize the MusicPD playlist with the contents of the
current EMMS playlist."
  (emms-player-mpd-clear)
  (with-current-emms-playlist
   (save-excursion
     (mapc #'emms-player-mpd-add
           (nreverse
            (emms-playlist-tracks-in-region (point-min) (point-max)))))
   (setq emms-player-mpd-playlist-id (emms-player-mpd-get-playlist-id))))

(defun emms-player-mpd-detect-song-change ()
  "Detect whether a song change has occurred.
This is usually called by a timer."
  (let ((song (emms-player-mpd-get-current-song)))
    (unless (or (null emms-player-mpd-current-song)
                (null song)
                (string= song emms-player-mpd-current-song))
      (setq emms-player-mpd-current-song song)
      (with-current-emms-playlist
        (emms-playlist-select (progn
                                (goto-line (1+ (string-to-number song)))
                                (point)))))))

(defun emms-player-mpd-get-filename (file)
  "Turn FILE into something that MusicPD can understand.
This usually means removing a prefix."
  (if (or (null emms-player-mpd-music-directory)
          (not (eq (aref file 0) ?/))
          (string-match "\\`http://" file))
      file
    (file-relative-name file emms-player-mpd-music-directory)))

;;; MusicPD commands

(defun emms-player-mpd-clear ()
  "Clear the playlist."
  (emms-player-mpd-send "clear"))

(defun emms-player-mpd-add-file (file)
  "Add FILE to the current MusicPD playlist.
If we succeed in adding the file, return non-nil, nil otherwise."
  (setq file (emms-player-mpd-get-filename file))
  (let ((output (emms-player-mpd-parse-response
                 (emms-player-mpd-send-and-wait (concat "add " file)))))
    (if (car output)
        (progn
          (when emms-player-mpd-verbose
            (message "MusicPD error: %s: %s" file (cdar output)))
          nil)
      t)))

(defun emms-player-mpd-add-playlist (playlist)
  "Load contents of PLAYLIST into MusicPD by adding each line.
This handles both m3u and pls type playlists."
  ;; This allows us to keep playlists anywhere and not worry about
  ;; having to mangle their names.  Also, mpd can't handle pls
  ;; playlists by itself.
  (let ((pls-p (if (string-match "\\.pls\\'" playlist) t nil))
        any-success)
    (mapc #'(lambda (file)
              (when pls-p
                (if (string-match "\\`File[0-9]*=\\(.*\\)\\'" file)
                    (setq file (match-string 1 file))
                  (setq file "")))
              (unless (or (string= file "")
                          (string-match "\\`#" file))
                (when (emms-player-mpd-add-file file)
                  (setq any-success t))))
          (split-string (with-temp-buffer
                          (insert-file-contents playlist)
                          (buffer-string))
                        "\n"))
    any-success))

(defun emms-player-mpd-add (track)
  "Add TRACK to the MusicPD playlist."
  (let ((name (emms-track-get track 'name))
        (type (emms-track-get track 'type)))
    (cond ((eq type 'url)
           (emms-player-mpd-add-file name))
          ((or (eq type 'playlist)
               (string-match "\\.\\(m3u\\|pls\\)\\'" name))
           (emms-player-mpd-add-playlist name))
          ((eq type 'file)
           (emms-player-mpd-add-file name)))))

;;; EMMS API

(defun emms-player-mpd-play (&optional id)
  "Play whatever is in the current MusicPD playlist.
If ID is specified, play the song at that position in the MusicPD
playlist."
  (if id
      (progn
        (unless (stringp id)
          (setq id (number-to-string id)))
        (emms-player-mpd-send (concat "play " id))
        (setq emms-player-mpd-current-song id))
    (emms-player-mpd-send "play")))

(defun emms-player-mpd-start-and-sync (track)
  "Starts a process playing TRACK.
This is called if `emms-player-mpd-sync-playlist' is non-nil.

It ensures that MusicPD's playlist is up-to-date with EMMS's
playlist, and then plays the current track."
  (let ((id (emms-player-mpd-get-playlist-id)))
    (unless (and (stringp emms-player-mpd-playlist-id)
                 (string= emms-player-mpd-playlist-id id))
      (emms-player-mpd-sync-from-emms))
    (with-current-emms-playlist
      (emms-player-mpd-play (1- (line-number-at-pos
                                 emms-playlist-selected-marker)))))
  (unless emms-player-mpd-status-timer
    (setq emms-player-mpd-status-timer
          (run-at-time t emms-player-mpd-check-interval
                       'emms-player-mpd-detect-song-change))))

(defun emms-player-mpd-start (track)
  "Starts a process playing TRACK."
  (interactive)
  (if emms-player-mpd-sync-playlist
      (emms-player-mpd-start-and-sync track)
    (emms-player-mpd-clear)
    (when (emms-player-mpd-add track)
      ;; if we have loaded the item successfully, play it
      (emms-player-mpd-play))))

(defun emms-player-mpd-stop ()
  "Stop the currently playing song."
  (interactive)
  (emms-cancel-timer emms-player-mpd-status-timer)
  (setq emms-player-mpd-status-timer nil)
  (emms-player-mpd-send "stop"))

(defun emms-player-mpd-pause ()
  "Pause the currently playing song."
  (interactive)
  (emms-player-mpd-send "pause"))

(defun emms-player-mpd-seek (sec)
  "Seek backward or forward by SEC seconds, depending on sign of SEC."
  (interactive)
  (emms-player-mpd-send (format "seek %s%d"
                                (if (> sec 0) "+" "")
                                sec)))

(defun emms-player-mpd-next ()
  "Move forward by one track in MusicPD's internal playlist."
  (interactive)
  (emms-player-mpd-send "next"))

(defun emms-player-mpd-previous ()
  "Move backward by one track in MusicPD's internal playlist."
  (interactive)
  (emms-player-mpd-send "previous"))

;;; Track info

(defun emms-info-mpd (track &optional info)
  "Add track information to TRACK.
This is a useful addition to `emms-info-functions'.
If INFO is specified, use that instead of acquiring the necessary
info from MusicPD."
  (let (file)
    (unless info
      (when (and (eq 'file (emms-track-type track))
                 emms-player-mpd-music-directory
                 (setq file (emms-player-mpd-get-filename
                             (emms-track-name track)))
                 (string-match emms-player-mpd-supported-regexp file))
        (setq info (emms-player-mpd-get-alist
                    (emms-player-mpd-parse-response
                     (emms-player-mpd-send-and-wait
                      (concat "find filename " file)))))))
    (when info
      (dolist (data info)
        (let ((name (car data))
              (value (cdr data)))
          (setq name (cond ((string= name "artist") 'info-artist)
                           ((string= name "title") 'info-title)
                           ((string= name "album") 'info-album)
                           ((string= name "track") 'info-tracknumber)
                           ((string= name "date") 'info-year)
                           ((string= name "genre") 'info-genre)
                           ((string= name "time")
                            (setq value (string-to-number value))
                            'info-playing-time)
                           (t nil)))
          (when name
            (emms-track-set track name value)))))))

(defun emms-player-mpd-show (&optional insertp)
  "Describe the current EMMS track in the minibuffer.
If INSERTP is non-nil, insert the description into the current buffer instead.
This function uses `emms-show-format' to format the current track.
It differs from `emms-show' in that it asks MusicPD for the current track,
rather than EMMS."
  (interactive "P")
  (let* ((info (emms-player-mpd-get-alist
                (emms-player-mpd-parse-response
                 (emms-player-mpd-send-and-wait "currentsong"))))
         (track (emms-dictionary '*track*))
         desc string)
    (when info
      (emms-track-set track 'type 'file)
      (emms-track-set track 'name (cdr (assoc "file" info)))
      (emms-info-mpd track info)
      (setq desc (emms-track-description track)))
    (setq string (if desc
                     (format emms-show-format desc)
                   "Nothing playing right now"))
    (if insertp
        (insert string)
      (message "%s" string))))

(provide 'emms-player-mpd)

;;; emms-player-mpd.el ends here
