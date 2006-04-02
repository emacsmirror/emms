;; emms-player-mpd.el --- MusicPD support for EMMS

;; Copyright (C) 2005, 2006 Free Software Foundation, Inc.

;; Author: Michael Olson (mwolson AT gnu DOT org)

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

;; You can set `emms-player-mpd-sync-playlist' to nil if your master
;; EMMS playlist contains only stored playlists.

;; If at any time you wish to replace the current EMMS playlist buffer
;; with the contents of the MusicPD playlist, type
;; M-x emms-player-mpd-connect.
;;
;; This will also run the relevant seek functions, so that if you use
;; emms-playing-time, the displayed time will be accurate.

(require 'emms-player-simple)
(require 'emms-source-playlist) ; for emms-source-file-parse-playlist

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

(defgroup emms-player-mpd nil
  "EMMS player for MusicPD."
  :group 'emms-player
  :prefix "emms-player-mpd-")

(defcustom emms-player-mpd (emms-player 'emms-player-mpd-start
                                        'emms-player-mpd-stop
                                        'emms-player-mpd-playable-p)
 "*Parameters for the MusicPD player."
 :type '(cons symbol alist)
 :group 'emms-player-mpd)

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

(defcustom emms-player-mpd-connect-function 'open-network-stream
  "Function used to initiate the connection to MusicPD.
It should take same arguments as `open-network-stream' does."
  :type 'function
  :group 'emms-player-mpd)

(defcustom emms-player-mpd-server-name (or (getenv "MPD_HOST") "localhost")
  "The MusicPD server that we should connect to."
  :type 'string
  :group 'emms-player-mpd)

(defcustom emms-player-mpd-server-port (or (getenv "MPD_PORT") "6600")
  "The port of the MusicPD server that we should connect to."
  :type '(choice number string)
  :group 'emms-player-mpd)

(defcustom emms-player-mpd-timeout 5
  "The maximum acceptable delay (in seconds) while waiting for a
response from the MusicPD server."
  :type 'integer
  :group 'emms-player-mpd)

(defcustom emms-player-mpd-check-interval 2
  "How long to wait before checking to see whether MusicPD has
moved on to the next song.  This may be an integer or a floating
point number.

This is used only if `emms-player-mpd-sync-playlist' is non-nil"
  :type 'integer
  :group 'emms-player-mpd)

(defcustom emms-player-mpd-verbose nil
  "Whether to provide notifications for server connection events
and errors."
  :type 'boolean
  :group 'emms-player-mpd)

(defcustom emms-player-mpd-sync-playlist t
  "Whether to syncronize the EMMS playlist with the MusicPD playlist.

If your EMMS playlist contains music files rather than playlists,
leave this set to non-nil.

If your EMMS playlist contains stored playlists, set this to nil."
  :type 'boolean
  :group 'emms-player-mpd)

(emms-player-set emms-player-mpd
                 'regex
                 emms-player-mpd-supported-regexp)

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

(defvar emms-player-mpd-blocked nil)
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

(defun emms-player-mpd-block ()
  "Block input for MusicPD, waiting if currently blocked.
The maximum amount is determined by `emms-player-mpd-timeout'."
  (with-timeout (emms-player-mpd-timeout)
    (while emms-player-mpd-blocked
      (sit-for 0.20)))
  (setq emms-player-mpd-blocked t))

(defun emms-player-mpd-unblock ()
  "Unblock input for MusicPD."
  (setq emms-player-mpd-blocked nil))

(defun emms-player-mpd-send (command)
  "Send the given COMMAND to the MusicPD server and await a response,
which is returned."
  (emms-player-mpd-ensure-process)
  (unless (string= (substring command -1) "\n")
    (setq command (concat command "\n")))
  (let (response)
    (unwind-protect
        (progn
          (emms-player-mpd-block)
          (setq emms-player-mpd-returned-data nil)
          (process-send-string emms-player-mpd-process command)
          (accept-process-output emms-player-mpd-process
                                 emms-player-mpd-timeout))
      (setq response emms-player-mpd-returned-data)
      (emms-player-mpd-unblock))
    response))

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

(defun emms-player-mpd-parse-line (line)
  "Turn the given LINE from MusicPD into a cons cell.
The format of the cell is (name . value)."
  (when (string-match "\\`\\([^:]+\\):\\s-*\\(.+\\)" line)
    (let ((name (match-string 1 line))
          (value (match-string 2 line)))
      (if (and name value)
          (progn
            (setq name (downcase name))
            (cons name value))
        nil))))

(defun emms-player-mpd-get-alist (info)
  "Turn the given parsed INFO from MusicPD into an alist.
The format of the alist is (name . value)."
  (when (and info
             (null (car info))          ; no error has occurred
             (cdr info))                ; data exists
    (let ((alist nil)
          cell old-cell)
      (dolist (line (cdr info))
        (when (setq cell (emms-player-mpd-parse-line line))
          (if (setq old-cell (assoc (car cell) alist))
              (setcdr old-cell (cdr cell))
            (setq alist (cons cell alist)))))
      alist)))

(defun emms-player-mpd-get-alists (info)
  "Turn the given parsed INFO from MusicPD into an list of alists.
The format of the alist is (name . value).

The list will be in reverse order."
  (when (and info
             (null (car info))          ; no error has occurred
             (cdr info))                ; data exists
    (let ((alists nil)
          (alist nil)
          cell)
      (dolist (line (cdr info))
        (when (setq cell (emms-player-mpd-parse-line line))
          (if (assoc (car cell) alist)
              (setq alists (cons alist alists)
                    alist (list cell))
            (setq alist (cons cell alist)))))
      (when alist
        (setq alists (cons alist alists)))
      alists)))

(defun emms-player-mpd-get-tracks ()
  "Get the current playlist from MusicPD in the form of a list of
EMMS tracks."
  (let ((songs (emms-player-mpd-get-alists
                (emms-player-mpd-parse-response
                 (emms-player-mpd-send "playlistinfo"))))
        (tracks nil))
    (when songs
      (dolist (song-info songs)
        (let (cell)
          (when (setq cell (assoc "file" song-info))
            (let ((track (emms-track 'file (cdr cell))))
              (emms-info-mpd track song-info)
              (setq tracks (cons track tracks))))))
      tracks)))

(defun emms-player-mpd-get-status ()
  "Get status information from MusicPD.
It will be returned in the form of an alist."
  (emms-player-mpd-get-alist
   (emms-player-mpd-parse-response
    (emms-player-mpd-send "status"))))

(defun emms-player-mpd-get-playlist-id (&optional info)
  "Get the current playlist ID from MusicPD.
If INFO is specified, use that instead of acquiring the necessary
info from MusicPD."
  (unless info
    (setq info (emms-player-mpd-get-status)))
  (cdr (assoc "playlist" info)))

(defun emms-player-mpd-get-current-song (&optional info)
  "Get the current song from MusicPD.
This is in the form of a number that indicates the position of
the song on the current playlist.

If INFO is specified, use that instead of acquiring the necessary
info from MusicPD."
  (unless info
    (setq info (emms-player-mpd-get-status)))
  (cdr (assoc "song" info)))

(defun emms-player-mpd-get-state (&optional info)
  "Get the current state of the MusicPD server.
This is either \"play\", \"stop\", or \"pause\".

If INFO is specified, use that instead of acquiring the necessary
info from MusicPD."
  (unless info
    (setq info (emms-player-mpd-get-status)))
  (cdr (assoc "state" info)))

(defun emms-player-mpd-get-playing-time (&optional info)
  "Get the number of seconds that the current song has been playing,
or nil if we cannot obtain this information.

If INFO is specified, use that instead of acquiring the necessary
info from MusicPD."
  (unless info
    (setq info (emms-player-mpd-get-status)))
  (let ((time (cdr (assoc "time" info))))
    (when (and time
               (string-match "\\`\\([0-9]+\\):" time))
      (string-to-number (match-string 1 time)))))

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

(defun emms-player-mpd-sync-from-mpd ()
  "Synchronize the EMMS playlist with the contents of the current
MusicPD playlist."
  (with-current-emms-playlist
    (emms-playlist-clear)
    (mapc #'emms-playlist-insert-track (emms-player-mpd-get-tracks))
    (let* ((info (emms-player-mpd-get-status))
           (id (emms-player-mpd-get-playlist-id info))
           (song (emms-player-mpd-get-current-song info)))
      (setq emms-player-mpd-playlist-id id)
      (if song
          (progn
            (goto-line (1+ (string-to-number song)))
            (emms-playlist-select (point)))
        (goto-char (point-min))))))

(defun emms-player-mpd-detect-song-change (&optional info)
  "Detect whether a song change has occurred.
This is usually called by a timer.

If INFO is specified, use that instead of acquiring the necessary
info from MusicPD."
  (unless info
    (setq info (emms-player-mpd-get-status)))
  (let ((song (emms-player-mpd-get-current-song info))
        (status (emms-player-mpd-get-state info))
        (time (emms-player-mpd-get-playing-time info)))
    (cond ((string= status "stop")
           (emms-cancel-timer emms-player-mpd-status-timer)
           (setq emms-player-mpd-status-timer nil)
           (setq emms-player-stopped-p t)
           (emms-player-stopped))
          ((string= status "pause")
           nil)
          ((string= status "play")
           (unless (or (null song)
                       (and emms-player-mpd-current-song
                            (string= song emms-player-mpd-current-song)))
             (setq emms-player-mpd-current-song song)
             (run-hooks 'emms-player-stopped-hook)
             (with-current-emms-playlist
               (emms-playlist-select (progn
                                       (goto-line (1+ (string-to-number song)))
                                       (point))))
             (run-hooks 'emms-player-started-hook)
             (when time
               (run-hook-with-args 'emms-player-seeked-functions time)))))))

(defun emms-player-mpd-get-filename (file)
  "Turn FILE into something that MusicPD can understand.
This usually means removing a prefix."
  (if (or (null emms-player-mpd-music-directory)
          (not (eq (aref file 0) ?/))
          (string-match "\\`http://" file))
      file
    (file-relative-name file emms-player-mpd-music-directory)))

(defun emms-player-mpd-quote-file (file)
  "Escape special characters in FILE and surround in double-quotes."
  (concat "\""
          (emms-replace-regexp-in-string
           "\"" "\\\\\""
           (emms-replace-regexp-in-string "\\\\" "\\\\\\\\" file))
          "\""))

;;; MusicPD commands

(defun emms-player-mpd-clear ()
  "Clear the playlist."
  (emms-player-mpd-send "clear"))

(defun emms-player-mpd-add-file (file)
  "Add FILE to the current MusicPD playlist.
If we succeed in adding the file, return non-nil, nil otherwise."
  (setq file (emms-player-mpd-get-filename file))
  (let ((output (emms-player-mpd-parse-response
                 (emms-player-mpd-send
                  (concat "add " (emms-player-mpd-quote-file file))))))
    (if (car output)
        (progn
          (when emms-player-mpd-verbose
            (message "MusicPD error: %s: %s" file (cdar output)))
          nil)
      t)))

(defun emms-player-mpd-add-playlist (playlist)
  "Load contents of PLAYLIST into MusicPD by adding each line.
This handles both m3u and pls type playlists."
  ;; This is useful for playlists of playlists
  (with-temp-buffer
    (insert-file-contents playlist)
    (goto-char (point-min))
    (let ((list (cond ((emms-source-playlist-m3u-p)
                       (emms-source-playlist-parse-m3u-1))
                      ((emms-source-playlist-pls-p)
                       (emms-source-playlist-parse-pls-1))
                      (t nil)))
          (any-success nil))
      (dolist (file list)
        (when (emms-player-mpd-add-file file)
          (setq any-success t)))
      any-success)))

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

(defun emms-player-mpd-playable-p (track)
  "Return non-nil when we can play this track."
  (and (memq (emms-track-type track) '(file url playlist))
       (string-match (emms-player-get emms-player-mpd 'regex)
                     (emms-track-name track))))

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
  (when emms-player-mpd-status-timer
    (emms-cancel-timer emms-player-mpd-status-timer))
  (setq emms-player-mpd-status-timer
        (run-at-time t emms-player-mpd-check-interval
                     'emms-player-mpd-detect-song-change)))

;;;###autoload
(defun emms-player-mpd-connect ()
  "Connect to MusicPD and retrieve its current playlist.
Afterward, the status of MusicPD will be tracked."
  (interactive)
  (when emms-player-mpd-status-timer
    (emms-cancel-timer emms-player-mpd-status-timer)
    (setq emms-player-mpd-status-timer nil))
  (emms-player-mpd-sync-from-mpd)
  (setq emms-player-mpd-current-song nil)
  (let* ((info (emms-player-mpd-get-status))
         (state (emms-player-mpd-get-state info)))
    (unless (string= state "stop")
      (setq emms-player-playing-p 'emms-player-mpd))
    (when (string= state "pause")
      (setq emms-player-paused-p t))
    (unless (string= state "stop")
      (emms-player-mpd-detect-song-change info)
      (setq emms-player-mpd-status-timer
            (run-at-time t emms-player-mpd-check-interval
                         'emms-player-mpd-detect-song-change)))))

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
  (setq emms-player-stopped-p t)
  (emms-player-mpd-send "stop")
  (emms-player-stopped))

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
  (unless info
    (let (file)
      (when (and (eq 'file (emms-track-type track))
                 emms-player-mpd-music-directory
                 (setq file (emms-player-mpd-get-filename
                             (emms-track-name track)))
                 (string-match emms-player-mpd-supported-regexp file)
                 (not (string-match "\\`http://" file)))
        (setq info (condition-case nil
                       (emms-player-mpd-get-alist
                        (emms-player-mpd-parse-response
                         (emms-player-mpd-send
                          (concat "find filename "
                                  (emms-player-mpd-quote-file file)))))
                     (error nil))))))
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
          (emms-track-set track name value))))))

;;;###autoload
(defun emms-player-mpd-show (&optional insertp)
  "Describe the current EMMS track in the minibuffer.
If INSERTP is non-nil, insert the description into the current buffer instead.
This function uses `emms-show-format' to format the current track.
It differs from `emms-show' in that it asks MusicPD for the current track,
rather than EMMS."
  (interactive "P")
  (let* ((info (emms-player-mpd-get-alist
                (emms-player-mpd-parse-response
                 (emms-player-mpd-send "currentsong"))))
         (track (emms-dictionary '*track*))
         (desc nil)
         string)
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
