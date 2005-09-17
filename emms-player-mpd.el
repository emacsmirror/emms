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

;; MusicPD features crossfade, very little skipping, many clients,
;; many supported output formats, and good abstraction of client and
;; server, among other things.

;;; MusicPD setup

;; You'll need to have both mpc and mpd installed.  The website is at
;; http://musicpd.org/.  Debian packages are available.  I recommend
;; getting the latest development version; see
;; http://mpd.wikicities.com/wiki/Subversion for nightly Debian
;; packages and the svn repo.
;;
;; Copy the example configuration for mpd into ~/.mpdconf and edit it
;; to your needs.  I using your top level music directory for
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

;; emms-pause and emms-seek are evil functions.  You should hack them
;; so that they accept emms-player-mpd.

;; Add "emms-player-mpd" to the top of `emms-player-list'.  If you use
;; absolute file names in your m3u playlists, make sure you set
;; `emms-player-mpd-music-directory' to the value of "music_directory"
;; from your MusicPD config.

;;; TODO

;; If you try to play individual songs, the tracks will not advance.
;; I recommend playing playlists instead.  This should be addresed
;; eventually, though, perhaps with a connection to the mpd process.
;;
;; Instead of relying on mpc, we should get MPD_HOST and MPD_PORT from
;; the environment (or specify them as options), open a network
;; connection, send the command you want, then "\nclose" to close the
;; connection.  Alternatively, the process can be left open (omitting
;; "close" but keeping "\n") for more commands.  Apparently the
;; process closes automatically after a while though ... wonder how
;; other clients handle that.
;;
;; It might also be good to "sync" the mpd playlist with the emms one.
;; Currently we just clear the mpd playlist, add the track, and play,
;; for each track.  Not the best approach, unless your track is a
;; playlist in itself, in which case all tracks from the playlist are
;; added immediately after clearing the mpd playlist.

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

(define-emms-simple-player mpd '(file url playlist)
  emms-player-mpd-supported-regexp "mpc")

(emms-player-set emms-player-mpd
                 'pause
                 'emms-player-mpd-pause)

(emms-player-set emms-player-mpd
                 'resume
                 'emms-player-mpd-pause)

(emms-player-set emms-player-mpd
                 'seek
                 'emms-player-mpd-seek)

(defun emms-player-mpd-clear ()
  "Clear the playlist."
  (shell-command-to-string (concat emms-player-mpd-command-name " clear")))

(defun emms-player-mpd-add (file)
  "Add FILE to the current MusicPD playlist.
If we succeeded in adding the file, return the string from the
process, nil otherwise."
  (when (and emms-player-mpd-music-directory
             (not (string-match "\\`http://" file)))
    (setq file (file-relative-name file emms-player-mpd-music-directory)))
  (let ((output (shell-command-to-string (concat emms-player-mpd-command-name
                                                 " add " file))))
    (when (and output (not (string= output "")))
      output)))

(defun emms-player-mpd-load (playlist)
  "Load contents of PLAYLIST into MusicPD by adding each line.
This handles both m3u and pls type playlists."
  ;; This allows us to keep playlists anywhere and not worry about
  ;; having to mangle their names.  Also, mpd can't handle pls
  ;; playlists by itself.
  (let ((pls-p (if (string-match "\\.pls\\'" playlist)
                   t
                 nil)))
    (mapc #'(lambda (file)
              (when pls-p
                (if (string-match "\\`File[0-9]*=\\(.*\\)\\'" file)
                    (setq file (match-string 1 file))
                  (setq file "")))
              (unless (or (string= file "")
                          (string-match "\\`#" file))
                (emms-player-mpd-add file)))
          (split-string (with-temp-buffer
                          (insert-file-contents playlist)
                          (buffer-string))
                        "\n"))))

(defun emms-player-mpd-play ()
  "Play whatever is in the current MusicPD playlist."
  (shell-command-to-string (concat emms-player-mpd-command-name " play")))

(defun emms-player-mpd-start (track)
  "Starts a process playing TRACK."
  (interactive)
  (emms-player-mpd-clear)
  (let ((name (emms-track-get track 'name)))
    ;; If it's a playlist, we have to `load' rather than `add' it
    (if (string-match "\\.\\(m3u\\|pls\\)\\'" name)
        (emms-player-mpd-load name)
      (emms-player-mpd-add name)))
  ;; Now that we've added/loaded the file/playlist, play it
  (emms-player-mpd-play))

(defun emms-player-mpd-stop ()
  "Stop the currently playing song."
  (interactive)
  (shell-command-to-string (concat emms-player-mpd-command-name " stop")))

(defun emms-player-mpd-pause ()
  "Pause the currently playing song."
  (interactive)
  (shell-command-to-string (concat emms-player-mpd-command-name " toggle")))

(defun emms-player-mpd-seek (sec)
  "Seek backward or forward by SEC seconds, depending on sign of SEC."
  (interactive)
  (shell-command-to-string (concat emms-player-mpd-command-name
                                   (format " seek %s%d"
                                           (if (> sec 0) "+" "")
                                           sec)))
  ;; Taking our cue from emms-player-mplayer-seek
  (when (fboundp 'emms-lyrics-seek)
    (emms-lyrics-seek sec)))

;; Not currently used by the API (to my knowledge), but I make use of
;; these to advance my playlists.
(defun emms-player-mpd-next ()
  "Move forward by one track in MusicPD's internal playlist."
  (interactive)
  (shell-command-to-string (concat emms-player-mpd-command-name " next")))

(defun emms-player-mpd-previous ()
  "Move backward by one track in MusicPD's internal playlist."
  (interactive)
  (shell-command-to-string (concat emms-player-mpd-command-name " previous")))

;; A "Now Playing" function -- I don't know how to integrate this into
;; emms-show.
(defun emms-player-mpd-show ()
  "Show the currently-playing track.  If nothing is playing, return nil."
  (interactive)
  (let ((np (car (split-string
                  (shell-command-to-string
                   (concat emms-player-mpd-command-name))
                  "\n"))))
    (when (and np
               (not (string= np ""))
               (not (string-match "\\`\\(volume\\|error\\):" np)))
      np)))

(provide 'emms-player-mpd)

;;; emms-player-mpd.el ends here
