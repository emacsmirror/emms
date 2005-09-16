;;; emms.el --- The Emacs Multimedia System

;; Copyright (C) 2003, 2004, 2005  Jorgen Schäfer

;; Author: Jorgen Schäfer <forcer@forcix.cx>
;; Keywords: emms, mp3, mpeg, multimedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301 USA

;;; Commentary:

;; This is the very core of EMMS.  It provides ways to play a track
;; using `emms-start', to go through the playlist using the commands
;; `emms-next' and `emms-previous', to stop the playback using
;; `emms-stop', and to see what's currently playing using `emms-show'.

;; But in itself, this core is useless, because it doesn't know how to
;; play any tracks --- you need players for this.  In fact, it doesn't
;; even know how to find any tracks to consider playing --- for this,
;; you need sources.

;; A sample configuration is offered in emms-default.el, so you might
;; just want to use that file.

;;; Code:

;; $Id: emms.el,v 1.63 2005/08/18 13:52:23 forcer Exp $
(defvar emms-version "1.5"
  "EMMS version string.")
;; FIXME: 1.5 will be 2.0 eventually


;;; User Customization

(defgroup emms nil
  "*The Emacs Multimedia System."
  :prefix "emms-"
  :group 'multimedia
  :group 'applications)

(defgroup emms-player nil
  "*Track players for EMMS."
  :prefix "emms-player-"
  :group 'emms)

(defgroup emms-source nil
  "*Track sources for EMMS."
  :prefix "emms-source-"
  :group 'emms)

(defcustom emms-player-list nil
  "*List of players that EMMS can use.  You need to set this!"
  :group 'emms
  :type '(repeat (symbol :tag "Player")))

(defcustom emms-show-format "Currently playing: %s"
  "*The format to use for `emms-show'.
Any \"%s\" is replaced by what `emms-track-description-function' returns
for the currently playing track."
  :group 'emms
  :type 'string)

(defcustom emms-repeat-playlist nil
  "*Non-nil if the EMMS playlist should automatically repeat.
If nil, playback will stop when the last track finishes playing.
If non-nil, EMMS will wrap back to the first track when that happens."
  :group 'emms
  :type 'boolean)

(defcustom emms-repeat-track nil
  "Non-nil, playback will repeat current track.  If nil, EMMS will play
track by track normally."
  :group 'emms
  :type 'boolean)

(defcustom emms-track-description-function 'emms-track-simple-description
  "*Function for describing an EMMS track in a user-friendly way."
  :group 'emms
  :type 'function)

(defcustom emms-player-delay 0
  "The delay to pause after a player finished.
This is a floating-point number of seconds.
This is necessary for some platforms where it takes a bit to free
the audio device after a player has finished. If EMMS is skipping
songs, increase this number."
  :type 'number
  :group 'emms)

(defcustom emms-playlist-shuffle-function 'emms-playlist-simple-shuffle
  "*The function to use for shuffling the playlist."
  :type 'function
  :group 'emms)

(defcustom emms-playlist-sort-function 'emms-playlist-simple-sort
  "*The function to use for sorting the playlist."
  :type 'function
  :group 'emms)

(defcustom emms-sort-lessp-function 'emms-sort-track-name-less-p
  "*Function for comparing two EMMS tracks.
The function should return non-nil if and only if the first track
sorts before the second (see `sort')."
  :group 'emms
  :type 'function)

(defcustom emms-playlist-buffer-name " *EMMS Playlist*"
  "*The default name of the EMMS playlist buffer."
  :type 'string
  :group 'emms)

(defcustom emms-playlist-default-major-mode default-major-mode
  "*The default major mode for EMMS playlist."
  :type 'function
  :group 'emms)

(defcustom emms-playlist-insert-track-function 'emms-playlist-simple-insert-track
  "*A function to insert a track into the playlist buffer."
  :group 'emms
  :type 'function)

(defcustom emms-playlist-delete-track-function 'emms-playlist-simple-delete-track
  "*A function to delete the track at point in the playlist buffer."
  :group 'emms
  :type 'function)

(defcustom emms-playlist-source-inserted-hook nil
  "*Hook run when a source got inserted into the playlist.
The buffer is narrowed to the new tracks."
  :type 'hook
  :group 'emms)

(defcustom emms-playlist-selection-changed-hook nil
  "*Hook run after another track is selected in the EMMS playlist."
  :group 'emms
  :type 'hook)

(defcustom emms-playlist-cleared-hook nil
  "*Hook run after the current EMMS playlist is cleared.
This happens both when the playlist is cleared and when a new
buffer is created for it."
  :group 'emms
  :type 'hook)

(defcustom emms-track-initialize-functions nil
  "*List of functions to call for each new EMMS track.
This can be used to initialize tracks with various info."
  :group 'emms
  :type 'hook)

(defcustom emms-player-started-hook nil
  "*Hook run when an EMMS player starts playing."
  :group 'emms
  :type 'hook
  :options '(emms-show))

(defcustom emms-player-stopped-hook nil
  "*Hook run when an EMMS player is stopped by the user.
See `emms-player-finished-hook'."
  :group 'emms
  :type 'hook)

(defcustom emms-player-finished-hook '(emms-next-noerror)
  "*Hook run when an EMMS player finishes playing a track.
Please pay attention to the differences between
`emms-player-finished-hook' and `emms-player-stopped-hook'.
The former is called only when the player is stopped interactively;
the latter, only when the player actually finishes playing a track."
  :group 'emms
  :type 'hook
  :options '(emms-next-noerror))

(defcustom emms-player-paused-hook nil
  "*Hook run when a player is paused or resumed.
Use `emms-player-paused-p' to find the current state."
  :group 'emms
  :type 'hook)

(defcustom emms-player-seeked-functions nil
  "*Functions called when a player is seeking.
The functions are called with a single argument, the amount of
seconds the player did seek."
  :group 'emms
  :type 'hook)

(defvar emms-player-playing-p nil
  "The currently playing EMMS player, or nil.")

(defvar emms-player-paused-p nil
  "Whether the current player is paused or not.")


;;; User Interface

(defun emms-start ()
  "Start playing the current track in the EMMS playlist."
  (interactive)
  (unless emms-player-playing-p
    (emms-player-start (emms-playlist-selected-track))))

(defun emms-stop ()
  "Stop any current EMMS playback."
  (interactive)
  (when emms-player-playing-p
    (emms-player-stop)))

(defun emms-next ()
  "Start playing the next track in the EMMS playlist.
This might behave funny if called from `emms-player-finished-hook',
so use `emms-next-noerror' in that case."
  (interactive)
  (when emms-player-playing-p
    (emms-stop))
  (emms-playlist-select-next)
  (emms-start))

(defun emms-next-noerror ()
  "Start playing the next track in the EMMS playlist.
Unlike `emms-next', this function doesn't signal an error when called
at the end of the playlist.
This function should only be called when no player is playing.
This is a good function to put in `emms-player-finished-hook'."
  (interactive)
  (when emms-player-playing-p
    (error "A track is already being played"))
  (cond (emms-repeat-track
	 (emms-start))
	((condition-case nil
             (progn
               (emms-playlist-select-next)
               t)
           (error nil))
	 (emms-start))
        (t
	 (message "No next track in playlist"))))

(defun emms-previous ()
  "Start playing the previous track in the EMMS playlist."
  (interactive)
  (when emms-player-playing-p
    (emms-stop))
  (emms-playlist-select-previous)
  (emms-start))

(defun emms-random ()
  "Jump to a random track."
  (interactive)
  (when emms-player-playing-p
    (emms-stop))
  (emms-playlist-select-random)
  (emms-start))

(defun emms-pause ()
  "Pause the current player."
  (interactive)
  (when emms-player-playing-p
    (emms-player-pause)))

(defun emms-seek (seconds)
  "Seek the current player SECONDS seconds.
This can be a floating point number for sub-second fractions.
It can also be negative to seek backwards."
  (interactive "nSeconds to seek: ")
  (if emms-player-playing-p
      (emms-player-seek seconds)
    (error "Nothing playing right now")))

(defun emms-seek-forward ()
  "Seek ten seconds forward."
  (interactive)
  (when emms-player-playing-p
    (emms-player-seek 10)))

(defun emms-seek-backward ()
  "Seek ten seconds backward."
  (interactive)
  (when emms-player-playing-p
    (emms-player-seek -10)))

(defun emms-show (&optional insertp)
  "Describe the current EMMS track in the minibuffer.
If INSERTP is non-nil, insert the description into the current buffer instead.
This function uses `emms-show-format' to format the current track."
  (interactive "P")
  (let ((string (format emms-show-format (emms-track-description
                                          (emms-playlist-selected-track)))))
    (if insertp
        (insert string)
      (message "%s" string))))

(defun emms-shuffle ()
  "Shuffle the current playlist.
This uses `emms-playlist-shuffle-function'."
  (interactive)
  (with-current-emms-playlist
    (save-excursion
      (funcall emms-playlist-shuffle-function))))

(defun emms-sort ()
  "Sort the current playlist.
This uses `emms-playlist-shuffle-function'."
  (interactive)
  (with-current-emms-playlist
    (save-excursion
      (funcall emms-playlist-sort-function))))

(defun emms-toggle-repeat-playlist ()
  "Toggle whether emms repeats the playlist after it is done.
See `emms-repeat-playlist'."
  (interactive)
  (setq emms-repeat-playlist (not emms-repeat-playlist))
  (if emms-repeat-playlist
      (message "Will repeat the playlist after it is done.")
    (message "Will stop after the playlist is over.")))

(defun emms-toggle-repeat-track ()
  "Toggle whether emms repeats the current track.
See  `emms-repeat-track'."
  (interactive)
  (setq emms-repeat-track (not emms-repeat-track))
  (if emms-repeat-track
      (message "Will repeat the current track.")
    (message "Will advance to the next track after this one.")))

(defun emms-sort-track-name-less-p (a b)
  "Return non-nil if the track name of A sorts before B."
  (string< (emms-track-name a)
           (emms-track-name b)))


;;; Tracks

;; This is a simple datatype to store track information.
;; Each track consists of a type (a symbol) and a name (a string).
;; In addition, each track has an associated dictionary of information.

(defun emms-track (type name)
  "Create an EMMS track with type TYPE and name NAME."
  (let ((track (emms-dictionary '*track*)))
    (emms-track-set track 'type type)
    (emms-track-set track 'name name)
    (run-hook-with-args 'emms-track-initialize-functions track)
    track))

(defun emms-track-type (track)
  "Return the type of TRACK."
  (emms-track-get track 'type))

(defun emms-track-name (track)
  "Return the name of TRACK."
  (emms-track-get track 'name))

(defun emms-track-get (track name &optional default)
  "Return the value of NAME for TRACK.
If there is no value, return DEFAULT (or nil, if not given)."
  (emms-dictionary-get track name default))

(defun emms-track-set (track name value)
  "Set the value of NAME for TRACK to VALUE."
  (emms-dictionary-set track name value))

(defun emms-track-description (track)
  "Return a description of TRACK.
This function uses `emms-track-description-function'."
  (funcall emms-track-description-function track))

(defun emms-track-simple-description (track)
  "Simple function to give a user-readable description of a track.
If it's a file track, just return the file name.
Otherwise, return the type and the name with a colon in between."
  (if (eq 'file (emms-track-type track))
      (emms-track-name track)
    (concat (symbol-name (emms-track-type track))
            ":"
            (emms-track-name track))))


;;; The Playlist

;; Playlists are stored in buffers. The current playlist buffer is
;; remembered in the `emms-playlist' variable. The buffer consists of
;; any kind of data. Strings of text with a `emms-track' property are
;; the tracks in the buffer.

(defvar emms-playlist-buffer nil
  "The current playlist buffer, if any.")

(defvar emms-playlist-selected-marker nil
  "The marker for the currently selected track.")
(make-variable-buffer-local 'emms-playlist-selected-marker)

(defvar emms-playlist-buffer-p nil
  "Non-nil when the current buffer is an EMMS playlist.")
(make-variable-buffer-local 'emms-playlist-buffer-p)

(defun emms-playlist-set-playlist-buffer (&optional buffer)
  "Set the current playlist buffer."
  (interactive "bNew playlist buffer: ")
  (setq emms-playlist-buffer (or (get-buffer buffer)
                                 (current-buffer))))

(defun emms-playlist-new (&optional name)
  "Create a new playlist buffer.
The buffer is named NAME, but made unique. NAME defaults to
`emms-playlist-buffer-name'.
If called interactively, the new buffer is also selected."
  (interactive)
  (let ((buf (generate-new-buffer (or name
                                      emms-playlist-buffer-name))))
    (with-current-buffer buf
      (when (not (eq major-mode emms-playlist-default-major-mode))
        (funcall emms-playlist-default-major-mode))
      (setq emms-playlist-buffer-p t))
    (when (called-interactively-p)
      (switch-to-buffer buf))
    buf))

(defun emms-playlist-clear ()
  "Clear the current playlist.
If no playlist exists, a new one is generated."
  (if (or (not emms-playlist-buffer)
          (not (buffer-live-p emms-playlist-buffer)))
      (setq emms-playlist-buffer (emms-playlist-new))
    (with-current-buffer emms-playlist-buffer
      (let ((inhibit-read-only t))
        (widen)
        (delete-region (point-min)
                       (point-max)))
      (run-hooks 'emms-playlist-cleared-hook))))

(defmacro with-current-emms-playlist (&rest body)
  "Run BODY with the current buffer being the current playlist buffer."
  `(progn
     (when (or (not emms-playlist-buffer)
               (not (buffer-live-p emms-playlist-buffer)))
       (emms-playlist-clear))
     (with-current-buffer emms-playlist-buffer
       ,@body)))
(put 'with-current-emms-playlist 'lisp-indent-function 0)

;;; Saving playlists.

(defun emms-playlist-save (playlist filename)
  "Save a playlist in the native EMMS format."
  (interactive "bPlaylist buffer name: \nFFile to save playlist as: ")
  (let ((tracklist '()))
    (condition-case nil
        (with-current-buffer playlist
          (save-excursion
            (emms-playlist-first)
            (while (emms-playlist-track-at)
              (setq tracklist (cons (emms-playlist-track-at)
                                    tracklist))
              (emms-playlist-next))))
      (error nil))
    (setq tracklist (nreverse tracklist))
    ;; tracklist complete, let's write it !
    (with-current-buffer (find-file-noselect filename)
      (erase-buffer)
      (prin1 tracklist (current-buffer))
      (insert "\n")
      (save-buffer)
      (kill-buffer (current-buffer)))))

(defun emms-playlist-save-active (filename)
  "Save the active EMMS playlist in native format."
  (interactive "FFile to save playlist as: ")
  (emms-playlist-save emms-playlist-buffer filename))

(defun emms-playlist-save-as-m3u (playlist filename)
  "Save a playlist in .m3u format."
  (interactive "bPlaylist buffer name: \nFFile to save playlist as: ")
  (let ((tracklist '()))
    (condition-case nil
        (with-current-buffer playlist
          (save-excursion
            (emms-playlist-first)
            (while (emms-playlist-track-at)
              (setq tracklist (cons (emms-playlist-track-at)
                                    tracklist))
              (emms-playlist-next))))
      (error nil))
    (setq tracklist (nreverse tracklist))
    ;; tracklist complete, let's write it !
    (with-current-buffer (find-file-noselect filename)
      (erase-buffer)
      (insert "#EXTM3U\n")
      (mapc (lambda (track)
              (let ((info (emms-info-get track)))
                (insert "#EXTINF:")
                (insert (emms-info-playing-time info) ",")
                (insert (emms-info-artist info) " - " (emms-info-title info) "\n")
                (insert (emms-track-get track 'name) "\n")))
            tracklist)
      (save-buffer)
      (kill-buffer (current-buffer)))))

(defun emms-playlist-save-active-as-m3u (filename)
  "Save the active EMMS playlist in m3u format."
  (interactive "FFile to save playlist as: ")
  (emms-playlist-save-as-m3u emms-playlist-buffer filename))

;;; Point movement within the playlist buffer.

(defun emms-playlist-track-at (&optional pos)
  "Return the track at POS (point if not given), or nil if none."
  (get-text-property (or pos (point))
                     'emms-track))

(defun emms-playlist-next ()
  "Move to the next track in the current buffer."
  (let ((next (next-single-property-change (point)
                                           'emms-track)))
    (when (not next)
      (error "No next track"))
    (when (not (emms-playlist-track-at next))
      (setq next (next-single-property-change next 'emms-track)))
    (when (or (not next)
              (= next (point-max)))
      (error "No next track"))
    (goto-char next)))

(defun emms-playlist-previous ()
  "Move to the previous track in the current buffer."
  (let ((prev (previous-single-property-change (point)
                                               'emms-track)))
    (when (not prev)
      (error "No previous track"))
    (when (not (get-text-property prev 'emms-track))
      (setq prev (or (previous-single-property-change prev 'emms-track)
                     (point-min))))
    (when (or (not prev)
              (not (get-text-property prev 'emms-track)))
      (error "No previous track"))
    (goto-char prev)))

(defun emms-playlist-first ()
  "Move to the first track in the current buffer."
  (let ((first (condition-case nil
                   (save-excursion
                     (goto-char (point-min))
                     (when (not (emms-playlist-track-at (point)))
                       (emms-playlist-next))
                     (point))
                 (error
                  nil))))
    (if first
        (goto-char first)
      (error "No first track"))))

(defun emms-playlist-last ()
  "Move to the last track in the current buffer."
  (let ((last (condition-case nil
                  (save-excursion
                    (goto-char (point-max))
                    (emms-playlist-previous)
                    (point))
                (error
                 nil))))
    (if last
        (goto-char last)
      (error "No last track"))))

(defun emms-playlist-delete-track ()
  "Delete the track at point."
  (funcall emms-playlist-delete-track-function))

;;; Track selection
(defun emms-playlist-selected-track ()
  "Return the currently selected track."
  (with-current-emms-playlist
    (when emms-playlist-selected-marker
      (emms-playlist-track-at emms-playlist-selected-marker))))

(defun emms-playlist-select (pos)
  "Select the track at POS."
  (with-current-emms-playlist
    (when (not (emms-playlist-track-at pos))
      (error "No track at position %s" pos))
    (when (not emms-playlist-selected-marker)
      (setq emms-playlist-selected-marker (make-marker)))
    (set-marker emms-playlist-selected-marker pos))
  (run-hooks 'emms-playlist-selection-changed-hook))

(defun emms-playlist-select-next ()
  "Select the next track in the playlist."
  (with-current-emms-playlist
    (save-excursion
      (goto-char (if (and emms-playlist-selected-marker
                          (marker-position emms-playlist-selected-marker))
                     emms-playlist-selected-marker
                   (point-min)))
      (condition-case nil
          (progn
            (if emms-repeat-playlist
                (condition-case nil
                    (emms-playlist-next)
                  (error
                   (emms-playlist-first)))
              (emms-playlist-next))
            (emms-playlist-select (point)))
        (error
         (error "No next track in playlist"))))))

(defun emms-playlist-select-previous ()
  "Select the previous track in the playlist."
  (with-current-emms-playlist
    (save-excursion
      (goto-char (if (and emms-playlist-selected-marker
                          (marker-position emms-playlist-selected-marker))
                     emms-playlist-selected-marker
                   (point-max)))
      (condition-case nil
          (progn
            (if emms-repeat-playlist
                (condition-case nil
                    (emms-playlist-previous)
                  (error
                   (emms-playlist-last)))
              (emms-playlist-previous))
            (emms-playlist-select (point)))
        (error
         (error "No previous track in playlist"))))))

(defun emms-playlist-select-random ()
  "Select a random track in the playlist."
  (with-current-emms-playlist
    ;; FIXME: This is rather inefficient.
    (save-excursion
      (let ((track-indices nil)
            (donep nil))
        (condition-case nil
            (progn
              (emms-playlist-first)
              (setq track-indices (cons (point)
                                        track-indices)))
          (error
           (setq donep t)))
        (while (not donep)
          (condition-case nil
              (progn
                (emms-playlist-next)
                (setq track-indices (cons (point)
                                          track-indices)))
            (error
             (setq donep t))))
        (setq track-indices (vconcat track-indices))
        (emms-playlist-select (aref track-indices
                                    (random (length track-indices))))))))

(defun emms-playlist-select-first ()
  "Select the first track in the playlist."
  (with-current-emms-playlist
    (save-excursion
      (emms-playlist-first)
      (emms-playlist-select (point)))))

(defun emms-playlist-select-last ()
  "Select the last track in the playlist."
  (with-current-emms-playlist
    (save-excursion
      (emms-playlist-last)
      (emms-playlist-select (point)))))

;;; Playlist manipulation
(defun emms-playlist-insert-track (track)
  "Insert TRACK at the current position into the playlist.
This uses `emms-playlist-insert-track-function'."
  (funcall emms-playlist-insert-track-function track))

(defun emms-playlist-insert-source (source &rest args)
  "Insert tracks from SOURCE, supplying ARGS as arguments."
  (with-current-emms-playlist
    (save-restriction
      (narrow-to-region (point)
                        (point))
      (apply source args)
      (run-hooks emms-playlist-source-inserted-hook))))

(defun emms-playlist-tracks-in-region (beg end)
  "Return all tracks between BEG and END."
  (let ((tracks nil)
        (donep nil))
    (save-restriction
      (narrow-to-region beg end)
      (condition-case nil
          (emms-playlist-first)
        (error
         (setq donep t)))
      (while (not donep)
        (setq tracks (cons (emms-playlist-track-at (point))
                           tracks))
        (condition-case nil
            (emms-playlist-next)
          (error
           (setq donep t)))))
    tracks))

;;; Simple playlist buffer
(defun emms-playlist-simple-insert-track (track)
  "Insert the description of TRACK at point."
  (insert (propertize (emms-track-description track)
                      'emms-track track)
          "\n"))

(defun emms-playlist-simple-delete-track ()
  "Delete the track at point."
  (when (not (emms-playlist-track-at (point)))
    (error "No track at point"))
  (let ((region (emms-property-region (point) 'emms-track)))
    (delete-region (car region)
                   (cdr region))))

(defun emms-playlist-simple-shuffle ()
  "Shuffle the whole playlist buffer."
  (let ((current nil))
    (widen)
    (when emms-player-playing-p
      (setq current (emms-playlist-selected-track))
      (goto-char emms-playlist-selected-marker)
      (emms-playlist-delete-track))
    (let* ((tracks (vconcat (emms-playlist-tracks-in-region (point-min)
                                                            (point-max))))
           (len (length tracks))
           (i 0))
      (delete-region (point-min)
                     (point-max))
      (run-hooks 'emms-playlist-cleared-hook)
      (emms-shuffle-vector tracks)
      (when current
        (emms-playlist-insert-track current))
      (while (< i len)
        (emms-playlist-insert-track (aref tracks i))
        (setq i (1+ i))))
    (emms-playlist-select-first)
    (goto-char (point-max))))

(defun emms-playlist-simple-sort ()
  "Sort the whole playlist buffer."
  (widen)
  (let ((current (emms-playlist-selected-track))
        (tracks (emms-playlist-tracks-in-region (point-min)
                                                (point-max))))
    (delete-region (point-min)
                   (point-max))
    (run-hooks 'emms-playlist-cleared-hook)
    (mapc 'emms-playlist-insert-track
          (sort tracks emms-sort-lessp-function))
    (let ((pos (text-property-any (point-min)
                                  (point-max)
                                  'emms-track current)))
      (if pos
          (emms-playlist-select pos)
        (emms-playlist-first)))))

;;; Helper functions
(defun emms-property-region (pos prop)
  "Return a pair of the beginning and end of the property PROP at POS."
  (let ((beg nil)
        (end nil))
    (save-excursion
      (goto-char pos)
      (while (and (not (bobp))
                  (get-text-property (point)
                                     prop))
        (backward-char))
      (when (not (get-text-property (point)
                                    prop))
        (forward-char))
      (setq beg (point))
      (goto-char pos)
      (while (and (not (eobp))
                  (get-text-property (point)
                                     prop))
        (forward-char))
      (setq end (point)))
    (cons beg end)))

(defun emms-shuffle-vector (vector)
  "Shuffle VECTOR."
  (let ((i (- (length vector) 1)))
    (while (>= i 0)
      (let* ((r (random (1+ i)))
             (old (aref vector r)))
        (aset vector r (aref vector i))
        (aset vector i old))
      (setq i (- i 1))))
  vector)




;;; Sources

;; A source is just a function which is called in a playlist buffer.
;; It should use `emms-playlist-insert-track' to insert the tracks it
;; knows about.
;;
;; The define-emms-source macro also defines functions emms-play-SOURCE
;; and emms-add-SOURCE.  The former will replace the current playlist,
;; while the latter will add to the end.

(defmacro define-emms-source (name arglist &rest body)
  "Define a new EMMS source called NAME.
This macro defines three functions: `emms-source-NAME', `emms-play-NAME'
and `emms-add-NAME'. BODY should use `emms-playlist-insert-track'
do insert all tracks to be played, which is exactly what
`emms-source-NAME' will do.
The other two functions will be simple wrappers around `emms-source-NAME';
any `interactive' form that you specify in BODY will end up in these.
See emms-source-file.el for some examples."
  (let ((source-name (intern (format "emms-source-%s" name)))
        (source-play (intern (format "emms-play-%s" name)))
        (source-add (intern (format "emms-add-%s" name)))
        (source-insert (intern (format "emms-insert-%s" name)))
        (docstring "A source of tracks for EMMS.")
        (interactive nil)
        (call-args (delete '&rest
                           (delete '&optional
                                   arglist))))
    (when (stringp (car body))
      (setq docstring (car body)
            body (cdr body)))
    (when (eq 'interactive (caar body))
      (setq interactive (car body)
            body (cdr body)))
    `(progn
       (defun ,source-name ,arglist
         ,docstring
         ,@body)
       (defun ,source-play ,arglist
         ,docstring
         ,interactive
         (emms-source-play ',source-name ,@call-args))
       (defun ,source-add ,arglist
         ,docstring
         ,interactive
         (emms-source-add ',source-name ,@call-args))
       (defun ,source-insert ,arglist
         ,docstring
         ,interactive
         (emms-source-insert ',source-name ,@call-args)))))

(defun emms-source-play (source &rest args)
  "Play the tracks of SOURCE, after first clearing the EMMS playlist."
  (emms-stop)
  (emms-playlist-clear)
  (apply 'emms-playlist-insert-source source args)
  (emms-playlist-select-first)
  (emms-start))

(defun emms-source-add (source &rest args)
  "Add the tracks of SOURCE at the current position in the playlist."
  (with-current-emms-playlist
    (save-excursion
      (goto-char (point-max))
      (apply 'emms-playlist-insert-source source args))
    (when (or (not emms-playlist-selected-marker)
              (not (marker-position emms-playlist-selected-marker)))
      (emms-playlist-select-first))))

(defun emms-source-insert (source &rest args)
  "Insert the tracks from SOURCE in the current buffer."
  (if (not emms-playlist-buffer-p)
      (error "Not in an EMMS playlist buffer")
    (apply emms-playlist-insert-source source args)))

;;; User-defined playlists
;;; FIXME: Shuffle is bogus here! (because of narrowing)
(defmacro define-emms-combined-source (name shufflep sources)
  "Define a `emms-play-X' and `emms-add-X' function for SOURCES."
  `(define-emms-source ,name ()
     "An EMMS source for a tracklist."
     (interactive)
     (mapc (lambda (source)
             (apply (car source)
                    (cdr source)))
           ,sources)
     ,(when shufflep
        '(save-restriction
           (widen)
           (emms-shuffle)))))


;;; Players

;; A player is a data structure created by `emms-player'.
;; See the docstring of that function for more information.

(defvar emms-player-stopped-p nil
  "Non-nil if the last EMMS player was stopped by the user.")

(defun emms-player (start stop playablep)
  "Create a new EMMS player.
The start function will be START, and the stop function STOP.
PLAYABLEP should return non-nil for tracks that this player can play.

When trying to play a track, EMMS walks `emms-player-list'.
For each player,it calls the PLAYABLEP function.
The player corresponding to the first PLAYABLEP function that returns
non-nil is used to play the track.
To actually play the track, EMMS calls the START function,
passing the chosen track as a parameter.

If the user tells EMMS to stop playing, the STOP function is called.
Once the player has finished playing, it should call `emms-player-stopped'
to let EMMS know."
  (let ((p (emms-dictionary '*player*)))
    (emms-player-set p 'start start)
    (emms-player-set p 'stop stop)
    (emms-player-set p 'playablep playablep)
    p))

(defun emms-player-get (player name &optional inexistent)
  "Return the value of entry NAME in PLAYER."
  (let ((p (if (symbolp player)
               (symbol-value player)
             player)))
    (emms-dictionary-get p name inexistent)))

(defun emms-player-set (player name value)
  "Set the value of entry NAME in PLAYER to VALUE."
  (let ((p (if (symbolp player)
               (symbol-value player)
             player)))
    (emms-dictionary-set p name value)))

(defun emms-player-for (track)
  "Return an EMMS player capable of playing TRACK.
This will be the first player whose PLAYABLEP function returns non-nil,
or nil if no such player exists."
  (let ((lis emms-player-list))
    (while (and lis
                (not (funcall (emms-player-get (car lis) 'playablep)
                              track)))
      (setq lis (cdr lis)))
    (if lis
        (car lis)
      nil)))

(defun emms-player-start (track)
  "Start playing TRACK."
  (if emms-player-playing-p
      (error "A player is already playing")
    (let ((player (emms-player-for track)))
      (if (not player)
          (error "Don't know how to play track: %s" track)
        (funcall (emms-player-get player 'start)
                 track)
        (setq emms-player-playing-p player)
        (run-hooks 'emms-player-started-hook)))))

(defun emms-player-stop ()
  "Stop the current EMMS player."
  (when emms-player-playing-p
    (let ((emms-player-stopped-p t))
      (funcall (emms-player-get emms-player-playing-p 'stop)))
    (setq emms-player-playing-p nil)))

(defun emms-player-stopped ()
  "Declare that the current EMMS player is finished.
This should only be done by the current player itself."
  (setq emms-player-playing-p nil)
  (if emms-player-stopped-p
      (run-hooks 'emms-player-stopped-hook)
    (sleep-for emms-player-delay)
    (run-hooks 'emms-player-finished-hook)))

(defun emms-player-pause ()
  "Pause the current EMMS player."
  (cond
   ((not emms-player-playing-p)
    (error "Can't pause player, nothing is playing"))
   (emms-player-paused-p
    (let ((resume (emms-player-get emms-player-playing-p 'resume))
          (pause (emms-player-get emms-player-playing-p 'pause)))
      (cond
       (resume
        (funcall resume))
       (pause
        (funcall pause))
       (t
        (error "Player does not know how to pause"))))
    (setq emms-player-paused-p nil)
    (run-hooks 'emms-player-paused-hook))
   (t
    (let ((pause (emms-player-get emms-player-playing-p 'pause)))
      (if pause
          (funcall pause)
        (error "Player does not know how to pause")))
    (setq emms-player-paused-p t)
    (run-hooks 'emms-player-paused-hook))))

(defun emms-player-seek (seconds)
  "Seek the current player by SECONDS seconds.
This can be a floating point number for fractions of a second,
or negative to seek backwards."
  (if (not emms-player-playing-p)
      (error "Can't seek player, nothing playing right now")
    (let ((seek (emms-player-get emms-player-playing-p 'seek)))
      (if (not seek)
          (error "Player does not know how to seek")
        (funcall seek seconds)
        (run-hook-with-args 'emms-player-seeked-functions seconds)))))


;;; Dictionaries

;; This is a simple helper data structure, used by both players
;; and tracks.

(defun emms-dictionary (name)
  "Create a new dictionary of type NAME."
  (list name))

(defun emms-dictionary-type (dict)
  "Return the type of the dictionary DICT."
  (car dict))

(defun emms-dictionary-get (dict name &optional default)
  "Return the value of NAME in DICT."
  (let ((item (assq name (cdr dict))))
    (if item
        (cdr item)
      default)))

(defun emms-dictionary-set (dict name value)
  "Set the value of NAME in DICT to VALUE."
  (let ((item (assq name (cdr dict))))
    (if item
        (setcdr item value)
      (setcdr dict (append (cdr dict)
                           (list (cons name value))))))
  dict)

(provide 'emms)
;;; emms.el ends here
