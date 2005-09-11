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
(defvar emms-version "1.3 $Revision: 1.63 $"
  "EMMS version string.")

(defmacro emms-define-obsolete-variable-alias
  (obsolete-name current-name &optional when docstring)
  "Make OBSOLETE-NAME an obsolete variable alias for CURRENT-NAME.
See `define-obsolete-variable-alias' in Emacs 22.1 and above."
  `(progn
     (when (fboundp 'defvaralias)
       (defvaralias ,obsolete-name ,current-name ,docstring))
     (make-obsolete-variable ,obsolete-name ,current-name ,when)))

(defmacro emms-define-obsolete-function-alias
  (obsolete-name current-name &optional when docstring)
  "Make OBSOLETE-NAME an obsolete function alias for CURRENT-NAME.
See `define-obsolete-function-alias' in Emacs 22.1 and above."
  `(progn
     (defalias ,obsolete-name ,current-name ,docstring)
     (make-obsolete ,obsolete-name ,current-name ,when)))


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

(defcustom emms-track-description-function 'emms-track-description
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

(defcustom emms-sort-lessp-function 'emms-sort-track-name-less-p
  "*Function for comparing two EMMS tracks.
The function should return non-nil if and only if the first track
sorts before the second (see `sort')."
  :group 'emms
  :type 'function)

(defcustom emms-playlist-changed-hook nil
  "*Hook run after the EMMS playlist changes."
  :group 'emms
  :type 'hook)

(emms-define-obsolete-variable-alias
  'emms-playlist-current-changed-hook
  'emms-playlist-current-track-changed-hook)

(defcustom emms-playlist-current-track-changed-hook nil
  "*Hook run after another track is selected in the EMMS playlist."
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

(defvar emms-player-playing-p nil
  "The currently playing EMMS player, or nil.")

(defvar emms-playlist []
  "The current EMMS playlist: a vector of tracks.")
(defvar emms-playlist-current nil
  "The zero-based playlist index of the current EMMS track.
If there is no playlist, this will be set to nil.")

(defcustom emms-playlist-sort-added-tracks-p nil
  "*If non-nil, sort tracks before adding them to the EMMS playlist."
  :group 'emms
  :type 'boolean)

(emms-define-obsolete-variable-alias
  'emms-sort-on-file-add
  'emms-playlist-sort-added-tracks-p)


;;; User Interface

(defun emms-start ()
  "Start playing the current track in the EMMS playlist."
  (interactive)
  (unless emms-player-playing-p
    (emms-player-start (emms-playlist-current-track))))

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
  (if (emms-playlist-next)
      (emms-start)
    (error "No next track in playlist")))

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
	((emms-playlist-next)
	 (emms-start))
	(emms-repeat-playlist
	 (setq emms-playlist-current 0)
	 (emms-start))
	(t
	 (message "No next track in playlist"))))

(defun emms-previous ()
  "Start playing the previous track in the EMMS playlist."
  (interactive)
  (when emms-player-playing-p
    (emms-stop))
  (if (emms-playlist-previous)
      (emms-start)
    (error "No previous track in playlist")))

(defun emms-show (&optional insertp)
  "Describe the current EMMS track in the minibuffer.
If INSERTP is non-nil, insert the description into the current buffer instead.
This function uses `emms-show-format' to format the current track."
  (interactive "P")
  (let ((string (format emms-show-format (emms-playlist-current))))
    (if insertp
        (insert string)
      (message "%s" string))))

(defun emms-shuffle ()
  "Shuffle the EMMS playlist."
  (interactive)
  (emms-playlist-shuffle))

(defun emms-sort ()
  "Sort the EMMS playlist."
  (interactive)
  (emms-playlist-sort))

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
  "Simple function to give a user-readable description of a track.
If it's a file track, just return the file name.
Otherwise, return the type and the name with a colon in between."
  (if (eq 'file (emms-track-type track))
      (emms-track-name track)
    (concat (symbol-name (emms-track-type track))
            ":"
            (emms-track-name track))))


;;; The Playlist

;; This is a simple vector storing the current playlist.  You should avoid
;; accessing the vector directly, and use the functions provided here instead.
;; If you can't avoid accessing the vector directly, be careful to call the
;; right hooks at the right times.

(defun emms-playlist-current ()
  "Return a description of the currently playing EMMS track.
This function uses `emms-track-description-function'."
  (funcall emms-track-description-function
           (emms-playlist-current-track)))

(defun emms-playlist-current-track ()
  "Return the currently playing EMMS track."
  (when emms-playlist-current
    (emms-playlist-get-track emms-playlist-current)))

(defun emms-playlist-get-track-description (track)
  "Return a description of TRACK.
This uses `emms-track-description-function'."
  (funcall emms-track-description-function track))

(defun emms-playlist-get (n)
  "Return a description of the Nth entry of the current EMMS playlist.
This uses `emms-track-description-function'"
  (funcall emms-track-description-function
           (emms-playlist-get-track n)))

(defun emms-playlist-get-track (n)
  "Return the Nth track of the current EMMS playlist."
  (aref emms-playlist n))

(defun emms-playlist-set-playlist (new)
  "Set the current EMMS playlist to NEW.
This runs `emms-playlist-changed-hook'."
  (setq emms-playlist new)
  (cond
   ((= 0 (length new))
    (setq emms-playlist-current nil))
   ((null emms-playlist-current)
    (setq emms-playlist-current 0))
   ((>= emms-playlist-current (length emms-playlist))
    (setq emms-playlist-current (- (length emms-playlist) 1))))
  (run-hooks 'emms-playlist-changed-hook))

(defun emms-playlist-get-playlist ()
  "Return the current EMMS playlist.
Avoid changing the structure returned by this function."
  emms-playlist)

(defun emms-playlist-set-current (n)
  "Set the current track in the EMMS playlist to N (a number).
This runs `emms-playlist-current-track-changed-hook'."
  (setq emms-playlist-current n)
  (run-hooks 'emms-playlist-current-track-changed-hook))

(defun emms-playlist-get-current ()
  "Return the index number of the current EMMS track.
If the playlist is empty, returns nil."
  emms-playlist-current)

(defun emms-playlist-next ()
  "Advance to the next entry in the EMMS playlist.
Return nil if there was no next track, or non-nil otherwise."
  (let ((cur (emms-playlist-get-current)))
    (when (and cur
               (< cur (- (length (emms-playlist-get-playlist)) 1)))
      (emms-playlist-set-current (+ 1 cur))
      t)))

(defun emms-playlist-previous ()
  "Back up to the previous entry in the EMMS playlist.
Return nil if there was no previous track, or non-nil otherwise."
  (let ((cur (emms-playlist-get-current)))
    (when (and cur
               (> cur 0))
      (emms-playlist-set-current (- cur 1))
      t)))

(defun emms-playlist-add (seq &optional idx)
  "Add each track of the sequence SEQ to the current playlist.
Insert at IDX, which defaults to the end."
  (let ((idx (or idx (length emms-playlist))))
    (emms-playlist-set-playlist
     (vconcat (substring emms-playlist 0 idx)
              (if emms-playlist-sort-added-tracks-p
                  (emms-playlist-sort-vector seq)
                seq)
              (substring emms-playlist idx)))))

(defun emms-playlist-remove (idx)
  "Remove track at IDX from the EMMS playlist."
  (emms-playlist-set-playlist
   (vconcat (substring emms-playlist 0 idx)
	    (substring emms-playlist (1+ idx)))))

(defun emms-playlist-search-vector (track vector)
  "Return the index of TRACK in VECTOR, or nil if not found.
Comparison is done with `eq'."
  (catch 'loop
    (let ((i 0))
      (while (< i (length vector))
        (if (eq track
                (elt vector i))
            (throw 'loop i)
          (setq i (1+ i)))))))

(defun emms-playlist-shuffle ()
  "Shuffle the current EMMS playlist.
If a track is currently being played, it will end up at the front
of the playlist after shuffling."
  (if (not emms-player-playing-p)
      (emms-playlist-set-playlist
       (emms-playlist-shuffle-vector
        (emms-playlist-get-playlist)))
    (let* ((current-track (emms-playlist-current-track))
           (playlist (emms-playlist-shuffle-vector
                      (emms-playlist-get-playlist)))
           (new-index (emms-playlist-search-vector current-track playlist))
           (first (elt playlist 0)))
      (aset playlist 0 (elt playlist new-index))
      (aset playlist new-index first)
      (emms-playlist-set-playlist playlist)
      (emms-playlist-set-current 0))))

(defun emms-playlist-sort ()
  "Sort the current EMMS playlist.
Comparison is done with `emms-sort-lessp-function'.
If a song is currently being played, it will remain the current track
after sorting, though its index may change as appropriate."
  (if (not emms-player-playing-p)
      (emms-playlist-set-playlist
       (emms-playlist-sort-vector
        (emms-playlist-get-playlist)))
    (let* ((current-track (emms-playlist-current-track))
           (playlist (emms-playlist-sort-vector
                      (emms-playlist-get-playlist)))
           (new-index (emms-playlist-search-vector current-track playlist)))
      (emms-playlist-set-playlist playlist)
      (emms-playlist-set-current new-index))))

(defun emms-playlist-shuffle-vector (vector)
  "Shuffle VECTOR."
  (let ((i (- (length vector) 1)))
    (while (>= i 0)
      (let* ((r (random (1+ i)))
             (old (aref vector r)))
        (aset vector r (aref vector i))
        (aset vector i old))
      (setq i (- i 1))))
  vector)

(defun emms-playlist-sort-vector (vector)
  "Sort VECTOR according to `emms-sort-lessp-function'."
  (vconcat (sort (append vector nil)
                 emms-sort-lessp-function)))


;;; User-defined playlists.
(defmacro define-emms-playlist (name shufflep tracklist)
  "Define a `emms-play-X' and `emms-add-X' function for TRACKLIST."
  `(define-emms-source ,name ()
     "An EMMS source for a tracklist."
     (interactive)
     (let* ((new (apply #'append
                        (mapcar (lambda (source)
                                  (apply (car source)
                                         (cdr source)))
                                ,tracklist))))
       ,(if shufflep
            '(append (emms-playlist-shuffle-vector (vconcat new)) nil)
          'new))))


;;; Sources

;; A source is just a function that returns a list of tracks.
;; The define-emms-source macro also defines functions emms-play-SOURCE
;; and emms-add-SOURCE.  The former will replace the current playlist,
;; while the latter will add to the end.

(defmacro define-emms-source (name arglist &rest body)
  "Define a new EMMS source called NAME.
This macro defines three functions: `emms-source-NAME', `emms-play-NAME'
and `emms-add-NAME'.  BODY should evaluate do a list of tracks to be played,
which is exactly what `emms-source-NAME' will return.
The other two functions will be simple wrappers around `emms-source-NAME';
any `interactive' form that you specify in BODY will end up in these.
See emms-source-file.el for some examples."
  (let ((source-name (intern (format "emms-source-%s" name)))
        (source-play (intern (format "emms-play-%s" name)))
        (source-add (intern (format "emms-add-%s" name)))
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
         (emms-source-play (,source-name ,@call-args)))
       (defun ,source-add ,arglist
         ,docstring
         ,interactive
         (emms-source-add (,source-name ,@call-args))))))

(defun emms-source-play (lis)
  "Play the tracks in LIS, after first clearing the EMMS playlist."
  (let ((new 
         (if emms-playlist-sort-added-tracks-p
             (emms-playlist-sort-vector (vconcat lis))
           (vconcat lis))))
    (when (zerop (length new))
      (error "No tracks found"))
    (emms-stop)
    (emms-playlist-set-playlist new)
    (emms-playlist-set-current 0)
    (emms-start)))

(defun emms-source-add (lis)
  "Add the tracks in LIS to the end of the EMMS playlist."
  (emms-playlist-add lis))


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
