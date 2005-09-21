;;; emms-scores.el --- Scoring system for mp3player

;; Copyright (C) 2003, 2004, 2005

;; Authors: Jean-Philippe Theberge <jphiltheberge@videotron.ca>, Yoni
;;          Rabkin <yonirabkin@member.fsf.org>
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

;;; Commentary:

;; NOTE: This is experimental stuff - comments welcome!  There
;; shouldn't worky anything in that file... scores aren't saved, they
;; even don't have any consequence on playing order and there's just
;; one mood in the moment. But it's a beginning and you can score down
;; or up tracks... :)
;;
;; * How to use scoring in emms
;;
;; When you load emms, you are set to a default mood
;; 'emms-default-mood' A mood is a one word string describing how
;; you feel (like "funny", "tired", "aggresive"...)  Each mood have is
;; own set of scoring rules.
;;
;; You can change your mood with M-x emms-score-change-mood.
;;
;; Every music file start with a default score of 0 the command
;; emms-score-up-playing and emms-score-down-playing modify the
;; score of the file you are curently listening by 1 In addition,
;; skipping a file (with emms-skip) automaticaly score the file
;; down.
;;
;; With scoring on (this mean the variable emms-use-scoring is t),
;; emms will compare the score of the file with your tolerance to
;; decide if it is played or not.
;;
;; The default tolerance level is 0 (or the variable
;; emms-score-min-score).  This mean files with a score of 0 or more will
;; be played and files with a score of -1 or less will be skipped.
;;
;; You can change the tolerance (by 1) with M-x
;; emms-score-lower-tolerance and M-x
;; emms-score-be-more-tolerant

;;; Code:

(defvar emms-scores-list nil)
(defvar emms-score-current-mood 'default)
(defvar emms-score-min-score 0)
(defvar emms-score-default-score 0)
(defvar emms-score-hash (make-hash-table :test 'equal))

(add-hook 'kill-emacs-hook 'emms-score-save-hash)

(defcustom emms-score-file "~/.emms/scores"
  "*Directory to store the score file."
  :type 'directory
  :group 'emms)

(defun emms-score-change-mood (mood)
  "Change the current MOOD.  
The score hash is automatically saved."
  (interactive "sMood: ")
  (emms-score-save-hash)
  (setq emms-score-current-mood (intern (downcase mood))))

(defun emms-score-save-hash ()
  "Save score hash in `emms-score-file'."
  (interactive)
  (unless (file-directory-p (file-name-directory emms-score-file))
    (make-directory (file-name-directory emms-score-file)))
  (with-temp-file emms-score-file
    (let ((standard-output (current-buffer)))
      (insert "(")
      (maphash (lambda (key value)
		 (prin1 (cons key value)))
	       emms-score-hash)
      (insert ")"))))

(defun emms-score-load-hash ()
  "Load score hash from `emms-score-file'."
  (interactive)
  (mapc (lambda (elt)
	  (puthash (car elt) (cdr elt) emms-score-hash))
	(read
	 (with-temp-buffer
	   (insert-file-contents emms-score-file)
	   (buffer-string)))))

(defun emms-score-get-plist (filename)
  (gethash filename emms-score-hash))

(defun emms-score-change-score (score filename)
  (let ((sp (emms-score-get-plist filename) )
	(sc (emms-score-get-score filename)))
    (puthash filename 
	     (plist-put sp emms-score-current-mood (+ sc score)) 
	     emms-score-hash)
    (message "New score is %s" (+ score sc))))

(defun emms-score-up-playing ()
  (interactive)
  (if emms-player-playing-p
      (emms-score-change-score 1 (emms-playlist-current-selected-track))
    (error "No track currently playing")))

(defun emms-score-down-playing ()
  (interactive)
  (if emms-player-playing-p
      (emms-score-change-score -1 (emms-playlist-current-selected-track))
    (error "No track currently playing")))

(defun emms-score-up-file-on-line ()
  (interactive)
  (emms-score-change-score 1 (emms-playlist-current-selected-track)))

(defun emms-score-down-file-on-line ()
  (interactive)
  (emms-score-change-score -1 (emms-playlist-current-selected-track)))

(defun emms-score (arg)
  "Turn on emms-score if prefix argument ARG is a positive integer,
off otherwise."
  (interactive "p")
  (if (and arg (> arg 0))
      (progn
	(emms-score-load-hash)
	(remove-hook 'emms-player-stopped-hook 'emms-next-noerror)
	(add-hook 'emms-player-stopped-hook 'emms-score-next-noerror))
    (emms-score-save-hash)
    (remove-hook 'emms-player-stopped-hook 'emms-score-next-noerror)
    (add-hook 'emms-player-stopped-hook 'emms-next-noerror)))

(defun emms-score-next-noerror ()
  "Play the next track in the playlist, but don't signal an error when
we're at the end. This should be called when no player is playing.
This is a suitable function to put in `emms-player-stopped-hook'."
  (interactive)
  (when emms-player-playing-p
    (error "A track is already playing."))  
  (if (emms-playlist-next) 
      (if (emms-score-check-score (emms-playlist-current-selected-track))
	  (emms-start)
	(emms-score-next-noerror))
    (message "No track in playlist that matches your score anymore")))

(defun emms-score-create-entry (filename)
  (puthash filename (list emms-score-current-mood emms-score-default-score) 
	   emms-score-hash))

(defun emms-score-get-score (filename)
  "Return score of TRACK."
  (let ((plist (emms-score-get-plist filename)))
    (if (member emms-score-current-mood plist)
	(plist-get plist emms-score-current-mood)
      (emms-score-create-entry filename)
      (emms-score-get-score filename))))
    
(defun emms-score-check-score (filename)
  (>= (emms-score-get-score filename) emms-score-min-score))

(defun emms-score-lower-tolerance ()
  "Only play mp3 with a higher score"
  (interactive)
  (setq emms-score-min-score (+ emms-score-min-score 1)))

(defun emms-score-be-more-tolerant ()
  "Allow playing of mp3 with a lower score"
  (interactive)
  (setq emms-score-min-score (- emms-score-min-score 1)))

(provide 'emms-score)

;;; emms-scores.el ends here
