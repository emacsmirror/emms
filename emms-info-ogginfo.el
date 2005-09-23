;;; emms-info-ogginfo.el --- Emms information from Ogg Vorbis files.

;;; Copyright 2005, Yoni Rabkin under the GNU General Public License.

;;; Commentary:
;;

;;; Code:

(defgroup emms-info-ogginfo nil
  "An EMMS-info method for getting, using the external ogginfo
program"
  :group 'emms-info)

(defcustom emms-info-ogginfo-coding-system 'latin-1
  "*Coding system used in the output of ogginfo."
  :type 'coding-system
  :group 'emms-info-ogginfo)

(defcustom emms-info-ogginfo-program-name "ogginfo"
  "*The name/path of the ogginfo tag program."
  :type 'string
  :group 'emms-info-ogginfo)

(defun emms-info-ogginfo (track)
  "Add track information to TRACK.
This is a useful element for `emms-info-functions'."
  (when (and (eq 'file (emms-track-type track))
             (string-match "\\.[Oo][Gg][Gg]\\'" (emms-track-name track)))

    (with-temp-buffer
      (when (zerop
	     (call-process emms-info-ogginfo-program-name
			   nil t nil (emms-track-name track)))

	;; play time, emms-info-ogg.el [U. Jensen]
	(goto-char (point-min))
	(re-search-forward "Playback length: \\([0-9]*\\)m:\\([0-9]*\\)")
	(let ((minutes (string-to-int (match-string 1)))
	      (seconds (string-to-int (match-string 2))))
	  (setq ptime-total (+ (* minutes 60) seconds)
		ptime-min minutes
		ptime-sec seconds))
	(emms-track-set track 'info-playing-time ptime-total)
	(emms-track-set track 'info-playing-time-min ptime-min)
	(emms-track-set track 'info-playing-time-sec ptime-sec)
	(emms-track-set track 'info-file (emms-track-name track))

	;; all the rest of the info available
	(goto-char (point-min))
	(when (re-search-forward "User comments section follows..." (point-max) t)
	  (while (zerop (forward-line 1))
	    (when (looking-at "^\t\\(.*\\)=\\(.*\\)$")
	      (let ((a (match-string 1))
		    (b (match-string 2)))
		(when (and (< 0 (length a))
			   (< 0 (length b)))
		  (emms-track-set track
				  (intern (downcase (concat "info-" (match-string 1))))
				  (match-string 2)))))))))))

(provide 'emms-info-ogginfo)

;;; emms-info-ogginfo.el ends here
