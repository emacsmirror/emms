;; emms-streams.el -- interface to add and play streams

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301 USA

;; Commentary: 

;; It is part of the EMMS package

;; Heavily based on bmk-mgr.el by Jose A Ortega Ruiz <jao@gnu.org>
;; thanks to you !

;; Code:

(defvar emms-stream-bookmarks-file "~/.emacs.d/emms-streams"
  "The file where you store your favorite emms streams")

(defvar emms-stream-list nil
  "The list that contains your current stream bookmarks.")

(defvar emms-stream-buffer-name "*EMMS Streams*"
  "The name of the buffer used by emms-stream interface.")

(defvar emms-stream-play-hook nil
  "*A hook run when you add or play an EMMS stream via the popup.")

(defvar emms-stream-current-stream nil
  "The stream currently being played.
Needed by the info method, as the track doesn't contain all the
needed info.")

(defvar emms-stream-default-action "add"
  "The default action when you press RET in the EMMS Stream interface.
Can be either \"add\" or \"play\". The default is \"add\".")

(defvar emms-stream-last-stream nil
  "The last stream added/played by EMMS.")

(defface emms-stream-name-face '((t (:bold t :foreground nil :weight bold)))
  "Face for stream names.")

(defface emms-stream-url-face '((t (:foreground "LightSteelBlue")))
  "Face for stream URLs.")

;; Format: (("descriptive name" url feed-number type))
;; type could be either url or playlist. If url, then it represents a
;; direct IP, otherwite it's a stream playlist
(defvar emms-stream-default-list
 '(("SomaFM: Beatblender"
    "http://www.somafm.com/beatblender.pls" 1 streamlist)
   ("SomaFM: Secret Agent"
    "http://www.somafm.com/secretagent.pls" 1 streamlist)
   ("SomaFM: Groove Salad"
    "http://www.somafm.com/groovesalad.pls" 1 streamlist)
   ("SomaFM: Drone Zone"
    "http://www.somafm.com/dronezone.pls" 1 streamlist)
   ("SomaFM: Tag's Trance"
    "http://www.somafm.com/tagstrance.pls" 1 streamlist)
   ("Digitally Imported, Trance"
    "http://www.digitallyimported.com/mp3/trance.pls" 1 streamlist)
   ("Digitally Imported, Deephouse"
    "http://www.digitallyimported.com/mp3/deephouse.pls" 1 streamlist)
   ("Digitally Imported, Mostly Classical"
    "http://www.digitallyimported.com/mp3/classical.pls" 1 streamlist)
   ("Digitally Imported, Chillout"
    "http://www.digitallyimported.com/mp3/chillout.pls" 1 streamlist)
   ("Digitally Imported, Drum and Bass"
    "http://www.digitallyimported.com/mp3/drumandbass.pls" 1 streamlist)
   ("Philosomatika, Goa-Trance"
    "http://www.philosomatika.com/Philosomatika.pls" 1 streamlist)
   ("Drum and Bass Radio, BassDrive"
    "http://www.bassdrive.com/BassDrive.m3u" 1 streamlist)
   ("Flaresound, Jazzmusique"
    "http://64.236.34.196:80/stream/1016" 1 url)
   ("Flaresound, Jazzmusique"
    "http://205.188.234.4:8004" 2 url)
   ("Flaresound, L'Electric"
    "http://www.bp6.com:8002" 1 url)
   ("Stangs Garage, Eclectic"
    "http://www.stangsgarage.com/listen.pls" 1 streamlist)
   ("DNA Lounge, Live"
    "http://www.dnalounge.com/audio/128.m3u" 1 streamlist)
   ("Virgin Radio, The Groove"
    "http://www.smgradio.com/core/audio/ogg/live.pls?service=grbb" 1 streamlist)
   ("Virgin Radio, Virgin Classic"
    "http://www.smgradio.com/core/audio/ogg/live.pls?service=vcbb" 1 streamlist)
   ("Virgin Radio, Virgin 1215AM"
    "http://www.smgradio.com/core/audio/ogg/live.pls?service=vrbb" 1 streamlist)
   ("WCPE, Classical Music"
    "http://www.ibiblio.org/wcpe/wcpe.pls" 1 streamlist)))

(defvar emms-stream-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map [(control ?a)] 'beginning-of-line)
    (define-key map [(control ?e)] 'end-of-line)
    (define-key map [(control ?k)] 'emms-stream-kill-bookmark)
    (define-key map [(control ?y)] 'emms-stream-yank-bookmark)
    (define-key map [(control ?n)] 'emms-stream-next-line)
    (define-key map [(control ?p)] 'emms-stream-previous-line)
    (define-key map [?Q] 'emms-stream-quit)
    (define-key map [?a] 'emms-stream-add-bookmark)
    (define-key map [?d] 'emms-stream-delete-bookmark)
    (define-key map [?e] 'emms-stream-edit-bookmark)
    (define-key map [?h] 'describe-mode)
    (define-key map [?n] 'emms-stream-next-line)
    (define-key map [?p] 'emms-stream-previous-line)
    (define-key map [?q] 'emms-stream-quit)
    (define-key map [?s] 'emms-stream-save-bookmarks-file)
;;    (define-key map [?u] 'emms-stream-move-bookmark-up)
    (define-key map [?i] 'emms-stream-info-bookmark)
    (define-key map (kbd "<up>") 'emms-stream-previous-line)
    (define-key map (kbd "<down>") 'emms-stream-next-line)
    (define-key map (kbd "<left>") 'beginning-of-line)
    (define-key map (kbd "<right>") 'end-of-line)
    (define-key map (kbd "RET") 'emms-stream-play)
    map)
  "Keymap for `emms-stream-menu'.")

(defun emms-stream-line-number-at-pos (&optional pos)
  "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location."
  (let ((opoint (or pos (point))) start)
    (save-excursion
      (goto-char (point-min))
      (setq start (point))
      (goto-char opoint)
      (forward-line 0)
      (1+ (count-lines start (point))))))

(defun emms-streams ()
  "Opens the EMMS Streams interface."
  (interactive)
  (kill-buffer (get-buffer-create emms-stream-buffer-name))
  (set-buffer (get-buffer-create emms-stream-buffer-name))
  (erase-buffer)
  (emms-stream-mode)
  (switch-to-buffer emms-stream-buffer-name))

(defun emms-stream-mode ()
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'emms-stream-mode)
  (setq mode-name "EMMS Streams")
  (use-local-map emms-stream-mode-map)
  (emms-stream-init)
  (set (make-local-variable 'truncate-lines) t)
  (set (make-local-variable 'automatic-hscrolling) t)
  (set (make-local-variable 'kill-whole-line) t)
  (set (make-local-variable 'next-line-add-newlines) nil)
  (goto-char 1)
  (emms-stream-display)
  (toggle-read-only 1)
  (message "EMMS Stream Menu"))

(defun emms-stream-popup-revert ()
  "Revert to the window-configuration from before if there is one,
otherwise just remove the special bindings from the stream menu."
  (interactive)
  (remove-hook 'emms-pbi-manually-change-song-hook 'emms-pbi-popup-revert)
  (let ((streambuffer (get-buffer emms-stream-buffer-name)))
    (when streambuffer
      (save-excursion
	(set-buffer streambuffer)
	;; (local-unset-key (kbd "q"))
	(local-unset-key (kbd "TAB")))))
        ;; (local-unset-key (kbd "RET")))))
  (when emms-stream-popup-old-conf
    (set-window-configuration emms-stream-popup-old-conf))
  (remove-hook 'emms-stream-play-hook 'emms-stream-popup-revert)
  (remove-hook 'emms-stream-quit-hook 'emms-stream-popup-revert))

(defun emms-stream-popup (&optional popup-height)
  "Pops up the stream Menu, for the new stream selection.

POPUP-HEIGHT is the height of the new frame, defaulting to
`emms-popup-default-height'."
  (interactive)
  (setq popup-height (or popup-height (/ (window-height) 2)))
  ;; Split the current screen, and make the stream menu popup
  (let ((new-window-height (- (window-height) popup-height)))
    (if (not (> new-window-height 0))
	(error "Current window too small to popup menu!"))
    ;; Save the current window-configuration
    (setq emms-stream-popup-old-conf (current-window-configuration))
    ;; Split and select the menu
    (let ((buffer-down
           (split-window-vertically new-window-height)))
      (select-window buffer-down))

      (kill-buffer (get-buffer-create emms-stream-buffer-name))
      (switch-to-buffer (get-buffer-create emms-stream-buffer-name))
      (erase-buffer)
      (emms-stream-mode)

      (add-hook 'emms-stream-play-hook 'emms-stream-popup-revert)
      (add-hook 'emms-stream-quit-hook 'emms-stream-popup-revert)
      (local-set-key (kbd "TAB") 'emms-stream-popup-revert)
      (local-set-key (kbd "RET") 'emms-stream-play)
      ;; (local-set-key (kbd "q") 'delete-window)
      ;; Also, forget about the whole thing if the user does something
      ;; to the window-configuration
      ;; (add-hook 'window-configuration-change-hook 'emms-stream-popup-forget-conf)))
      ))

(defun emms-stream-init ()
  (setq emms-stream-list (emms-stream-read-file emms-stream-bookmarks-file)))

(defun emms-stream-read-file (file)
  "Returns a sexp."
  (let ((file (expand-file-name file)))
    (if (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents-literally file)
          (goto-char (point-min))
          (read (current-buffer)))
      emms-stream-default-list)))

(defun emms-stream-save-bookmarks-file ()
  (interactive)
  (let ((buffer (find-file-noselect emms-stream-bookmarks-file)))
    (set-buffer buffer)
    (erase-buffer)
    (prin1 emms-stream-list buffer)
    (save-buffer)
    (kill-buffer buffer)))

(defun emms-stream-display-line (line)
  (insert (emms-stream-name line))
  (add-text-properties (point-at-bol) (point-at-eol) '(face emms-stream-name-face))
  (add-text-properties (point-at-bol) (point-at-eol) `(emms-stream ,line))
  (insert "\n      ")
  (insert (emms-stream-url  line))
  (add-text-properties (point-at-bol) (point-at-eol) '(face emms-stream-url-face))
  (insert "\n"))

(defun emms-stream-display ()
  "Displays the bookmark list in the current buffer, in a human
  readable way."
  (mapc 'emms-stream-display-line emms-stream-list)
  (goto-char (point-min)))

;; Helper functions
(defun take (n list)
  "Takes N elements from LIST."
  (let ((idx  0)
        (res '()))
    (while (< idx n)
      (setq res (append res (list (nth idx list))))
      (setq idx (+ idx 1)))
    res))

(defun insert-at (n elt list)
  "Inserts the element ELT in LIST, *before* position N.
Positions are counted starting with 0."
  (let* ((n-1     (- n 1))
         (before (take n-1 list))
         (after  (last list (- (length list) n-1))))
    (append before (list elt) after)))

(defun emms-stream-get-bookmark-at-point ()
  "Returns the bookmark under point."
  (get-text-property (point) 'emms-stream))


(defun emms-stream-redisplay ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (goto-char 1)
    (emms-stream-display)))

(defun emms-stream-add-bookmark (name url fd type)
  "Creates a new bookmark, and inserts it at point position.

Don't forget to run `emms-stream-save-bookmarks-file' after !"
  (interactive "sName of the bookmark: 
sURL: 
nFeed descriptor: 
SType (url or streamlist): ")
  (let* ((line     (emms-stream-line-number-at-pos (point)))
         (index    (+ (/ line 2) 1)))
    (setq emms-stream-list (insert-at index (list name url fd type) emms-stream-list))
    (emms-stream-redisplay)
    (goto-line line)))

(defun emms-stream-delete-bookmark ()
  "Deletes the bookmark under the point.

Don't forget to save your modifications !"
  (interactive)
  (let ((line     (emms-stream-line-number-at-pos (point))))
    (setq emms-stream-list
          (remove (emms-stream-get-bookmark-at-point) emms-stream-list))
    (emms-stream-redisplay)
    (goto-line line)))

(defun emms-stream-edit-bookmark ()
  "Change the information of current bookmark."
  (interactive)
  (let* ((bookmark (emms-stream-get-bookmark-at-point))
         (name     (read-from-minibuffer "Description: "
                                         (emms-stream-name bookmark)))
         (url      (read-from-minibuffer "URL: "
                                         (emms-stream-url bookmark)))
         (fd       (read-from-minibuffer "Feed Descriptor: "
                                         (int-to-string (emms-stream-fd bookmark))))
         (type     (read-from-minibuffer "Type (url or streamlist): "
                                         (format "%s" (emms-stream-type bookmark)))))
    (emms-stream-delete-bookmark)
    (emms-stream-add-bookmark name url (string-to-int fd) type)))

(defun emms-stream-name (el)
  (car el))
(defun emms-stream-url (el)
  (cadr el))
(defun emms-stream-fd (el)
  (caddr el))
(defun emms-stream-type (el)
  (cadddr el))

(defun emms-stream-play ()
  (interactive)
  (let* ((line  (get-text-property (point) 'emms-stream))
         (name  (emms-stream-name line))
         (url   (emms-stream-url  line))
         (fd    (emms-stream-fd   line))
         (type  (emms-stream-type line))
         (player (read (concat "emms-" emms-stream-default-action "-" (format "%s" type)))))
    (setq emms-stream-last-stream line)
;;    (funcall emms-stream-default-action url)
    (funcall player url)
    (if (string= emms-stream-default-action "add")
        (message "URL added to playlist")))
  (later-do 'emms-mode-line-alter)
  (run-hooks 'emms-stream-play-hook))

(defun emms-stream-info-bookmark ()
  "Return the station and track information for the streaming audio station under point."
  (interactive)
  (if (fboundp 'emms-stream-info-message)
      (let* ((line (get-text-property (point) 'emms-stream))
	     (url (emms-stream-url line)))
	(emms-stream-info-message url))
    (message "Streaming media info not available.")))

;; Navigation
(defun emms-stream-next-line ()
  (interactive)
  (forward-line 2))

(defun emms-stream-previous-line ()
  (interactive)
  (forward-line -2))

(defun emms-stream-quit ()
  (interactive)
  (kill-this-buffer)
  (run-hooks 'emms-stream-quit-hook))

(defun emms-stream-toggle-default-action ()
  (interactive)
  (if (string= emms-stream-default-action "play")
      (progn
        (setq emms-stream-default-action "add")
        (message "Default action is now add"))
    (setq emms-stream-default-action "play")
    (message "Default action is now play")))

;; info part
; (define-emms-info-method emms-info-url
;    :providep 'emms-info-url-providep
;    :get 'emms-info-url-get)
;;   :set 'emms-info-url-set)

;; A way to get the last element.  it is either the only one, or the
;; last one added by emms-add-url. so in both cases, that's what we
;; want.
;; FIXME : not working with the new design. Yrk ?
(defun emms-stream-last-element ()
  (elt emms-playlist (- (length emms-playlist) 1)))

(defun emms-info-url-providep (track)
  (if (eq (emms-track-type track) 'url)
      t
    nil))

; (defun emms-info-url-get (track)
;   (make-emms-info
;    :title (emms-stream-url (emms-track-get track 'metadata))
;    :artist (emms-stream-name (emms-track-get track 'metadata))
;    :album " "
;    :note " "
;    :year " "
;    :genre " "
;    :file (emms-stream-url (emms-track-get track 'metadata))))

;; Then you register it with emms-info, by adding it to
;; `emms-info-methods-list'.

; (add-to-list 'emms-info-methods-list 'emms-info-url)

(defun emms-info-file-info-song-artist (track)
  "Returns a description of TRACK, build from its comments.

If the track already indicates artist and title, use it.
Otherwise return the name of the track."
  (let ((name (and track (emms-track-name track))))
    (if (null name)
        "Invalid track!"
      (let ((artist (emms-track-get track 'info-artist))
            (title (emms-track-get track 'info-title)))
        (if (and artist (not (string= artist ""))
                 title (not (string= title "")))
            (concat artist " - " title)
          (file-name-sans-extension (file-name-nondirectory name)))))))

(defun emms-stream-add-data-to-track (track)
  (emms-track-set track 'metadata emms-stream-last-stream))

(add-to-list 'emms-track-initialize-functions
             'emms-stream-add-data-to-track)

; (when (featurep 'emms-info)
;   (eval-when-compile (require 'emms-info)) ; appease byte-compiler
;   (add-to-list 'emms-info-methods-list 'emms-info-streamlist)
;   (defun emms-info-streamlist-providep (track)
;     (if (eq (emms-track-type track) 'streamlist)
;         t
;       nil))
;   (define-emms-info-method emms-info-streamlist  ;; FIXME-PLS ?
;     :providep 'emms-info-streamlist-providep ;; FIXME-PLS ?
;     :get 'emms-info-url-get))

(provide 'emms-streams)
;;; emms-streams.el ends here
