;; emms-gstreamer.el --- EMMS Gstreamer interaction

;; License : GPL v2.1 or later

;; currently outside other files, as it's very preliminary support

;; The wrapper concept is easier to set up than a generic gstreamer
;; support, but in the long term, it's probably not a good idea.

;; Installation instructions :

;; 1. Put (require 'emms-gstreamer) in your ~/.emacs or whatever you
;; use to configure EMMS.

;; 2. Put the wrappers in your `exec-path' :
;;      (add-to-list 'exec-path "/path/to/wrappers") or the other way,
;;      by moving them in an already present directory.

(require 'emms-player-simple)

(defvar emms-gst-sink "alsasink"
  "The audio output sink to use")

(defvar emms-gstreamer-paused-p nil)

(define-emms-simple-player gst-mp3  '(file) "\\.[mM][pP][23]$" "gst-mp3-wrapper")
(define-emms-simple-player gst-ogg  '(file) (regexp-opt '(".ogg" ".OGG")) "gst-ogg-wrapper")
(define-emms-simple-player gst-flac '(file) (regexp-opt '(".FLAC" ".flac" )) "gst-flac-wrapper")
(define-emms-simple-player gst-mod  '(file) (regexp-opt '(".xm" ".it" ".ft" ".mod")) "gst-mod-wrapper")

(add-to-list 'emms-player-list 'emms-player-gst-mp3)
(add-to-list 'emms-player-list 'emms-player-gst-ogg)
(add-to-list 'emms-player-list 'emms-player-gst-flac)
(add-to-list 'emms-player-list 'emms-player-gst-mod)

(setq emms-player-gst-mp3-parameters `(,emms-gst-sink))
(setq emms-player-gst-ogg-parameters `(,emms-gst-sink))
(setq emms-player-gst-flac-parameters `(,emms-gst-sink))
(setq emms-player-gst-mod-parameters `(,emms-gst-sink))


(defun emms-gstreamer-play/pause ()
  (interactive)
  (if emms-gstreamer-paused-p
      (emms-gstreamer-resume)
    (emms-gstreamer-pause)))

(defun emms-gstreamer-pause ()
  (interactive)
  (signal-process (shell-command-to-string "pgrep gst-launch") 'SIGSTOP)
  (setq emms-gstreamer-paused-p t))

(defun emms-gstreamer-resume ()
  (interactive)
  (signal-process (shell-command-to-string "pgrep gst-launch") 'SIGCONT)
  (setq emms-gstreamer-paused-p nil))

(provide 'emms-player-gstreamer)
