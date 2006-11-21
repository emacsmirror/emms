;;; emms-mark.el.gz ---

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Time-stamp: <Ye Wenbin 2006-11-21 17:02:28>
;; Version: $Id: emms-mark.el,v 0.0 <2006-11-21 14:06:38> ywb Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Provide mark operation to tracks

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'emms-mark)

;;; Code:

(provide 'emms-mark)
(require 'emms)
(eval-when-compile
  (require 'cl))

;;{{{  set new description-function
(defun emms-mark-track-description (track)
  "Return a description of the current track."
  (concat "  "                          ; for mark char
          (let ((artist (emms-track-get track 'info-artist))
                (title (emms-track-get track 'info-title)))
            (if (and artist title)
                (format "%s - %s" artist title)
              (emms-track-simple-description track)))))

(setq emms-track-description-function 'emms-mark-track-description)
;;}}}

;;{{{ functions to mark tracks
(defvar emms-mark-char ?*)
(defvar emms-mark-face-alist
  '((?* . font-lock-warning-face)
    (?\040 . emms-playlist-track-face)))

(defun emms-mark-track (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (let ((face (assoc-default emms-mark-char emms-mark-face-alist))
        buffer-read-only track)
    (save-excursion
      (beginning-of-line)
      (while (and (not (eobp))
                  (> arg 0))
        (setq track (get-text-property (point) 'emms-track))
        (delete-char 1)
        (insert (propertize (string emms-mark-char)
                            'emms-track track))
        (backward-char 1)
        (put-text-property (point) (progn (forward-line 1) (point))
                           'face face)
        (setq arg (1- arg))))))

(defun emms-mark-unmark-track (&optional arg)
  (interactive "p")
  (let ((emms-mark-char ?\040))
    (emms-mark-track arg)))

(defun emms-mark-forward (arg)
  (interactive "p")
  (emms-mark-track arg)
  (forward-line arg))

(defun emms-mark-unmark-forward (arg)
  (interactive "p")
  (emms-mark-unmark-track arg)
  (forward-line arg))

(defun emms-mark-all ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (emms-mark-track (count-lines (point-min) (point-max)))))

(defun emms-mark-unmark-all ()
  (interactive)
  (emms-mark-do-with-marked-track 'emms-mark-unmark-track))

(defun emms-mark-regexp (regexp)
  (interactive "sMark track match: ")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (emms-mark-track 1)
      (forward-line 1))))

(defun emms-mark-unmark-regexp (regexp)
  (interactive "sUnmark track match: ")
  (let ((emms-mark-char ?\040))
    (emms-mark-regexp regexp)))

(defun emms-mark-toggle ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (buffer-read-only)
      (while (not (eobp))
        (if (eq ?\040 (following-char))
            (emms-mark-track)
          (emms-mark-unmark-track))
        (forward-line 1)))))
;;}}}

;;{{{ functions to operate marked tracks
(defun emms-mark-do-with-marked-track (func)
  (let ((regexp (format "^[%c]" emms-mark-char)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (backward-char 1)               ; move to beginning of line
        (funcall func)))))

(defun emms-mark-mapcar-marked-track (func)
  (let (result)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[*]" nil t)
        (backward-char 1)               ; move to beginning of line
        (setq result (cons (funcall func) result)))
      (nreverse result))))

(defun emms-mark-delete-marked-tracks ()
  (interactive)
  (emms-with-inhibit-read-only-t
   (emms-mark-do-with-marked-track
    (lambda nil (delete-region (point)
                               (progn (forward-line 1) (point)))))))

(defun emms-mark-kill-marked-tracks ()
  (interactive)
  (let (tracks buffer-read-only)
    (emms-mark-do-with-marked-track
     (lambda nil
       (setq tracks
             (concat tracks
                     (delete-and-extract-region (point)
                                                (progn (forward-line 1) (point)))))))
    (kill-new tracks)))

(defun emms-mark-copy-marked-tracks ()
  (interactive)
  (let (tracks)
    (emms-mark-do-with-marked-track
     (lambda nil
       (setq tracks
             (concat tracks
                     (buffer-substring (point)
                                       (progn (forward-line 1) (point)))))))
    (kill-new tracks)))
;;}}}

(let ((map emms-playlist-mode-map))
  (define-key map "m" 'emms-mark-forward)
  (define-key map "u" 'emms-mark-unmark-forward)
  (define-key map "U" 'emms-mark-unmark-all)
  (define-key map "t" 'emms-mark-toggle)
  (define-key map "%m" 'emms-mark-regexp)
  (define-key map "%u" 'emms-mark-unmark-regexp))

;;; emms-mark.el ends here
