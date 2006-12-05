;;; emms-mark.el ---

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Time-stamp: <Ye Wenbin 2006-12-04 22:54:28>
;; Version: $Id: emms-mark.el,v 1.3 2006/12/04 14:54:33 ywb Exp $
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

(defvar emms-mark-track-desc-functions
  '(emms-track-simple-description
    emms-info-track-description)
  "A list of track description function. If you want emms support
mark, you should add your favorite track description function to this
list and use `emms-mark-select-desc-function' to set the new track
description function.")

(defvar emms-mark-selected-desc-function
  emms-track-description-function)

;;{{{  set new description-function
(defun emms-mark-track-description (track)
  "Return a description of the current track."
  (assert (not (eq emms-mark-selected-desc-function
                   'emms-mark-track-description))
          nil "Should never set emms-mark-selected-desc-function to emms-mark-track-description.")
  (concat "  " (funcall emms-mark-selected-desc-function track)))

(setq emms-track-description-function 'emms-mark-track-description)

(defun emms-mark-select-desc-function (func)
  (interactive
   (list (intern
          (completing-read "Set description function to: "
                           (mapcar 'list
                                   emms-mark-track-desc-functions) nil
                           t "emms-"))))
  (setq emms-mark-selected-desc-function func
        emms-track-description-function 'emms-mark-track-description)
  (emms-with-inhibit-read-only-t
   (save-excursion
     (dolist (buf (remove-if-not 'buffer-live-p
                                 (emms-playlist-buffer-list)))
       (set-buffer buf)
       (let ((tracks (nreverse
                      (emms-playlist-tracks-in-region (point-min)
                                                      (point-max)))))
         (erase-buffer)
         (emms-with-inhibit-read-only-t
          (mapc 'emms-playlist-insert-track
                tracks)))))))
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

(defsubst emms-mark-has-markedp ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "^[%c]" emms-mark-char) nil t)))

;;}}}

;;{{{ functions to operate marked tracks
(defun emms-mark-do-with-marked-track (func &optional move)
  "If your function don't move forward, set move to non-nil."
  (let ((regexp (format "^[%c]" emms-mark-char))
        (newfunc func))
    (if move
        (setq newfunc (lambda () (funcall func) (forward-line 1))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (backward-char 1)               ; move to beginning of line
        (funcall newfunc)))))

(defun emms-mark-mapcar-marked-track (func &optional move)
  (let ((regexp (format "^[%c]" emms-mark-char))
        result (newfunc func))
    (if move
        (setq newfunc (lambda () (let ((res (funcall func)))
                                   (forward-line 1) res))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (backward-char 1)               ; move to beginning of line
        (setq result (cons (funcall newfunc) result)))
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
