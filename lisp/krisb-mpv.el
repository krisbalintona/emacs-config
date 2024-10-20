;;; krisb-mpv.el --- Bespoke MPV extensions          -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Extending the functionality of mpv.el for my idiosyncratic note-taking
;; preferences.

;;; Code:
(require 'mpv)

;;; Custom MPV notes
;;;; Functions
(defun krisb-mpv-play ()
  "Call `mpv-start'.
Prompts for file in `org-attach-directory' if existent.  Otherwise,
prompts for file in `default-directory'.

Behaves specially in Dired buffers.  In those cases case, marked files
will be played as a playlist as chronologically displayed in the Dired
buffer.  If no files are marked, just the file at point is played.  (It
is useful to `dired-sort-toggle-or-edit' to control the ordering of
files.  To reverse the order, pass the \"-r\" flag to the listing
switches, done by calling `dired-sort-toggle-or-edit' with `C-u'.)"
  (interactive)
  (let* ((dir (if (org-attach-dir)
                  (file-name-as-directory (org-attach-dir))
                default-directory))
         (files
          (or (cl-remove-if
               (lambda (f)
                 "Ensure F is not a directory and is a video file."
                 (not (and (not (directory-name-p f))
                           (member (file-name-extension f)
                                   ;; OPTIMIZE 2024-03-31: I hard-code this,
                                   ;; but I don't know if there's a better way
                                   ;; to recognize video extensions
                                   (list "mp4" "avi" "mkv" "mov" "wmv" "flv" "webm" "mpeg" "m4v" "3gp")))))
               ;; NOTE 2024-03-31: If no files are marked, the file at point
               ;; is treated as marked
               (dired-get-marked-files))
              (expand-file-name (read-file-name "Select video: " dir)))))
    (apply #'mpv-start files)))

(defun krisb-mpv-jump-to-playback-position (time)
  "Prompt user for a TIME to jump to.
Takes HH:MM:SS time format.  Uses `org-timer-hms-to-secs' to parse user
input."
  (interactive "MJump to time (HH:MM:SS format): ")
  (let ((secs (org-timer-hms-to-secs time)))
    (mpv-seek secs)))

;;;; Keymap
(defvar-keymap krisb-mpv-map
  :doc "Keymap for my mpv.el commands for use in `org-mode'.
Commands that control MPV playback mimic MPV keybinds."
  :repeat (mpv-pause mpv-seek-backward mpv-seek-forward)
  "o" #'krisb-mpv-play
  "O" #'mpv-play-url
  "k" #'mpv-kill
  "p" #'mpv-pause
  "b" #'mpv-seek-backward
  "f" #'mpv-seek-forward
  "g" #'krisb-mpv-jump-to-playback-position
  "9" #'mpv-volume-decrease
  "0" #'mpv-volume-increase
  "[" #'mpv-speed-decrease
  "]" #'mpv-speed-increase
  "P" #'mpv-jump-to-playlist-entry
  "i" #'mpv-insert-playback-position)

;; Taken from https://github.com/kljohann/mpv.el/wiki
(defun krisb-mpv-org-metareturn-insert-playback-position ()
  "When on an `org-timer' formatted list, insert playback position."
  (when-let ((item-beg (org-in-item-p)))
    (when (and (not (bound-and-true-p org-timer-start-time))
               (mpv-live-p)
               (save-excursion
                 (goto-char item-beg)
                 (and (not (org-invisible-p)) (org-at-item-timer-p))))
      (mpv-insert-playback-position t))))
(with-eval-after-load 'org
  (add-hook 'org-metareturn-hook #'krisb-mpv-org-metareturn-insert-playback-position))

;; Go to timestamps with `org-open-at-point'
(with-eval-after-load 'org
  (add-hook 'org-open-at-point-functions 'mpv-seek-to-position-at-point))

;;; Provide
(provide 'krisb-mpv)
;;; krisb-mpv.el ends here
