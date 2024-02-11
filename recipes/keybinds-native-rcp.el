;;; keybinds-native-rcp.el --- Miscellaneous keybindings  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords:

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

;; Keybinds for built-in Emacs commands.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Commands
;;;;; Inserting empty lines
(defun kb/open-line-above-goto ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current
mode. Credit to
https://emacsredux.com/blog/2013/06/15/open-line-above/"
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (indent-according-to-mode))

(defun kb/open-line-below-goto ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current
mode. Credit to
https://emacsredux.com/blog/2013/03/26/smarter-open-line/"
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun kb/open-line-above-insert ()
  "Insert an empty line above the current one without going to it."
  (interactive)
  (save-mark-and-excursion (kb/open-line-above-goto)))

(defun kb/open-line-below-insert ()
  "Insert an empty line above the current one without going to it."
  (interactive)
  (save-excursion (kb/open-line-below-goto)))

;;;;; Join lines
(defun kb/join-line-above ()
  "Join the current line with the line above."
  (interactive)
  (save-excursion (delete-indentation))
  (when (string-match-p "\\`\\s-*$" (thing-at-point 'line))
    (funcall indent-line-function)))

(defun kb/join-line-below ()
  "Join the current line with the line below."
  (interactive)
  (save-excursion (delete-indentation t))
  (when (bolp)
    (funcall indent-line-function)))

;;;;; Scrolling
(general-define-key
 "H-P" 'scroll-down-line
 "H-N" 'scroll-up-line)

;;;; Text editing
(general-define-key
 "C-S-p" 'kb/open-line-above-goto
 "C-S-n" 'kb/open-line-below-goto
 "C-S-k" 'kb/join-line-above
 "C-S-j" 'kb/join-line-below
 (general-chord "[ ") 'kb/open-line-above-insert
 (general-chord "] ") 'kb/open-line-below-insert)

;;;; Other
(defun kb/restart-or-kill-emacs (&optional arg restart)
  "Kill Emacs.
If called with RESTART or `universal-argument’, restart Emacs
instead. Passes ARG to `save-buffers-kill-emacs'."
  (interactive "P")
  (save-buffers-kill-emacs arg (or restart (equal arg '(4)))))

(general-define-key
 [remap save-buffers-kill-terminal] 'kb/restart-or-kill-emacs)

(provide 'keybinds-native-rcp)
;;; keybinds-native-rcp.el ends here
