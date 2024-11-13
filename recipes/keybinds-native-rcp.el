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
(defun kb/open-line-above-insert ()
  "Insert an empty line above the current one without going to it."
  (interactive)
  (save-mark-and-excursion (kb/open-line-above-goto)))

(defun kb/open-line-below-insert ()
  "Insert an empty line above the current one without going to it."
  (interactive)
  (save-excursion (kb/open-line-below-goto)))

;;;; Text editing
(bind-chords ("[ " . kb/open-line-above-insert))
(bind-chords ("] " . kb/open-line-below-insert))

(provide 'keybinds-native-rcp)
;;; keybinds-native-rcp.el ends here
