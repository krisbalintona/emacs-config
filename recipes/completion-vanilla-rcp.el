;;; completion-vanilla-rcp.el --- Completing-read based completion  -*- lexical-binding: t; -*-

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

;; Completion framework and cousin packages which are lightweight and faithful
;; to the base Emacs architecture.
;;
;; Additionally, you can see a benchmark of fuzzy finding completions in Emacs
;; here: https://github.com/axelf4/emacs-completion-bench#readme

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

(provide 'completion-vanilla-rcp)
;;; completion-vanilla-rcp.el ends here
