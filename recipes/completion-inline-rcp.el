;;; completion-inline-rcp.el --- Inline completion   -*- lexical-binding: t; -*-

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

;; Packages that configure the vanilla `completion-at-point' functionality.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Nerd-icons-corfu
;; Use nerd-icons with corfu
(use-package nerd-icons-corfu
  :disabled ; Deprecated because I've created a setup with kind-icon that pulls nerd-icon icons
  :after corfu
  :demand
  :autoload nerd-icons-corfu-formatter
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
(provide 'completion-inline-rcp)
;;; completion-inline-rcp.el ends here
