;;; persistence-rcp.el --- Persistence across Emacs sessions  -*- lexical-binding: t; -*-

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

;; Packages relevant to saving and loading information across Emacs sessions.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Recentf
;; Enable logging of recent files
(use-package recentf
  :ensure nil
  :hook
  (on-first-input . recentf-mode)
  :bind
  ( :map krisb-file-keymap
    ("r" . recentf-open-files))
  :custom
  (recentf-max-saved-items 1000)
  (recentf-max-menu-items 15)
  :config
  (recentf-mode 1))

(provide 'persistence-rcp)
;;; persistence-rcp.el ends here
