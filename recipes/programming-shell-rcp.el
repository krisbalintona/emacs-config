;;; programming-shell-rcp.el --- Shell               -*- lexical-binding: t; -*-

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

;; Packages related to developing in shell languages (e.g. bash).

;;; Code:
(require 'keybinds-general-rcp)

;;;; Sh-script
;; Built-in for sh-mode
(use-package sh-script
  :disabled                  ; FIXME 2023-07-16: Can't find spellcheck package?
  :ensure nil
  :after flycheck
  :ensure-system-package shellcheck
  :mode (("\\.bats\\'" . sh-mode)
         ("\\.\\(?:zunit\\|env\\)\\'" . sh-mode)
         ("/bspwmrc\\'" . sh-mode))
  :custom
  (flycheck-sh-shellcheck-executable "shellcheck"))

;;;; Ssh-config-mode
;; For ~/.ssh/config
(use-package ssh-config-mode
  :hook
  (ssh-config-mode . display-line-numbers-mode))

(provide 'programming-shell-rcp)
;;; programming-shell-rcp.el ends here
