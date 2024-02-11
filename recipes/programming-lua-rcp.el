;;; programming-lua-rcp.el --- Lua                   -*- lexical-binding: t; -*-

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

;; These are packages that are helpful for programming in elisp.

;;; Code:
(require 'general)
(require 'keybinds-general-rcp)

;;;; Lua-mode
;; Major-mode for the Lua language.
;; Install directions for system package described here:
;; https://github.com/sumneko/lua-language-server/wiki/Build-and-Run-(Standalone)
;; NOTE: For lsp-mode support, install lua-language-server, a separate system package
(use-package lua-mode
  :ensure-system-package (lua-language-server)
  :general
  (:keymaps 'lua-mode-map
            :states '(normal visual motion)
            "K" 'join-line)
  :custom
  (lua-indent-level 4)           ; This is the convention
  (lua-indent-string-contents t) ; Contents of a multi-line string will be indented
  :config
  (with-eval-after-load 'eglot
    (setf (alist-get 'lua-mode eglot-server-programs)
          '("lua-language-server"))))


;;;; Company-lua
;; Company backend for Lua
(use-package company-lua
  :after company
  :hook (lua-mode . (lambda ()
                      (add-to-list 'company-backends 'company-lua))))

(provide 'programming-lua-rcp)
;;; programming-lua-rcp.el ends here
