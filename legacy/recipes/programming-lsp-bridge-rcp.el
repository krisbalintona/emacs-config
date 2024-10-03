;;; programming-lsp-bridge-rcp.el --- Lsp-bridge     -*- lexical-binding: t; -*-

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

;; All configuration related to lsp-bridge.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Lsp-bridge
;; Minimal LSP client whose benefit is asynchrony
(use-package lsp-bridge
  :disabled t
  :ensure (lsp-bridge :type git
                      :host github
                      :repo "manateelazycat/lsp-bridge"
                      :files (:defaults "*.py" "langserver" "acm"))
  :hook (lsp-bridge-mode . (lambda ()
                             "Disable `eglot' and `corfu' when enabling `lsp-bridge-mode'."
                             (when (bound-and-true-p eglot--managed-mode)
                               (corfu-mode 0)
                               (eglot-shutdown (eglot-current-server)))))
  :init
  ;; (global-lsp-bridge-mode)
  )

(provide 'programming-lsp-bridge-rcp)
;;; programming-lsp-bridge-rcp.el ends here
