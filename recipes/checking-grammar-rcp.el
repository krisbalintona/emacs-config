;;; checking-grammar-rcp.el --- Grammar checking     -*- lexical-binding: t; -*-

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

;; I don't know grammar either... Right now I prefer grammarly via eglot. The
;; next best alternative with respect to LSPs would be ltex-ls, which is just
;; LanguageTool put into an LSP that supports more than just straightforward
;; text files. I suppose the next best thing after that would be
;; `flymake-languagetool' since it at least has "code actions" via
;; `flymake-languagetool-correct-dwim'.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Grammarly with Eglot
;; The formal `eglot-grammarly' package is useless; the following code is
;; basically the package. See all settings for the grammarly LSP here:
;; https://github.com/emacs-grammarly/grammarly-language-server/blob/main/extension/package.json
(with-eval-after-load 'eglot
  (unless (system-packages-package-installed-p "grammarly-languageserver")
    (system-packages-ensure "sudo npm install -g @emacs-grammarly/grammarly-languageserver"))
  (add-to-list 'eglot-server-programs
               '(org-mode "grammarly-languageserver" "--stdio"
                          :initializationOptions (:clientId "client_BaDkMgx4X19X9UxxYRCXZo"))))

;;;; Eglot-ltex
;; LanguageTool grammar and spelling errors detected in markup documents. Can
;; read more about ltex-ls here: https://github.com/valentjn/ltex-ls
(use-package eglot-ltex
  :disabled
  :ensure-system-package (ltex-ls .  ltex-ls-bin)
  :ensure (:type git :host github :repo "emacs-languagetool/eglot-ltex")
  :custom
  ;; Found via paru -Ql ltex-ls-bin
  (eglot-ltex-server-path "/usr/share/ltex-ls/"))

(provide 'checking-grammar-rcp)
;;; checking-grammar-rcp.el ends here
