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

;; I don't know grammar either...

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Langtool
;; Use langtool program to check grammar of current buffer
(use-package langtool
  ;; NOTE 2021-08-19: Can't use `ensure-system-package' becuase the installation
  ;; of `languagetool' involves many steps (unless on Arch).
  :if (system-packages-package-installed-p "languagetool")
  :custom
  (langtool-default-language "en-US")
  (langtool-autoshow-message-function 'langtool-autoshow-detail-popup)
  ;; (langtool-java-classpath "~/Downloads/LanguageTool-5.3-stable")

  ;; See https://github.com/languagetool-org/languagetool for installation
  ;; instructions
  (langtool-bin nil)
  (langtool-language-tool-jar "~/Downloads/languagetool/LanguageTool-6.3-stable/languagetool-commandline.jar")
  (langtool-language-tool-server-jar "~/Downloads/languagetool/LanguageTool-6.3-stable/languagetool-server.jar")
  (langtool-java-bin (executable-find "java"))

  (langtool-server-user-arguments '("-p" "8082"))
  (langtool-http-server-host 'nil)
  (langtool-http-server-port 8082)
  (langtool-http-server-stream-type 'tls)
  (langtool-default-language "en-US")
  :init
  (defun langtool-autoshow-detail-popup (overlays)
    (when (require 'popup nil t)
      ;; Do not interrupt current popup
      (unless (or popup-instances
                  ;; suppress popup after type `C-g` .
                  (memq last-command '(keyboard-quit)))
        (let ((msg (langtool-details-error-message overlays)))
          (popup-tip msg)))))
  :config
  (pretty-hydra-define hydra:langtool
                       (:color green :hint t :foreign-keys run :quit-key "q" :exit t)
                       ("Correct grammar"
                        (("b" #'langtool-check-buffer "Check")
                         ("c" #'langtool-correct-buffer "Correct")
                         ("d" #'langtool-check-done "Done")))))

;;;; Flymake-languagetool
(use-package flymake-languagetool
  :disabled
  :hook ((org-mode LaTeX-mode org-mode markdown-mode) . flymake-languagetool-load)
  :general (:keymaps 'flymake-mode-map
                     (general-chord "``") 'flymake-languagetool-correct-dwim)
  :custom
  ;; See https://github.com/languagetool-org/languagetool for installation
  ;; instructions
  (flymake-languagetool-server-jar
   (expand-file-name "languagetool-server.jar" "~/Downloads/languagetool/LanguageTool-6.3-stable/"))
  (flymake-languagetool-active-modes
   '(text-mode latex-mode org-mode markdown-mode message-mode))
  (flymake-languagetool-check-spelling nil)
  ;; See https://community.languagetool.org/rule/list?lang=en for IDs
  (flymake-languagetool-disabled-rules
   '("DATE_NEW_YEAR" "WHITESPACE_RULE" "ARROWS")))

;;;; Lsp-grammarly
(use-package lsp-grammarly
  :after lsp-mode
  :hook (lsp-grammarly-ls-after-open . (lambda () (lsp-ui-mode -1))))

;;;; Eglot-grammarly
;; The formal `eglot-grammarly' package is useless; the following code is
;; basically the package
(with-eval-after-load 'eglot
  (unless (system-packages-package-installed-p "grammarly-languageserver")
    (system-packages-ensure "sudo npm install -g @emacs-grammarly/grammarly-languageserver"))
  (add-to-list 'eglot-server-programs
               `((text-mode latex-mode org-mode markdown-mode)
                 "grammarly-languageserver" "--stdio"
                 :initializationOptions (:clientId "client_BaDkMgx4X19X9UxxYRCXZo"))))

(provide 'checking-grammar-rcp)
;;; checking-grammar-rcp.el ends here
