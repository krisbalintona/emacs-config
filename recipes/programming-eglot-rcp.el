;;; programming-eglot-rcp.el --- Eglot               -*- lexical-binding: t; -*-

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

;; Eglot-related configuration.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Eglot
(use-package eglot
  :hook ((eglot-managed-mode . (lambda ()
                                 "Configure `eldoc'"
                                 ;; Use `eglot--setq-saving' to restore original
                                 ;; values. Make sure "eldoc," or a similar
                                 ;; regexp, isn't in `eglot-stay-out-of'
                                 (when (bound-and-true-p eldoc-box-hover-mode)
                                   (eglot--setq-saving eldoc-box-cleanup-interval 2))
                                 (eglot--setq-saving eldoc-echo-area-use-multiline-p nil)))
         (eglot-managed-mode . (lambda ()
                                 "Add `eglot-flymake-backend' to the beginning of
`flymake-diagnostic-functions', appending to the original
functions."
                                 (push (cons 'flymake-diagnostic-functions flymake-diagnostic-functions)
                                       eglot--saved-bindings) ; Manually add to saved values
                                 (add-to-list 'flymake-diagnostic-functions 'eglot-flymake-backend))))
  :general
  ("C-c e e" 'eglot)
  (:keymaps 'eglot-mode-map
            :prefix "C-c e"
            "r" 'eglot-rename
            "a" 'eglot-code-actions
            "=" 'eglot-format
            "R" 'eglot-reconnect
            "s" 'eglot-shutdown
            "S" 'eglot-shutdown-all)
  (:keymaps 'eglot-mode-map
            (general-chord "``") 'eglot-code-actions)
  :custom
  ;; NOTE 2023-07-11: Set to 0 if I want performance in exchange for no events
  ;; printed to a buffer. Fyi: Pretty printing to lisp is much much slower than
  ;; keeping it json.
  ;; (eglot-events-buffer-config '(:size 2000000 :format lisp))
  ;; (eglot-events-buffer-config '(:size 2000000 :format full))
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-connect-timeout 15)
  (eglot-autoreconnect 3)
  (eglot-sync-connect 3)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.5)
  (eglot-extend-to-xref t)
  (eglot-report-progress 'messages)
  (eglot-ignored-server-capabilities '(:inlayHintProvider)) ; Disable inlay hints globally
  :custom-face
  (eglot-highlight-symbol-face ((t (:box (:line-width -1 :style nil)))))
  :config
  ;; Not a `defcustom', so use `setq'
  (setq eglot-stay-out-of '("flymake")))

;;;; Eglot-booster
;; Boosts Eglot's communication with the server. There's also a version for LSP.
(use-package eglot-booster
  ;; NOTE 2024-01-10: Must install the `emacs-lsp-booster' binary from
  ;; https://github.com/blahgeek/emacs-lsp-booster/releases
  ;; :ensure (:type git :host github :repo "jdtsmith/eglot-booster")
  :vc (:url "https://github.com/jdtsmith/eglot-booster.git"
            :rev :newest)
  :after eglot
  :demand
  :config
  (eglot-booster-mode 1))

;;;; Languages
;;;;; Eglot-java
(use-package eglot-java
  :ensure-system-package jdtls
  :requires eglot
  :hook ((java-mode . eglot-ensure))
  :custom
  (eglot-java-server-install-dir (no-littering-expand-var-file-name "eglot-java/"))
  (eglot-java-workspace-folder (expand-file-name eglot-java-server-install-dir "workspace/"))
  (eglot-java-eclipse-jdt-args (list "--enable-preview"))
  (eglot-java-prefix-key "C-c e")
  (eglot-java-default-bindings-enabled t) ; See `eglot-java--setup'
  :config
  (eglot-java-init))                    ; Add to hook to `java-mode-hook'

;;;; Consult-eglot
;; Equivalent to `consult-lsp'; adds `consult-eglot-symbols'.
(use-package consult-eglot
  :after eglot
  :general (:keymaps 'eglot-mode-map
                     [remap xref-find-apropos] #'consult-eglot-symbols))

(provide 'programming-eglot-rcp)
;;; programming-eglot-rcp.el ends here
