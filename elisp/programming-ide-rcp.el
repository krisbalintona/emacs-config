;;; programming-ide-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Making my Emacs coding environment closer to a full-featured IDE.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)

;;;; Lsp-mode
;; Use the language server protocol as a backend for Emacs.
(use-package lsp-mode
  :ghook ('prog-mode-mode 'lsp-deferred) ; Lsp-mode only when buffer is visible
  :gfhook 'lsp-enable-which-key-integration 'lsp-headerline-breadcrumb-mode
  :general
  (:keymaps 'lsp-mode-map
            "TAB" 'company-indent-or-complete-common
            )
  (kb/leader-keys
    "ld" 'xref-find-definitions
    "lr" 'xref-find-references
    "ln" 'lsp-ui-find-next-reference
    "lp" 'lsp-ui-find-prev-reference
    "le" 'lsp-ui-flycheck-list
    "lS" 'lsp-ui-sideline-mode
    "lX" 'lsp-execute-code-action
    )
  :custom
  (lsp-keymap-prefix "C-x l")
  (lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols))
  )

;;;; Lsp-ui
;; Fancy frame and sideline overlay which shows useful information about what's
;; on the point.
(use-package lsp-ui
  :ghook 'lsp-mode-hook
  :custom
  (lsp-ui-doc-position 'top)
  )
;;;; Dev-docs
;; Viewing documentation within Emacs
(use-package devdocs
  :hook ((python-mode . (lambda () (setq-local devdocs-current-docs '("python~3.9"))))
         (haskell-mode . (lambda () (setq-local devdocs-current-docs '("haskell~8"))))
         )
  :general
  (:keymaps 'devdocs-mode-map
            "go" '(browse-url :which-key "Open in browser"))
  (kb/leader-keys
    :keymaps 'prog-mode-map
    :states '(normal visual motion)
    "dl" '(devdocs-lookup :which-key "Devdocs lookup"))
  )

;;; programming-ide-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-ide-rcp)
