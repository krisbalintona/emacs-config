;;; programming-ide-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Making my Emacs coding environment closer to a full-featured IDE.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Lsp-mode
;; Use the language server protocol as a backend for Emacs.
(use-package lsp-mode
  :ghook ('prog-mode-mode-hook 'lsp-deferred) ; Lsp-mode only when buffer is visible
  :gfhook 'lsp-enable-which-key-integration 'lsp-headerline-breadcrumb-mode
  :general
  (:keymaps 'lsp-mode-map
            "TAB" 'company-indent-or-complete-common)
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
;;;; Dap-mode
(use-package dap-mode
  ;; :ensure-system-package ("pip install \"ptvsd>=4.2\"")
  )

;;;; Ancillary
;;;;; Company-lsp
;; Company integration with lsp-mode
(use-package company-lsp
  :requires company
  :after company
  :hook (lsp-mode . (lambda ()
                      (add-to-list 'company-backends 'company-lsp)))
  :custom
  (company-lsp-cache-candidates t)      ; Cache all candidates
  (company-lsp-async t)
  (compnay-lsp-enable-snippet t)
  (company-lsp-enable-recompletion t)   ; Reenables completion when before another trigger character
  )

;;;;; Dev-docs
;; Viewing documentation within Emacs. Requires internet connection.
(use-package devdocs
  :hook ((python-mode . (lambda () (setq-local devdocs-current-docs '("python~3.9"))))
         (haskell-mode . (lambda () (setq-local devdocs-current-docs '("haskell~8"))))
         (js2-mode . (lambda () (setq-local devdocs-current-docs '("JavaScript"))))
         (lua-mode . (lambda () (setq-local devdocs-current-docs '("lua~5.3"))))
         (LaTeX-mode . (lambda () (setq-local devdocs-current-docs '("latex"))))
         )
  :general
  (kb/leader-keys
    :keymaps 'prog-mode-map
    :states 'normal
    "di" '(devdocs-install :which-key "Install documentation for a language")
    "dl" '(devdocs-lookup :which-key "Documentation lookup")
    "dL" '(devdocs-search :which-key "Search for docs in site"))
  )

;;; programming-ide-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-ide-rcp)
