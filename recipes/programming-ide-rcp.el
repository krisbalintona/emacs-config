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

;;; Lsp-mode
;; Use the language server protocol as a backend for Emacs.
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  ;; Lsp-mode only when buffer is visible
  :gfhook
  'lsp-enable-which-key-integration
  'lsp-headerline-breadcrumb-mode
  :custom
  (lsp-keymap-prefix "C-x l")
  (lsp-auto-guess-root nil)
  (lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-semantic-tokens-enable t)
  (lsp-modeline-diagnostics-scope :workspace)
  (lsp-modeline-code-actions-segments '(count icon name))
  (lsp-enable-file-watchers t) ; Affects performance. Enable if I do actual programming
  (lsp-log-io nil)             ; If set to true can cause a performance hit

  ;; When you hover over a symbol
  (lsp-enable-symbol-highlighting t)
  (lsp-symbol-highlighting-skip-current nil) ; When highlighting, don't highlight symbol on point
  :custom-face
  (lsp-face-highlight-read ((t (:inherit nil :box (:line-width -1 :style nil)))))
  ;; (lsp-face-highlight-textual ((t (:inherit nil :box t))))
  ;; (lsp-face-highlight-write ((t (:inherit nil :box t))))
  )

;;; Lsp-ui
;; Fancy frame and sideline overlay which shows useful information about what's
;; on the point.
(use-package lsp-ui
  :ghook 'lsp-mode-hook
  :hook ((lsp-ui-imenu-mode . hide-mode-line-mode)
         (lsp-mode . lsp-ui-mode))
  :general (:keymaps 'lsp-ui-mode-map
                     [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
                     [remap xref-find-references] #'lsp-ui-peek-find-references
                     [remap imenu-list] #'lsp-ui-imenu
                     )
  :custom
  ;; Lsp-ui-peek - Peek in a child frame
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)

  ;; Lsp-ui-sideline - Info at the side
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)  ; Show diagnostics messages in sideline?
  (lsp-ui-sideline-show-hover nil)      ; Show hover messages in sideline?
  (lsp-ui-sideline-show-code-actions t) ; Show code actions in sideline?
  ;; When set to 'line' the information will be updated when user changes
  ;; current line otherwise the information will be updated when user changes
  ;; current point
  (lsp-ui-sideline-update-mode 'point)
  (lsp-ui-sideline-delay 0.5)          ; Seconds to wait before showing sideline

  ;; Lsp-eldoc - Info in the echo area
  (lsp-eldoc-hook nil)
  (lsp-eldoc-render-all t)

  ;; Lsp-ui-doc - Show documentation
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-delay 1.5)
  (lsp-ui-doc-show-with-cursor t)       ; Point hover (alongside cursor!)
  (lsp-ui-doc-show-with-mouse t)        ; Point hover (alongside cursor!)
  ;; Appearance
  (lsp-ui-doc-alignment 'frame)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-max-height 10)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-text-scale-level -1)
  (lsp-ui-doc-use-webkit nil)

  ;; Lsp-ui-imenu - Imenu integration
  (lsp-ui-imenu-window-width 70)
  (lsp-ui-imenu-auto-refresh 'after-save) ; Auto refresh
  (lsp-ui-imenu-auto-refresh-delay 1.0)   ; Variable doesn't exist?
  )

;;; Dap-mode
(use-package dap-mode
  :demand t
  :after lsp-mode
  :gfhook
  'dap-ui-mode      ; Mouse hover support
  'dap-tooltip-mode ; Use tooltips for mouse hover over symbols, otherwise use the minibuffer
  'tooltip-mode     ; Displays floating panel with debug buttons
  'dap-ui-controls-mode
  :general (:keymaps 'lsp-mode-map
                     :prefix lsp-keymap-prefix
                     "d" '(:ignore t :which-key "DAP")
                     "dd" '(dap-debug :which-key "Debug")
                     "dl" '(dap-debug-last :which-key "Debug")
                     "dh" '(dap-hydra :which-key "Hydra")

                     "db" '(:ignore t :which-key "Breakpoints")
                     "dbt" '(dap-breakpoint-toggle :which-key "Toggle breakpoint")
                     "dba" '(dap-breakpoint-toggle :which-key "Toggle breakpoint")
                     "dbd" '(dap-breakpoint-delete :which-key "Delete breakpoint")
                     "dbD" '(dap-breakpoint-delete-all :which-key "Delete all breakpoints")
                     "dbl" '(dap-breakpoint-log-message :which-key "Breakpoint log")
                     "dbc" '(dap-breakpoint-condition :which-key "Breakpoint condition")

                     "de" '(:ignore t :which-key "Expressions")
                     "dea" '(dap-ui-expressions-add :which-key "Expression add")
                     "der" '(dap-ui-expressions-add :which-key "Expression add")
                     "dr" '(dap-ui-repl :which-key "REPL")
                     )
  :custom (dap-auto-configure-features '(;; sessions
                                         locals
                                         breakpoints
                                         expressions
                                         controls
                                         tooltip
                                         ))
  )

;;; Ancillary
;;;; Consult-lsp
(use-package consult-lsp
  :after lsp-mode
  :gfhook consult-lsp-marginalia-mode
  :general
  (:keymaps 'lsp-mode-map
            [remap consult-flycheck] '(consult-lsp-diagnostics :which-key "Consult lsp diagnostics")
            (concat lsp-keymap-prefix "gs") '(consult-lsp-symbols :which-key "Consult lsp symbols regexp")
            (concat lsp-keymap-prefix "gf") '(consult-lsp-file-symbols :which-key "Consult lsp file symbols list")
            )
  )

;;;; Dev-docs
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

;;;; Treemacs
(use-package treemacs
  :gfhook 'hide-mode-line-mode
  :custom
  (treemacs-no-png-images nil)
  (treemacs-width 24)
  :general (kb/leader-keys
             "ft" '(treemacs :which-key "Treemacs"))
  )

;;;; Tree-sitter
;; Create a syntax tree (e.g. the role of each piece of code) and add syntax
;; highlighting from it (rather than regex and indentation). Additionally, the
;; syntax tree itself can help debug and quick editing in some cases. The
;; following are the currently supported languages (provided by
;; `tree-sitter-langs')
;; C
;; C++
;; CSS
;; Go
;; HTML
;; Java
;; JavaScript
;; PHP
;; Python
;; Ruby
;; Rust
;; TypeScript
(use-package tree-sitter
  :straight (tree-sitter :type git :host github :repo "emacs-tree-sitter/elisp-tree-sitter")
  :demand t
  :hook (tree-sitter-after-on . tree-sitter-hl-mode) ; Enable syntax highlighting whenever possible
  :config
  (global-tree-sitter-mode)     ; Enable for all supported tree-sitter languages

  ;; Supported language bundle for tree-sitter
  (use-package tree-sitter-langs
    :demand t
    :config
    (tree-sitter-langs-install-grammars
     t
     tree-sitter-langs--bundle-version
     tree-sitter-langs--os)
    )
  )

;;;; Lsp-treemacs
;; Treemacs-like buffer that shows files, errors, symbol hierarchy, etc.
(use-package lsp-treemacs
  :requires treemacs
  :hook ((lsp-mode . lsp-treemacs-sync-mode)
         (lsp-treemacs-generic-mode . hide-mode-line-mode)
         (lsp-treemacs-error-list-mode . hide-mode-line-mode)
         (lsp-treemacs-deps-list-mode . hide-mode-line-mode)
         )
  :general
  (:keymaps 'lsp-mode-map
            (concat lsp-keymap-prefix "Ft") '(lsp-treemacs-symbols :which-key "Lsp-treemacs"))
  (:keymaps 'lsp-treemacs-error-list-mode-map
            :states 'normal
            "x" 'lsp-treemacs-quick-fix)
  )

;;; programming-ide-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-ide-rcp)
