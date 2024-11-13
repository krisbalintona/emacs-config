;;; Treesit
;; Built-in tree-sitter support.
(use-package treesit
  :ensure nil
  :custom
  ;; HACK 2024-10-20: The car of
  ;; `treesit--install-language-grammar-out-dir-history' is used as the default
  ;; output directory for installing grammars via
  ;; `treesit-install-language-grammar'. See the definition of
  ;; `treesit-install-language-grammar'.
  (treesit--install-language-grammar-out-dir-history
   (list (no-littering-expand-var-file-name "treesit")))
  (treesit-font-lock-level 3))

;;; Treesit-auto
;; Automatically install tree-sitter grammars.
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  (treesit-extra-load-path treesit--install-language-grammar-out-dir-history)
  :config
  ;; Add all *-ts-modes to `auto-mode-alist'
  (global-treesit-auto-mode 1)
  (treesit-auto-add-to-auto-mode-alist 'all))

;;; Provide
(provide 'krisb-treesit)
