;; -*- lexical-binding: t; -*-

;;; Treesit
;; Built-in tree-sitter support.
(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level '((t . 3))))

;;; Treesit-auto
;; Provide sources for many tree-sitter grammars.
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; Add all *-ts-modes to `auto-mode-alist'
  (global-treesit-auto-mode 1)
  (treesit-auto-add-to-auto-mode-alist 'all))

;;; Provide
(provide 'krisb-treesit)
