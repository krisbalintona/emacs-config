;; -*- lexical-binding: t; -*-

;;; Treesit
;; Built-in tree-sitter support.
(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level '((t . 3)))
  ;; HACK 2024-10-20: `treesit--install-language-grammar-out-dir-history' is a
  ;; history variable for directories tree-sitter grammars have been installed
  ;; to.  However, its car (i.e., the latest history item) is used as the
  ;; default output directory for installing grammars (see the definition of
  ;; `treesit-install-language-grammar').  So we set the initial car of this
  ;; variable to change the default install directory, which is currently
  ;; hard-coded to the "tree-sitter" subdirectory under the
  ;; `user-emacs-directory'.
  (treesit--install-language-grammar-out-dir-history
   (list (no-littering-expand-var-file-name "treesit"))))

;;; Treesit-auto
;; Automatically install tree-sitter grammars.
(use-package treesit-auto
  :disabled t ; 2025-04-21: Don't need this with chnages to treesit.el in Emacs 31
  :custom
  (treesit-auto-install 'prompt)
  (treesit-extra-load-path treesit--install-language-grammar-out-dir-history)
  :config
  ;; Add all *-ts-modes to `auto-mode-alist'
  (global-treesit-auto-mode 1)
  (treesit-auto-add-to-auto-mode-alist 'all))

;;; Provide
(provide 'krisb-treesit)
