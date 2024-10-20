;;; Newcomment
(use-package newcomment
  :ensure nil
  :custom
  (comment-empty-lines t)
  (comment-fill-column nil)
  (comment-multi-line t)
  (comment-style 'indent))

;;; Electric
(use-package electric
  :ensure nil
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-default-inhibit)

  (electric-quote-comment nil)
  (electric-quote-string nil)
  (electric-quote-context-sensitive t)
  (electric-quote-replace-double t)
  (electric-quote-inhibit-functions nil)
  :config
  (electric-pair-mode 1))

;;; Eldoc
(use-package eldoc
  :diminish
  :bind ( :map help-map
          ("\." . eldoc-doc-buffer))
  :custom
  (eldoc-print-after-edit nil)
  (eldoc-idle-delay 0.2)
  (eldoc-documentation-strategy
   'eldoc-documentation-compose-eagerly) ; Mash multiple sources together and display eagerly
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit) ; Also respects `max-mini-window-height'
  (eldoc-echo-area-display-truncation-message t)
  (eldoc-echo-area-prefer-doc-buffer t))

;;; Treesit
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
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  (treesit-extra-load-path treesit--install-language-grammar-out-dir-history)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))

;;; Form-feed
;; Display  (page breaks) fancily. Visit the readme for alternatives and their
;; differences
(use-package form-feed
  :diminish
  :custom
  (form-feed-include-modes
   '(prog-mode conf-mode text-mode help-mode emacs-news-view-mode))
  (form-feed-exclude-modes nil)
  :config
  ;; Also see the command `treesit-auto-install-all'
  (global-form-feed-mode 1))

;;; Breadcrumb
;; Which-function stuff but more performant and prettier formatting. Read
;; package commentary for a description on how.
(use-package breadcrumb
  :demand t
  :hook (eglot-managed-mode . (lambda () (when (derived-mode-p 'prog-mode) (breadcrumb-local-mode))))
  :init
  (with-eval-after-load 'which-func
    (which-func-functions '(breadcrumb-imenu-crumbs))))

;;; Provide
(provide 'krisb-programming)
