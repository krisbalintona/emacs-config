;;; Writing
;;;; Newcomment
(use-package newcomment
  :ensure nil
  :custom
  (comment-empty-lines t)
  (comment-fill-column nil)
  (comment-multi-line t)
  (comment-style 'indent))

;;;; Electric
(use-package electric
  :ensure nil
  :hook (prog-mode . electric-block-comment-mode) ; Emacs 31.1
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-default-inhibit)

  (electric-quote-comment nil)
  (electric-quote-string nil)
  (electric-quote-context-sensitive t)
  (electric-quote-replace-double t)
  (electric-quote-inhibit-functions nil)
  :config
  (electric-pair-mode 1))

;;; Visual organization
;;;; Form-feed
;; Display  (page breaks) fancily. Visit the readme for alternatives and their
;; differences
(use-package form-feed
  :diminish
  :custom
  (form-feed-include-modes
   '(prog-mode
     conf-mode
     text-mode
     ;; TOOD 2025-03-25: Enabling form feed in `help-mode' buffers removes the
     ;; fontification of function arguments (face: `help-argument-name').
     ;; help-mode
     emacs-news-view-mode
     compilation-mode))
  (form-feed-exclude-modes nil)
  :config
  (global-form-feed-mode 1))

;;;; Which-func
(use-package which-func
  :hook (on-first-file . which-function-mode)
  :custom
  (which-func-modes '(prog-mode)))

;;;; Breadcrumb
;; Which-function stuff but more performant and prettier formatting. Read
;; package commentary for a description on how.
(use-package breadcrumb
  :demand t
  :hook (eglot-managed-mode . (lambda () (when (derived-mode-p 'prog-mode) (breadcrumb-local-mode))))
  :init
  (with-eval-after-load 'which-func
    (setopt which-func-functions '(breadcrumb-imenu-crumbs))))

;;; Convenience
;;;; Lorem-ipsum
;; Insert sample text
(use-package lorem-ipsum
  :config
  (setq-default lorem-ipsum-list-bullet "- "))

;;;; Sudo-edit
;; Utilities to edit files as root
(use-package sudo-edit
  :bind ( :map krisb-file-keymap
          ("U" . sudo-edit-find-file)
          ("u" . sudo-edit))
  :config
  (sudo-edit-indicator-mode 1))

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
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-help-at-pt t))                 ; Emacs 31.1

;;; Rainbow-mode
;; Colorify color codes
(use-package rainbow-mode
  :diminish
  ;; TOOD 2025-03-25: Enabling form feed in `help-mode' buffers removes the
  ;; fontification of function arguments (face: `help-argument-name').
  ;; :hook (help-mode . rainbow-mode)
  )

;;; Embark
;; Allow an equivalent to ivy-actions to regular completing-read minibuffers
(use-package embark
  :bind (("C-.". embark-act)
         ("C-h B". embark-bindings)
         :map vertico-map
         ("C-.". embark-act)
         :map embark-symbol-map
         ("R". raise-sexp)
         :map embark-org-heading-map
         ("C-j" . org-clock-goto))
  :custom
  ;; Embark Actions menu
  (embark-prompter 'embark-keymap-prompter)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (prefix-help-command #'embark-prefix-help-command) ; Use completing read when typing ? after prefix key

  (embark-mixed-indicator-delay 1.5)
  (embark-collect-live-initial-delay 0.8)
  (embark-collect-live-update-delay 0.5)
  :config
  (add-to-list 'embark-keymap-alist '(raise-sexp . embark-symbol-map)))

;;; Embark-consult
(use-package embark-consult
  :requires embark consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; Provide
(provide 'krisb-programming-essentials)
