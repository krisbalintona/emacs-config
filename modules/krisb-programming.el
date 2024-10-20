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
  (global-form-feed-mode 1))

;;; Provide
(provide 'krisb-programming)
