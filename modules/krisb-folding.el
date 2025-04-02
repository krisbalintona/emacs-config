;;; Outline
(use-package outline
  :ensure nil
  :diminish outline-minor-mode
  :custom
  (outline-minor-mode-cycle t)
  (outline-minor-mode-cycle-filter nil)
  (outline-minor-mode-highlight 'append)
  (outline-blank-line t))

;;; Outshine
;; Outline-minor-mode but with better keybindings and more support.
(use-package outshine
  :disabled t ; 2025-04-02: Well-functioning but heavy and sometimes opinionated.  Prefer outli
  :diminish outshine-mode
  :hook ((LaTeX-mode prog-mode conf-mode) . outshine-mode)
  :bind ( :map outshine-mode-map
          ("C-x n s". outshine-narrow-to-subtree)
          :map diff-mode-map
          ("S-<iso-lefttab>" . outshine-cycle-buffer)
          ("<tab>" . outshine-cycle)
          ("C-x n s" . outshine-narrow-to-subtree))
  :custom
  (outshine-use-speed-commands t))

;;; Outline-indent
(use-package outline-indent
  :disabled t                    ; 2025-04-02: Prefer the more lightweight outli
  :hook (prog-mode . outline-indent-minor-mode)
  :bind ( :map krisb-toggle-keymap
          ("o" . krisb-outline-indent-dispatch))
  :config
  (require 'transient)
  (transient-define-prefix krisb-outline-indent-dispatch ()
    "Invoke a transient menu for `tmr'."
    [["At point"
      ("o" "Open" outline-indent-open-fold)
      ("C" "Open recursively" outline-indent-open-fold-rec)
      ("T" "Close" outline-indent-close-fold)]
     ["At point toggle"
      ("t" "Toggle" outline-indent-toggle-fold)
      ("l" "Toggle level at point" outline-indent-toggle-level-at-point)]]
    ["All folds"
     [("O" "Open" outline-indent-open-folds)
      ("C" "Close" outline-indent-close-folds)]]))

;;; Outli
;; Coding language-agnostic file outlines.  Lightweight and close to the
;; built-in outline.el.
(use-package outli
  :vc ( :url "https://github.com/jdtsmith/outli"
        :rev :newest)
  :hook ((prog-mode text-mode) . outli-mode)
  :bind ( :map outline-minor-mode-map
          ;; 2025-04-02: Assumes `outline-minor-mode-prefix' is "C-c @"
          ("C-c @ C-<return>" . outli-insert-heading-respect-content))
  :custom
  (outli-allow-indented-headlines t)
  (outli-default-nobar nil)             ; No horizontal rule?
  (outli-blend 0.0) ; 0.0 rather than nil lets the horizontal rule begin at the stem, not after
  :config
  ;; Update outli faces on theme change

  ;; NOTE 2025-04-02: We can't call `outli-reset-all-faces' directly yet because
  ;; `enable-theme-functions' should be functions that take a single argument.
  ;; If we call `outli-reset-all-faces' directly, we'll get a warning because it
  ;; does not accept any arguments.  I've made an issue about this:
  ;; https://github.com/jdtsmith/outli/issues/21
  (add-hook 'enable-theme-functions (lambda (_) (outli-reset-all-faces)))

  ;; Add "Heading" (which outli headings are categorized as) imenu group.  Taken
  ;; from https://github.com/jdtsmith/outli?tab=readme-ov-file#faq
  (with-eval-after-load 'consult-imenu
    (push '(?h "Headings")
          (plist-get (cdr (assoc 'emacs-lisp-mode consult-imenu-config)) :types))))

;;; Provide
(provide 'krisb-folding)
