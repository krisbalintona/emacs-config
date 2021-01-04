;;; programming-general-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Language-agnostic packages helpful or required for programming
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Completion
;;;;; Yasnippet
;; Template-expansion system (doesn't include templates)
(use-package yasnippet
  :hook ((text-mode prog-mode snippet-mode) . yas-minor-mode-on)
  :preface
  (defvar kb/personal-snippets-dir
    (concat user-emacs-directory "snippets/"))
  :config
  (add-to-list 'yas-snippet-dirs 'kb/personal-snippets-dir) ; Where I keep my own snippets
  )

;;;;; Doom-snippets
;; Large library of snippet templates
(use-package doom-snippets
  :after yasnippet
  :hook (after-init . yas-reload-all)
  :straight (doom-snippts :type git :host github :repo "hlissner/doom-snippets")
  ;; :quelpa (doom-snippets :fetcher git :url "https://github.com/hlissner/doom-snippets")
  )

;;;;; Org-tempo
;; Completion for org-blocks
(use-package org-tempo
  :straight nil
  :config
  (dolist (expansion '(("sh" . "src sh")
                       ("el" . "src emacs-lisp")
                       ("py" . "src python")
                       ;; ("sc" . "src scheme")
                       ;; ("ts" . "src typescript")
                       ;; ("yaml" . "src yaml")
                       ;; ("json" . "src json")
                       )
                     org-structure-template-alist)
    (push expansion org-structure-template-alist))
  )

;;;; Project management
;;;;; Magit
;; The best git interface. Mostly taken from Mostly taken from
;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#version-control
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (auto-revert-check-vc-info nil) ; Fixes VC info on a timer in order to take into account changes made outside of Emacs - causes micro-stutters when too many version controlled buffers
  (magit-diff-highlight-hunk-body nil)
  (magit-diff-highlight-hunk-region-functions
   '(magit-diff-highlight-hunk-region-dim-outside magit-diff-highlight-hunk-region-using-face))
  (magit-module-sections-nested nil)
  (magit-popup-display-buffer-action '((display-buffer-same-window)))
  (magit-refs-show-commit-count 'all)
  (magit-section-initial-visibility-alist '((modules . show)
                                            (stashes . show)
                                            (unpulled . show)
                                            (unpushed . show)))
  (magit-section-show-child-count t)
  (transient-mode-line-format nil)
  :config
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-modules-overview 'magit-insert-status-headers t)
  (remove-hook 'magit-section-highlight-hook #'magit-section-highlight)

  (kb/leader-keys
    "g"  '(:ignore t :which-key "Magit")
    "gg"  '(magit-status :which-key "Status")
    "gs"  '(magit-status :which-key "Status")
    "gd"  'magit-diff-unstaged
    "gc"  'magit-branch-or-checkout
    "gl"   '(:ignore t :which-key "Logs")
    "glc" 'magit-log-current
    "glf" 'magit-log-buffer-file
    "gb"  'magit-branch
    "gP"  '(magit-push-current :which-key "Push")
    "gp"  'magit-pull-branch
    "gf"  'magit-fetch
    "gF"  'magit-fetch-all
    "gr"  '(magit-rebase :which-key "Rebase")
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-general-rcp)
;;;;; Projectile
;; Navigate and manage project directories easier
(use-package projectile
  :hook (after-init . projectile-mode)
  :init
  (when (file-directory-p user-emacs-directory)
    (setq projectile-project-search-path `(,user-emacs-directory)))
  (setq projectile-switch-project-action #'projectile-dired)
  :custom
  (projectile-completion-system 'default) ; Use selectrum
  (projectile-enable-caching t)
  (projectile-track-known-projects-automatically nil) ; Don't create projects automatically
  :config
  ;; Hydra menu
  (pretty-hydra-define hydra:selectrum-projectile
    (:color blue :hint t :foreign-keys run :quit-key "q" :exit t)
    ("Projectile"
     (("i" projectile-invalidate-cache :color red)
      ("n" projectile-add-known-project))
     "Buffers"
     (("b" projectile-switch-to-buffer)
      ("K" projectile-kill-buffers)
      ("S" projectile-save-project-buffers))
     "Find"
     (("d" projectile-find-dir)
      ("D" projectile-dired)
      ("f" projectile-find-file)
      ("p" projectile-switch-project))
     "Search"
     (("r" projectile-replace)
      ("R" projectile-replace-regexp)
      ("s" counsel-projectile-rg))
     ))

  (kb/leader-keys
    "p" '(:ignore t :which-key "Projectile")
    "p?" '(hydra:selectrum-projectile/body :which-key "Help menu")
    ;; "pf"  'projectile-find-file
    "pp"  'projectile-switch-project
    ;; "ps"  'counsel-projectile-rg
    "pb"  'projectile-switch-to-buffer
    "pD"  'projectile-dired
    ;; "pc"  'projectile-compile-project
    )
  )

;;;;; Counsel-projectile
;; Use Ivy as projectile interface
(use-package counsel-projectile
  :after (counsel projectile)
  :hook (counsel-mode . counsel-projectile-mode)
  ;; :custom
  ;; (projectile-completion-system 'ivy) ; Set to default because I use selectrum now
  :config
  ;; ;; Hydra menu
  ;; (pretty-hydra-define hydra:counsel-projectile
  ;;   (:color blue :hint t :foreign-keys run :quit-key "q" :exit t)
  ;;   ("Projectile"
  ;;    (("i" projectile-invalidate-cache :color red)
  ;;     ("n" projectile-add-known-project))
  ;;    "Buffers"
  ;;    (("b" counsel-projectile-switch-to-buffer)
  ;;     ("K" projectile-kill-buffers)
  ;;     ("S" projectile-save-project-buffers))
  ;;    "Find"
  ;;    (("d" counsel-projectile-find-dir)
  ;;     ("D" projectile-dired)
  ;;     ("f" counsel-projectile-find-file)
  ;;     ("p" counsel-projectile-switch-project))
  ;;    "Search"
  ;;    (("r" projectile-replace)
  ;;     ("R" projectile-replace-regexp)
  ;;     ("s" counsel-projectile-rg))
  ;;    ))

  ;; (kb/leader-keys
  ;;   "p" '(:ignore t :which-key "Projectile")
  ;;   "p?" '(hydra:counsel-projectile/body :which-key "Help menu")
  ;;   "pf"  'counsel-projectile-find-file
  ;;   "pp"  'counsel-projectile-switch-project
  ;;   "ps"  'counsel-projectile-rg
  ;;   "pb"  'counsel-projectile-switch-to-buffer
  ;;   "pD"  'projectile-dired
  ;;   ;; "pc"  'projectile-compile-project
  ;;   )
  )

;;;;; Helm-projectile
;; Projectile with helm
(use-package helm-projectile
  :disabled t ; Now I use selectrum instead
  :after (helm projectile)
  ;; :hook (projectile-mode . helm-projectile-on)
  :preface (use-package helm-rg) ; Required for helm-projectile-rg
  :custom
  (projectile-completion-system 'helm)
  (projectile-switch-project-action 'helm-projectile)
  )

;;;; Aesthetics
;;;;; Git-gutter-fringes
;; Show diffs in fringes. Taken from
;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#version-control
(use-package git-gutter-fringe
  :disabled t ; Until I figure out how to disable in org-mode
  :hook (after-change-major-mode . kb/git-gutter-enable)
  :preface
  (defun kb/git-gutter-enable ()
    (when-let* ((buffer (buffer-file-name))
                (backend (vc-backend buffer)))
      (require 'git-gutter)
      (require 'git-gutter-fringe)
      (git-gutter-mode t)))
  :custom
  (git-gutter-fr:side 'left-side)
  (git-gutter:disabled-modes '(org-mode))
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center t))
  (define-fringe-bitmap 'git-gutter-fr:deleted [240 240 240 240] nil nil 'bottom)
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center t))
  )

;;;;; Highlight-indent-guides
;; Show indicator for indentation levels (like in VS Code)
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'column)
  (highlight-indent-guides-character ?⏐)
  :config
  ;; (set-face-attribute 'highlight-indent-guides-character-face nil :inherit 'org-block) ;:(background "#232635") ; Same as org-block background
  (highlight-indent-guides-auto-set-faces) ; Set faces based on theme
  )

;;;;; Rainbow-mode
;; Colorify color codes
(use-package rainbow-mode
  :hook ((text-mode prog-mode) . rainbow-mode)
  )

;;;;; Highlight-quoted
;; Make quoted symbols easier to distinguish from free variables by highlighting
;; them
(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode)
  )

;;;;; Smartparens
;; Autopairing parentheses
(use-package smartparens
  :hook (((sh-mode lua-mode text-mode) . smartparens-mode)
         (smartparens-mode . show-smartparens-mode)) ; Subtlely highlight matching parentheses
  :custom
  (sp-show-pair-from-inside t)
  :config
  (sp-pair "'" nil :actions :rem) ; Don't pair '
  )

;;;;; Paren
;; Highlight matching parentheses
(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode t)
  )

;;;;; Prettier
;; Re-formats code automatically for some languages. Dependent on the prettier
;; npm package
(use-package prettier
  :disabled t ; I don't quite know what this package does or how to use it
  :hook ((prog-mode text-mode) . global-prettier-mode)
  )

;;;; Syntax checking
;;;;; Flycheck
;; Check your code
(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit) ; Use load-path for Emacs session

  (flycheck-check-syntax-automatically '(save mode-enabled idle-change idle-buffer-switch)) ; When to check
  (flycheck-idle-buffer-switch-delay 1.5) ; Wait 2 secons after buffer switch
  (flycheck-buffer-switch-check-intermediate-buffers t) ; Run flycheck even if visiting buffer quickly (reliant on idle-buffer-switch)

  (flycheck-display-errors-delay 0.5) ; Time to show an error on point
  (flycheck-indication-mode 'right-margin)
  (flycheck-highlighting-mode 'lines)

  (flycheck-standard-error-navigation t) ; Use standard M-g n/p error navigation keybindings
  (flycheck-navigation-minimum-level nil)

  ;; Errors from other files
  (flycheck-relevant-error-other-file-show nil)
  (flycheck-relevant-error-other-file-minimum-level 'error)
  :config
  ;; Make the flycheck buffer occupy the bottom third of the screen
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33)))

  (kb/leader-keys
    "lf" '(flycheck-list-errors :which-key "List flycheck errors")
    )
  )

;;;;; Flycheck-pos-tip-mode
;; Shows flycheck errors in pos-tip popup
(use-package flycheck-pos-tip
  :after flycheck
  :hook (flycheck-mode . flycheck-pos-tip-mode)
  )

;;;;; Flychek-grammarly
;; Use grammarly API with flycheck
(use-package flycheck-grammarly
  :disabled t ; Broken in non-trivial files https://github.com/jcs-elpa/flycheck-grammarly/issues/3
  :straight (flycheck-grammarly :type git :host github :repo "jcs-elpa/flycheck-grammarly")
  :after flycheck
  :init (require 'grammarly)
  :custom
  ;; If you have a paid subscription
  (grammarly-username "")
  (grammarly-password "")

  (flycheck-grammarly-check-time 2)
  )

;;;;; Flychek-color-mode-line
;; Changes font color of modeline face based on flycheck status
(use-package flycheck-color-mode-line
  :disabled t ; Doesn't look pretty
  :after flycheck
  :hook (flycheck-mode . flycheck-color-mode-line-mode)
  )

;;;;; Flychec-status-emoji
;; Use emojis to display flycheck statuses
(use-package flycheck-status-emoji
  :after flycheck
  :hook (flycheck-mode . flycheck-status-emoji-mode)
  )

;;;;; Consult-flycheck
;; List flycheck errors in minibuffer with consult
(use-package consult-flycheck
  :after consult
  :hook (text-mode . (lambda ()
                       (kb/leader-keys
                         "le" '(consult-flycheck :which-key "Consult flycheck"))
                       ))
  )

;;;; IDE-like features
;;;;; Lsp-mode
;; More IDE features in Emacs
(use-package lsp-mode
  :disabled t ; Don't use for now and makes things a bit laggy
  :hook ((prog-mode . lsp-deferred) ; Lsp-mode only when buffer is visible
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-headerline-breadcrumb-mode))
  :commands (lsp lsp-deferred) ; Defer until either of these commands are run
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols))
  :config
  ;; Put flymake diagnostic buffer at the bottom third of the window
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flymake diagnostics for")
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33)))

  (general-define-key
   :keymaps 'lsp-mode-map
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
  )

;;;;; Lsp-ui
;; Fancy frame and sideline overlay which shows useful information about what's on the point.
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'top)
  )

;;;;; Lsp-ivy
;; Search through symbols with Ivy
(use-package lsp-ivy
  :config
  (kb/leader-keys
    "ls" 'lsp-ivy-workspace-symbol
    )
  )

;;; programming-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-general-rcp)
