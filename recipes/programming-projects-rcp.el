;;; programming-projects-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Everything to do with managing projects.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Project management
;;;; Projectile
;; Navigate and manage project directories easier
(use-package projectile
  :disabled t ; In favor of `project.el'
  :general
  (kb/general-keys
   "p" '(:ignore t :wk "Projectile")
   "p?" '(hydra:selectrum-projectile/body :wk "Help menu")
   ;; "pf"  'projectile-find-file
   "pp"  'projectile-switch-project
   ;; "ps"  'counsel-projectile-rg
   "pb"  'projectile-switch-to-buffer
   "pD"  'projectile-dired
   ;; "pc"  'projectile-compile-project
   )
  :custom
  (projectile-completion-system 'default) ; Use selectrum
  (projectile-enable-caching t)
  (projectile-track-known-projects-automatically nil) ; Don't create projects automatically
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
  :config
  (projectile-mode)

  (when (file-directory-p user-emacs-directory)
    (setq projectile-project-search-path `(,user-emacs-directory)))
  (setq projectile-switch-project-action #'projectile-dired)
  )

;;;; Counsel-projectile
;; Use Ivy as projectile interface
(use-package counsel-projectile
  :after (counsel projectile)
  :ghook 'counsel-mode-hook
  )

;;;; Project
(use-package project
  :general
  ([remap project-find-regexp] 'consult-ripgrep)
  (:keymaps 'project-prefix-map
   "m" #'magit-project-status)
  :custom
  (magit-bind-magit-project-status nil) ; Don't Automatically bind `magit-project-status' to `m' since I manually do it
  (project-switch-commands
   '((affe-find "Find file" "f")
     (consult-ripgrep "Regexp" "g")
     (magit-project-status "Magit")
     (project-vc-dir "VC-git")
     (project-switch-to-buffer "Buffer" "b")
     (project-query-replace-regexp "Replace regexp")
     (project-dired "Open dired")
     (project-find-dir "Open directory in dired")
     (project-compile "Compile")
     (project-eshell "Eshell")
     (project-shell "Shell")))
  (project-vc-merge-submodules nil)) ; Consider submodules as their own projects?

;;;; Xref
(use-package xref
  :general ("H-?" 'xref-find-references-and-replace) ; Emacs 29.1
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  (xref-show-xrefs-function #'xref-show-definitions-buffer)
  (xref-file-name-display 'project-relative)
  (xref-search-program 'ripgrep)
  :config
  (with-eval-after-load 'consult
    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)))

;;;; Dumb-jump
(use-package dumb-jump
  :custom
  (dumb-jump-quiet nil)
  (dumb-jump-default-project (file-name-concat user-emacs-directory "recipes/"))
  ;; See https://github.com/jacktasia/dumb-jump#configuration to see how it
  ;; behaves
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-rg-search-args "--hidden --glob '!.git' --pcre2")
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 95)) ; Last

;;; Magit
;;;; Itself
;; The best git interface. Mostly taken from Mostly taken from
;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#version-control
(use-package magit
  :hook ((magit-diff-mode magit-process-mode) . visual-line-mode)
  :general
  (:keymaps 'magit-mode-map
   "C-<tab>" 'magit-section-toggle-children)
  :custom
  ;; How opened magit buffers (e.g. commit) are shown
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)

  ;; How opened magit buffers are closed
  (magit-bury-buffer-function 'magit-restore-window-configuration) ; Restore the window configuration used before calling magit when closing it

  ;; Displaying hunks
  (magit-diff-highlight-hunk-body t)
  (magit-diff-highlight-hunk-region-functions
   '(magit-diff-highlight-hunk-region-dim-outside
     magit-diff-highlight-hunk-region-using-overlays
     magit-diff-highlight-hunk-region-using-face
     ))

  ;; Sections
  (magit-module-sections-nested t)
  (magit-section-show-child-count t)
  (magit-refs-show-commit-count 'all)   ; Show branches and tags
  (magit-section-initial-visibility-alist '((modules . hide) ; Modules overview
                                            (stashes . show)
                                            (unpulled . show)
                                            (unpushed . show)
                                            ))
  (transient-mode-line-format nil)

  ;; Save transient changes to a different location (since I changed the
  ;; transient-level for certain magit transient popups)
  (transient-history-file (concat no-littering-var-directory "transient/history.el"))
  (transient-values-file (concat no-littering-var-directory "transient/values.el"))
  (transient-levels-file (concat no-littering-var-directory "transient/levels.el"))

  ;; Performance-related variables
  (magit-refresh-status-buffer t)       ; Change to nil as last resort
  (magit-refresh-verbose nil) ; Help troubleshoot bottlenecks (check `*Messages*' buffer)?
  ;; Prefer these to be nil for performance boosts
  (magit-diff-highlight-indentation t) ; Highlight wrong indentation?
  (magit-diff-highlight-trailing t)    ; Highlight trailing spaces?
  (magit-diff-paint-whitespace nil)    ; Where to highlight whitespace errors?
  (magit-diff-highlight-hunk-body t)   ; Highlight hunks?
  (magit-diff-refine-hunk t)           ; Extra-highlight word-level differences?

  ;; Removes functions ran in `magit-status-sections-hook'. Can also improve
  ;; performance. Use `magit-refresh-verbose' to diagnose which of these should
  ;; be removed.
  (magit-disabled-section-inserters
   '(magit-insert-upstream-branch-header
     ;; magit-insert-tags-header
     ;; magit-insert-status-headers
     ;; magit-insert-unpushed-to-pushremote
     ;; magit-insert-unpushed-to-upstream-or-recent
     ;; magit-insert-unpulled-from-upstream
     ;; magit-insert-unpulled-from-pushremote
     ;; magit-insert-push-branch-header
     ))
  :preface
  ;; NOTE 2022-06-01: Set this to `nil' before `magit' is loaded so when `forge'
  ;; is loaded it does not add keybinds which conflict with `evil-collection'
  (when (featurep 'evil)
    (setq forge-add-default-bindings nil))
  :config
  ;; NOTE 2021-08-20: Provides useful functionality, such as `magit-project-status'
  (require 'magit-extras)               ; Load the remaining magit libraries

  (magit-auto-revert-mode)

  (when (bound-and-true-p evil-local-mode)
    (evil-set-initial-state 'git-commit-mode 'insert))

  ;; Adds hooks to `magit-status-sections-hook'. Should be a separate call for
  ;; each.
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-overview
                          'magit-insert-staged-changes t))

;;;; Magit-log date headers
(with-eval-after-load 'magit
  ;; Add dates to magit-logs
  (use-package ov :demand) ; Dependency

  (defun unpackaged/magit-log--add-date-headers (&rest _ignore)
    "Add date headers to Magit log buffers."
    (when (derived-mode-p 'magit-log-mode)
      (save-excursion
        (ov-clear 'date-header t)
        (goto-char (point-min))
        (cl-loop with last-age
                 for this-age = (-some--> (ov-in 'before-string 'any (line-beginning-position) (line-end-position))
                                  car
                                  (overlay-get it 'before-string)
                                  (get-text-property 0 'display it)
                                  cadr
                                  (s-match (rx (group (1+ digit) ; number
                                                      " "
                                                      (1+ (not blank))) ; unit
                                               (1+ blank) eos)
                                           it)
                                  cadr)
                 do (when (and this-age
                               (not (equal this-age last-age)))
                      (ov (line-beginning-position) (line-beginning-position)
                          'after-string (propertize (concat " " this-age "\n")
                                                    'face 'magit-section-heading)
                          'date-header t)
                      (setq last-age this-age))
                 do (forward-line 1)
                 until (eobp)))))
  (define-minor-mode unpackaged/magit-log-date-headers-mode
    "Display date/time headers in `magit-log' buffers."
    :global t
    (if unpackaged/magit-log-date-headers-mode
        (progn
          ;; Enable mode
          (add-hook 'magit-post-refresh-hook #'unpackaged/magit-log--add-date-headers)
          (advice-add #'magit-setup-buffer-internal :after #'unpackaged/magit-log--add-date-headers))
      ;; Disable mode
      (remove-hook 'magit-post-refresh-hook #'unpackaged/magit-log--add-date-headers)
      (advice-remove #'magit-setup-buffer-internal #'unpackaged/magit-log--add-date-headers)))
  (add-hook 'magit-mode-hook #'unpackaged/magit-log-date-headers-mode) ; Enable the minor mode
  )

;;;; Magit-lfs
(use-package magit-lfs
  :demand t
  :after magit
  )

;;;; Forge
;; Support for git forges (e.g. GitLab and GitHub). NOTE 2022-06-01: Make sure a
;; github and/or gitlab token is stored in either ~/.authinfo, ~/.authinfo.gpg,
;; or ~/.netrc. See https://magit.vc/manual/ghub/Storing-a-Token.html
(use-package forge
  :demand
  :after magit)

;;;; Magit-todos
(use-package magit-todos
  :disabled t                           ; Not very useful for now
  :after magit
  :custom
  (magit-todos-item-cache t)
  (magit-todos-keywords 'alt-comment-dwim-keyword-faces)
  (magit-todos-insert-at 'bottom)
  (magit-todos-keyword-suffix (rx " ")) ; Match the type of comments I make
  (magit-todos-branch-list 'branch)
  (magit-todos-branch-list-merge-base-ref (magit-main-branch))
  (magit-todos-auto-group-items 10)
  (magit-todos-group-by
   '(magit-todos-item-keyword
     magit-todos-item-filename
     ))
  (magit-todos-max-items 5)
  (magit-todos-sort-order
   '(magit-todos--sort-by-keyword
     magit-todos--sort-by-filename
     magit-todos--sort-by-position
     ))
  :init
  (magit-todos-mode)
  )

;;;; Abdridge-diff
(use-package abridge-diff
  :after magit
  :diminish
  :init
  (abridge-diff-mode))

;;; VC
;;;; Itself
(use-package vc
  :elpaca nil
  :general (:keymaps 'vc-dir-mode-map
            "G" 'vc-revert)
  :custom
  (vc-git-log-edit-summary-max-len 70)
  (vc-git-log-edit-summary-target-len 50)
  (vc-git-diff-switches              ; Have diff headers look similar to Magit's
   '("--patch-with-stat" "--histogram"))
  (vc-git-root-log-format               ; Taken from Prot
   `("%d %h %ai %an: %s"
     ;; The first shy group matches the characters drawn by --graph.
     ;; We use numbered groups because `log-view-message-re' wants the
     ;; revision number to be group 1.
     ,(concat "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?"
              "\\(?2: ([^)]+) \\)?\\(?1:[0-9a-z]+\\) "
              "\\(?4:[0-9]\\{4\\}-[0-9-]\\{4\\}[0-9\s+:-]\\{16\\}\\) "
              "\\(?3:.*?\\):")
     ((1 'log-view-message)
      (2 'change-log-list nil lax)
      (3 'change-log-name)
      (4 'change-log-date)))))

;;;; Log-edit
(use-package log-edit
  :elpaca nil
  :general (:keymaps 'log-edit-mode-map
            [remap log-edit-comment-search-backward] 'consult-history))

;;;; Diff-mode
(use-package diff-mode
  :elpaca nil
  :gfhook
  'outshine-mode
  '(lambda ()                  ; FIXME 2022-12-30: Not sure why this doesn't work...
           (display-line-numbers-mode -1))
  :general (:keymaps 'diff-mode-map
            "S-<iso-lefttab>" 'outshine-cycle-buffer
            "<tab>" 'outshine-cycle
            "C-x n s" 'outshine-narrow-to-subtree
            "L" 'vc-print-root-log
            "v" 'vc-next-action)
  :custom
  (diff-refine 'navigation) ; FIXME 2022-12-30: Now exactly sure what this does...
  (diff-font-lock-syntax 'hunk-also)) ; Fontify diffs with syntax highlighting of the language

;;;; Ediff
(use-package ediff
  :elpaca nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain) ; Keep everything in the same frame
  (ediff-highlight-all-diffs nil))      ; Only highlight currently selected diff

;;; QoL
;;;; Git-gutter
(use-package git-gutter
  :hook ((window-configuration-change . git-gutter:update-all-windows)
         ;; (prog-mode . git-gutter-mode) ; NOTE 2022-12-29: Trying `diff-hl' for now
         )
  :custom
  (git-gutter-fr:side 'left-fringe)
  ;; 0 is actually on-save, so we put this as low as possible to effectively
  ;; have real-time updating
  (git-gutter:update-interval 0.02)
  ;; (git-gutter:disabled-modes '(org-mode)) ; Using this to disable in modes yields annoying echo messages
  )

;;;; Git-gutter-fringe
(use-package git-gutter-fringe
  :after git-gutter
  :custom-face
  ;; Colors taken from `uninspiring-dark-theme'
  (git-gutter-fr:added ((t (:foreground "#98C379" :weight bold :inherit nil))))
  (git-gutter-fr:deleted ((t (:foreground "#E06C75" :weight bold :inherit nil))))
  (git-gutter-fr:modified ((t (:foreground "#D19A66" :weight bold :inherit nil))))
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(top t))
  (define-fringe-bitmap 'git-gutter-fr:deleted [240 240 240 240] nil nil '(top t))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(top t)))

;;;; Diff-hl
;; Diff information in the margins. Also provides commands for navigating and
;; viewing diff info of the current file
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         ;; Magit integration
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-draw-borders nil)
  (diff-hl-show-staged-changes nil))

;;;; Git-timemachine
;; Enable in current buffer to iterate through git revision history
(use-package git-timemachine)

;;;; Better Magic status
;;;###autoload
(defun unpackaged/magit-status ()
  "Open a `magit-status' buffer and close the other window so only Magit is visible.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
  (interactive)
  (let* ((buffer-file-path (when buffer-file-name
                             (file-relative-name buffer-file-name
                                                 (locate-dominating-file buffer-file-name ".git"))))
         (section-ident `((file . ,buffer-file-path) (unstaged) (status))))
    (call-interactively #'magit-status)
    (delete-other-windows)
    (when buffer-file-path
      (goto-char (point-min))
      (cl-loop until (when (equal section-ident (magit-section-ident (magit-current-section)))
                       (magit-section-show (magit-current-section))
                       (recenter)
                       t)
               do (condition-case nil
                      (magit-section-forward)
                    (error (cl-return (magit-status-goto-initial-section))))
               ))
    ))
(general-define-key [remap magit-status] #'unpackaged/magit-status)

;;;; Deadgrep
;; Grep but with a convenient magit-like interface (with visibility toggles)
(use-package deadgrep
  :general ("<f1>" 'deadgrep)
  )

;;; programming-projects-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-projects-rcp)
