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
  (kb/project-keys
    "f" '(affe-find :wk "Project find file")
    "r" '(consult-ripgrep :wk "Consult rg")
    "b" '(project-switch-to-buffer :wk "Project switch to buffer")
    "p" '(project-switch-project :wk "Project.el switch project")
    "d" '(project-dired :wk "Project dired")
    )
  (:keymaps 'project-prefix-map
            "m" #'magit-project-status)
  :custom
  (magit-bind-magit-project-status nil) ; Don't Automatically bind `magit-project-status' to `m' since I manually do it
  (project-switch-commands
   '((affe-find "Find file" "f")
     (consult-ripgrep "Regexp" "r")
     (magit-project-status "Magit")
     (project-switch-to-buffer "Buffer" "b")
     (project-find-regexp "Xref regexp")
     (project-dired "Open dired")
     (project-query-replace-regexp "Replace regexp" "R")
     (project-find-dir "Open directory in dired")
     (project-eshell "Eshell")
     ))

  (project-vc-merge-submodules nil)  ; Consider submodules as their own projects
  )

;;;; Project-x
;; Companion to project.el. Saves window configurations for projects.
(use-package project-x
  :straight (project-x :type git :host github :repo "karthink/project-x")
  :hook (after-init . project-x-mode)   ; Adds hooks and keybinds
  :general (kb/project-keys
             "w" '(project-x-window-state-save :wk "Project-x save")
             "j" '(project-x-window-state-load :wk "Project-x load")
             )
  :custom
  (project-x-window-list-file
   (no-littering-expand-var-file-name "project-x/project-window-list.el"))
  (project-x-save-interval nil)         ; I'll save myself
  (project-x-local-identifier ".project") ; File name(s) which indicate that a directory is a project
  )

;;;; Xref
(use-package xref
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  (xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
  (xref-file-name-display 'project-relative)
  (xref-search-program 'ripgrep)
  )

;;; Version control
;;;; Magit
;; The best git interface. Mostly taken from Mostly taken from
;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#version-control
(use-package magit
  :straight (magit :type git :host github :repo "magit/magit")
  :hook ((magit-diff-mode magit-process-mode) . visual-line-mode)
  :general
  (:keymaps 'magit-mode-map
            "C-<tab>" 'magit-section-toggle-children)
  (:keymaps 'magit-mode-map
            :states '(normal visual motion)
            "D" '(magit-cherry-donate :wk "Cherry conate"))
  (kb/magit-keys
    "F"  'magit-fetch-all
    "P"  '(magit-push-current :wk "Push")
    "b"  'magit-branch
    "c"  'magit-branch-or-checkout
    "d"  'magit-diff-unstaged
    "f"  'magit-fetch
    "g"  '(magit-status :wk "Status")
    "l"   '(:ignore t :wk "Logs")
    "lc" 'magit-log-current
    "lf" 'magit-log-buffer-file
    "p"  'magit-pull-branch
    "r"  '(magit-rebase :wk "Rebase")
    "s"  '(magit-status :wk "Status")
    )
  :custom
  ;; How opened magit buffers (e.g. commit) are shown
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)

  ;; How opened magit buffers are closed
  (magit-bury-buffer-function 'magit-restore-window-configuration) ; Restore the window configuration used before calling magit when closing it

  ;; Displaying hunks
  ;; (magit-diff-highlight-hunk-body nil)
  (magit-diff-highlight-hunk-body t)
  (magit-diff-highlight-hunk-region-functions
   '(magit-diff-highlight-hunk-region-dim-outside
     magit-diff-highlight-hunk-region-using-overlays
     magit-diff-highlight-hunk-region-using-face
     ))

  ;; Sections
  (magit-module-sections-nested t)
  (magit-section-show-child-count t)
  (magit-refs-show-commit-count 'all) ; Show branches and tags
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
  (magit-refresh-status-buffer t) ; Change to nil as last resort
  (magit-refresh-verbose nil) ; Help troubleshoot bottlenecks (check `*Messages*' buffer)?
  ;; Prefer these to be nil for performance boosts
  (magit-diff-highlight-indentation t) ; Highlight wrong indentation?
  (magit-diff-highlight-trailing t) ; Highlight trailing spaces?
  (magit-diff-paint-whitespace nil) ; Where to highlight whitespace errors?
  (magit-diff-highlight-hunk-body t) ; Highlight hunks?
  (magit-diff-refine-hunk t) ; Extra-highlight word-level differences?

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
  :config
  (evil-set-initial-state 'git-commit-mode 'insert)

  ;; NOTE 2021-08-20: Provides useful functionality, such as `magit-project-status'
  (require 'magit-extras) ; Load the remaining magit libraries

  ;; Adds hooks to `magit-status-sections-hook'. Should be a separate call for
  ;; each.
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-overview
                          'magit-insert-staged-changes t)
  )

;;;; Magit-log date headers
(with-eval-after-load 'magit
  ;; Add dates to magit-logs
  (straight-use-package 'ov) ; Dependency

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

;;;; Forge
;; Support for git forges (e.g. GitLab and GitHub).
(use-package forge
  :after magit
  )

;;;; Ediff
(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain) ; Keep everything in the same frame
  (ediff-highlight-all-diffs nil) ; Only highlight currently selected diff
  )

;;;; Magit-todos
(use-package magit-todos
  :after magit
  :custom
  (magit-todos-item-cache t)
  (magit-todos-keywords 'kb/comment-keyword-faces)
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

;;; QoL
;;;; Git-gutter-fringes
;; Indicate diff areas in fringe
(use-package git-gutter-fringe
  :demand t ; Need or this won't load and set bitmaps correctly
  :hook (window-configuration-change . git-gutter:update-all-windows)
  :custom
  (left-fringe-width 16)
  (right-fringe-width 3)
  (git-gutter-fr:side 'left-fringe)
  (git-gutter:update-interval 0)
  (git-gutter:disabled-modes '(org-mode))
  :config
  (global-git-gutter-mode)

  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(top t))
  (define-fringe-bitmap 'git-gutter-fr:deleted [240 240 240 240] nil nil '(top t))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(top t))
  )

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
                    (error (cl-return (magit-status-goto-initial-section-1))))
               ))
    ))
(general-define-key [remap magit-status] #'unpackaged/magit-status)

;;; programming-projects-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-projects-rcp)
