;;; programming-projects-rcp.el --- Project management  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Everything to do with managing projects.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Project management
;;;;; Projectile
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

;;;;; Counsel-projectile
;; Use Ivy as projectile interface
(use-package counsel-projectile
  :after (counsel projectile)
  :ghook 'counsel-mode-hook
  )

;;;;; Project
(use-package project
  :general
  (:keymaps 'project-prefix-map
            "g" 'consult-git-grep
            "r" 'consult-ripgrep
            "R" 'project-query-replace-regexp
            "m" 'magit-project-status
            "a" 'project-any-command
            "o" 'kb/project-multi-occur)
  :custom
  (magit-bind-magit-project-status nil) ; Don't Automatically bind `magit-project-status' to `m' since I manually do it
  (project-find-functions '(kb/project-special-dir project-try-vc))
  (project-file-history-behavior 'relativize)
  ;; NOTE 2024-01-31: Replace via project-switch-commands to project-prefix-or-any-command
  ;; (project-switch-commands
  ;;  '((affe-find "Find file" "f")
  ;;    (consult-ripgrep "Regexp" "g")
  ;;    (magit-project-status "Magit")
  ;;    (project-switch-to-buffer "Buffer" "b")
  ;;    (project-query-replace-regexp "Replace regexp")
  ;;    (kb/project-multi-occur "Multi-occur" "o")
  ;;    (project-dired "Open dired")
  ;;    (project-find-dir "Open directory in dired")
  ;;    (project-compile "Compile")
  ;;    (project-eshell "Eshell")
  ;;    (project-shell "Shell")))
  (project-vc-merge-submodules nil) ; Consider submodules as their own projects?
  :init
  (defun kb/project-special-dir (dir)
    "Return project if DIR is noticed as special.
As directory is special if I've decided it is!"
    (require 'denote)
    (let ((projectp))
      (dolist (specialp (list (expand-file-name "papers" denote-directory)
                              (expand-file-name "buoy" denote-directory)))
        (when (file-in-directory-p dir specialp)
          (setq projectp t)))
      (when projectp (list 'vc 'Git dir))))

  ;; Inspired by `projectile-multi-occur'
  (defun kb/project-multi-occur (&optional nlines)
    "Do a `multi-occur' in the project's buffers.
With a prefix argument, show NLINES of context."
    (interactive "P")
    (let ((project (project-current)))
      (multi-occur (project-buffers project)
                   (car (occur-read-primary-args))
                   nlines)))
  :config
  ;; Found in Emacs News
  (setopt project-switch-commands #'project-prefix-or-any-command))

;;;;; Xref
(use-package xref
  :general ("C-M-?" 'xref-find-references-and-replace) ; Emacs 29.1
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

;;;;; Dumb-jump
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

;;;; Magit
;;;;; Itself
;; The best git interface. Mostly taken from Mostly taken from
;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#version-control

;; NOTE 2024-01-05: Temporarily update seq library to avoid dependency errors.
;; See https://github.com/progfolio/elpaca/issues/216
(elpaca '(seq :type git :host nil :repo "https://git.savannah.gnu.org/git/emacs/elpa.git" :branch "externals/seq")
  (progn (unload-feature 'seq t) (require 'seq)))

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

;;;;; Magit-log date headers
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

;;;;; Magit-lfs
(use-package magit-lfs
  :demand t
  :after magit
  )

;;;;; Forge
;; Support for git forges (e.g. GitLab and GitHub).
;; NOTE 2022-06-01: Make sure a github and/or gitlab token is stored in either
;; ~/.authinfo, ~/.authinfo.gpg, or ~/.netrc. See
;; https://magit.vc/manual/ghub/Storing-a-Token.html
(use-package forge
  :disabled                             ; FIXME 2024-02-05: Main breaks things
  :demand
  :after magit)

;;;;; Magit-todos
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

;;;;; Abdridge-diff
(use-package abridge-diff
  :after magit
  :diminish
  :init
  (abridge-diff-mode))

;;;;; Keychain-environment
(use-package keychain-environment
  ;; Ensure SSH_AGENT_PID and SSH_AUTH_SOCK are updated when committing since
  ;; their values may change (ever since I started using keychain in the Fish
  ;; shell to call ssh-agent). Requires the "keychain" package; sources
  ;; appropriate file in ~/.keychain/ to update environment variables
  :hook ((elpaca-after-init git-commit-post-finish) . keychain-refresh-environment))

;;;; VC
;;;;; Itself
(use-package vc
  :ensure nil
  :general (:keymaps 'vc-dir-mode-map
                     "G" 'vc-revert)
  :custom
  (vc-git-log-edit-summary-target-len 50)
  (vc-git-log-edit-summary-max-len 70)
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
      (4 'change-log-date))))
  (vc-annotate-display-mode 'scale)     ; Scale to oldest
  (add-log-mailing-address "krisbalintona@gmail.com")
  (add-log-keep-changes-together t)
  :config
  ;; Put in `global-mode-string'
  (or (member '(:eval vc-mode) global-mode-string)
      (push '(:eval vc-mode) global-mode-string))

  ;; Restore window configuration when when making commits with VC like you can
  ;; with org-agenda via the `org-agenda-restore-windows-after-quit' user option
  (defvar kb/vc-pre-window-conf nil)

  (defun kb/vc-set-window-conf (&rest _)
    "Set the value of `kb/vc-pre-window-conf'."
    (setq kb/vc-pre-window-conf (current-window-configuration)))
  (advice-add 'vc-next-action :before #'kb/vc-set-window-conf)

  (defun kb/vc-restore-window-conf (&rest _)
    "Set the value of `kb/vc-pre-window-conf'."
    (set-window-configuration kb/vc-pre-window-conf)
    (setq kb/vc-pre-window-conf nil))
  (advice-add 'log-edit-done :after #'kb/vc-restore-window-conf)
  (advice-add 'log-edit-kill-buffer :after #'kb/vc-restore-window-conf))

;;;;; Log-edit
(use-package log-edit
  :ensure nil
  :general (:keymaps 'log-edit-mode-map
                     [remap log-edit-comment-search-backward] 'consult-history)
  :custom
  (log-edit-setup-add-author nil)
  :config
  ;; I can see the files from the Diff with C-c C-d
  (remove-hook 'log-edit-hook #'log-edit-show-files))

;;;;; Diff-mode
(use-package diff-mode
  :ensure nil
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
  (diff-font-lock-prettify t)
  (diff-refine 'navigation)             ; Font lock hunk when it is navigated to
  (diff-font-lock-syntax 'hunk-also) ; Fontify diffs with syntax highlighting of the language
  :config
  (set-face-attribute 'diff-header nil :inherit 'modus-themes-prominent-note))

;;;;; Ediff
(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain) ; Keep everything in the same frame
  (ediff-highlight-all-diffs nil))      ; Only highlight currently selected diff

;;;;; Vc-msg
;; See a line's corresponding commit information (like git blame)
(use-package vc-msg
  :general ("H-v" 'vc-msg-show))

;;;; QoL
;;;;; Git-gutter
(use-package git-gutter
  :disabled                          ; NOTE 2022-12-29: Trying `diff-hl' for now
  :hook ((window-configuration-change . git-gutter:update-all-windows)
         (prog-mode . git-gutter-mode))
  :custom
  (git-gutter-fr:side 'left-fringe)
  ;; 0 is actually on-save, so we put this as low as possible to effectively
  ;; have real-time updating
  (git-gutter:update-interval 0.02)
  ;; (git-gutter:disabled-modes '(org-mode)) ; Using this to disable in modes yields annoying echo messages
  )

;;;;; Git-gutter-fringe
(use-package git-gutter-fringe
  :after git-gutter
  :custom-face
  ;; Colors taken from `uninspiring-dark-theme'
  (git-gutter-fr:added ((t (:foreground "#98C379" :weight bold :inherit nil))))
  (git-gutter-fr:deleted ((t (:foreground "#E06C75" :weight bold :inherit nil))))
  (git-gutter-fr:modified ((t (:foreground "#D19A66" :weight bold :inherit nil))))
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(top t))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(top t))
  (define-fringe-bitmap 'git-gutter-fr:deleted [240 240 240 240] nil nil '(top t)))

;;;;; Diff-hl
;; Diff information in the margins. Also provides commands for navigating and
;; viewing diff info of the current file. Faster and cleaner than git-gutter and
;; git-gutter-fringes
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (vc-dir-mode . diff-hl-dir-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-draw-borders nil)
  (diff-hl-show-staged-changes nil)
  (diff-hl-update-async t)
  (diff-hl-side 'right)
  (diff-hl-flydiff-delay 0.5)           ; See `diff-hl-flydiff-mode'
  :config
  (global-diff-hl-show-hunk-mouse-mode))

;;;;; Git-timemachine
;; Enable in current buffer to iterate through git revision history
(use-package git-timemachine)

;;;;; Better Magic status
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

;;;;; Deadgrep
;; Grep but with a convenient magit-like interface (with visibility toggles)
(use-package deadgrep
  :general ("<f6>" 'deadgrep))

(provide 'programming-projects-rcp)
;;; programming-projects-rcp.el ends here
