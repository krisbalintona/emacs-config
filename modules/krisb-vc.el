;;; Built-in
;;;; VC
(use-package vc
  :ensure nil
  :hook (vc-git-log-edit-mode . auto-fill-mode)
  :bind ( :map vc-git-log-edit-mode-map
          ("<tab>" . completion-at-point))
  :custom
  (vc-follow-symlinks t)
  ;; Improves performance by not having to check for other backends. Expand this
  ;; list when necessary
  (vc-handled-backends '(Git))

  (vc-git-diff-switches              ; Have diff headers look similar to Magit's
   '("--patch-with-stat" "--histogram"))
  (vc-git-root-log-format
   `("%h %ad (%ar) %aN%d%n  %s"
     ;; The first shy group matches the characters drawn by --graph. We use
     ;; numbered groups because `log-view-message-re' wants the revision number
     ;; to be group 1.
     ,(concat "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?"
              "\\(?1:[0-9a-z]+\\)"      ; %h
              " "
              "\\(?4:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} (.*? ago)\\)?" ; %ad (%ar)
              " "
              "\\(?3:\\(?:[[:alpha:]]+\\.?[\n ]\\)+\\)" ; %aN
              "\\(?2:([^)]+)\\)?")                      ; %d
     ((1 'log-view-message)
      (2 'change-log-list nil lax)
      (3 'change-log-name)
      (4 'change-log-date))))
  (vc-revert-show-diff t)

  (vc-annotate-display-mode 'fullscale)

  (vc-find-revision-no-save t)

  (vc-git-log-edit-summary-target-len (+ 50 (length "Summary")))
  (vc-git-log-edit-summary-max-len (+ 70 (length "Summary"))))

;;;; Vc-dir
(use-package vc-dir ; NOTE 2024-10-19: Is not required by vc, so have its own use-package
  :ensure nil
  :bind ( :map vc-dir-mode-map
          ("G" . vc-revert)))

;;;; Log-edit
(use-package log-edit
  :ensure nil
  :custom
  (log-edit-headers-alist
   '(("Summary" . log-edit-summary)
     ("Fixes")
     ("Author")))
  (log-edit-setup-add-author nil)
  :custom-face
  (log-edit-summary ((t (:family ,(face-attribute 'variable-pitch :family)))))
  :config
  ;; I can see the files from the Diff with C-c C-d when I want
  (remove-hook 'log-edit-hook #'log-edit-show-files))

;;;; Diff-mode
(use-package diff-mode
  :ensure nil
  :hook (diff-mode . diff-delete-empty-files)

  :bind ( :map diff-mode-map
          ("S-<iso-lefttab>" . outshine-cycle-buffer)
          ("<tab>" . outshine-cycle)
          ("C-x n s" . outshine-narrow-to-subtree)
          ("L" . vc-print-root-log)
          ("v" . vc-next-action))
  :custom
  (diff-font-lock-prettify t)
  (diff-refine 'font-lock)
  (diff-font-lock-syntax 'hunk-also) ; Fontify diffs with syntax highlighting of the language
  :config
  (krisb-modus-themes-setup-faces
   "diff-mode"
   (set-face-attribute 'diff-header nil
                       :height 1.2
                       :overline t
                       :width 'expanded
                       :foreground (modus-themes-with-colors fg-alt)
                       :extend t)
   (set-face-attribute 'diff-hunk-header nil
                       :height 1.1
                       :slant 'italic
                       :foreground 'unspecified
                       :background (modus-themes-with-colors bg-dim))))

;;;; Agitate
;; QoL stuff for built-in VC workflow
(use-package agitate
  :after vc
  :demand t
  :hook (diff-mode . agitate-diff-enable-outline-minor-mode)
  :bind ( :map vc-prefix-map
          ("=" . agitate-diff-buffer-or-file)
          ("f" . agitate-vc-git-find-revision)
          ("s" . agitate-vc-git-show)
          ("w" . agitate-vc-git-kill-commit-message)
          ("p p" . agitate-vc-git-format-patch-single)
          ("p n" . agitate-vc-git-format-patch-n-from-head)
          :map diff-mode-map
          ([remap diff-refine-hunk] . agitate-diff-refine-cycle)
          ([remap diff-restrict-view] . agitate-diff-narrow-dwim)
          :map log-view-mode-map
          ("w" . agitate-log-view-kill-revision)
          ("W" . agitate-log-view-kill-revision-expanded)
          :map vc-git-log-view-mode-map
          ("c" . agitate-vc-git-format-patch-single)
          :map log-edit-mode-map
          ("C-c C-i C-n" . agitate-log-edit-insert-file-name)
          ;; See user options `agitate-log-edit-emoji-collection' and
          ;; `agitate-log-edit-conventional-commits-collection'.
          ("C-c C-i C-e" . agitate-log-edit-emoji-commit)
          ("C-c C-i C-c" . agitate-log-edit-conventional-commit))
  :custom
  (diff-refine nil)                     ; We use `agitate-diff-refine-cycle' now
  (agitate-log-edit-informative-show-root-log nil)
  (agitate-log-edit-informative-show-files t)
  :config
  (agitate-log-edit-informative-mode 1)

  (with-eval-after-load 'vc-git
    (advice-add #'vc-git-push :override #'agitate-vc-git-push-prompt-for-remote)))

;;; Magit
;;;; Magit
(use-package magit
  :hook ((magit-diff-mode magit-process-mode) . visual-line-mode)
  :bind (("C-x g" . magit)
         :map magit-mode-map
         ("C-<tab>". magit-section-toggle-children)
         :map git-commit-mode-map
         ("<tab>" . completion-at-point))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration)

  (magit-refs-show-commit-count 'all)   ; Show branches and tags

  ;; Refinement in hunks
  (magit-diff-highlight-hunk-body t)
  (magit-diff-highlight-hunk-region-functions
   '(magit-diff-highlight-hunk-region-dim-outside
     magit-diff-highlight-hunk-region-using-overlays
     magit-diff-highlight-hunk-region-using-face
     ))

  ;; Sections
  (magit-module-sections-nested t)
  (magit-section-show-child-count t)
  (magit-section-initial-visibility-alist '((stashes . show)
                                            (modules . hide)
                                            (unpushed . show)
                                            (unpulled . hide)))
  :custom-face
  (git-commit-summary ((t (:family ,(face-attribute 'variable-pitch :family)))))
  :config
  (magit-auto-revert-mode 1))

;;; Keychain-environment
;; Ensure SSH_AGENT_PID and SSH_AUTH_SOCK are updated before committing since
;; their values may change. Sources them to ~/.keychain/
(use-package keychain-environment
  :ensure-system-package keychain
  :hook ((after-init vc-before-checkin git-commit-setup) . keychain-refresh-environment))

;;; Provide
(provide 'krisb-vc)
