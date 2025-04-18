;;; Built-in
;;;; VC
(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  ;; Improves performance by not having to check for other backends. Expand this
  ;; list when necessary
  (vc-handled-backends '(Git))
  (vc-revert-show-diff t)
  (vc-annotate-display-mode 'fullscale)
  (vc-find-revision-no-save t)
  (vc-allow-rewriting-published-history 'ask)) ; Emacs 31

;;;; Vc-dir
(use-package vc-dir ; NOTE 2024-10-19: Is not required by vc, so have its own use-package
  :ensure nil
  :bind ( :map vc-dir-mode-map
          ("G" . vc-revert)))

;;;; Vc-git
(use-package vc-git
  :ensure nil
  :hook (vc-git-log-edit-mode . auto-fill-mode)
  :bind ( :map vc-git-log-edit-mode-map
          ("<tab>" . completion-at-point))
  :custom
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
  (vc-git-log-edit-summary-target-len (+ 50 (length "Summary")))
  (vc-git-log-edit-summary-max-len (+ 70 (length "Summary")))
  (vc-git-revision-complete-only-branches t))

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

  (magit-format-file-function #'magit-format-file-nerd-icons) ; Fancy file icons


  ;; Refinement (diffs) in hunks
  (magit-diff-refine-hunk t)
  (magit-diff-highlight-hunk-body t)
  (magit-diff-highlight-hunk-region-functions
   '(magit-diff-highlight-hunk-region-using-overlays
     ;; magit-diff-highlight-hunk-region-dim-outside
     magit-diff-highlight-hunk-region-using-face))

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
  (magit-auto-revert-mode 1)

  ;; Add dates to magit-logs.  Taken from
  ;; https://github.com/alphapapa/unpackaged.el?tab=readme-ov-file#magit-log-date-headers
  (defun krisb-magit-log--add-date-headers (&rest _ignore)
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
  ;; Use the above function appropriately
  (add-hook 'magit-post-refresh-hook #'krisb-magit-log--add-date-headers)
  (advice-add 'magit-setup-buffer-internal :after #'krisb-magit-log--add-date-headers))

;;;; Forge
;; Support for git forges (e.g. GitLab and GitHub).
;; NOTE 2022-06-01: Make sure a github and/or gitlab token is stored in either
;; ~/.authinfo, ~/.authinfo.gpg, or ~/.netrc. See
;; https://magit.vc/manual/ghub/Storing-a-Token.html
(use-package forge
  :hook (forge-issue-mode . visual-line-mode)
  :custom
  (forge-owned-accounts '(("krisbalintona" . nil)))
  :config
  ;; I don't know why the hook definition enables flyspell-mode...
  (remove-hook 'forge-post-mode-hook #'turn-on-flyspell))

;;; Keychain-environment
;; Ensure SSH_AGENT_PID and SSH_AUTH_SOCK are updated before committing since
;; their values may change. Sources them to ~/.keychain/
(use-package keychain-environment
  ;; For AUR:
  ;; :ensure-system-package keychain
  )

;;; Epg-config
;; Epg-config is responsible for querying passphrases
(use-package epg-config
  :ensure nil
  :custom
  (epg-pinentry-mode 'loopback)) ; Ask through the minibuffer instead of external Pinentry program

;;; Pinentry
;; (Discovered from angrybacon's dotemacs:
;; https://github.com/angrybacon/dotemacs.)  Start a pinentry service
;; automatically in order for Emacs to be able to prompt passphrases from the
;; minibuffer.  If Emacs doesn't redirect prompts regardless of the value for
;; `epg-pinentry-mode', add "allow-emacs-pinentry" to ~/.gnupg/gpg-agent.conf.
(use-package pinentry
  :disabled t                   ; 2024-11-01: Not sure what this package does...
  :config
  (pinentry-start))

;;; Diff-mode
(use-package diff-mode
  :ensure nil
  :hook (diff-mode . diff-delete-empty-files)
  :bind ( :map diff-mode-map
          ("L" . vc-print-root-log)
          ("v" . vc-next-action))
  :custom
  (diff-default-read-only t)
  (diff-font-lock-prettify t)
  (diff-refine 'font-lock)
  (diff-font-lock-syntax t)
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

;;; Ediff
(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain) ; Keep everything in the same frame
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-highlight-all-diffs nil))      ; Only highlight currently selected diff

;;; Vc-jj
;; Best jj integration with vc currently (2025-03-13).
(use-package vc-jj
  :pin gnu-elpa-devel
  :config
  ;; Project integration with JJ
  (with-eval-after-load 'project
    (require 'project-jj)))

;;; Git-share
;; Share a web URL to the commit responsible for the change at point or the
;; remote version of the file at point.
(use-package git-share
  :vc (:url "https://github.com/mgmarlow/git-share")
  :custom
  (git-share-open-links-in-browser nil))

;;; Provide
(provide 'krisb-vc)
