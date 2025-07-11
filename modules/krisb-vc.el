;; -*- lexical-binding: t; -*-

;;; Built-in

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

;;; Git-share
;; Share a web URL to the commit responsible for the change at point or the
;; remote version of the file at point.
(use-package git-share
  :vc (:url "https://github.com/mgmarlow/git-share")
  :custom
  (git-share-open-links-in-browser nil))

;;; Provide
(provide 'krisb-vc)
