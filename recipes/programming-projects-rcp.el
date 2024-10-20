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

;;;;; Project
(use-package project
  :bind ( :map project-prefix-map
          ("g". consult-git-grep)
          ("r". consult-ripgrep)
          ("R". project-query-replace-regexp)
          ("a". project-any-command))
  :custom
  (magit-bind-magit-project-status nil) ; Don't Automatically bind `magit-project-status' to `m' since I manually do it
  (project-find-functions '(kb/project-special-dir project-try-vc))
  (project-switch-commands #'project-prefix-or-any-command)
  ;; NOTE 2024-01-31: Prefer `project-switch-commands' st to
  ;; `project-prefix-or-any-command'
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
  :config
  ;; This is a regular variable
  (setq project-mode-line-format
        '(:eval (when-let ((project (project-mode-line-format)))
                  (propertize (concat "[" (string-trim (format-mode-line project)) "] ")
                              'face project-mode-line-face))))

  (defun kb/project-special-dir (dir)
    "Return project if DIR is noticed as special.
As directory is special if I've decided it is!"
    (let ((projectp))
      (dolist (specialp (list (expand-file-name "papers" denote-directory)
                              (expand-file-name "buoy" denote-directory)))
        (when (file-in-directory-p dir specialp)
          (setq projectp t)))
      (when projectp (list 'vc 'Git dir)))))

;;;; Magit
;;;;; Itself
;; The best git interface. Mostly taken from Mostly taken from
;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#version-control
(use-package magit
  :custom
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
  :config
  ;; NOTE 2021-08-20: Provides useful functionality, such as `magit-project-status'
  (require 'magit-extras)               ; Load the remaining magit libraries

  (magit-auto-revert-mode)

  ;; Adds hooks to `magit-status-sections-hook'. Should be a separate call for
  ;; each.
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-overview
                          'magit-insert-staged-changes t))

;;;;; Magit-log date headers
;; Add dates to magit-logs
(with-eval-after-load 'magit
  (require 'ov)                         ; Dependency

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

;;;;; Forge
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

;;;;; Orgit-forge
;; Org links to forge buffers
(use-package orgit-forge)

;;;;; Consult-gh
;; Use consult to interface with Github CLI. (And Forge, with
;; `consult-gh-forge').
(use-package consult-gh
  :ensure-system-package (gh . github-cli)
  :bind ("C-c g" . consult-gh)
  :custom
  (consult-gh-default-interactive-command #'consult-gh-transient)

  ;; Cloning
  (consult-gh-default-clone-directory (expand-file-name "Repos" "~"))

  ;; Actions
  (consult-gh-code-action #'consult-gh--code-view-action)
  (consult-gh-discussion-action #'consult-gh--discussion-browse-url-action)
  (consult-gh-file-action #'consult-gh--files-browse-url-action)
  (consult-gh-issue-action #'consult-gh-forge--issue-view-action)

  ;; Previews
  (consult-gh-show-preview t)
  (consult-gh-preview-key "C-;")
  (consult-gh-issue-preview-mode 'markdown-mode)
  (consult-gh-repo-preview-mode 'markdown-mode)
  :config
  ;; FIXME 2024-10-07: For some reason this file isn't loaded, nor is
  ;; `consult-gh-transient' autoloaded
  (require 'consult-gh-transient)

  ;; FIXME 2024-10-09: I have to require this since I set
  ;; `consult-gh-issue-action' to `consult-gh-forge--issue-view-action'...
  (require 'consult-gh-forge)

  (add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
  (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list))

(use-package consult-gh-forge
  :diminish
  :custom
  (consult-gh-forge-timeout-seconds 10)
  :init
  (with-eval-after-load 'forge
    (consult-gh-forge-mode 1)))

(use-package consult-gh-embark
  :diminish
  :init
  (with-eval-after-load 'embark
    (consult-gh-embark-mode 1)))

;;;;; Abdridge-diff
(use-package abridge-diff
  :diminish
  :after diff
  :demand
  :config
  (abridge-diff-mode 1))

;;;; VC
;;;;; Itself
(use-package vc
  :custom
  (add-log-mailing-address "krisbalintona@gmail.com")
  (add-log-keep-changes-together t))

;;;;; Ediff
(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain) ; Keep everything in the same frame
  (ediff-highlight-all-diffs nil))      ; Only highlight currently selected diff

;;;;; Vc-msg
;; See a line's corresponding commit information (like git blame)
(use-package vc-msg
  :bind
  ("C-M-s-v". vc-msg-show))

;;;; QoL

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
         (conf-mode . diff-hl-mode)
         (vc-dir-mode . diff-hl-dir-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-draw-borders nil)
  (diff-hl-show-staged-changes nil)
  (diff-hl-update-async t)
  (diff-hl-side 'right)
  (diff-hl-flydiff-delay 1)             ; See `diff-hl-flydiff-mode'
  :config
  (global-diff-hl-show-hunk-mouse-mode 1)

  ;; Ensure buffer is widened before calling `diff-hl-stage-dwim' because the
  ;; buffer cannot be narrowed for it to succeed
  (advice-add 'diff-hl-stage-dwim :around (lambda (orig-fun &rest args)
                                            (save-restriction
                                              (widen)
                                              (apply orig-fun args)))))

;;;;; Git-timemachine
;; Enable in current buffer to iterate through git revision history
(use-package git-timemachine)

;;;;; Better Magit status
(with-eval-after-load 'magit
  (defun unpackaged/magit-status ()
    "Open a `magit-status' buffer and close all other windows.
If a file was visited in the buffer that was active when this command
was called, go to its unstaged changes section."
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
  (bind-key [remap magit-status] #'unpackaged/magit-status))

;;;;; Deadgrep
;; Grep but with a convenient magit-like interface (with visibility toggles)
(use-package deadgrep
  :bind
  ("<f6>". deadgrep))

(provide 'programming-projects-rcp)
;;; programming-projects-rcp.el ends here
