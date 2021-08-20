;;; programming-vc-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Everything to do with version control.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)

;;;; Version control
;;;;; Magit
;; The best git interface. Mostly taken from Mostly taken from
;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#version-control
(use-package magit
  :straight (magit :type git :host github :repo "magit/magit")
  :hook (magit-process-mode . visual-line-mode)
  :general
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
  :custom
  ;; How opened magit buffers (e.g. commit) are shown
  ;; (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1)

  ;; Displaying hunks
  ;; (magit-diff-highlight-hunk-body nil)
  (magit-diff-highlight-hunk-body t)
  (magit-diff-highlight-hunk-region-functions
   '(magit-diff-highlight-hunk-region-dim-outside
     magit-diff-highlight-hunk-region-using-overlays
     magit-diff-highlight-hunk-region-using-face))

  ;; Sections
  (magit-module-sections-nested t)
  (magit-section-show-child-count t)
  (magit-refs-show-commit-count 'all) ; Show branches and tags
  (magit-section-initial-visibility-alist '((modules . show)
                                            (stashes . show)
                                            (unpulled . show)
                                            (unpushed . show)))
  (transient-mode-line-format nil)

  ;; Save transient changes to a different location (since I changed the
  ;; transient-level for certain magit transient popups)
  (transient-history-file (concat no-littering-var-directory "transient/history.el"))
  (transient-values-file (concat no-littering-var-directory "transient/values.el"))
  (transient-levels-file (concat no-littering-var-directory "transient/levels.el"))

  ;; Performance-related variables
  (magit-refresh-status-buffer t) ; Change to nil as last resort
  (magit-refresh-verbose t) ; Help troubleshoot bottlenecks (check `*Messages*' buffer)
  ;; Prefer these to be nil for performance boosts
  (magit-diff-highlight-indentation t) ; Highlight wrong indentation?
  (magit-diff-highlight-trailing t) ; Highlight trailing spaces?
  (magit-diff-paint-whitespace 'uncommitted) ; Where to highlight whitespace errors?
  (magit-diff-highlight-hunk-body t) ; Highlight hunks?
  (magit-diff-refine-hunk t) ; Extra-highlight word-level differences?
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-overview ; Have modules section
                          'magit-insert-status-headers ; Insert header sections for `magit-status'
                          t)
  )

;;;;; Magit-log date headers
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

;;;;; Ediff
(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain) ; Keep everything in the same frame
  (ediff-highlight-all-diffs nil) ; Only highlight currently selected diff
  )

;;;; QoL
;;;;; Git-gutter-fringes
;; Indicate diff areas in fringe
(use-package git-gutter-fringe
  :ghook ('prog-mode-hook 'git-gutter-mode)
  :custom
  (git-gutter:disabled-modes '(org-mode))
  (git-gutter-fr:side 'left-fringe)
  :config
  (setq-default left-fringe-width  10)
  (setq-default right-fringe-width 10)

  ;; (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center t))
  ;; (define-fringe-bitmap 'git-gutter-fr:deleted [240 240 240 240] nil nil 'bottom)
  ;; (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center t))

  ;; TODO 2021-08-19: Change these bitmaps (and associated fringe width) to be
  ;; more appealing
  (fringe-helper-define 'git-gutter-fr:added nil
    ".XXXXXX."
    "XX....XX"
    "X......X"
    "X......X"
    "XXXXXXXX"
    "XXXXXXXX"
    "X......X"
    "X......X")
  (fringe-helper-define 'git-gutter-fr:deleted nil
    "XXXXXX.."
    "XX....X."
    "XX.....X"
    "XX.....X"
    "XX.....X"
    "XX.....X"
    "XX....X."
    "XXXXXX..")
  (fringe-helper-define 'git-gutter-fr:modified nil
    "XXXXXXXX"
    "X..XX..X"
    "X..XX..X"
    "X..XX..X"
    "X..XX..X"
    "X..XX..X"
    "X..XX..X"
    "X..XX..X")
  )

;;;;; Git-timemachine
;; Enable in current buffer to iterate through git revision history
(use-package git-timemachine)

;;; programming-vc-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-vc-rcp)
