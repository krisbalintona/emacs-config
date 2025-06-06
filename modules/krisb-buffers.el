;; -*- lexical-binding: t; -*-

;;; Bufler
(use-package bufler
  :custom
  (bufler-groups
   (bufler-defgroups
     (group
      ;; Subgroup collecting buffers in `org-directory' (or "~/org" if
      ;; `org-directory' is not yet defined).
      (dir (if (bound-and-true-p org-directory)
               org-directory
             "~/org"))
      (group
       ;; Subgroup collecting indirect Org buffers, grouping them by file.
       ;; This is very useful when used with `org-tree-to-indirect-buffer'.
       (auto-indirect)
       (auto-file))
      ;; Group remaining buffers by whether they're file backed, then by mode.
      (group-not "*special*" (auto-file))
      (auto-mode))
     ;; All buffers under "~/.emacs.d" (or wherever it is).
     (dir user-emacs-directory)
     (group
      ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
      (group-or "*Help/Info*"
                (mode-match "*Help*" (rx bos "help-"))
                (mode-match "*Info*" (rx bos "info-"))))
     (group
      ;; Subgroup collecting all special buffers (i.e. ones that are not file-backed),
      ;; except certain ones like Dired, Forge, or Magit buffers (which are allowed to
      ;; fall through to other groups, so they end up grouped with their project buffers).
      (group-not "*Special"
                 (group-or "*Special*"
                           (mode-match "Magit" (rx bos "magit-"))
                           (mode-match "Forge" (rx bos "forge-"))
                           (mode-match "Dired" (rx bos "dired"))
                           (mode-match "grep" (rx bos "grep-"))
                           (mode-match "compilation" (rx bos "compilation-"))
                           (auto-file)))
      (group
       ;; Subgroup collecting these "special special" buffers
       ;; separately for convenience.
       (name-match "**Special**"
                   (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
      (group
       ;; Subgroup collecting all other Magit buffers, grouped by directory.
       (mode-match "*Magit* (non-status)" (rx bos "magit-"))
       (auto-directory))
      ;; Remaining special buffers are grouped automatically by mode.
      (auto-mode))
     (group
      ;; Subgroup collecting buffers in a version-control project,
      ;; grouping them by directory (using the parent project keeps,
      ;; e.g. git worktrees with their parent repos).
      (auto-parent-project)
      (group-not "special"
                 ;; This subgroup collects special buffers so they are
                 ;; easily distinguished from file buffers.
                 (group-or "Non-file-backed and neither Dired nor Magit"
                           (mode-match "Magit Status" (rx bos "magit-status"))
                           (mode-match "Dired" (rx bos "dired-"))
                           (auto-file))))
     ;; Group remaining buffers by directory
     (auto-directory))))

;;; Ibuffer
(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer))
  :bind* ( :map ibuffer-mode-map
           ("SPC" . scroll-up-command)
           ("DEL" . scroll-down-command)
           ("* d" . krisb-ibuffer-mark-displayed-buffers))
  :custom
  (ibuffer-save-with-custom nil)
  (ibuffer-default-sorting-mode 'recency)
  (ibuffer-jump-offer-only-visible-buffers t)
  (ibuffer-old-time 48)
  (ibuffer-expert nil)
  (ibuffer-show-empty-filter-groups t)
  (ibuffer-filter-group-name-face '(:inherit (success bold)))
  ;; Be aware that this value gets overridden by `all-the-icons-ibuffer-formats'
  ;; and `nerd-icons-ibuffer-mode'
  (ibuffer-formats
   '((mark modified read-only locked
           " " (name 18 18 :left :elide)
           " " (krisb-size 9 -1 :right)
           " " (mode 16 16 :right :elide)
           " " filename-and-process)
     (mark " " (name 16 -1) " " filename)))
  :config
  ;; Bespoke size column
  (define-ibuffer-column krisb-size
    (:name "Size"
           :inline t
           :header-mouse-map ibuffer-size-header-map)
    (file-size-human-readable (buffer-size)))

  ;; Bespoke command for marking buffers displayed across all tab-bar tabs
  (defun krisb-ibuffer--get-displayed-buffers ()
    "Return a list of buffers visible in all windows across all tab-bar tabs."
    (let (displayed-buffers)
      (dolist (frame (frame-list))
        (save-window-excursion
          (dolist (tab (tab-bar-tabs frame))
            ;; The current tab does not have a stored window configuration, so
            ;; we don't need to switch to its window configuration for its
            ;; window list
            (when (eq 'tab (car tab))
              (let* ((tab-info (cdr tab))
                     (tab-window-conf (cdr (assq 'wc tab-info))))
                (set-window-configuration tab-window-conf)))
            (dolist (win (window-list nil 'never))
              (cl-pushnew (window-buffer win) displayed-buffers)))))
      displayed-buffers))

  (defun krisb-ibuffer-mark-displayed-buffers ()
    "Mark all buffers visible in any windows across all tab-bar tabs."
    (interactive)
    ;; We re-implement `ibuffer-mark-on-buffer' to call the expensive
    ;; `krisb-ibuffer--get-displayed-buffers' only once
    (let* ((displayed-buffers (krisb-ibuffer--get-displayed-buffers))
           (count
            (ibuffer-map-lines
             (lambda (buf _mark)
               (when (member buf displayed-buffers)
                 (ibuffer-set-mark-1 ibuffer-marked-char)
                 t))
             nil nil)))
      (ibuffer-redisplay t)
      (message "Marked %s buffers" count))))

;;; Nerd-icons-ibuffer
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :custom
  (nerd-icons-ibuffer-color-icon t)
  (nerd-icons-ibuffer-icon-size 0.97)
  (nerd-icons-ibuffer-formats           ; See my value for `ibuffer-formats'
   '((mark modified read-only locked
           " " (icon 2 2 :right)
           " " (name 18 18 :left :elide)
           " " (size-h 12 -1 :right)
           " " (mode+ 16 16 :right :elide)
           " " filename-and-process+)
     (mark " " (name 16 -1) " " filename))))

;;; Buffer-terminator
(use-package buffer-terminator
  :disabled t
  :custom
  (buffer-terminator-inactivity-timeout (* 60 60)) ; 60 minutes
  (buffer-terminator-interval (* 60 10))           ; 10 minutes
  (buffer-terminator-verbose t)
  :init
  (defun krisb-buffer-terminator-predicate ()
    "Buffer predicate for buffer-terminator.
Meant to be the value of `buffer-terminator-predicate'.  See its
docstring for the expected return values."
    (let* ((buffer (current-buffer))
           (buffer-name (buffer-name buffer))
           (file (or (buffer-file-name) (buffer-base-buffer buffer))))
      (cond
       ((and file
             (functionp 'org-agenda-files)
             (member file (org-agenda-files)))
        :keep)
       ((buffer-local-value 'org-capture-mode buffer)
        :keep)
       ((and file (file-in-directory-p file user-emacs-directory))
        :keep)
       ((string-match-p "^marginalia\\.org$" buffer-name)
        :keep)
       ((string-match-p "^diary$" buffer-name)
        :keep)
       (t nil))))                       ; buffer-terminator decides
  :config
  ;; This is a variable currently so we use `setq'
  (setq buffer-terminator-rules-alist
        '((keep-buffer-property . special)
          (keep-buffer-property . process)
          (keep-buffer-property . visible)
          (kill-buffer-property . inactive)
          (call-function . krisb-buffer-terminator-predicate)))

  (buffer-terminator-mode 1))

;;; Provide
(provide 'krisb-buffers)
