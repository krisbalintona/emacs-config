;; -*- lexical-binding: t; -*-

;;; Org-remark
(use-package org-remark
  :diminish (org-remark-mode org-remark-global-tracking-mode)
  :hook (on-first-file . org-remark-global-tracking-mode)
  :bind ( :map org-remark-mode-map
          ("C-c m r" . (lambda () (interactive) (org-remark-highlights-load)))
          ("C-c m l" . org-remark-mark-line)
          ("C-c m d" . org-remark-delete)
          ("C-c m c" . org-remark-change)
          ("C-c m t" . org-remark-toggle)
          ("C-c m o" . org-remark-open)
          ("C-c m v" . org-remark-view)
          ("C-c m n" . org-remark-next)
          ("C-c m p" . org-remark-prev)
          :repeat-map krisb-org-remark-mode-repeat-map
          ("d" . org-remark-delete)
          ("c" . org-remark-change)
          ("t" . org-remark-toggle)
          ("o" . org-remark-open)
          ("v" . org-remark-view)
          ("n" . org-remark-next)
          ("p" . org-remark-prev))
  :custom
  (org-remark-source-file-name 'abbreviate-file-name)
  (org-remark-notes-file-name
   (no-littering-expand-var-file-name "org-remark/marginalia.org"))
  (org-remark-notes-display-buffer-action `((display-buffer-in-side-window)
                                            (side . right)
                                            (slot . 1)))
  (org-remark-create-default-pen-set nil) ; Make my own pens
  (org-remark-notes-auto-delete nil)
  (org-remark-report-no-highlights nil)
  :init
  (with-eval-after-load 'eww
    (org-remark-eww-mode 1))
  (with-eval-after-load 'nov
    (org-remark-nov-mode 1))
  (with-eval-after-load 'info
    (org-remark-info-mode 1))
  :config
  ;; Ensure that `org-remark-notes-file-name' exists
  (when (stringp org-remark-notes-file-name)
    (make-directory (file-name-directory org-remark-notes-file-name) t))

  (with-eval-after-load 'nerd-icons
    (setopt org-remark-icon-notes (nerd-icons-mdicon "nf-md-thought_bubble")
            org-remark-icon-position-adjusted (nerd-icons-mdicon "nf-md-exclamation_thick")
            org-remark-line-icon (nerd-icons-mdicon "nf-md-note"))))

;;; Kris-org-remark-ext
(use-package krisb-org-remark-ext
  :ensure nil
  :after org-remark
  :bind ( :map org-remark-mode-map
          ("C-c m m" . krisb-org-remark-mark-transient))
  :config
  ;; FIXME 2025-04-06: Faces not being applied for some reason?
  (krisb-modus-themes-setup-faces
   "org-remark-ext"
   (set-face-attribute 'krisb-org-remark-resonant-face nil
                       :background bg-red-intense)
   (set-face-attribute 'krisb-org-remark-resonant-minor-face nil
                       :underline `(:color ,bg-red-intense :style wave))

   (set-face-attribute 'krisb-org-remark-thesis-face nil
                       :background bg-yellow-subtle)
   (set-face-attribute 'krisb-org-remark-thesis-minor-face nil
                       :underline `(:color ,bg-yellow-subtle :style wave))

   (set-face-attribute 'krisb-org-remark-detail-face nil
                       :background bg-blue-subtle)
   (set-face-attribute 'krisb-org-remark-detail-minor-face nil
                       :underline `(:color ,bg-blue-subtle :style wave))

   (set-face-attribute 'krisb-org-remark-outline-face nil
                       :background bg-green-subtle)
   (set-face-attribute 'krisb-org-remark-outline-minor-face nil
                       :underline `(:color ,bg-green-subtle :style wave))

   (set-face-attribute 'krisb-org-remark-external-face nil
                       :background bg-magenta-intense)
   (set-face-attribute 'krisb-org-remark-external-minor-face nil
                       :underline `(:color ,bg-magenta-intense :style wave))))

;;; Provide
(provide 'krisb-web-annotations)
