;;; Org-remark
(use-package org-remark
  :diminish (org-remark-mode org-remark-global-tracking-mode)
  :hook (on-first-file . org-remark-global-tracking-mode)
  :bind ( :map org-remark-mode-map
          ("C-c r r" . (lambda () (interactive) (org-remark-highlights-load)))
          ("C-c r l" . org-remark-mark-line)
          ("C-c r d" . org-remark-delete)
          ("C-c r c" . org-remark-change)
          ("C-c r t" . org-remark-toggle)
          ("C-c r o" . org-remark-open)
          ("C-c r v" . org-remark-view)
          ("C-c r n" . org-remark-next)
          ("C-c r p" . org-remark-prev)
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
  :init
  (with-eval-after-load 'eww
    (org-remark-eww-mode 1))
  (with-eval-after-load 'nov
    (org-remark-nov-mode 1))
  (with-eval-after-load 'info
    (org-remark-info-mode 1))
  :config
  (use-package all-the-icons
    :config
    (setopt org-remark-icon-notes (all-the-icons-material "details")
            org-remark-icon-position-adjusted (all-the-icons-material "error")
            org-remark-line-icon (all-the-icons-faicon "sticky-note"))))

;;; Kris-org-remark-ext
(use-package krisb-org-remark-ext
  :ensure nil
  :after org-remark
  :bind ( :map org-remark-mode-map
          ("C-c r m" . krisb-org-remark-mark-transient))
  :config
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
