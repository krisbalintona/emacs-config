;;; Olivetti
(use-package olivetti
  :hook ((org-mode Info-mode emacs-news-view-mode org-msg-edit-mode) . olivetti-mode)
  :custom
  (olivetti-lighter nil)
  (olivetti-body-width 0.6)
  (olivetti-minimum-body-width 80)
  (olivetti-margin-width 8)
  (olivetti-style 'fancy)              ; Fancy makes the buffer look like a page
  ;; FIXME 2024-01-11: This is a temporary solution. Olivetti's changing of
  ;; margins and fringes messes with the calculation of
  ;; `mode--line-format-right-align', which determines where the right side of
  ;; the mode line is placed.
  (mode-line-format-right-align
   '(:eval (if (and (bound-and-true-p olivetti-mode)
                    olivetti-style)     ; 'fringes or 'fancy
               (let ((mode-line-right-align-edge 'right-fringe))
                 (mode--line-format-right-align))
             (mode--line-format-right-align))))
  :config
  (krisb-modus-themes-setup-faces
   "olivetti"
   (set-face-attribute 'olivetti-fringe nil
                       :background bg-dim
                       :inherit 'unspecified)))

;;; Astute.el
(use-package astute
  :hook (org-mode . astute-mode)
  :custom
  (astute-lighter "")
  (astute-prefix-single-quote-exceptions
   '("bout"
     "em"
     "n'"
     "cause"
     "round"
     "twas"
     "tis")))

;;; Darkroom
(use-package darkroom
  :bind ( :map krisb-toggle-keymap
          ("d" . darkroom-mode)
          ("D" . darkroom-tentative-mode))
  :custom
  (darkroom-text-scale-increase 1.3))

;;; Provide
(provide 'krisb-prose)
