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
  (defun krisb-olivetti--setup-faces (&optional _theme)
    "Set custom colors for `olivetti'."
    (when (fboundp 'modus-themes-with-colors)
      (set-face-attribute 'olivetti-fringe nil
                          :background (modus-themes-with-colors bg-dim)
                          :inherit 'unspecified)))
  (add-hook 'enable-theme-functions #'krisb-olivetti--setup-faces))

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

;;; Provide
(provide 'krisb-prose)
