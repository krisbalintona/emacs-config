;;; Marginalia
;; Enable richer annotations in minibuffer (companion package of consult.el)
(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  (marginalia-field-width 80)
  (marginalia-align-offset -2))         ; Two to the left

;;; Vertico
;;;; Itself
(use-package vertico
  :pin gnu-elpa-devel
  :bind ("C-M-s-." . vertico-repeat)
  :hook (minibuffer-setup . vertico-repeat-save)
  :custom
  (vertico-count 13)
  (vertico-resize 'grow-only)
  (vertico-cycle nil)
  :config
  (vertico-mode 1)
  (require 'krisb-vertico))

;;;; Vertico-directory
;; More convenient path modification commands
(use-package vertico-directory
  :requires vertico
  :ensure nil
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;;;; Vertico-multiform
(use-package vertico-multiform
  :requires vertico
  :ensure nil
  :demand
  :custom
  (vertico-multiform-categories
   '((consult-grep buffer)
     (imenu buffer)
     (buffer (vertico-sort-function . nil))
     (citar (vertico-sort-function . vertico-sort-history-alpha))))
  (vertico-multiform-commands
   '(;; I use jinx now, but I think it's better to not apply a grid layout to it
     ;; since its use of vertico-groups is useful
     ("flyspell-correct-*" grid (vertico-grid-annotate . 20))
     (pdf-view-goto-label (vertico-sort-function . nil))
     (".+-history" (vertico-sort-function . nil))))
  :config
  (vertico-multiform-mode 1))

;;;; Vertico-buffer
(use-package vertico-buffer
  :requires vertico
  :ensure nil
  :custom
  (vertico-buffer-hide-prompt nil)
  (vertico-buffer-display-action '(display-buffer-reuse-window)))

;;;; Vertico-prescient
(use-package vertico-prescient
  :requires prescient
  :after vertico
  :custom
  (vertico-prescient-completion-styles '(prescient flex))
  (vertico-prescient-enable-filtering nil) ; We want orderless to do the filtering
  (vertico-prescient-enable-sorting t)
  (vertico-prescient-override-sorting nil)
  ;; Only set if `vertico-prescient-enable-filtering' is non-nil. See also
  ;; `prescient--completion-recommended-overrides'
  (vertico-prescient-completion-category-overrides
   '(;; Include `partial-completion' to enable wildcards and partial paths.
     (file (styles partial-completion prescient))
     ;; Eglot forces `flex' by default.
     (eglot (styles prescient flex))))
  :config
  (vertico-prescient-mode 1))



;;; Provide
(provide 'krisb-completion)
