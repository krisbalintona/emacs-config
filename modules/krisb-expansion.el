;; -*- lexical-binding: t; -*-

;;; Abbrev
;; Automatically correct typed strings (e.g. words).  Most useful for correcting
;; spelling mistakes as they are made.
(use-package abbrev
  :ensure nil
  :diminish
  :custom
  (save-abbrevs 'silently)
  (abbrev-suggest t)
  (abbrev-suggest-hint-threshold 2)
  :config
  ;; Enable the mode globally
  (setq-default abbrev-mode t))

;;; "Guessing" at point

;;;; Dabbrev
;; Use Dabbrev with Corfu!
(use-package dabbrev
  :ensure nil
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (dolist (mode '(doc-view-mode
                  pdf-view-mode
                  tags-table-mode))
    (add-to-list 'dabbrev-ignored-buffer-modes mode)))

;;;; Hippie-expand
(use-package hippie-exp
  :ensure nil
  :bind ([remap dabbrev-expand] . hippie-expand))

;;; Templates

;;;; Tempel
(use-package tempel
  :hook ((prog-mode text-mode) . krisb-tempel-setup-capf)
  :bind ("M-*" . tempel-insert)
  :custom
  ;; Applies to `tempel-expand' and `tempel-complete'.  We prefer non-pair
  ;; characters to avoid inserting an extra pair from `electric-pair-mode'.  It
  ;; should also ideally be an unused (or at least very rare) comment delimiter
  ;; to avoid TAB indenting the line when `tab-always-indent' is \\='complete
  (tempel-trigger-prefix "=")
  :init
  (defun krisb-tempel-setup-capf ()
    "Add `tempel-expand' to the beginning of local `completion-at-point-functions'.
We also add `tempel-expand' to the beginning of the global value for
`completion-at-point-functions'.  The difference here is that we want
`tempel-expand' to be the first `completion-at-point' function for the
buffers in which this function is run."
    (add-hook 'completion-at-point-functions 'tempel-expand -90 t))
  :config
  ;; Place `tempel-complete' at the beginning of the fallback (global value)
  ;; `completion-at-point-functions'
  (add-hook 'completion-at-point-functions #'tempel-complete -90)

  ;; Element that expands other templates by name.  E.g., (i header) expands the
  ;; template named "header."
  (defun krisb-tempel-include (elt)
    (when (eq (car-safe elt) 'include)
      (if-let (template (alist-get (cadr elt) (tempel--templates)))
          (cons 'l template)
        (message "Template %s not found" (cadr elt))
        nil)))
  (add-to-list 'tempel-user-elements #'krisb-tempel-include))

;;; Provide
(provide 'krisb-expansion)
