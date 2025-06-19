;; -*- lexical-binding: t; -*-

;;; Writing

;;; Visual organization

;;;; Form-feed
;; Display  (page breaks) fancily. Visit the readme for alternatives and their
;; differences
;; 2025-03-31: I've found that form-feed is superior to page-break-lines for
;; graphical systems, since it uses font-lock instead of glyph composition.  In
;; practice, the biggest effect is the consistency of the length of the
;; horizontal line regardless of font: the length of the horizontal rule of
;; page-break-lines is not guaranteed to fill the width of the window, whereas
;; it is for form-feed.
(use-package form-feed
  :diminish
  :custom
  (form-feed-include-modes
   '(prog-mode
     conf-mode
     text-mode
     ;; TOOD 2025-03-25: Enabling form feed in `help-mode' buffers removes the
     ;; fontification of function arguments (face: `help-argument-name').
     ;; help-mode
     emacs-news-view-mode
     compilation-mode))
  (form-feed-exclude-modes nil)
  :config
  (global-form-feed-mode 1))

;;;; Which-func
(use-package which-func
  :hook (on-first-file . which-function-mode)
  :custom
  (which-func-modes '(prog-mode)))

;;;; Breadcrumb
;; Which-function stuff but more performant and prettier formatting. Read
;; package commentary for a description on how.
(use-package breadcrumb
  :demand t
  :hook (eglot-managed-mode . (lambda () (when (derived-mode-p 'prog-mode) (breadcrumb-local-mode))))
  :init
  (with-eval-after-load 'which-func
    (setopt which-func-functions '(breadcrumb-imenu-crumbs))))

;;; Convenience

;;;; Sudo-edit
;; Utilities to edit files as root
(use-package sudo-edit
  :bind ( :map krisb-file-keymap
          ("U" . sudo-edit-find-file)
          ("u" . sudo-edit))
  :config
  (sudo-edit-indicator-mode 1))

;;;; Rainbow-mode
;; Colorify color codes
(use-package rainbow-mode
  :diminish
  :hook (;; TOOD 2025-03-25: Enabling form feed in `help-mode' buffers removes
         ;; the fontification of function arguments (face:
         ;; `help-argument-name').
         ;; (help-mode . rainbow-mode)
         (prog-mode . rainbow-mode)))

;;; Provide
(provide 'krisb-programming-essentials)
