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

;;; Provide
(provide 'krisb-expansion)
