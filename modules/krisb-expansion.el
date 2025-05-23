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

;;; Templates

;;; Provide
(provide 'krisb-expansion)
