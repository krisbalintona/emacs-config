;; -*- lexical-binding: t; -*-

;;; Diredfl
;; A little more fontification in dired buffers.
(use-package diredfl
  :config
  (diredfl-global-mode 1))

;;; Dired-x
(use-package dired-x
  :ensure nil
  :custom
  (dired-omit-verbose nil))

;;; Nerd-icons-dired
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;;; Provide
(provide 'krisb-directories)
