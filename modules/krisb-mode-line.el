;; -*- lexical-binding: t; -*-

;;; Diminish
(use-package diminish
  :config
  (with-eval-after-load 'subword
    (diminish 'subword-mode))
  (with-eval-after-load 'simple
    (diminish 'visual-line-mode))
  (with-eval-after-load 'face-remap
    (diminish 'buffer-face-mode)))

;;; Provide
(provide 'krisb-mode-line)
