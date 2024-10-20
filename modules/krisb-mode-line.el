;;; Diminish
(use-package diminish
  :config
  (with-eval-after-load 'subword
    (diminish 'subword-mode))
  (with-eval-after-load 'simple
    (diminish 'visual-line-mode))
  (with-eval-after-load 'face-remap
    (diminish 'buffer-face-mode)))

;;; Recursion-indicator
(use-package recursion-indicator
  :custom
  (recursion-indicator-symbols
   '((completion "C" recursion-indicator-completion)
     (prompt     "P" recursion-indicator-prompt)
     (suspend    "S" recursion-indicator-suspend)
     (t          "R" recursion-indicator-default)))
  :config
  (recursion-indicator-mode 1)
  (minibuffer-depth-indicate-mode -1))

;;; Provide
(provide 'krisb-mode-line)
