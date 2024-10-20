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
