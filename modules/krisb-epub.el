;;; Nov-mode
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook ((nov-mode . visual-line-mode)
         (nov-mode . olivetti-mode)))

;;; Provide
(provide 'krisb-epub)
