;;; Markdown-mode
(use-package markdown-mode
  :mode ("INSTALL\\'" "CONTRIBUTORS\\'" "LICENSE\\'" "README\\'")
  :hook (markdown-mode . visual-line-mode))

;;; Provide
(provide 'krisb-spelling)
