;;; Mermaid-mode
(use-package mermaid-mode)

;;; Ob-mermaid
;; Mermaid diagrams
(use-package ob-mermaid
  :ensure-system-package (mmdc . mermaid-cli)
  :custom
  (ob-mermaid-cli-path (executable-find "mmdc"))
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append '((mermaid . t)) org-babel-load-languages)))

;;; Provide
(provide 'krisb-mermaid)
