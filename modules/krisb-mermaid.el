;;; Mermaid-mode
(use-package mermaid-mode)

;;; Ob-mermaid
;; Mermaid diagrams
(use-package ob-mermaid
  :ensure-system-package (mmdc . mermaid-cli)
  :custom
  (ob-mermaid-cli-path (executable-find "mmdc"))
  :config
  (with-eval-after-load 'org
    ;; We use setopt to start the evaluation of the :set keyword of
    ;; `org-babel-load-languages'
    ;; REVIEW 2024-10-18: Check if setopt is necessary.
    (setopt org-babel-load-languages
            (append '((mermaid . t)) org-babel-load-languages))))

;;; Provide
(provide 'krisb-mermaid)
