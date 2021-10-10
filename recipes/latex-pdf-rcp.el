;;; latex-pdf-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Live PDF previews for latex
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Auctex-latexmk
;; Integration with `latexmk' for more compilation support (particularly
;; citations)
(use-package auctex-latexmk
  :defer 6
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t) ; Pass -pdf flag if TeX-PDF-mode is active
  :config
  (auctex-latexmk-setup)

  (font-lock-add-keywords 'latex-mode
                          '(;; true
                            ("true" 0 '(t :foreground "green") t)
                            ;; false
                            ("false" 0 '(t :foreground "red") t)
                            ;; For table (tabular) columns
                            ("rowmac" 0 '(t :background "blue") t)
                            ))
  )

;;; Latex-preview-pane
(use-package latex-preview-pane
  :disabled t ; Use my makeshift `latexmk-mode' instead for previews
  :straight (latex-preview-pane :type git :host github :repo "jsinglet/latex-preview-pane")
  :custom
  (pdf-latex-command "latexmk") ; Use latexmk for citation support
  )

;;; Latexmk-mode
;; My own minor-mode creating automatically updating pdf-tools LaTeX preview
(with-eval-after-load 'auctex-latexmk
  (define-minor-mode latexmk-mode
    "Toggle LatexMK mode."
    :init-value nil
    :lighter " LatexMK "
    )

  (defun kb/run-latexmk ()
    "Start external latexmk process and run in current buffer."
    (interactive)
    (start-process "latexmk" "latexmk output" "latexmk" "--silent" "--pdf" (buffer-file-name (current-buffer)))
    )

  (defun kb/try-run-latexmk ()
    "Try to execute kb/run-latexmk."
    (if (and (bound-and-true-p latexmk-mode) (eql major-mode 'latex-mode))
        (kb/run-latexmk))
    )

  (add-hook 'LaTeX-mode-hook 'latexmk-mode)
  (add-hook 'after-save-hook #'kb/try-run-latexmk)
  )

;;; latex-pdf-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'latex-pdf-rcp)
