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
                            ("\\\\rowmac" 0 'font-latex-math-face t)
                            ;; For natural deduction tables via `lplfitch'
                            ("\\\\fitchprf" 0 'font-lock-keyword-face t)
                            ("\\\\pline" 0 'font-latex-math-face t)
                            ("\\\\subproof" 0 'font-latex-warning-face t)
                            ("\\\\boxedsubproof" 0 'font-latex-warning-face t)
                            ("\\\\brokenform" 0 'font-latex-warning-face t)
                            ("\\\\formula" 0 'font-latex-math-face t)
                            ("\\\\fitcharg" 0 'font-lock-keyword-face t)
                            ("\\\\\fitchctx" 0 'font-lock-keyword-face t)
                            ("\\\\fpline" 0 'font-latex-math-face t)
                            ("\\\\tpline" 0 'font-latex-math-face t)
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

  (add-hook 'find-file-hook
            (lambda ()
              "Enable `latexmk-mode' in files whose extension is `.tex'."
              (when (string= (file-name-extension buffer-file-name) "tex")
                (latexmk-mode))
              ))
  (add-hook 'after-save-hook #'kb/try-run-latexmk)
  )

;;; latex-pdf-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'latex-pdf-rcp)
