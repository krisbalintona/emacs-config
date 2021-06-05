;;; latex-pdf-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Live PDF previews
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Latex-preview-pane
(use-package latex-preview-pane
  :disabled t ; Use my makeshift `latexmk-mode' instead for previews
  :straight (latex-preview-pane :type git :host github :repo "jsinglet/latex-preview-pane")
  :custom
  (pdf-latex-command "latexmk") ; Use latexmk for citation support
  )

;;;; Auctex-latexmk
;; Integration with latexmk for more compilation support (particularly
;; citations)
(use-package auctex-latexmk
  :hook (after-init . auctex-latexmk-setup)
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t) ; Pass -pdf flag if TeX-PDF-mode is active
  )

;;;; Latexmk-mode
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

  (add-hook 'after-save-hook 'kb/try-run-latexmk)
  (add-hook 'LaTeX-mode-hook 'latexmk-mode)
  )

;;; latex-pdf-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'latex-pdf-rcp)
