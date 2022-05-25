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
  :disabled t                   ; I think my `kb/latexmk-mode' is enough for me?
  :demand t
  :after latex
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t) ; Pass -pdf flag if TeX-PDF-mode is active
  :config (auctex-latexmk-setup)
  )

;;; Latexmk-mode
;; My own minor-mode creating automatically updating pdf-tools LaTeX preview
(define-minor-mode kb/latexmk-mode
  "Toggle LatexMK mode."
  :init-value nil
  :lighter " LatexMK "
  (cond
   (kb/latexmk-mode (add-hook 'after-save-hook 'kb/run-latexmk 0 t))
   (t (remove-hook 'after-save-hook 'kb/run-latexmk t)))
  )
(add-hook 'latex-mode-hook #'kb/latexmk-mode)

(defun kb/run-latexmk ()
  "Start external latexmk process and run in current buffer."
  (interactive)
  (start-process "latexmk" "latexmk output" "latexmk" "--silent" "--pdf" (buffer-file-name (current-buffer)))
  )

;;; latex-pdf-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'latex-pdf-rcp)
