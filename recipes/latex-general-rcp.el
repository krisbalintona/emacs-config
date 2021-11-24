;;; latex-general-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Basic TeX (auctex) configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; TeX
;;;; AucTeX
;; A lot taken from https://github.com/MatthewZMD/.emacs.d#auctex
(use-package tex
  :straight auctex
  :after prog-mode
  :custom
  (TeX-source-correlate-start-server t)
  (TeX-source-correlate-method 'synctex)

  ;; For multi-file documents and external style files
  (TeX-parse-self t)
  (TeX-auto-save t)
  (TeX-master t)

  ;; To use pdf-tools with auctex
  (TeX-PDF-mode t)
  (TeX-view-program-selection '((output-pdf "pdf-tools")))
  (TeX-view-program-list '(("pdf-tools" TeX-pdf-tools-sync-view)))
  (TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ; Revert PDF after compilation
  )

;;;; Reftex
;; Manage references, citations, and labels with AUCTeX
(use-package reftex
  :ghook 'LaTeX-mode-hook
  :custom
  (reftex-plug-into-AUCTeX t) ; Plug-in flags for AUCTeX interface.
  (reftex-cite-prompt-optional-args 'maybe) ; Prompt for empty optional arguments in cite?
  )

;;;; Company-auctex
;; Auctex suggestions using the company backend
(use-package company-auctex
  :after company
  :hook (LaTeX-mode . (lambda () ;; I manually add backends from `company-auctex-init'
                        (add-to-list 'company-backends
                                     '(company-auctex-labels company-auctex-bibs (company-auctex-macros company-auctex-symbols company-auctex-environments)))))
  )

;;; LaTeX
;;;; Latex
(use-package latex
  :straight nil
  :ensure-system-package (latex . texlive-full)
  :after (tex prog-mode)
  :hook (latex-mode . (lambda ()
                        (require 'prog-mode)
                        (push '("\\Dashv" . ?⫤) prettify-symbols-alist)
                        (push '("\\DashVDash" . ?⟚) prettify-symbols-alist)
                        (push '("\\dashVdash" . ?⊢) prettify-symbols-alist)
                        (delete '("--" . 8211) prettify-symbols-alist)
                        (delete '("---" . 8212) prettify-symbols-alist)
                        ;; For `lplfitch'. Slightly higher than `\vdots'. Using
                        ;; the `\pline{\vdots}' results in the ellipses not
                        ;; being centered on the line.
                        (push '("\\ellipsesline" . ?⋮) prettify-symbols-alist)
                        ;; Circled numbers from the pifont package
                        (push '("\\ding{192}" . ?①) prettify-symbols-alist)
                        (push '("\\ding{193}" . ?②) prettify-symbols-alist)
                        (push '("\\ding{194}" . ?③) prettify-symbols-alist)
                        (push '("\\ding{195}" . ?④) prettify-symbols-alist)
                        (push '("\\ding{196}" . ?⑤) prettify-symbols-alist)
                        (push '("\\ding{197}" . ?⑥) prettify-symbols-alist)
                        (push '("\\ding{198}" . ?⑦) prettify-symbols-alist)
                        (push '("\\ding{199}" . ?⑧) prettify-symbols-alist)
                        (push '("\\ding{200}" . ?⑨) prettify-symbols-alist)
                        (push '("\\ding{201}" . ?⑩) prettify-symbols-alist)
                        ))
  :gfhook
  'prettify-symbols-mode
  'reftex-isearch-minor-mode
  'TeX-source-correlate-mode ; Minor mode for forward and inverse search.
  'display-line-numbers-mode
  'LaTeX-math-mode ; Access to math macros
  'visual-line-mode ; So evil can respect true lines
  '(lambda () (mixed-pitch-mode -1))
  :preface (defvaralias 'latex-mode-hook 'LaTeX-mode-hook "For easier use-package declaration.")
  :config
  ;; Add font locked words to latex-mode
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
                            ("\\\\fitchctx" 0 'font-lock-keyword-face t)
                            ("\\\\fpline" 0 'font-latex-math-face t)
                            ("\\\\tpline" 0 'font-latex-math-face t)
                            ))
  )

;;;; Cdlatex
;; Faster LaTeX inputs
(use-package cdlatex
  :demand t
  :after latex
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (LaTeX-mode . (lambda ()
                         (push '("pline" "\\pline[]{?}[]" nil) cdlatex-env-alist-comb)
                         (push '("fitchprf" "\\fitchprf{\n?\n}\n{\n\n}" nil) cdlatex-env-alist-comb)
                         (push '("subproof" "\\subproof{\n?\n}\n{\n\n}" nil) cdlatex-env-alist-comb)
                         ))
         )
  )

;;;; QoL
;;;;; Align table cells
;; From
;; https://tex.stackexchange.com/questions/557959/emacs-auctex-tabular-vertical-alignment-of-cells
(defun kb/tabular-magic ()
  "Align all cells in current LaTeX buffer."
  (interactive)
  (unless (string= (LaTeX-current-environment) "document")
    (let ((s (make-marker))
          (e (make-marker)))
      (set-marker s (save-excursion
                      (LaTeX-find-matching-begin)
                      (forward-line)
                      (point)))
      (set-marker e (save-excursion
                      (LaTeX-find-matching-end)
                      (forward-line -1)
                      (end-of-line)
                      (point)))
      ;; Delete the next 2 lines if you don't like indenting and removal
      ;; of whitespaces:
      (LaTeX-fill-environment nil)
      (whitespace-cleanup-region s e)
      (align-regexp s e "\\(\\s-*\\)&" 1 1 t)
      (align-regexp s e "\\(\\s-*\\)\\\\\\\\")
      (set-marker s nil)
      (set-marker e nil))))

;;; latex-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'latex-general-rcp)
