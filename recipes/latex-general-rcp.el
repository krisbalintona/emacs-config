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
  :requires company
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
  :after tex
  :hook (latex-mode . (lambda ()
                        (push '("\\Dashv" . ?⫤) prettify-symbols-alist)
                        (push '("\\DashVDash" . ?⟚) prettify-symbols-alist)
                        ))

  :gfhook
  'prettify-symbols-mode
  'reftex-isearch-minor-mode
  'TeX-source-correlate-mode ; Minor mode for forward and inverse search.
  'display-line-numbers-mode
  'LaTeX-math-mode ; Access to math macros
  'visual-line-mode ; So evil can respect true lines
  '(lambda () (mixed-pitch-mode -1))
  :custom
  ;; Processes for org-to-latex conversion
  ;; (org-latex-pdf-process
  ;;  '("%latex -interaction nonstopmode -output-directory %o %f"
  ;;    "biber --output-directory %o $(basename %f .tex)"
  ;;    "%latex -interaction nonstopmode -output-directory %o %f"
  ;;    "%latex -interaction nonstopmode -output-directory %o %f")
  ;;  )
  (org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")) ; From https://github.com/jkitchin/org-ref
  (org-latex-with-hyperref nil) ; Don't use the hyperref LaTeX package when exporting from org-mode
  :preface (defvaralias 'latex-mode-hook 'LaTeX-mode-hook "For easier use-package declaration.")
  )

;;;; Cdlatex
;; Faster LaTeX inputs
(use-package cdlatex
  :demand t
  :hook (LaTeX-mode . turn-on-cdlatex)
  )

;;;; Magic-latex-buffer
;; Magically enhance LaTeX-mode font-locking
(use-package magic-latex-buffer
  :disabled t ; Too laggy for now
  :ghook ('LaTeX-mode-hook 'magic-latex-buffer)
  :custom
  ;; All available customizations
  (magic-latex-enable-block-highlight t)
  (magic-latex-enable-suscript t)
  (magic-latex-enable-pretty-symbols t)
  (magic-latex-enable-block-align t)
  (magic-latex-enable-inline-image t)
  (magic-latex-enable-minibuffer-echo t)
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
(advice-add 'kb/indent-whole-buffer :after (lambda ()
                                             (if (eq major-mode 'latex-mode)
                                                 (kb/tabular-magic))))

;;; latex-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'latex-general-rcp)
