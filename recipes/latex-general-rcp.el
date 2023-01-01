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
  (TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ; Revert PDF after compilation
  (TeX-view-program-selection '((output-pdf "pdf-tools")))
  (TeX-view-program-list '(("pdf-tools" TeX-pdf-tools-sync-view)))
  :config
  (add-to-list 'TeX-command-list
               '("LuaLaTeX" "%`lualatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t)))

;;;; Reftex
;; Manage references, citations, and labels with AUCTeX
(use-package reftex
  :ghook 'LaTeX-mode-hook
  :custom
  (reftex-plug-into-AUCTeX t) ; Plug-in flags for AUCTeX interface.
  (reftex-cite-prompt-optional-args 'maybe)) ; Prompt for empty optional arguments in cite?

;;;; Company-auctex
;; Auctex suggestions using the company backend
(use-package company-auctex
  :after company
  :hook (LaTeX-mode . (lambda () ;; I manually add backends from `company-auctex-init'
                        (add-to-list 'company-backends
                                     '(company-auctex-labels company-auctex-bibs (company-auctex-macros company-auctex-symbols company-auctex-environments))))))

;;; LaTeX
;;;; Latex
(use-package latex
  :straight nil
  :ensure-system-package (latex . texlive-full)
  :gfhook
  'prettify-symbols-mode
  'reftex-isearch-minor-mode
  'TeX-source-correlate-mode ; Minor mode for forward and inverse search.
  'display-line-numbers-mode
  'LaTeX-math-mode ; Access to math macros
  'visual-line-mode ; So evil can respect true lines
  '(lambda () (mixed-pitch-mode -1))
  'hl-line-mode
  '(lambda () (flycheck-mode -1))
  :preface (defvaralias 'latex-mode-hook 'LaTeX-mode-hook "For easier use-package declaration.")
  :config
  ;; Add font locked words to latex-mode
  (with-eval-after-load 'tex
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

    ;; Using EAF's pdf viewer
    (require 'eaf)
    (defun kb/eaf-pdf-synctex-forward-view ()
      "View the PDF file of Tex synchronously."
      (interactive)
      ;; So that the pdf opens in a vsplit
      (split-window-right)
      (windmove-right)
      (let* ((pdf-url (expand-file-name (TeX-active-master (TeX-output-extension))))
             (tex-buffer (window-buffer (minibuffer-selected-window)))
             (tex-file (buffer-file-name tex-buffer))
             (line-num (progn (set-buffer tex-buffer) (line-number-at-pos)))
             (opened-buffer (eaf-pdf--find-buffer pdf-url))
             (synctex-info (eaf-pdf--get-synctex-info tex-file line-num pdf-url)))
        (if (not opened-buffer)
            (eaf-open
             ;; (prin1-to-string pdf-url)    ; This causes an error
             pdf-url                      ; This fixes the error
             "pdf-viewer" (format "synctex_info=%s" synctex-info))
          (pop-to-buffer opened-buffer)
          (eaf-call-sync "call_function_with_args" eaf--buffer-id
                         "jump_to_page_synctex" (format "%s" synctex-info)))))
    (add-to-list 'TeX-view-program-list '("eaf" kb/eaf-pdf-synctex-forward-view))
    (add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))))

;;;; Cdlatex
;; Faster LaTeX inputs
(use-package cdlatex
  :demand t
  :after latex
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (LaTeX-mode . (lambda ()
                         (push '("pline" "\\pline[]{?}[]" nil) cdlatex-env-alist)
                         (push '("fitchproof" "\\fitchprf{\n?\n}\n{\n\n}" nil) cdlatex-env-alist)
                         (push '("subproof" "\\subproof{\n?\n}\n{\n\n}" nil) cdlatex-env-alist)
                         ))))

;;;; Evil-tex
(use-package evil-tex
  :after (evil tex)
  :hook (latex-mode . evil-tex-mode)
  :custom
  (evil-tex-toggle-override-t t))

;;;; Lsp-latex
(use-package lsp-latex
  :disabled t                           ; Not useful, for now
  :ensure-system-package ("~/.cargo/bin/texlab" . "cargo install --git https://github.com/latex-lsp/texlab.git --locked") ; Quite long of an install since compiling from source
  :hook (latex-mode . (lambda ()
                        (require 'lsp-latex)
                        (lsp-deferred)
                        ))
  :custom
  (lsp-latex-texlab-executable "~/.cargo/bin/texlab")
  (lsp-latex-texlab-executable-argument-list nil))

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

;;;;; Kb/latex-mk-mode
;; My own minor-mode creating automatically updating pdf-tools LaTeX preview
(define-minor-mode kb/latexmk-mode
  "Toggle LatexMK mode."
  :init-value nil
  :lighter " LatexMK "
  (cond
   (kb/latexmk-mode (add-hook 'after-save-hook 'kb/run-latexmk 0 t))
   (t (remove-hook 'after-save-hook 'kb/run-latexmk t))))
(add-hook 'latex-mode-hook #'kb/latexmk-mode)

(defun kb/run-latexmk ()
  "Start external latexmk process and run in current buffer."
  (interactive)
  (start-process "latexmk" "latexmk output" "latexmk" "--silent" "--pdf" (buffer-file-name (current-buffer))))

;;; latex-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'latex-general-rcp)
