;;; latex-general-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Basic TeX (auctex) configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; AucTeX
;; A lot taken from https://github.com/MatthewZMD/.emacs.d#auctex
(use-package tex
  :demand t ; Other latex packages are reliant on this one
  :ensure-system-package (latex . texlive-scheme-full)
  :straight auctex
  :hook
  ((LaTeX-mode . (lambda ()
                   (setq reftex-plug-into-AUCTeX t)
                   (reftex-isearch-minor-mode)
                   (setq TeX-PDF-mode t)
                   (setq TeX-source-correlate-mode t)
                   (setq TeX-source-correlate-method 'synctex)
                   (setq TeX-source-correlate-start-server t)))
   (LaTeX-mode . prettify-symbols-mode)
   (LaTeX-mode . display-line-numbers-mode)
   (LaTeX-mode . LaTeX-math-mode) ; Access to math macros
   (LaTeX-mode . visual-line-mode) ; So evil can respect true lines
   )
  :custom
  ;; If you want to make AUCTeX aware of style files and multi-file documents right away, insert the following in your ‘.emacs’ file.
  (TeX-parse-self t)
  (TeX-auto-save t)
  (TeX-master t)
  ;; (TeX-master nil) ; if you often use \include or \input, you should make AUCTeX aware of the multi-file document structure. Each time you open a new file, AUCTeX will then ask you for a master file

  ;; To use pdf-tools with auctex
  (TeX-view-program-selection '((output-pdf "pdf-tools")))
  (TeX-view-program-list '(("pdf-tools" TeX-pdf-tools-sync-view)))
  (TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ; Revert PDF after compilation

  ;; Procusses for org-to-latex conversion
  (org-latex-pdf-process
   '("%latex -interaction nonstopmode -output-directory %o %f"
     "biber --output-directory %o $(basename %f .tex)"
     "%latex -interaction nonstopmode -output-directory %o %f"
     "%latex -interaction nonstopmode -output-directory %o %f")
   )
  )
;;;; Reftex
;; Manage references, citations, and labels with AUCTeX
(use-package reftex
  :hook (LaTeX-mode . reftex-mode)
  :custom
  (reftex-cite-prompt-optional-args 'maybe) ; Prompt for empty optional arguments in cite?
  )

;;;; Company-auctex
;; Auctex suggestions using the company backend
(use-package company-auctex
  :after company
  :hook (company-mode . company-auctex-init)
  )

;;;; Magic-latex-buffer
;; Magically enhance LaTeX-mode font-locking
(use-package magic-latex-buffer
  :disabled t ; Too laggy for now
  :hook (LaTeX-mode . magic-latex-buffer)
  :custom
  ;; All available customizations
  (magic-latex-enable-block-highlight t)
  (magic-latex-enable-suscript t)
  (magic-latex-enable-pretty-symbols t)
  (magic-latex-enable-block-align t)
  (magic-latex-enable-inline-image t)
  (magic-latex-enable-minibuffer-echo t)
  )

;;;; Mathpix.el
;; Use mathpix AI to turn pictures of math into LaTeX
(use-package mathpix
  :disabled t ; Don't use this and my mathpix api key might not work anymore
  :straight (mathpix :type git :host github :repo "jethrokuan/mathpix.el")
  :custom
  ;; From mathpix api
  (mathpix-app-id "kristoffer_balintona_brown_edu_282770_f79d0b")
  (mathpix-app-key "2b7d2df8ebba358834bf")
  (mathpix-screenshot-method "xclip -selection clipboard -t image/png -o > %s") ; Take an image that is already on the clipboard, for Linux
  :config
  (general-define-key
   "C-c m" '(mathpix-screenshot :which-key "Mathpix")
   )
  )

;;; latex-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'latex-general-rcp)
