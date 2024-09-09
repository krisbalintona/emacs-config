;;; latex-general-rcp.el --- LaTeX                   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; My LaTeX configuration.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Auctex (auctex)
;;;;; This
;; Collection for most of the following packages. **Package name should be
;; loaded as `auctex' for elpaca, according to my testing**
;; NOTE 2023-07-12: If I haven't already, download the `texlive' and
;; `texlive-langs' package groups
(use-package auctex
  ;; NOTE 2023-07-12: I had trouble with the recipe with elpaca, but I found the
  ;; proper one here (and in Doom):
  ;; https://github.com/radian-software/radian/blob/f403244e244ccca695ff6c73c62b1265e521afa7/emacs/radian.el#L3386-L3506
  ;; :ensure (auctex :type git
  ;;                 :host github
  ;;                 :repo "emacs-straight/auctex"
  ;;                 :files ("*.el" "*.info" "dir"
  ;;                         "doc" "etc" "images" "latex" "style")
  ;;                 :pre-build (("chmod" "775" "autogen.sh") ("./autogen.sh")))
  :ensure-system-package biber)

;;;;; Tex
(use-package tex
  :ensure nil
  :after auctex
  :hook ((TeX-mode . (lambda ()
                       ;; Tell Emacs how to parse TeX files
                       (setq ispell-parser 'tex)
                       ;; Don't auto-fill in math blocks
                       (setq fill-nobreak-predicate
                             (cons #'texmathp fill-nobreak-predicate))
                       (visual-line-mode)))
         (TeX-update-style . rainbow-delimiters-mode))
  :custom
  ;; Base settings
  (TeX-engine 'luatex)
  (TeX-command-default "LuaLaTeX")
  (TeX-source-correlate-start-server nil)
  (TeX-master 'dwim)
  (TeX-parse-self t)
  (TeX-auto-save t)
  (TeX-save-query nil)            ; Just save, don't ask before each compilation

  ;; Compilation
  (TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ; Revert PDF after compilation
  (TeX-command-extra-options "-shell-escape") ; Enables system commands? (because we're out of the shell?)
  (TeX-show-compilation nil)

  ;; Other
  (TeX-electric-sub-and-superscript t)
  (TeX-electric-math '("\\(" . ""))
  :config
  ;; Viewing
  (add-to-list 'TeX-view-program-list '("pdf-tools" TeX-pdf-tools-sync-view))
  (add-to-list 'TeX-view-program-selection '(output-pdf "pdf-tools"))

  ;; Command list
  (add-to-list 'TeX-command-list '("LuaLaTeX" "lualatex --synctex=%(mode)% %t" TeX-run-TeX nil t))
  (add-to-list 'TeX-command-list '("View (Evince)" "evince %(O?pdf)" TeX-run-TeX nil t))

  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 -H %s")) ; Set-up chktex (syntax checking utility)

;;;;; Latex
(use-package latex
  :ensure nil
  :after auctex
  :mode ("\\.[tT]e[xX]\\'" . LaTeX-mode)
  :general (:keymaps 'LaTeX-mode-map
                     "C-<return>" 'LaTeX-insert-item)
  :hook (LaTeX-mode . (lambda ()
                        (TeX-PDF-mode)
                        (TeX-source-correlate-mode) ; Minor mode for forward and inverse search.
                        (TeX-fold-mode)
                        (LaTeX-math-mode) ; Math macros
                        (visual-line-mode)
                        (display-line-numbers-mode)
                        (prettify-symbols-mode)
                        (outshine-mode)
                        (flymake-mode)))
  :custom
  ;; Add the TOC entry to the sectioning hooks
  (LaTeX-section-hook
   '(LaTeX-section-heading
     LaTeX-section-title
     LaTeX-section-toc
     LaTeX-section-section
     LaTeX-section-label))
  (LaTeX-fill-break-at-separators nil) ; Don't be afraid to break inline math between lines
  (LaTeX-item-indent 0)
  (LaTeX-math-menu-unicode t)

  ;; Fontification taken from https://tex.stackexchange.com/a/86119/81279, which
  ;; I discovered from Doom
  (font-latex-match-reference-keywords
   '(;; BibLaTeX.
     ("printbibliography" "[{")
     ("addbibresource" "[{")
     ;; Standard commands.
     ("cite" "[{")
     ("citep" "[{")
     ("citet" "[{")
     ("Cite" "[{")
     ("parencite" "[{")
     ("Parencite" "[{")
     ("footcite" "[{")
     ("footcitetext" "[{")
     ;; Style-specific commands.
     ("textcite" "[{")
     ("Textcite" "[{")
     ("smartcite" "[{")
     ("Smartcite" "[{")
     ("cite*" "[{")
     ("parencite*" "[{")
     ("supercite" "[{")
     ;; Qualified citation lists.
     ("cites" "[{")
     ("Cites" "[{")
     ("parencites" "[{")
     ("Parencites" "[{")
     ("footcites" "[{")
     ("footcitetexts" "[{")
     ("smartcites" "[{")
     ("Smartcites" "[{")
     ("textcites" "[{")
     ("Textcites" "[{")
     ("supercites" "[{")
     ;; Style-independent commands.
     ("autocite" "[{")
     ("Autocite" "[{")
     ("autocite*" "[{")
     ("Autocite*" "[{")
     ("autocites" "[{")
     ("Autocites" "[{")
     ;; Text commands.
     ("citeauthor" "[{")
     ("Citeauthor" "[{")
     ("citetitle" "[{")
     ("citetitle*" "[{")
     ("citeyear" "[{")
     ("citedate" "[{")
     ("citeurl" "[{")
     ;; Special commands.
     ("fullcite" "[{")
     ;; Cleveref.
     ("cref" "{")
     ("Cref" "{")
     ("cpageref" "{")
     ("Cpageref" "{")
     ("cpagerefrange" "{")
     ("Cpagerefrange" "{")
     ("crefrange" "{")
     ("Crefrange" "{")
     ("labelcref" "{")))

  (font-latex-match-textual-keywords
   '(;; BibLaTeX brackets.
     ("parentext" "{")
     ("brackettext" "{")
     ("hybridblockquote" "[{")
     ;; Auxiliary commands.
     ("textelp" "{")
     ("textelp*" "{")
     ("textins" "{")
     ("textins*" "{")
     ;; Subcaption.
     ("subcaption" "[{")))

  (font-latex-match-variable-keywords
   '(;; Amsmath.
     ("numberwithin" "{")
     ;; Enumitem.
     ("setlist" "[{")
     ("setlist*" "[{")
     ("newlist" "{")
     ("renewlist" "{")
     ("setlistdepth" "{")
     ("restartlist" "{")
     ("crefname" "{")))
  :config
  ;; Font locking for personal convenience
  (font-lock-add-keywords 'latex-mode
                          '(;; True
                            ("true" 0 '(t :foreground "green") t)
                            ;; False
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
                            ("\\\\tpline" 0 'font-latex-math-face t)))

  (with-eval-after-load 'eaf
    ;; Using EAF's pdf viewer
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

;;;;; Tex-fold
(use-package tex-fold
  :ensure nil
  :after auctex
  :hook ((TeX-mode . TeX-fold-mode)
         (mixed-pitch-mode . (lambda ()
                               "Fix folded things invariably getting fixed pitch when using
mixed-pitch. Math faces should stay fixed by the mixed-pitch
blacklist, this is mostly for \\section etc."
                               (when mixed-pitch-mode
                                 ;; Adding to this list makes mixed-pitch clean
                                 ;; the face remaps after us
                                 (add-to-list 'mixed-pitch-fixed-cookie
                                              (face-remap-add-relative
                                               'TeX-fold-folded-face
                                               :family (face-attribute 'variable-pitch :family)
                                               :height (face-attribute 'variable-pitch :height))))))))

;;;; Cdlatex
;; Faster LaTeX inputs
(use-package cdlatex
  :after auctex
  :hook ((LaTeX-mode . cdlatex-mode)
         (org-mode . org-cdlatex-mode))
  :diminish (org-cdlatex-mode . "")
  :general
  (:keymaps 'cdlatex-mode-map
            ;; Other packages take care of inserting closing delimiters
            "$" nil
            "(" nil
            "{" nil
            "[" nil
            "|" nil
            "<" nil
            ;; AUCTeX takes care of auto-inserting {} on _^ if you want, with
            ;; `TeX-electric-sub-and-superscript'.
            "^" nil
            "_" nil
            ;; Don't affect tab behavior
            "TAB" nil
            ;; AUCTeX already provides this functionality with `LaTeX-insert-item'
            ;; (albeit in another binding; at least was reserve this one)
            "C-<return>" nil)
  :custom
  (cdlatex-env-alist
   '(("pline" "\\pline[]{?}[]" nil)
     ("fitchproof" "\\fitchprf{\n?\n}\n{\n\n}" nil)
     ("subproof" "\\subproof{\n?\n}\n{\n\n}" nil))))

;;;; Auctex-latexmk
;; Quicker insertion and filling-out of macros. Taken from Doom
(use-package auctex-latexmk
  :after tex
  :custom
  (TeX-command-default "LatexMk")
  ;; Pass the -pdf flag when TeX-PDF-mode is active.
  (auctex-latexmk-inherit-TeX-PDF-mode t)
  :init
  ;; Add LatexMk as a TeX command
  (auctex-latexmk-setup)

  (define-minor-mode kb/auctex-latexmk-mode
    "Compiles on buffer save using LatexMk command."
    :init-value nil
    :lighter " LMk"
    (let ((cmd (lambda () (TeX-command "LatexMk" #'TeX-master-file))))
      (if kb/auctex-latexmk-mode
          (add-hook 'after-save-hook cmd nil t)
        (remove-hook 'after-save-hook cmd t)))))

;;;; Preview
;; "Seamless" embedding of generated images (i.e. preview) into LaTeX source
;; code. Taken from Doom
(use-package preview
  :ensure nil
  :after auctex
  :hook (LaTeX-mode . LaTeX-preview-setup)
  :config
  (setq-default preview-scale 1.4
                preview-scale-function
                (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale)))
  ;; Don't cache preamble, it creates issues with SyncTeX. Let users enable
  ;; caching if they have compilation times that long.
  (setq preview-auto-cache-preamble nil))

;;;; Popweb
;; Use EAF to have popups for LaTeX math and bing/youdao Chinese translations.
;; **Don't forget to install the dependencies found on the README.** (And don't
;; forget that `pipx' is an option)
(use-package popweb
  :disabled
  ;; :ensure (:type git
  ;;                :host github
  ;;                :repo "manateelazycat/popweb"
  ;;                :files (:defaults "*.py" "*.js" "extension/*/*"))
  :vc (:url "https://github.com/manateelazycat/popweb.git"
            :rev :newest)
  :hook (LaTeX-mode . popweb-latex-mode)
  :general (:keymaps '(LaTeX-mode-map org-mode-map)
                     "C-M-s-'" 'popweb-latex-show)
  :custom
  (popweb-config-location (no-littering-expand-var-file-name "popweb"))
  (popweb-popup-pos "point-bottom")
  :config
  (require 'popweb-latex))

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

(provide 'latex-general-rcp)
;;; latex-general-rcp.el ends here
