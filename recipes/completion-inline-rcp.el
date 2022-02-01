;;; completion-inline-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages that configure the vanilla `completion-at-point' functionality.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Dabbrev
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :general ("M-/" 'dabbrev-completion
            "C-M-/" 'dabbrev-expand)
  )

;;; Corfu
;; Faster, minimal, and more lightweight autocomplete that is more faithful to
;; the Emacs infrastructure
(use-package corfu
  :hook (lsp-completion-mode . kb/lsp-mode-setup-completion) ; Use corfu for lsp completion
  :general
  (:keymaps 'global-map
            :states 'insert
            ;; NOTE 2021-08-31: These keybinds override very annoying bindings.
            ;; This should be set later in the config (after evil) other wise
            ;; evil will overwrite those bindings
            "C-n" #'next-line                ; `corfu-next'
            "C-p" #'previous-line            ; `corfu-previous'
            "<tab>" #'indent-for-tab-command ; `completion-at-point' or `indent-relative'
            )
  (:keymaps 'corfu-map
            "<escape>" #'corfu-quit
            "<return>" #'corfu-insert
            "M-d" #'corfu-show-documentation
            "M-l" #'corfu-show-location)
  :custom
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)      ; Always show candidates in menu

  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.15)

  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-count 13)
  (corfu-cycle nil)

  (corfu-echo-documentation nil)        ; Already use corfu-doc
  (corfu-quit-at-boundary nil)          ; Necessary for orderless
  (corfu-quit-no-match 1.0) ; Quit if 0 matches, assuming completion started within this integer
  (corfu-commit-predicate 'corfu-candidate-previewed-p)

  (corfu-preview-current t)             ; Preview current candidate?
  (corfu-preselect-first t)             ; Preselect first candidate?

  ;; Other
  (lsp-completion-provider :none)       ; Use corfu instead for lsp completions
  :init
  (corfu-global-mode)
  :config
  ;; Enable Corfu more generally for every minibuffer, as long as no other
  ;; completion UI is active. If you use Mct or Vertico as your main minibuffer
  ;; completion UI. From
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)       ; Ensure auto completion is disabled
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  ;; Setup lsp to use corfu for lsp completion
  (defun kb/lsp-mode-setup-completion ()
    "Use orderless completion style with lsp-capf instead of the
default lsp-passthrough."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  )

;;; Kind-icon
;; Icons for corfu!
(use-package kind-icon
  :demand t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-blend-background nil)  ; Mix foreground and background ("blended")?
  (kind-icon-blend-frac 0.08)
  (kind-icon-default-face 'corfu-default) ; To compute blended backgrounds correctly

  ;; Svg-lib dependency
  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable

  ;; Add hook to reset cache so the icon colors match my theme
  (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache)))
  )

;;; Corfu-doc
;; Documentation window for corfu!
(use-package corfu-doc
  :straight (corfu-doc :type git :host github :repo "galeo/corfu-doc")
  :after corfu
  :general (:keymaps 'corfu-map
                     [remap corfu-show-documentation] #'corfu-doc-toggle
                     ;; Scroll in the documentation window
                     "M-n" #'corfu-doc-scroll-up
                     "M-p" #'corfu-doc-scroll-down
                     )
  :custom
  (corfu-doc-delay 1.2)
  (corfu-doc-max-width 70)
  (corfu-doc-max-height 20)
  )

;;; Cape
;; Expand capf functionality with corfu!
;; Defines the following capf functions:
;; `cape-file' - File name
;; `cape-tex' - Unicode char in TeX (e.g. \hbar)
;; `cape-dabbrev' - From open buffers
;; `cape-keyword' - Programming language keyword
;; `cape-sgml' - Unicode char from Sgml entity (e.g., &alpha)
;; `cape-rfc1345' - Unicode chars using RFC 1345 mnemonics.
;; `cape-abbrev' - Complete abbreviation (i.e. `add-global-abbrev', `add-mode-abbrev')
;; `cape-ispell' - Word from ispell
;; `cape-dict' - Word from dictionary file
;; `cape-symbol' - Elisp symbol
;; `cape-line' - From line in file
(use-package cape
  :hook ((emacs-lisp-mode .  kb/cape-capf-setup-elisp)
         (lsp-completion-mode . kb/cape-capf-setup-lsp)
         ((lsp-mode prog-mode text-mode) . kb/cape-capf-setup-yasnippet)
         (org-mode . kb/cape-capf-setup-org)
         (eshell-mode . kb/cape-capf-setup-eshell)
         (git-commit-mode . kb/cape-capf-setup-git-commit)
         (LaTeX-mode . kb/cape-capf-setup-latex)
         (sh-mode . kb/cape-capf-setup-sh)
         )
  :general (:prefix "M-c"               ; Particular completion function
                    "p" 'completion-at-point
                    "t" 'complete-tag   ; etags
                    "d" 'cape-dabbrev   ; or dabbrev-completion
                    "f" 'cape-file
                    "k" 'cape-keyword
                    "s" 'cape-symbol
                    "a" 'cape-abbrev
                    "i" 'cape-ispell
                    "l" 'cape-line
                    "w" 'cape-dict
                    "\\"' cape-tex
                    "_" 'cape-tex
                    "^" 'cape-tex
                    "&" 'cape-sgml
                    "r" 'cape-rfc1345
                    )
  :init
  ;; Yasnippet
  (defun kb/cape-capf-setup-yasnippet ()
    (require 'company-yasnippet)
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet)))

  ;; Elisp
  (defun kb/cape-capf-ignore-keywords-elisp (cand)
    "Ignore keywords with forms that begin with \":\" (e.g.
:history)."
    (or (not (keywordp cand))
        (eq (char-after (car completion-in-region--data)) ?:)))
  (defun kb/cape-capf-setup-elisp ()
    "Replace the default `elisp-completion-at-point'
completion-at-point-function. Doing it this way will prevent
disrupting the addition of other capfs (e.g. merely setting the
variable entirely, or adding to list).

Additionally, add `cape-file' as early as possible to the list."
    (setf (elt (cl-member 'elisp-completion-at-point completion-at-point-functions) 0)
          (cape-super-capf
           #'elisp-completion-at-point
           #'tempel-complete ; Prefer this over the exact match from `tempel-expand'
           ))
    (add-to-list 'completion-at-point-functions #'cape-symbol)
    ;; I prefer this being early/first in the list
    (add-to-list 'completion-at-point-functions #'cape-file)
    (require 'company-yasnippet)
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet))
    )

  ;; LSP
  (defun kb/cape-capf-setup-lsp ()
    "Replace the default `lsp-completion-at-point' with its
`cape-capf-buster' version."
    (setf (elt (cl-member 'lsp-completion-at-point completion-at-point-functions) 0)
          (cape-capf-buster #'lsp-completion-at-point))
    (push #'cape-file completion-at-point-functions)
    (require 'company-yasnippet)
    (push (cape-company-to-capf #'company-yasnippet) completion-at-point-functions))

  ;; Org
  (defun kb/cape-capf-setup-org ()
    (let ((result))
      (dolist (element `(cape-ispell ,(cape-company-to-capf #'company-yasnippet)) result)
        (push element completion-at-point-functions))))

  ;; Eshell
  (defun kb/cape-capf-setup-eshell ()
    (let ((result))
      (dolist (element '(pcomplete-completions-at-point cape-file) result)
        (add-to-list 'completion-at-point-functions element))
      ))

  ;; Git-commit
  (defun kb/cape-capf-setup-git-commit ()
    (general-define-key
     :keymaps 'local
     :states 'insert
     "<tab>" 'completion-at-point)      ; Keybinding for `completion-at-point'
    (let ((result))
      (dolist (element '(cape-symbol) result)
        (push element completion-at-point-functions))
      ))

  ;; LaTeX
  (defun kb/cape-capf-setup-latex ()
    (require 'company-auctex)
    (let ((result))
      (dolist (element (list
                        ;; First add `company-yasnippet'
                        (cape-company-to-capf #'company-yasnippet)
                        ;; Then add `cape-tex'
                        #'cape-tex
                        ;; Then add `company-auctex' in the order it adds its
                        ;; backends.
                        (cape-company-to-capf #'company-auctex-bibs)
                        (cape-company-to-capf #'company-auctex-labels)
                        (cape-company-to-capf
                         (apply-partially #'company--multi-backend-adapter
                                          '(company-auctex-macros company-auctex-symbols company-auctex-environments))))
                       result)
        (push element completion-at-point-functions))))

  ;; Sh
  (defun kb/cape-capf-setup-sh ()
    (require 'company-shell)
    (push (cape-company-to-capf #'company-shell) completion-at-point-functions))
  )

;;; completion-inline-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-inline-rcp)
