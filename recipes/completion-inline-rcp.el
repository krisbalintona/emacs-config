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

;;; Corfu
;; Faster, minimal, and more lightweight autocomplete that is more faithful to
;; the Emacs infrastructure
(use-package corfu
  :hook (lsp-completion-mode . kb/corfu-setup-lsp) ; Use corfu for lsp completion
  :general
  (:keymaps 'corfu-map
            :states 'insert
            "C-n" #'corfu-next
            "C-p" #'corfu-previous
            "<escape>" #'corfu-quit
            "<return>" #'corfu-insert
            "H-SPC" #'corfu-insert-separator
            "SPC" #'corfu-insert-separator
            "M-d" #'corfu-show-documentation
            "C-g" #'corfu-quit
            "M-l" #'corfu-show-location)
  :custom
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)      ; Always show candidates in menu

  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)

  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)     ; Always have the same width
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)

  (corfu-echo-documentation nil)        ; Already use corfu-doc
  (corfu-separator ?\s)                 ; Necessary for use with orderless
  (corfu-quit-no-match 'separator)

  (corfu-preview-current 'insert)       ; Preview current candidate?
  (corfu-preselect-first t)             ; Preselect first candidate?

  ;; Other
  (lsp-completion-provider :none)       ; Use corfu instead for lsp completions
  :init
  (corfu-global-mode)
  :config
  ;; NOTE 2022-03-01: See
  ;; https://github.com/minad/corfu/issues/12#issuecomment-869037519
  ;; (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  ;; (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  (evil-make-overriding-map corfu-map)
  (general-add-advice '(corfu--setup corfu--teardown) :after 'evil-normalize-keymaps)

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
  (defun kb/corfu-setup-lsp ()
    "Use orderless completion style with lsp-capf instead of the
default lsp-passthrough."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))))

;;; Kind-icon
;; Icons for corfu!
(use-package kind-icon
  :demand t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; To unify background color
  (kind-icon-blend-background nil)  ; Mix foreground and background ("blended")?
  (kind-icon-blend-frac 0.08)

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
  :hook (corfu-mode . corfu-doc-mode)
  :general (:keymaps 'corfu-map
                     [remap corfu-show-documentation] #'corfu-doc-toggle
                     ;; Scroll in the documentation window
                     "M-n" #'corfu-doc-scroll-up
                     "M-p" #'corfu-doc-scroll-down)
  :custom
  (corfu-doc-delay 0.5)
  (corfu-doc-max-width 70)
  (corfu-doc-max-height 20))

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
  :custom
  (cape-dabbrev-min-length 3)
  :init
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
          #'elisp-completion-at-point)
    (add-to-list 'completion-at-point-functions #'cape-symbol)
    ;; I prefer this being early/first in the list
    (add-to-list 'completion-at-point-functions #'cape-file)
    (require 'company-yasnippet)
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet)))

  ;; LSP
  (defun kb/cape-capf-setup-lsp ()
    "Replace the default `lsp-completion-at-point' with its
`cape-capf-buster' version. Also add `cape-file' and
`company-yasnippet' backends."
    (setf (elt (cl-member 'lsp-completion-at-point completion-at-point-functions) 0)
          (cape-capf-buster #'lsp-completion-at-point))
    ;; TODO 2022-02-28: Maybe use `cape-wrap-predicate' to have candidates
    ;; listed when I want?
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet))
    (add-to-list 'completion-at-point-functions #'cape-dabbrev t))

  ;; Org
  (defun kb/cape-capf-setup-org ()
    (require 'org-roam)
    (if (org-roam-file-p)
        (org-roam--register-completion-functions-h)
      (let (result)
        (dolist (element (list
                          (cape-super-capf #'cape-ispell #'cape-abbrev)
                          (cape-company-to-capf #'company-yasnippet))
                         result)
          (add-to-list 'completion-at-point-functions element)))
      ))

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
        (add-to-list 'completion-at-point-functions element))))

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
        (add-to-list 'completion-at-point-functions element))))


  ;; Sh
  (defun kb/cape-capf-setup-sh ()
    (require 'company-shell)
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-shell)))
  :config
  ;; For pcomplete. For now these two advices are strongly recommended to
  ;; achieve a sane Eshell experience. See
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-shell-or-eshell

  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure that pcomplete does not write to the buffer and behaves as a pure
  ;; `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

;;; completion-inline-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-inline-rcp)
