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
;;;; Itself
(use-package corfu
  :elpaca (corfu :files (:defaults "extensions/*"))
  :hook (lsp-completion-mode . kb/corfu-setup-lsp) ; Use corfu for lsp completion
  :general
  (:keymaps 'corfu-map
            "M-d" 'corfu-info-documentation
            ;; "H-SPC" 'corfu-insert-separator
            )
  (:keymaps 'corfu-map
            :states 'insert
            "C-n" 'corfu-next
            "C-p" 'corfu-previous
            "<escape>" 'corfu-quit
            "<return>" 'corfu-insert
            "H-SPC" 'corfu-insert-separator
            ;; "SPC" 'corfu-insert-separator ; Use when `corfu-quit-at-boundary' is non-nil
            "M-d" 'corfu-show-documentation
            "C-g" 'corfu-quit
            "M-l" 'corfu-show-location)
  :custom
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  (tab-always-indent 'complete)
  (tab-first-completion 'word)

  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  (corfu-on-exact-match nil)         ; Don't do anything fancy for exact matches

  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)     ; Always have the same width
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)
  (completion-cycle-threshold nil)

  ;; `nil' means to ignore `corfu-separator' behavior, that is, use the older
  ;; `corfu-quit-at-boundary' = nil behavior. Set this to separator if using
  ;; `corfu-auto' = `t' workflow (in that case, make sure you also set up
  ;; `corfu-separator' and a keybind for `corfu-insert-separator', which my
  ;; configuration already has pre-prepared). Necessary for manual corfu usage with
  ;; orderless, otherwise first component is ignored, unless `corfu-separator'
  ;; is inserted.
  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s)            ; Use space
  (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
  (corfu-preview-current 'insert)  ; Preview first candidate. Insert on input if only one
  (corfu-preselect-first t)        ; Preselect first candidate?

  ;; Other
  (corfu-echo-documentation nil)        ; Already use corfu-doc
  (lsp-completion-provider :none)       ; Use corfu instead for lsp completions
  :init
  (global-corfu-mode)
  :config
  ;; NOTE 2022-03-01: This allows for a more evil-esque way to have
  ;; `corfu-insert-separator' work with space in insert mode without resorting to
  ;; overriding keybindings with `general-override-mode-map'. See
  ;; https://github.com/minad/corfu/issues/12#issuecomment-869037519
  ;; Alternatively, add advice without `general.el':
  ;; (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  ;; (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  (when (featurep 'evil)
    (general-add-advice '(corfu--setup corfu--teardown) :after 'evil-normalize-keymaps)
    (evil-make-overriding-map corfu-map))

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

;;;; Corfu-history
;; Save the history across Emacs sessions
(use-package corfu-history
  :elpaca nil
  :ghook 'corfu-mode-hook
  :config
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;;;; Corfu-popupinfo
;; Documentation window for corfu!
(use-package corfu-popupinfo
  :elpaca nil
  :ghook 'corfu-mode-hook
  :general
  (:keymaps 'corfu-mode-map
            "H-d" 'corfu-popupinfo-toggle
            "H-l" 'corfu-popupinfo-location)
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.3))
  (corfu-popupinfo-direction '(right left vertical))
  (corfu-popupinfo-hide t)
  (corfu-popupinfo-resize t)
  (corfu-popupinfo-max-height 20)
  (corfu-popupinfo-max-width 70)
  (corfu-popupinfo-min-height 1)
  (corfu-popupinfo-min-width 30))

;;; Kind-icon
;; Icons for corfu!
(use-package kind-icon
  :disabled ; Prefer `nerd-icons-corfu' with its lighter weight icons (doesn't use svgs)
  :after corfu
  ;; Add hook to reset cache so the icon colors match my theme
  :hook (kb/themes . (lambda () (call-interactively 'kind-icon-reset-cache)))
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; To unify background color
  (kind-icon-blend-background nil)  ; Mix foreground and background ("blended")?
  (kind-icon-blend-frac 0.08)
  (kind-icon-default-style ; FIXME 2024-02-03: Keyword annotations get cut off because icons too wide
   '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 1.0))
  ;; Use VSCode's icons. Taken from
  ;; https://github.com/jdtsmith/kind-icon/wiki#using-vs-code-icons-as-an-alternative
  (kind-icon-mapping
   '((array          "a"   :icon "symbol-array"       :face font-lock-type-face              :collection "vscode")
     (boolean        "b"   :icon "symbol-boolean"     :face font-lock-builtin-face           :collection "vscode")
     (color          "#"   :icon "symbol-color"       :face success                          :collection "vscode")
     (command        "cm"  :icon "chevron-right"      :face default                          :collection "vscode")
     (constant       "co"  :icon "symbol-constant"    :face font-lock-constant-face          :collection "vscode")
     (class          "c"   :icon "symbol-class"       :face font-lock-type-face              :collection "vscode")
     (constructor    "cn"  :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
     (enum           "e"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
     (enummember     "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
     (enum-member    "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
     (event          "ev"  :icon "symbol-event"       :face font-lock-warning-face           :collection "vscode")
     (field          "fd"  :icon "symbol-field"       :face font-lock-variable-name-face     :collection "vscode")
     (file           "f"   :icon "symbol-file"        :face font-lock-string-face            :collection "vscode")
     (folder         "d"   :icon "folder"             :face font-lock-doc-face               :collection "vscode")
     (function       "f"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
     (interface      "if"  :icon "symbol-interface"   :face font-lock-type-face              :collection "vscode")
     (keyword        "kw"  :icon "symbol-keyword"     :face font-lock-keyword-face           :collection "vscode")
     (macro          "mc"  :icon "lambda"             :face font-lock-keyword-face)
     (magic          "ma"  :icon "lightbulb-autofix"  :face font-lock-builtin-face           :collection "vscode")
     (method         "m"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
     (module         "{"   :icon "file-code-outline"  :face font-lock-preprocessor-face)
     (numeric        "nu"  :icon "symbol-numeric"     :face font-lock-builtin-face           :collection "vscode")
     (operator       "op"  :icon "symbol-operator"    :face font-lock-comment-delimiter-face :collection "vscode")
     (param          "pa"  :icon "gear"               :face default                          :collection "vscode")
     (property       "pr"  :icon "symbol-property"    :face font-lock-variable-name-face     :collection "vscode")
     (reference      "rf"  :icon "library"            :face font-lock-variable-name-face     :collection "vscode")
     (snippet        "S"   :icon "symbol-snippet"     :face font-lock-string-face            :collection "vscode")
     (string         "s"   :icon "symbol-string"      :face font-lock-string-face            :collection "vscode")
     (struct         "%"   :icon "symbol-structure"   :face font-lock-variable-name-face     :collection "vscode")
     (text           "tx"  :icon "symbol-key"         :face font-lock-doc-face               :collection "vscode")
     (typeparameter  "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
     (type-parameter "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
     (unit           "u"   :icon "symbol-ruler"       :face font-lock-constant-face          :collection "vscode")
     (value          "v"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
     (variable       "va"  :icon "symbol-variable"    :face font-lock-variable-name-face     :collection "vscode")
     (t              "."   :icon "question"           :face font-lock-warning-face           :collection "vscode")))
  :init
  ;; TODO 2022-05-24: See if I can use the cooler icons from
  ;; `lsp-bridge-icon--icons' without requiring the package
  (with-eval-after-load 'corfu          ; Enable
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

;;; Nerd-icons-corfu
;; Use nerd-icons with corfu
(use-package nerd-icons-corfu
  :after (nerd-icons corfu)
  :init
  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

;;; Cape
;; Expand capf functionality with corfu! See an updated list of the defined capf
;; functions in the package's commentary.
(use-package cape
  :hook ((emacs-lisp-mode .  kb/cape-capf-setup-elisp)
         (lsp-completion-mode . kb/cape-capf-setup-lsp)
         (git-commit-mode . kb/cape-capf-setup-git-commit))
  :general
  (:prefix "H-c"               ; Particular completion function
           "p" 'completion-at-point
           "d" 'cape-dabbrev
           "h" 'cape-history
           "f" 'cape-file
           "k" 'cape-keyword
           "s" 'cape-elisp-symbol
           "a" 'cape-abbrev
           "w" 'cape-dict
           "l" 'cape-line
           "\\" 'cape-tex
           "_" 'cape-tex
           "^" 'cape-tex
           "&" 'cape-sgml
           "r" 'cape-rfc1345
           "y" (cape-capf-interactive (cape-company-to-capf #'company-yasnippet)))
  ([remap dabbrev-completion] 'cape-dabbrev)
  (:keymaps 'corfu-map
            :states 'insert
            [remap evil-normal-state] '(lambda ()
                                         (interactive)
                                         (evil-normal-state)
                                         (corfu-quit)))
  :custom
  (cape-dabbrev-min-length 2)
  :init
  ;; Elisp
  (defun kb/cape-capf-setup-elisp ()
    "Replace the default `elisp-completion-at-point'
completion-at-point-function. Doing it this way will prevent
disrupting the addition of other capfs (e.g. merely setting the
variable entirely, or adding to list).

Additionally, add `cape-file' as early as possible to the list."
    (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
    ;; I prefer this being early/first in the list
    (add-to-list 'completion-at-point-functions #'cape-file))

  ;; LSP
  (defun kb/cape-capf-setup-lsp ()
    "Replace the default `lsp-completion-at-point' with its
`cape-capf-buster' version."
    (setq completion-at-point-functions
          (-replace-first 'lsp-completion-at-point (cape-capf-buster #'lsp-completion-at-point)
                          completion-at-point-functions))
    ;; TODO 2022-02-28: Maybe use `cape-wrap-predicate' to have candidates
    ;; listed when I want?
    (add-to-list 'completion-at-point-functions #'cape-dabbrev t))

  ;; Git-commit
  (defun kb/cape-capf-setup-git-commit ()
    (define-key git-commit-mode-map (kbd "<tab>") 'completion-at-point)
    (let ((result))
      (dolist (element '(cape-dabbrev cape-elisp-symbol) result)
        (add-to-list 'completion-at-point-functions element))))
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
