;; -*- lexical-binding: t; -*-

;;; Vertico

;;;; Vertico-buffer
(use-package vertico-buffer
  :requires vertico
  :ensure nil
  :custom
  (vertico-buffer-hide-prompt nil)
  (vertico-buffer-display-action '(display-buffer-reuse-window)))

;;;; Vertico-prescient
(use-package vertico-prescient
  :requires vertico
  :after prescient
  :custom
  ;; Sorting
  (vertico-prescient-enable-sorting t)
  (vertico-prescient-override-sorting nil) ; Don't override `display-sort-function'

  ;; Filtering. Below only applies when `vertico-prescient-enable-filtering' is
  ;; non-nil
  (vertico-prescient-enable-filtering nil) ; We want orderless to do the filtering
  (vertico-prescient-completion-styles '(prescient flex))
  ;; Only set if `vertico-prescient-enable-filtering' is non-nil. See also
  ;; `prescient--completion-recommended-overrides'
  (vertico-prescient-completion-category-overrides
   '(;; Include `partial-completion' to enable wildcards and partial paths.
     (file (styles partial-completion prescient))
     ;; Eglot forces `flex' by default.
     (eglot (styles prescient flex))))
  :config
  (vertico-prescient-mode 1))

;;; Corfu

;;;;; Corfu-history
;; Save the history across Emacs sessions
(use-package corfu-history
  :disabled t                    ; 2025-04-05: I use corfu-prescient for sorting
  :ensure nil
  :hook (corfu-mode . corfu-history-mode)
  :config
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;;;;; Corfu-popupinfo
;; Documentation window for corfu!
(use-package corfu-popupinfo
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :bind ( :map corfu-map
          ([remap corfu-info-documentation] . corfu-popupinfo-toggle)
          ("M-l" . corfu-popupinfo-location))
  :custom
  (corfu-popupinfo-delay '(nil . 0.3))  ; Don't display initially
  (corfu-popupinfo-direction '(right left vertical))
  (corfu-popupinfo-hide t)
  (corfu-popupinfo-resize t)
  (corfu-popupinfo-max-height 70)
  (corfu-popupinfo-max-width 80)
  (corfu-popupinfo-min-height 1)
  (corfu-popupinfo-min-width 25))

;;;;; Corfu-prescient
(use-package corfu-prescient
  :demand t
  :after corfu prescient
  :custom
  ;; Sorting
  (corfu-prescient-enable-sorting t)
  (corfu-prescient-override-sorting nil) ; Don't override `display-sort-function'

  ;; Filtering.  Below only applies when `corfu-prescient-enable-filtering' is
  ;; non-nil
  (corfu-prescient-enable-filtering nil) ; We want orderless to do the filtering
  (corfu-prescient-completion-styles '(prescient flex))
  ;; See also `prescient--completion-recommended-overrides'
  (corfu-prescient-completion-category-overrides
   '(;; Include `partial-completion' to enable wildcards and partial paths.
     (file (styles partial-completion prescient))
     ;; Eglot forces `flex' by default.
     (eglot (styles prescient flex))))
  :config
  (corfu-prescient-mode 1))

;;; Kind-icon
;; Icons for corfu! An alternative is nerd-icons-corfu for specifically nerd
;; icons.
(use-package kind-icon
  :requires corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; To unify background color
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  (kind-icon-default-style
   '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 0.9))
  ;; Use VSCode's icons (i.e. nerd icons' codicons set). Read about it from my
  ;; write-up in the kind-icon wiki here:
  ;; https://github.com/jdtsmith/kind-icon/wiki#using-svg-icons-from-a-nerd-fonts-icon-collection
  (kind-icon-mapping
   '((array          "a"   :icon "symbol-array"       :face font-lock-type-face              :collection "nerd-fonts-codicons")
     (boolean        "b"   :icon "symbol-boolean"     :face font-lock-builtin-face           :collection "nerd-fonts-codicons")
     (color          "#"   :icon "symbol-color"       :face success                          :collection "nerd-fonts-codicons")
     (command        "cm"  :icon "chevron-right"      :face default                          :collection "nerd-fonts-codicons")
     (constant       "co"  :icon "symbol-constant"    :face font-lock-constant-face          :collection "nerd-fonts-codicons")
     (class          "c"   :icon "symbol-class"       :face font-lock-type-face              :collection "nerd-fonts-codicons")
     (constructor    "cn"  :icon "symbol-method"      :face font-lock-function-name-face     :collection "nerd-fonts-codicons")
     (enum           "e"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "nerd-fonts-codicons")
     (enummember     "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "nerd-fonts-codicons")
     (enum-member    "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "nerd-fonts-codicons")
     (event          "ev"  :icon "symbol-event"       :face font-lock-warning-face           :collection "nerd-fonts-codicons")
     (field          "fd"  :icon "symbol-field"       :face font-lock-variable-name-face     :collection "nerd-fonts-codicons")
     (file           "f"   :icon "symbol-file"        :face font-lock-string-face            :collection "nerd-fonts-codicons")
     (folder         "d"   :icon "folder"             :face font-lock-doc-face               :collection "nerd-fonts-codicons")
     (function       "f"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "nerd-fonts-codicons")
     (interface      "if"  :icon "symbol-interface"   :face font-lock-type-face              :collection "nerd-fonts-codicons")
     (keyword        "kw"  :icon "symbol-keyword"     :face font-lock-keyword-face           :collection "nerd-fonts-codicons")
     (macro          "mc"  :icon "lambda"             :face font-lock-keyword-face)
     (magic          "ma"  :icon "lightbulb-autofix"  :face font-lock-builtin-face           :collection "nerd-fonts-codicons")
     (method         "m"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "nerd-fonts-codicons")
     (module         "{"   :icon "file-code-outline"  :face font-lock-preprocessor-face)
     (numeric        "nu"  :icon "symbol-numeric"     :face font-lock-builtin-face           :collection "nerd-fonts-codicons")
     (operator       "op"  :icon "symbol-operator"    :face font-lock-comment-delimiter-face :collection "nerd-fonts-codicons")
     (param          "pa"  :icon "gear"               :face default                          :collection "nerd-fonts-codicons")
     (property       "pr"  :icon "symbol-property"    :face font-lock-variable-name-face     :collection "nerd-fonts-codicons")
     (reference      "rf"  :icon "library"            :face font-lock-variable-name-face     :collection "nerd-fonts-codicons")
     (snippet        "S"   :icon "symbol-snippet"     :face font-lock-string-face            :collection "nerd-fonts-codicons")
     (string         "s"   :icon "symbol-string"      :face font-lock-string-face            :collection "nerd-fonts-codicons")
     (struct         "%"   :icon "symbol-structure"   :face font-lock-variable-name-face     :collection "nerd-fonts-codicons")
     (text           "tx"  :icon "symbol-key"         :face font-lock-doc-face               :collection "nerd-fonts-codicons")
     (typeparameter  "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "nerd-fonts-codicons")
     (type-parameter "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "nerd-fonts-codicons")
     (unit           "u"   :icon "symbol-ruler"       :face font-lock-constant-face          :collection "nerd-fonts-codicons")
     (value          "v"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "nerd-fonts-codicons")
     (variable       "va"  :icon "symbol-variable"    :face font-lock-variable-name-face     :collection "nerd-fonts-codicons")
     (t              "."   :icon "question"           :face font-lock-warning-face           :collection "nerd-fonts-codicons")))
  :init
  (require 'svg-lib)
  (add-to-list 'svg-lib-icon-collections
               '("nerd-fonts-codicons" . "https://github.com/microsoft/vscode-codicons/raw/HEAD/src/icons/%s.svg"))
  :config
  ;; TODO 2022-05-24: See if I can use the cooler icons from
  ;; `lsp-bridge-icon--icons' without requiring the package
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

  ;; Reset cache on loading new theme
  (add-hook 'enable-theme-functions (lambda (_) (funcall-interactively 'kind-icon-reset-cache))))

;;; Cape
;; Expand capf functionality with corfu! See an updated list of the defined capf
;; functions in the package's commentary.
(use-package cape
  :bind (("C-c C p" . completion-at-point)
         ("C-c C d" . cape-dabbrev)
         ("C-c C h" . cape-history)
         ("C-c C f" . cape-file)
         ("C-c C k" . cape-keyword)
         ("C-c C s" . cape-elisp-symbol)
         ("C-c C a" . cape-abbrev)
         ("C-c C w" . cape-dict)
         ([remap ispell-complete-word] . cape-dict)
         ("C-c C l" . cape-line)
         ("C-c C \\" . cape-tex)
         ("C-c C _" . cape-tex)
         ("C-c C ^" . cape-tex)
         ("C-c C &" . cape-sgml)
         ("C-c C r" . cape-rfc1345)
         ([remap dabbrev-completion] . cape-dabbrev))
  :custom
  (cape-dabbrev-min-length 2)
  ;; Recommended in
  ;; https://github.com/minad/corfu?tab=readme-ov-file#configuration: Emacs 30
  ;; and newer: Disable Ispell completion function.  Try `cape-dict' as an
  ;; alternative.
  (text-mode-ispell-word-completion nil)
  :init
  ;; These are added to the global definition of
  ;; `completion-at-point-functions', which acts as a fallback if buffer-local
  ;; values end in `t'. Read (info "(cape) Configuration") for an explanation.

  ;; TODO 2025-03-26: Should I just add these as separate capfs? The use for
  ;; super-capfs is described here:
  ;; (info "(cape) Super-Capf - Merging multiple Capfs")
  (defun krisb-cape-super-capf--dict-dabbrev ()
    "Super-capf of `cape-dict' and `cape-dabbrev'."
    (cape-wrap-super 'cape-dict :with 'cape-dabbrev))

  ;; Capfs added to the end of the global value of
  ;; `completion-at-point-functions'.  Consequently, they act as fallback backends.
  (dolist (capf (reverse '(cape-elisp-symbol krisb-cape-super-capf--dict-dabbrev)))
    (add-hook 'completion-at-point-functions capf 100))

  ;; Macro to help adding capfs via hooks
  (defmacro krisb-cape-setup-capfs (label hooks capfs)
    "Set up `completion-at-point-functions' for HOOKS.
CAPFS are a list of `completion-at-point-functions'. Adds CAPFS when a
hook in HOOKS is run. These effects are added by a defined function with
LABEL appended to `krisb-cape-setup-capfs-'.

The order of elements in CAPFS are the order they will appear in
`completion-at-point-functions' for that buffer. That is, the first
element in CAPFS will be the first element in
`completion-at-point-functions'.

This macro does not affect capfs already in
`completion-at-point-functions' nor how later capfs are added to
`completion-at-point-functions'."
    (declare (indent 0))
    `(dolist (hook ,hooks)
       (add-hook hook
                 (defun ,(intern (concat "krisb-cape-setup-capfs-" label)) ()
                   (dolist (capf (reverse ,capfs))
                     (add-hook 'completion-at-point-functions capf -50 t))))))

  (krisb-cape-setup-capfs
    "elisp"
    '(emacs-lisp-mode-hook lisp-interaction-mode-hook)
    (list #'cape-file #'cape-elisp-symbol))

  (krisb-cape-setup-capfs
    "commit"
    '(git-commit-setup-hook log-edit-mode-hook)
    (list #'cape-elisp-symbol #'cape-dabbrev))

  (krisb-cape-setup-capfs
    "shells"
    '(eshell-mode-hook comint-mode-hook)
    (list #'cape-file #'cape-history))
  :config
  ;; Use enchant en_US dictionary
  (with-eval-after-load 'jinx
    (setopt cape-dict-file
            (list (expand-file-name "enchant/en_US.dic" (xdg-config-home)))))

  ;; Resolve the undesirable behavior of `cape-elisp-symbol' and the *Help*
  ;; buffer described in
  ;; https://github.com/minad/corfu/discussions/504#discussioncomment-12592545.
  (defun krisb-corfu-popupinfo--doc-buffer (str)
    "Wrapper around `elisp--company-doc-buffer'.
This function is a replacement for `elisp--company-doc-buffer', which
normally returns the main Help buffer (returned by `help-buffer').
Instead, this function returns a separate buffer to use as the Help
buffer.

Accepts the same argument as `elisp--company-doc-buffer' (STR).

Meant to be used with `cape-capf-properties' on the `cape-elisp-symbol'
completion at point function.  This ameliorates the sometimes
undesirable issue described in
https://github.com/minad/corfu/discussions/504#discussioncomment-12592545.

This solution was taken from the suggestion of
https://github.com/minad/corfu/discussions/504#discussioncomment-12593463."
    (let* ((help-xref-following t)
           (new-help-buf-name
            "*corfu-popupinfo documentation*")
           (new-help-buf (get-buffer-create new-help-buf-name)))
      (with-current-buffer new-help-buf
        (help-mode)
        (elisp--company-doc-buffer str))))

  (defun krisb-cape-elisp--around-advice (orig-fun &rest _args)
    "Advice to use a different doc buffer for documentation.
This solution was taken from the suggestion of
https://github.com/minad/corfu/discussions/504#discussioncomment-12593463."
    (cape-wrap-properties orig-fun :company-doc-buffer #'krisb-corfu-popupinfo--doc-buffer))

  (dolist (capf '(cape-elisp-symbol elisp-completion-at-point))
    (advice-add capf :around #'krisb-cape-elisp--around-advice))

  ;; NOTE 2025-03-26: The below does not apply because I've set
  ;; `text-mode-ispell-word-completion' to nil.  I've left it here for future
  ;; reference and just in case I revert the value to 'completion-at-point.
  ;; Resolve `ispell-completion-at-point' error when there is no dictionary
  ;; available
  (defun krisb-cape-ispell--around-advice (orig-fun &rest _args)
    "Advice to remove an error from missing ispell dictionary.
There is an error when using `ispell-completion-at-point' without a
dictionary.  The error is this:

(error \"ispell-lookup-words: No plain word-list found at systemdefault locations.  Customize ‘ispell-alternate-dictionary’ to set yours.\")

ORIG-FUN should be `ispell-completion-at-point'."
    (cape-wrap-silent orig-fun))

  (advice-add 'ispell-completion-at-point :around #'krisb-cape-ispell--around-advice)

  ;; Make eglot's capf non-exclusive
  (with-eval-after-load 'eglot
    (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)))

;;; Krisb-auto-completion
(use-package krisb-auto-completion
  :ensure nil
  ;; 2025-04-08: I didn't end up liking this behavior.  Might revisit this in
  ;; the future; keeping here for reference, if anything.
  ;; :hook ((log-edit-mode eval-expression-minibuffer-setup) . krisb-auto-completion-mode)
  )

;;; Nerd-icons-completion
;; Use nerd-icons in completing-read interfaces. An alternative would be
;; all-the-icons-completion which uses all-the-icons -- I prefer nerd-icons.
(use-package nerd-icons-completion
  :demand t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode 1))

;;; Prescient
;; Sorting and filtering of minibuffer candidates. The difference between
;; `orderless' and this package is that `orderless' filters but does not sort -
;; it leaves that up to the "candidate source and the completion UI."
;; Additionally, `orderless' has style "dispatchers," i.e., I can define
;; predicates for what filtering style to use for which token
(use-package prescient
  :custom
  ;; (completion-styles '(prescient flex))
  ;; NOTE 2024-02-03: Flex is chosen as a backup in case nothing in prescient is
  ;; matched, which only happens if I'm clueless about what I'm searching for.
  ;; We prefer this over adding the fuzzy matching in `prescient-filter-method'
  ;; because we don't want a bunch of random results included in the filtered
  ;; prescient results and cluttering it
  (prescient-filter-method '(literal initialism regexp))
  (prescient-aggressive-file-save t)
  (prescient-sort-length-enable nil)
  (prescient-sort-full-matches-first t)
  (prescient-history-length 200)
  (prescient-frequency-decay 0.997)
  (prescient-frequency-threshold 0.05)
  :config
  (prescient-persist-mode 1))

;;; Provide
(provide 'krisb-completion)
