;;; Minibuffer.el
(use-package minibuffer
  :ensure nil
  :hook (after-init . krisb-completion-styles-setup)
  :custom
  (completion-cycle-threshold nil)
  (completion-lazy-hilit t)             ; Lazy highlighting; added Emacs 30.1
  (completions-max-height 20)
  (completion-ignore-case t)
  (completion-flex-nospace t)
  (minibuffer-default-prompt-format " [%s]") ; Format of portion for default value

  ;; Completions buffer
  (completion-auto-help 'visible)
  (completion-auto-select 'second-tab)
  (completions-format 'one-column)
  (completions-detailed t) ; Show more details in completion minibuffer (inspired by `marginalia')
  (completions-group t)    ; Groups; Emacs 28
  (completions-sort 'historical)        ; Emacs 30.1

  ;; Category settings. A non-exhaustve list of known completion categories:
  ;; - `bookmark'
  ;; - `buffer'
  ;; - `charset'
  ;; - `coding-system'
  ;; - `color'
  ;; - `command' (e.g. `M-x')
  ;; - `customize-group'
  ;; - `environment-variable'
  ;; - `expression'
  ;; - `face'
  ;; - `file'
  ;; - `function' (the `describe-function' command bound to `C-h f')
  ;; - `info-menu'
  ;; - `imenu'
  ;; - `input-method'
  ;; - `kill-ring'
  ;; - `library'
  ;; - `minor-mode'
  ;; - `multi-category'
  ;; - `package'
  ;; - `project-file'
  ;; - `symbol' (the `describe-symbol' command bound to `C-h o')
  ;; - `theme'
  ;; - `unicode-name' (the `insert-char' command bound to `C-x 8 RET')
  ;; - `variable' (the `describe-variable' command bound to `C-h v')
  ;; - `consult-grep'
  ;; - `consult-isearch'
  ;; - `consult-kmacro'
  ;; - `consult-location'
  ;; - `embark-keybinding'
  (completion-category-defaults
   '((calendar-month (display-sort-function . identity))))
  (completion-category-overrides
   '((file (styles . (basic partial-completion flex))) ; Include `partial-completion' to enable wildcards and partial paths.
     (citar-candidate (styles basic substring flex))))
  :config
  (defun krisb-completion-styles-setup ()
    "Set up `completion-styes'."
    ;; I do this manually last because the final styles I want depend on the
    ;; packages I want enabled, and so setting this within each use-package,
    ;; independently of other use-packages, means I have to make sure various
    ;; packages are loaded after other ones so my `completion-styles' setting
    ;; isn't overridden in an undesirable way.  Instead, I opt to just set it
    ;; finally after all those packages are set.
    (setopt completion-styles (list (if (featurep 'orderless)
                                        'orderless 'basic)
                                    (if (featurep 'hotfuzz)
                                        'hotfuzz 'flex)))))

;;; Completion-preview
(use-package completion-preview
  :ensure nil
  :hook (((prog-mode log-edit-mode eval-expression-minibuffer-setup) . completion-preview-mode)
         (eshell-mode . krisb-completion-preview-mode-setup-eshell))
  :bind ( :map completion-preview-active-mode-map
          ("M-n" . completion-preview-next-candidate)
          ("M-p" . completion-preview-prev-candidate))
  :custom
  (completion-preview-ignore-case t)
  (completion-preview-minimum-symbol-length 3)
  :config
  ;; Use prescient or corfu-prescient's sorting function if they are available.
  ;; With this, the completion candidates shown by corfu align with the
  ;; completion candidate shown by `completion-preview-mode'.  The reason we use
  ;; this variable watcher is that it is an inexpensive solution to changing
  ;; `corfu-sort-function' values.
  (with-eval-after-load 'prescient
    ;; Use this as a fallback value: if `corfu-sort-function' isn't changed,
    ;; `completion-preview-sort-function' will remain
    ;; `prescient-completion-sort'
    (setopt completion-preview-sort-function #'prescient-completion-sort))
  (add-variable-watcher 'corfu-sort-function
                        (lambda (_symbol newval operation where)
                          "Match the value of `completion-preview-sort-function' to `corfu-sort-function'.
If `corfu-sort-function' is set buffer-locally, also set
`completion-preview-sort-function' buffer-locally.  Otherwise, change
the default value of `completion-preview-sort-function' accordingly.

This action only applies when the value of `corfu-sort-function' is
set (i.e., OPERATION is \\='set).  This excludes, e.g., let bindings."
                          (when (equal operation 'set)
                            (if where
                                (with-current-buffer where
                                  (setq-local completion-preview-sort-function newval))
                              (setopt completion-preview-sort-function newval)))))

  ;; Add these bespoke self-insert commands to the list of recognized preview
  ;; commands
  (dolist (command '(org-self-insert-command
                     outshine-self-insert-command))
    (add-to-list 'completion-preview-commands command))

  ;; Special settings for eshell buffers
  (defun krisb-completion-preview-mode-setup-eshell ()
    "Set specific settings in eshell buffers."
    (setq-local completion-preview-minimum-symbol-length 1)
    (completion-preview-mode 1)))

;;; Crm
(use-package crm
  :ensure nil
  :config
  ;; Add prompt indicator to `completing-read-multiple'. We display
  ;; [CRM<separator>], e.g., [CRM,] if the separator is a comma. Taken from
  ;; https://github.com/minad/vertico
  (defun krisb-crm-indicator (args)
    (cons (format "[completing-read-multiple: %s]  %s"
                  (propertize
                   (replace-regexp-in-string
                    "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                    crm-separator)
                   'face 'error)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'krisb-crm-indicator))

;;; Vertico
;;;; Itself
(use-package vertico
  :pin gnu-elpa-devel
  :demand t
  :bind (("C-c v r" . vertico-repeat)
         ("C-c v s" . vertico-suspend))
  :hook (minibuffer-setup . vertico-repeat-save)
  :custom
  (vertico-count 13)
  (vertico-resize 'grow-only)
  (vertico-cycle nil)
  :config
  (vertico-mode 1)
  (require 'krisb-vertico))

;;;; Vertico-directory
;; More convenient path modification commands
(use-package vertico-directory
  :requires vertico
  :ensure nil
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;;;; Vertico-multiform
(use-package vertico-multiform
  :requires vertico
  :ensure nil
  :custom
  (vertico-multiform-categories
   '((buffer (vertico-sort-function . nil))
     (color (vertico-sort-function . vertico-sort-history-length-alpha))
     (jinx grid
           (vertico-grid-annotate . 20)
           (vertico-grid-max-columns . 12)
           (vertico-grid-separator
            . #("    |    " 4 5 (display (space :width (1)) face (:inherit shadow :inverse-video t)))))))
  (vertico-multiform-commands
   '((pdf-view-goto-label (vertico-sort-function . nil))
     (".+-history" (vertico-sort-function . nil))
     ("^org-node-"
      (completion-styles . (orderless))
      (orderless-matching-styles . (orderless-prefixes orderless-regexp orderless-literal orderless-flex)))))
  :config
  (vertico-multiform-mode 1))

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
;; Faster, minimal, and more lightweight autocomplete that is more faithful to
;; the Emacs infrastructure
;;;;; Itself
(use-package corfu
  :demand t
  :bind (("M-i" . completion-at-point) ; For harmony with "M-i" in `completion-preview-active-mode-map'
         :map corfu-map
         ("M-d" . corfu-info-documentation)
         ("M-m" . krisb-corfu-move-to-minibuffer))
  :custom
  (corfu-auto nil)
  (corfu-preselect 'valid)
  (corfu-preview-current t)
  (corfu-on-exact-match 'insert)

  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)     ; Always have the same width
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)

  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s)            ; Use space
  (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
  :custom-face
  ;; Always use a fixed-pitched font for corfu; variable pitch fonts (which will
  ;; be adopted in a variable pitch buffer) have inconsistent spacing
  (corfu-default ((t (:inherit 'default))))
  :config
  (global-corfu-mode 1)

  ;; Enable corfu in minibuffer if `vertico-mode' is disabled.  From
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
  (defun krisb-corfu-enable-in-minibuffer-conditionally ()
    "Enable Corfu in the minibuffer if vertico is not active."
    (unless (bound-and-true-p vertico-mode)
      (setq-local corfu-auto nil)       ; Ensure auto completion is disabled
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'krisb-corfu-enable-in-minibuffer-conditionally 1)

  ;; Transfer corfu completion to the minibuffer
  (defun krisb-corfu-move-to-minibuffer ()
    "Transfer corfu completion to the minibuffer.
Taken from
https://github.com/minad/corfu?tab=readme-ov-file#transfer-completion-to-the-minibuffer."
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (add-to-list 'corfu-continue-commands #'krisb-corfu-move-to-minibuffer))

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

  (defvar krisb-cape-fallback-capfs '(krisb-cape-super-capf--dict-dabbrev)
    "Capfs added to the end of the global value of `completion-at-point-functions'.")

  (dolist (capf krisb-cape-fallback-capfs)
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
                     (add-to-list 'completion-at-point-functions capf))))))

  (krisb-cape-setup-capfs
    "elisp"
    '(emacs-lisp-mode-hook lisp-interaction-mode-hook)
    (list #'cape-file #'cape-elisp-symbol))

  (krisb-cape-setup-capfs
    "commit"
    '(git-commit-setup-hook log-edit-mode-hook)
    (list #'cape-elisp-symbol #'cape-dabbrev))
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

;;; Marginalia
;; Enable richer annotations in minibuffer (companion package of consult.el)
(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  (marginalia-field-width 80)
  (marginalia-align-offset -2)          ; Two to the left
  :config
  (marginalia-mode 1))

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

;;; Orderless
;; Alternative and powerful completion style (i.e. filters candidates)
(use-package orderless
  :custom
  (orderless-matching-styles
   '(orderless-regexp
     orderless-prefixes
     orderless-initialism
     ;; orderless-literal
     ;; orderless-flex
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  :config
  ;; Eglot forces `flex' by default.
  (add-to-list 'completion-category-overrides '(eglot (styles . (orderless flex)))))

;;; Hotfuzz
;; Faster version of the flex completion style.  Hotfuzz is a much faster
;; version of the built-in flex style.  See
;; https://github.com/axelf4/emacs-completion-bench#readme
(use-package hotfuzz)

;;; Provide
(provide 'krisb-completion)
