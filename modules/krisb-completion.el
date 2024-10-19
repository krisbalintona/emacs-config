;;; Minibuffer
(use-package minibuffer
  :ensure nil
  :custom
  (completion-cycle-threshold nil)
  (completion-lazy-hilit t)             ; Lazy highlighting; added Emacs 30.1
  (completion-auto-select 'second-tab)
  (completions-max-height 20)
  (completion-ignore-case t)
  (completion-flex-nospace t)
  (minibuffer-default-prompt-format " [%s]") ; Format of portion for default value

  ;; Completions buffer
  (completions-format 'one-column)
  (completions-detailed t) ; Show more details in completion minibuffer (inspired by `marginalia')
  (completions-group t)    ; Groups; Emacs 28

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
   '((buffer (styles . (basic substring)))
     (unicode-name (styles . (basic substring)))
     (project-file (styles . (substring)))
     (xref-location (styles . (substring)))
     (info-menu (styles . (basic substring)))
     (symbol-help (styles . (basic shorthand substring)))
     (calendar-month (display-sort-function . identity))))
  (completion-category-overrides
   '((file (styles . (basic partial-completion flex))) ; Include `partial-completion' to enable wildcards and partial paths.
     (citar-candidate (styles basic substring)))))

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
  (prescient-sort-full-matches-first t)
  (prescient-history-length 200)
  (prescient-frequency-decay 0.997)
  (prescient-frequency-threshold 0.05)
  :config
  (prescient-persist-mode 1))

;;; Vertico
;;;; Itself
(use-package vertico
  :pin gnu-elpa-devel
  :bind ("C-M-s-." . vertico-repeat)
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
   '((consult-grep buffer)
     (imenu buffer)
     (buffer (vertico-sort-function . nil))
     (citar (vertico-sort-function . vertico-sort-history-alpha))))
  (vertico-multiform-commands
   '(;; I use jinx now, but I think it's better to not apply a grid layout to it
     ;; since its use of vertico-groups is useful
     ("flyspell-correct-*" grid (vertico-grid-annotate . 20))
     (pdf-view-goto-label (vertico-sort-function . nil))
     (".+-history" (vertico-sort-function . nil))))
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
  :requires prescient
  :after vertico
  :custom
  (vertico-prescient-completion-styles '(prescient flex))
  (vertico-prescient-enable-filtering nil) ; We want orderless to do the filtering
  (vertico-prescient-enable-sorting t)
  (vertico-prescient-override-sorting nil)
  ;; Only set if `vertico-prescient-enable-filtering' is non-nil. See also
  ;; `prescient--completion-recommended-overrides'
  (vertico-prescient-completion-category-overrides
   '(;; Include `partial-completion' to enable wildcards and partial paths.
     (file (styles partial-completion prescient))
     ;; Eglot forces `flex' by default.
     (eglot (styles prescient flex))))
  :config
  (vertico-prescient-mode 1))

;;; Orderless
;; Alternative and powerful completion style (i.e. filters candidates)
(use-package orderless
  :custom
  (completion-styles '(orderless flex))
  (orderless-matching-styles
   '(orderless-regexp
     orderless-prefixes
     orderless-initialism
     ;; orderless-literal
     ;; orderless-flex
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-style-dispatchers '(krisb-orderless-consult-dispatch))
  :config
  ;; Eglot forces `flex' by default.
  (add-to-list 'completion-category-overrides '(eglot (styles . (orderless flex))))

  ;; Taken from Doom
  (defun krisb-orderless-consult-dispatch (pattern _index _total)
    "Basically `orderless-affix-dispatch-alist' but with prefixes too."
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ((string-suffix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1 -1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "," pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "," pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1))))))

;;; Provide
(provide 'krisb-completion)
