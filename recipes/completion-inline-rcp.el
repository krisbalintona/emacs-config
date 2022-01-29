;;; completion-inline-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages that configure inline completion.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Company
;;;; Company-mode
;; Point auto-completion backend
(use-package company
  ;; NOTE 2021-08-26: Keeping this active since it is necessary to keep `:after'
  ;; and `:requires' statements happy.
  :demand t                             ; Necessary for other packages since I don't use it to load it anymore
  :after evil
  :gfhook
  'evil-normalize-keymaps
  :general
  (:keymaps 'company-active-map
            "<escape>" '(lambda () (interactive) (company-abort) (evil-normal-state))
            "<return>" '(lambda () (interactive) (company-abort) (newline) (indent-according-to-mode))
            "<tab>" 'company-complete-selection
            "C-j" 'company-select-next-or-abort
            "C-k" 'company-select-previous-or-abort
            "C-n" 'company-select-next-or-abort
            "C-p" 'company-select-previous-or-abort)
  :custom
  (company-idle-delay 0.3)
  (company-tooltip-idle-delay 0.2)
  (company-minimum-prefix-length 1)
  (company-require-match 'never)
  (company-selection-wrap-around nil) ; Cycle?
  (company-global-modes '(not shell-mode))

  (company-show-numbers nil)
  (company-tooltip-offset-display 'lines)
  (company-tooltip-minimum-width 85)
  (company-tooltip-maximum-width 85)
  (company-tooltip-width-grow-only t) ; Don't decrease the width?
  (company-tooltip-flip-when-above t)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 12)

  (company-dabbrev-other-buffers nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)

  ;; NOTE 2021-08-22: I've set the initial backends to be minimal, removing a
  ;; lot of the backends that company initially sets. Most notably, I removed
  ;; `company-abbrev' because it slows down the performance significantly and I
  ;; don't use it.
  (company-backends '(company-bbdb company-yasnippet company-files
                                   (company-gtags company-etags company-keywords)
                                   company-capf))
  ;; NOTE 2021-08-25: Uncomment the code below if `company-box' isn't being used
  ;; (company-frontends
  ;;  '(company-preview-common-frontend ; Preview common part of candidates
  ;;    company-preview-if-just-one-frontend ; Always preview if only 1 candidate
  ;;    company-echo-metadata-frontend  ; Show symbol metadata in echo area
  ;;    company-pseudo-tooltip-unless-just-one-frontend-with-delay ; Respect `company-idle-delay'
  ;;    ))
  :init (require 'company-autoloads) ; Make sure all company-backends are loaded
  :config
  ;; (global-company-mode)                 ; Disabled for now

  ;; Make `company-backends' buffer-local so that I can configure the enabled
  ;; backends based on major-mode rather than adding every backend to the global
  ;; list.
  (make-variable-buffer-local 'company-backends)
  )

;;;; Company-prescient
(use-package company-prescient
  :ghook 'company-prescient-mode
  :custom
  (company-prescient-sort-length-enable nil) ; Don't sort by length since some backends already do this in the background
  )

;;;; Company-box
;; A pretty company autocomplete frontend that also displays candidate
;; documentation
(use-package company-box
  :after (company all-the-icons)
  :ghook 'company-mode-hook
  :custom
  (company-box-show-single-candidate 'always)
  (company-box-backends-colors nil)
  (company-box-max-candidates company-tooltip-limit)
  (company-box-doc-delay 0.4)
  (company-box-scrollbar nil)
  (company-box-frame-behavior 'default)   ; Follow point horizontally as I type?
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  (company-box-icons-all-the-icons
   (let ((all-the-icons-scale-factor 0.8))
     `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
       (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
       (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
       (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
       (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
       (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
       (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
       (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
       (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
       (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
       (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
       (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
       (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
       (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
       (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
       (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
       (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
       (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
       (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
       (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
       (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
       (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
       (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
       (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
       (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
       (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
       (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))
       (ElispFunction . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
       (ElispVariable . ,(all-the-icons-material "check_circle"             :face 'all-the-icons-blue))
       (ElispFeature  . ,(all-the-icons-material "stars"                    :face 'all-the-icons-orange))
       (ElispFace     . ,(all-the-icons-material "format_paint"             :face 'all-the-icons-pink)))
     ))
  :config
  ;; (with-no-warnings
  ;;   ;; Prettify icons
  ;;   (defun my-company-box-icons--elisp (candidate)
  ;;     (when (derived-mode-p 'emacs-lisp-mode)
  ;;       (let ((sym (intern candidate)))
  ;;         (cond ((fboundp sym) 'Function)
  ;;               ((featurep sym) 'Module)
  ;;               ((facep sym) 'Color)
  ;;               ((boundp sym) 'Variable)
  ;;               ((symbolp sym) 'Text)
  ;;               (t . nil)))))
  ;;   (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp))
  (delq 'company-echo-metadata-frontend company-frontends) ; Redundant with `company-box-doc'
  )

;;; For the built-in `completion-at-point'
;;;; Corfu
;; Faster, minimal, and more lightweight autocomplete that is more faithful to
;; the Emacs infrastructure
(use-package corfu
  :demand t
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
  :config
  (corfu-global-mode)

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

;;;; Kind-icon
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
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  )

;;;; Corfu-doc
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

;;;; Cape
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
    )

  ;; LSP
  (defun kb/cape-capf-setup-lsp ()
    "Replace the default `lsp-completion-at-point' with its
`cape-capf-buster' version. Additionally, add `cape-file' to the
list of capfs."
    (setq-local completion-at-point-functions
                (list #'cape-file
                      (cape-capf-buster #'lsp-completion-at-point)
                      )))

  ;; Org
  (defun kb/cape-capf-setup-org ()
    (let ((result))
      (dolist (element '(cape-ispell tempel-complete) result)
        (push element completion-at-point-functions))
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
        (push element completion-at-point-functions))
      ))
  )

;;;; Custom completions
(autoload 'ffap-file-at-point "ffap")
(defun kb/complete-path-at-point ()
  "Return completion data for UNIX path at point."
  (let ((fn (ffap-file-at-point))
        (fap (thing-at-point 'filename)))
    (when (and (or fn (equal "/" fap))
               (save-excursion
                 (search-backward fap (line-beginning-position) t)))
      (list (match-beginning 0)
            (match-end 0)
            #'completion-file-name-table :exclusive 'no))))
(add-to-list 'completion-at-point-functions #'kb/complete-path-at-point)

;;;; Dabbrev
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :general ("M-/" 'dabbrev-completion
            "C-M-/" 'dabbrev-expand)
  )

;;; Expansion
;;;; Yasnippet
;; Template-expansion system (doesn't include templates)
(use-package yasnippet
  :hook (window-setup . yas-global-mode)
  )

;;;; Doom-snippets
;; Large library of snippet templates
(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippts :type git :host github :repo "hlissner/doom-snippets")
  :config (doom-snippets-initialize)
  )

;;;; Org-tempo
;; Completion for org-block types. Adds to the functionality of `org-structure'.
(use-package org-tempo
  :straight nil
  :defer 5
  :config
  (dolist (expansion '(;; ("sc" . "src scheme")
                       ;; ("ts" . "src typescript")
                       ;; ("yaml" . "src yaml")
                       ;; ("json" . "src json")
                       )
                     org-structure-template-alist)
    (push expansion org-structure-template-alist))
  )

;;; completion-inline-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-inline-rcp)
