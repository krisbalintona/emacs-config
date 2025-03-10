;;; completion-inline-rcp.el --- Inline completion   -*- lexical-binding: t; -*-

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

;; Packages that configure the vanilla `completion-at-point' functionality.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Corfu
;; Faster, minimal, and more lightweight autocomplete that is more faithful to
;; the Emacs infrastructure
;;;;; Itself
(use-package corfu
  ;; :ensure (corfu :files (:defaults "extensions/*"))
  :vc ( :url "https://github.com/minad/corfu.git"
        :rev :newest)
  :hook
  ((on-first-buffer . global-corfu-mode)
   (lsp-completion-mode . kb/corfu-setup-lsp)) ; Use corfu for lsp completion
  :bind
  ( :map corfu-map
    ("M-d" . corfu-info-documentation)
    ;; ("C-M-s-SPC" . corfu-insert-separator)
    )
  :custom
  (corfu-auto nil) ; REVIEW 2024-09-20: Perhaps try https://github.com/minad/corfu?tab=readme-ov-file#auto-completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  (corfu-on-exact-match 'insert)

  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)     ; Always have the same width
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)

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
  (corfu-preview-current t)
  (corfu-preselect 'valid)
  :config
  ;; Always use a fixed-pitched font for corfu; variable pitch fonts (which will
  ;; be adopted in a variable pitch buffer) have inconsistent spacing
  (set-face-attribute 'corfu-default nil :height 0.95 :inherit 'default)

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
  (with-eval-after-load 'lsp
    (setopt lsp-completion-provider :none)) ; Use corfu instead for lsp completions
  (defun kb/corfu-setup-lsp ()
    "Use orderless completion style with lsp-capf instead of the
default lsp-passthrough."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))))

;;;;; Corfu-history
;; Save the history across Emacs sessions
(use-package corfu-history
  :ensure nil
  :hook
  (corfu-mode . corfu-history-mode)
  :config
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;;;;; Corfu-popupinfo
;; Documentation window for corfu!
(use-package corfu-popupinfo
  :ensure nil
  :hook
  (corfu-mode . corfu-popupinfo-mode)
  :bind
  ( :map corfu-map
    ([remap corfu-info-documentation] . corfu-popupinfo-toggle)
    ("M-l" . corfu-popupinfo-location))
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.3))
  (corfu-popupinfo-direction '(right left vertical))
  (corfu-popupinfo-hide t)
  (corfu-popupinfo-resize t)
  (corfu-popupinfo-max-height 20)
  (corfu-popupinfo-max-width 70)
  (corfu-popupinfo-min-height 1)
  (corfu-popupinfo-min-width 30))

;;;;; Corfu-prescient
(use-package corfu-prescient
  :after (prescient corfu)
  :custom
  (corfu-prescient-completion-styles '(prescient flex))
  (corfu-prescient-enable-filtering nil) ; We want orderless to do the filtering
  (corfu-prescient-enable-sorting t)
  (corfu-prescient-override-sorting nil)
  ;; Only set if `corfu-prescient-enable-filtering' is non-nil. See also
  ;; `prescient--completion-recommended-overrides'
  (corfu-prescient-completion-category-overrides
   '(;; Include `partial-completion' to enable wildcards and partial paths.
     (file (styles partial-completion prescient))
     ;; Eglot forces `flex' by default.
     (eglot (styles prescient flex))))
  :init
  (with-eval-after-load 'corfu
    (corfu-prescient-mode 1)))

;;;; Kind-icon
;; Icons for corfu!
(use-package kind-icon
  :after corfu
  :demand
  :hook (kb/themes . (lambda ()
                       "Add hook to reset cache so the icon colors match my theme"
                       (call-interactively 'kind-icon-reset-cache)))
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
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;;; Nerd-icons-corfu
;; Use nerd-icons with corfu
(use-package nerd-icons-corfu
  :disabled ; Deprecated because I've created a setup with kind-icon that pulls nerd-icon icons
  :after corfu
  :demand
  :autoload nerd-icons-corfu-formatter
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;;; Cape
;; Expand capf functionality with corfu! See an updated list of the defined capf
;; functions in the package's commentary.
(use-package cape
  :bind
  (("C-M-s-c p" . completion-at-point)
   ("C-M-s-c d" . cape-dabbrev)
   ("C-M-s-c h" . cape-history)
   ("C-M-s-c f" . cape-file)
   ("C-M-s-c k" . cape-keyword)
   ("C-M-s-c s" . cape-elisp-symbol)
   ("C-M-s-c a" . cape-abbrev)
   ("C-M-s-c w" . cape-dict)
   ("C-M-s-c l" . cape-line)
   ("C-M-s-c \\" . cape-tex)
   ("C-M-s-c _" . cape-tex)
   ("C-M-s-c ^" . cape-tex)
   ("C-M-s-c &" . cape-sgml)
   ("C-M-s-c r" . cape-rfc1345)
   ([remap dabbrev-completion] . cape-dabbrev))
  :custom
  (cape-dabbrev-min-length 2)
  :init
  ;; These are added to the global definition of
  ;; `completion-at-point-functions', which acts as a fallback if buffer-local
  ;; values end in `t'. Read (info "(cape) Configuration") for an explanation.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)

  ;; Macro to help adding capfs via hooks
  (defmacro kb/cape-setup-capfs (label hooks capfs)
    "Set up `completion-at-point-functions' for HOOKS.
CAPFS are a list of `completion-at-point-functions'. Adds CAPFS when a
hook in HOOKS is run. These effects are added by a defined function with
LABEL appended to `kb/cape-setup-capfs-'.

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
                 (defun ,(intern (concat "kb/cape-setup-capfs-" label)) ()
                   (dolist (capf (reverse ,capfs))
                     (add-to-list 'completion-at-point-functions capf))))))

  (kb/cape-setup-capfs
    "elisp"
    '(emacs-lisp-mode-hook lisp-interaction-mode-hook)
    (list #'cape-file #'cape-elisp-symbol))

  (kb/cape-setup-capfs
    "commit"
    '(git-commit-mode-hook vc-git-log-edit-mode-hook)
    (list #'cape-elisp-symbol #'cape-dabbrev)))

(provide 'completion-inline-rcp)
;;; completion-inline-rcp.el ends here
