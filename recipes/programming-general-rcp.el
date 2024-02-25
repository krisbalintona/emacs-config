;;; programming-general-rcp.el --- General programming stuff  -*- lexical-binding: t; -*-

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

;; Language-agnostic packages helpful or required for programming.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Electric
(use-package electric
  :ensure nil
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-default-inhibit)
  (electric-quote-comment nil)
  (electric-quote-string nil)
  (electric-quote-context-sensitive t)
  (electric-quote-replace-double t)
  (electric-quote-inhibit-functions nil)
  :init
  (electric-pair-mode))

;;;; Eldoc
(use-package eldoc
  :diminish
  :custom
  (eldoc-print-after-edit nil)
  (eldoc-idle-delay 0.2)
  (eldoc-documentation-strategy
   'eldoc-documentation-compose-eagerly) ; Mash multiple sources together and display eagerly
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit) ; Also respects `max-mini-window-height'
  (eldoc-echo-area-display-truncation-message t)
  (eldoc-echo-area-prefer-doc-buffer t))

;;;; Eldoc-box
(use-package eldoc-box
  :disabled                             ; Only intrusive
  :diminish eldoc-box-hover-mode
  ;; :hook (eldoc-mode . eldoc-box-hover-mode)
  :general
  ([remap eldoc-doc-buffer] 'eldoc-box-help-at-point)
  (:keymaps 'eglot-mode-map
            [remap eldoc-box-help-at-point] 'eldoc-box-eglot-help-at-point)
  :custom
  (eldoc-box-max-pixel-width 650)
  (eldoc-box-max-pixel-height 400)
  (eldoc-box-cleanup-interval 0.5)
  (eldoc-box-only-multi-line t)
  (eldoc-box-fringe-use-same-bg t)
  (eldoc-box-self-insert-command-list
   '(self-insert-command outshine-self-insert-command))
  :config
  ;; Workaround for many hyphen characters wrapping in an ugly way in
  ;; `eldoc-box' frame
  (defun kb/eglot--format-markup (markup)
    "Format MARKUP according to LSP's spec."
    (pcase-let ((`(,string ,mode)
                 (if (stringp markup) (list markup 'gfm-view-mode)
                   (list (plist-get markup :value)
                         (pcase (plist-get markup :kind)
                           ("markdown" 'gfm-view-mode)
                           ("plaintext" 'text-mode)
                           (_ major-mode))))))
      (with-temp-buffer
        (setq-local markdown-fontify-code-blocks-natively t)

        ;; In markdown, replace the horizontal rule, which is three hyphens in
        ;; the markup, with X number of hyphens-like characters, with X being
        ;; enough to cover the width of `eldoc-box-max-pixel-width'. We can't
        ;; simply replace with more hyphens since `gfm-view-mode' renders any
        ;; set of three hyphens as a horizontal rule
        (setq string (string-replace
                      "---"
                      (make-string (floor (/ eldoc-box-max-pixel-width (window-font-width))) ?⎺)
                      string))

        (insert string)
        (delete-trailing-whitespace) ; Also remove trailing whitespace while we're here
        (let ((inhibit-message t)
              (message-log-max nil)
              match)
          (ignore-errors (delay-mode-hooks (funcall mode)))
          (font-lock-ensure)
          (goto-char (point-min))
          (let ((inhibit-read-only t))
            (when (fboundp 'text-property-search-forward) ;; FIXME: use compat
              (while (setq match (text-property-search-forward 'invisible))
                (delete-region (prop-match-beginning match)
                               (prop-match-end match)))))
          (string-trim (buffer-string))))))
  (advice-add 'eglot--format-markup :override #'kb/eglot--format-markup))

;;;; Compile
(use-package compile
  :ensure nil
  :general ("<f5>" 'recompile)
  :custom
  (compilation-scroll-output 'first-error) ; Scroll with compile buffer
  (compilation-auto-jump-to-first-error t))

;;;; Consult
;; Counsel equivalent for default Emacs completion. It provides many useful
;; commands.
(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :general
  ("C-x B" 'consult-buffer
   "C-x r B" 'consult-bookmark)
  ;; Put remaps here
  ([remap yank-pop] 'consult-yank-pop
   [remap repeat-complex-command] 'consult-complex-command
   [remap goto-line] 'consult-goto-line
   [remap imenu] 'kb/consult-imenu-versatile
   [remap recentf-open-files] 'consult-recent-file
   [remap flymake-show-buffer-diagnostics] 'consult-flymake)
  (:keymaps 'goto-map
            ;; Uses the `M-g' prefix
            "e" 'consult-compile-error
            "f" 'consult-flymake
            "o" 'consult-outline
            "m" 'consult-mark
            "M" 'consult-global-mark
            "I" 'consult-imenu-multi)
  (:keymaps 'search-map
            ;; Uses the `M-s' prefix
            "g" 'consult-git-grep
            "G" 'consult-grep
            "r" 'consult-ripgrep
            "f" 'consult-find
            "F" 'consult-locate
            "l" 'consult-line
            "i" 'consult-info)
  (:keymaps 'consult-narrow-map "?" 'consult-narrow-help) ; Show available narrow keys
  (:keymaps 'help-map [remap apropos-command] 'consult-apropos)
  (:keymaps 'org-mode-map [remap consult-outline] 'consult-org-heading)
  (:keymaps 'comint-mode-map [remap comint-history-isearch-backward-regexp] 'consult-history)
  (:keymaps 'minibuffer-local-map
            [remap next-matching-history-element] 'consult-history
            [remap previous-matching-history-element] 'consult-history)
  :custom
  (consult-mode-histories   ; What variable consult-history looks at for history
   '((eshell-mode eshell-history-ring eshell-history-index)
     (comint-mode comint-input-ring comint-input-ring-index)
     (term-mode term-input-ring term-input-ring-index)
     (log-edit-mode log-edit-comment-ring log-edit-comment-ring-index)))
  (consult-ripgrep-args
   (concat
    "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
   --smart-case --no-heading --with-filename --line-number --search-zip"
    ;; Additional args
    " --line-number --hidden"))
  :init
  (defun kb/consult-imenu-versatile (&optional arg)
    "Call `consult-imenu'. With prefix-command ARG, call
    `consult-imenu-multi'."
    (interactive "P")
    (if arg (consult-imenu-multi) (consult-imenu)))
  :config
  ;; Use the faster locate rather than locate
  (when (executable-find "plocate")
    (setq consult-locate-args "plocate --ignore-case --existing --regexp"))

  ;; Have line centered in previews. Make sure `recenter' is called after
  ;; `consult--maybe-recenter'
  (add-to-list 'consult-after-jump-hook 'recenter t)

  ;; Customize consult commands
  (consult-customize
   ;; For `consult-*-grep'
   consult-grep :preview-key "C-M-;"
   consult-git-grep :preview-key "C-M-;"
   consult-ripgrep :preview-key "C-M-;"
   ;; For `consult-fdfind'. Make sure this is after the definition of
   ;; `consult-recent-file'
   consult-recent-file :preview-key "C-M-;"
   ;; `consult-find'
   consult-find :preview-key "C-M-;"))

;;;; Embark
;; Allow an equivalent to ivy-actions to regular complete-read minibuffers (and
;; thus selectrum!)
(use-package embark
  :commands embark-act
  :general
  ("C-." 'embark-act
   "C-h B" 'embark-bindings)
  (:keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
            [remap xref-find-definitions] 'embark-dwim) ; Check README for why it's sensible to overwrite `xref-find-definitions'
  (:keymaps 'vertico-map
            "C-." 'embark-act
            "C->" 'embark-become)
  (:keymaps 'embark-symbol-map
            "R" 'raise-sexp)
  :custom
  ;; Embark Actions menu
  (prefix-help-command 'embark-prefix-help-command) ; Use completing read when typing ? after prefix key
  (embark-prompter 'embark-keymap-prompter) ; What interface do I want to use for Embark Actions?
  (embark-indicators                    ; How the Embark Actions menu appears
   '(embark-mixed-indicator
     embark-highlight-indicator
     ;; embark-isearch-highlight-indicator
     ;; embark-verbose-indicator
     ))
  (embark-mixed-indicator-delay 1.5)

  ;; Misc
  (embark-collect-live-initial-delay 0.8)
  (embark-collect-live-update-delay 0.5)
  :config
  (add-to-list 'embark-keymap-alist '(raise-sexp . embark-symbol-map)))

;;;; Embark-consult
;; Companion package for embark
(use-package embark-consult
  :demand
  :requires (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;;; Scratch.el
;; Easily create scratch buffers for different modes
(use-package scratch
  ;; :demand t ; For the initial scratch buffer at startup
  :hook (scratch-create-buffer . kb/scratch-buffer-setup)
  :general (kb/open-keys
             "s" 'scratch)
  :preface
  (defun kb/scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly.
 Taken from
 https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore/"
    (let* ((mode (format "%s" major-mode))
           (string (concat "Scratch buffer for: " mode "\n\n")))
      (when scratch-buffer
        (save-excursion
          (insert string)
          (goto-char (point-min))
          (comment-region (point-at-bol) (point-at-eol)))
        (forward-line 2))
      (rename-buffer (concat "*Scratch for " mode "*") t))))

;;;; File or buffer utilities
;;;;; Autorevert
;; Automatically update buffers as files are externally modified
(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-interval 5)
  (auto-revert-avoid-polling t)
  (auto-revert-check-vc-info t)
  (auto-revert-verbose t)
  :init
  (global-auto-revert-mode))

;;;;; Whitespace
;; Remove whitespace on save
(use-package whitespace
  :ensure nil
  :custom
  (whitespace-style '(face empty indentation::space tab)))

;;;;; Sudo-edit
;; Utilities to edit files as root
(use-package sudo-edit
  :general (kb/file-keys
             "U" '(sudo-edit-find-file :wk "Sudo find-file")
             "u" '(sudo-edit :wk "Sudo this file"))
  :config (sudo-edit-indicator-mode))

;;;;; Hi-lock
(use-package hi-lock
  :ensure nil
  :custom
  (hi-lock-file-patterns-policy
   '(lambda (_pattern) t))
  :init
  (global-hi-lock-mode 1))

;;;;; Re-builder
;; Interactively build regexps
(use-package re-builder
  :ensure nil
  :custom
  (reb-re-syntax 'rx))

;;;; Modes
;;;;; Conf-mode
;; For Unix config files
(use-package conf-mode
  :ensure nil
  :mode ("\\.rs\\'" . conf-mode)
  :gfhook 'outshine-mode)

;;;;; Vimrc-mode
;; For editing vim/nvim config files
(use-package vimrc-mode)

;;;; Aesthetics
;;;;; Prog-mode
(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . goto-address-prog-mode)
         (window-setup . global-prettify-symbols-mode)
         (org-mode . (lambda () (setq-local prettify-symbols-compose-predicate 'kb/prettify-symbols-compose-p))))
  :init
  (defun kb/prettify-symbols-compose-p (start end _match)
    "Returns nil if the character before and after MATCH isn't a
punctuation."
    (let ((before (or (char-before start) ? ))
          (after (or (char-after end) ? )))
      (not (or (string-match-p (rx (any punct)) (char-to-string before))
               (string-match-p (rx (any punct)) (char-to-string after))))))
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (add-to-list 'prettify-symbols-alist '("..." . ?…))
              (add-to-list 'prettify-symbols-alist '("--" . ?–))
              (add-to-list 'prettify-symbols-alist '("---" . ?—))
              (add-to-list 'prettify-symbols-alist '("->" . ?➡))
              (add-to-list 'prettify-symbols-alist '("<-" . ?⬅))))
  (add-hook 'latex-mode-hook
            (lambda ()
              (add-to-list 'prettify-symbols-alist '("\\Dashv" . ?⫤))
              (add-to-list 'prettify-symbols-alist '("\\DashVDash" . ?⟚))
              (add-to-list 'prettify-symbols-alist '("\\dashVdash" . ?⊢))
              (delete '("--" . 8211) prettify-symbols-alist)
              (delete '("---" . 8212) prettify-symbols-alist)
              ;; For `lplfitch'. Slightly higher than `\vdots'. Using the
              ;; `\pline{\vdots}' results in the ellipses not being centered
              ;; on the line.
              (add-to-list 'prettify-symbols-alist '("\\ellipsesline" . ?⋮))
              ;; Circled numbers from the pifont package
              (add-to-list 'prettify-symbols-alist '("\\ding{192}" . ?①))
              (add-to-list 'prettify-symbols-alist '("\\ding{193}" . ?②))
              (add-to-list 'prettify-symbols-alist '("\\ding{194}" . ?③))
              (add-to-list 'prettify-symbols-alist '("\\ding{195}" . ?④))
              (add-to-list 'prettify-symbols-alist '("\\ding{196}" . ?⑤))
              (add-to-list 'prettify-symbols-alist '("\\ding{197}" . ?⑥))
              (add-to-list 'prettify-symbols-alist '("\\ding{198}" . ?⑦))
              (add-to-list 'prettify-symbols-alist '("\\ding{199}" . ?⑧))
              (add-to-list 'prettify-symbols-alist '("\\ding{200}" . ?⑨))
              (add-to-list 'prettify-symbols-alist '("\\ding{201}" . ?⑩))
              ;; Angle brackets for text (non-math)
              (add-to-list 'prettify-symbols-alist '("\\textlangle" . 10216))
              (add-to-list 'prettify-symbols-alist '("\\textrangle" . 10217))))
  (add-hook 'python-base-mode-hook
            (lambda ()
              (add-to-list 'prettify-symbols-alist '("->" . ?»))
              (add-to-list 'prettify-symbols-alist '("lambda" . ?λ)))))

;;;;; Hl-line
(use-package hl-line
  :ensure nil
  :ghook 'prog-mode-hook 'conf-mode-hook)

;;;;; Hl-todo
;; OPTIMIZE 2023-07-14: Also consider synergy with
;; https://codeberg.org/ideasman42/emacs-prog-face-refine
(use-package hl-todo
  :general (:keymaps 'hl-todo-mode-map
                     :prefix "M-s t"
                     "n" 'hl-todo-next
                     "p" 'hl-todo-previous
                     "o" 'hl-todo-occur)
  :custom
  (hl-todo-include-modes '(prog-mode text-mode))
  (hl-todo-text-modes '(markdown-mode text-mode))
  (hl-todo-exclude-modes nil)
  ;; TODO 2022-02-07: Change `kb-comment' such that I am able to leverage
  ;; hl-todo's punctuation highlighting.
  (hl-todo-require-punctuation nil)
  (hl-todo-highlight-punctuation "")
  :init
  (global-hl-todo-mode)
  :config
  (with-eval-after-load 'alt-comment-dwim
    ;; Make sure to have all words in `alt-comment-dwim-keywords-coding' and
    ;; `alt-comment-dwim-keywords-writing' in this list, otherwise those words will not
    ;; appear in any calls to `alt-comment-dwim-dwim'.
    (setq hl-todo-keyword-faces alt-comment-dwim-keyword-faces)))

;;;;; Ansi-color
;; Apply ANSI terminal color escape codes.
;; <http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html>
(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . endless/colorize-compilation)
  :config
  (defun endless/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point)))))

;;;;; Fancy-compilation
;; Better compilation buffers. Has the following features:
;; * Support color output.
;; * Support progress updates on a single line (as used by ninja, sphinx and many other build systems).
;; * Use scrolling behavior similar to most terminals.
;; * Optionally use foreground & background independent of theme colors.
(use-package fancy-compilation
  :custom
  (fancy-compilation-override-colors nil)
  ;; Briefer text
  (fancy-compilation-quiet-prelude t)
  (fancy-compilation-quiet-prolog t)
  :init
  (with-eval-after-load 'compile
    (fancy-compilation-mode)))

;;;;; Indent-bars
;; Show indicator for indentation levels (like in VS Code)
(use-package indent-bars
  :disabled t                           ; FIXME 2023-08-18: Causes errors I think...
  :ensure (:type git :host github :repo "jdtsmith/indent-bars")
  ;; OPTIMIZE 2023-08-15: Have to add to `after-init-hook' because of issues
  ;; with daemon
  :hook ((kb/themes . indent-bars-reset)
         ((prog-mode conf-mode) . indent-bars-mode))
  :custom
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.25)
  (indent-bars-pad-frac 0.25)
  (indent-bars-color-by-depth nil)
  (indent-bars-highlight-current-depth '(:face default :blend 0.4))
  (indent-bars-display-on-blank-lines t))

;;;;; Rainbow-mode
;; Colorify color codes
(use-package rainbow-mode
  :diminish
  :ghook 'text-mode-hook 'prog-mode-hook 'help-mode-hook)

;;;;; Highlight-defined
;; Very useful for emacs configuration! Fontify symbols. Additionally, fontify
;; text which is the symbol of a face.
(use-package highlight-defined
  :ghook 'prog-mode-hook
  :custom
  (highlight-defined-face-use-itself t))

;;;;; Highlight-quoted
;; Make (lisp) quotes and quoted symbols easier to distinguish from free variables by highlighting
;; them
(use-package highlight-quoted
  :ghook 'emacs-lisp-mode-hook)

;;;;; Paren
;; Highlight matching delimiters
(use-package paren
  :ensure nil
  :custom
  (show-paren-context-when-offscreen 'overlay)
  :init (show-paren-mode))

;;;;; Display-fill-column-indicator
(use-package display-fill-column-indicator
  :ensure nil
  :custom
  (display-fill-column-indicator-character ?│)
  :custom-face
  (fill-column-indicator ((t (:inherit line-number)))))

;;;;; Adaptive-wrap
;; Visually indent lines wrapped visually!
(use-package adaptive-wrap
  ;; NOTE 2024-02-15: This makes long-lines in lists properly indented!
  :hook ((prog-mode org-mode) . adaptive-wrap-prefix-mode))

;;;; Other
;;;;; Outshine
;; Outline-minor-mode but with better keybindings and more support.
;; `outline-minor-mode-prefix' must be set prior to the package's loading
(defvar outline-minor-mode-prefix (kbd "C-c \\"))
(use-package outshine
  :diminish (outshine-mode outline-minor-mode)
  :ghook 'LaTeX-mode-hook 'css-mode-hook 'prog-mode-hook
  :gfhook 'visual-line-mode
  :general
  (:keymaps 'outshine-mode-map
            "C-x n s" 'outshine-narrow-to-subtree)
  :custom
  (outshine-use-speed-commands t) ; Use speedy commands on headlines (or other defined locations)
  :init
  ;; More convenient `outline-insert-heading'
  (defun kb/around-outline-insert-heading (orig_fun &rest args)
    "Insert a new heading at same depth at point.

Also insert newline before if an empty line isn’t there already.

Also end in `evil-insert-state’ if `evil-local-mode’ is active in
this buffer."
    ;; Check for if previous line is empty
    (unless (save-excursion
              (widen)
              (forward-line -1)
              (looking-at-p "[[:blank:]]*$"))
      (insert "\n"))

    (apply orig_fun args)

    ;; Enter evil insert
    (when (bound-and-true-p evil-local-mode)
      (evil-insert-state)))
  (advice-add 'outline-insert-heading :around 'kb/around-outline-insert-heading))

;;;;; Anzu
;; Display search information in mode-line.
(use-package anzu
  :disabled
  :diminish
  :general ([remap query-replace] 'anzu-query-replace
            [remap query-replace-regexp] 'anzu-query-replace-regexp)
  :custom
  (anzu-cons-mode-line-p nil)
  :init
  (global-anzu-mode))

;;;;; Lorem-ipsum
;; Sample text
(use-package lorem-ipsum
  :init
  (setq-default lorem-ipsum-list-bullet "- "))

(provide 'programming-general-rcp)
;;; programming-general-rcp.el ends here
