;;; programming-general-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Language-agnostic packages helpful or required for programming.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Aesthetics
;;;; Prog-mode
(use-package prog-mode
  :elpaca nil
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

;;;; Hl-line
(use-package hl-line
  :elpaca nil
  :ghook 'prog-mode-hook 'conf-mode-hook)

;;;; Hl-todo
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
  (hl-todo-exclude-modes '(org-mode))   ; Interferes with org todos
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

;;;; Indent-bars
;; Show indicator for indentation levels (like in VS Code)
(use-package indent-bars
  :elpaca (:type git :host github :repo "jdtsmith/indent-bars")
  ;; OPTIMIZE 2023-08-15: Have to add to `after-init-hook' because of issues
  ;; with daemon
  :hook (elpaca-after-init . (lambda ()
                               (dolist (hook '(prog-mode-hook conf-mode-hook))
                                 (add-hook hook 'indent-bars-mode))))
  :custom
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.25)
  (indent-bars-pad-frac 0.25)
  (indent-bars-color-by-depth nil)
  (indent-bars-highlight-current-depth '(:face default :blend 0.4))
  (indent-bars-display-on-blank-lines t))

;;;; Rainbow-mode
;; Colorify color codes
(use-package rainbow-mode
  :diminish
  :ghook 'text-mode-hook 'prog-mode-hook 'help-mode-hook)

;;;; Highlight-defined
;; Very useful for emacs configuration! Fontify symbols. Additionally, fontify
;; text which is the symbol of a face.
(use-package highlight-defined
  :ghook 'prog-mode-hook
  :custom
  (highlight-defined-face-use-itself t))

;;;; Highlight-quoted
;; Make (lisp) quotes and quoted symbols easier to distinguish from free variables by highlighting
;; them
(use-package highlight-quoted
  :ghook 'emacs-lisp-mode-hook)

;;;; Paren
;; Highlight matching delimiters
(use-package paren
  :elpaca nil
  :custom
  (show-paren-context-when-offscreen 'overlay)
  :init (show-paren-mode))

;;;; Display-fill-column-indicator
(use-package display-fill-column-indicator
  :elpaca nil
  :custom
  (display-fill-column-indicator-character ?│)
  :custom-face
  (fill-column-indicator ((t (:inherit line-number)))))

;;;; Adaptive-wrap
;; Wrap lines as if they were hard newlines (like `fill-paragraph'). In other
;; words, lines preserve indentation.
(use-package adaptive-wrap
  :hook (prog-mode . adaptive-wrap-prefix-mode))

;;; General utility
;;;; Consult
;; Counsel equivalent for default Emacs completion. It provides many useful
;; commands.
(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :general
  ("C-x B" 'consult-buffer)
  ;; Put remaps here
  ([remap bookmark-jump] 'consult-bookmark
   [remap yank-pop] 'consult-yank-pop
   [remap repeat-complex-command] 'consult-complex-command
   [remap goto-line] 'consult-goto-line
   [remap imenu] 'kb/consult-imenu-versatile)
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
   "g" 'consult-grep
   "G" 'consult-git-grep
   "r" 'consult-ripgrep
   "f" 'consult-find
   "F" 'consult-locate
   "l" 'consult-line)
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
   ;; For `consult-buffer'
   consult-buffer :prompt "Can use b, m, f, p, e..." :preview-key "C-M-;"
   ;; For `consult-ripgrep'
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
   "M-." 'embark-dwim ; Check README for why it's sensible to overwrite `xref-find-definitions'
   "C-h B" 'embark-bindings)
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

;;;;; Embark-consult
;; Companion package for embark
(use-package embark-consult
  :demand
  :requires (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; File or buffer utilities
;;;; Autorevert
;; Automatically update buffers as files are externally modified
(use-package autorevert
  :demand
  :elpaca nil
  :custom
  (auto-revert-interval 3)
  (auto-revert-check-vc-info t)
  (auto-revert-verbose t)
  :config
  (global-auto-revert-mode))

;;;; Whitespace
;; Remove whitespace on save
(use-package whitespace
  :elpaca nil
  :hook (before-save . whitespace-cleanup)
  :custom
  (whitespace-style '(face empty indentation::space tab)))

;;;; Sudo-edit
;; Utilities to edit files as root
(use-package sudo-edit
  :general (kb/file-keys
             "U" '(sudo-edit-find-file :wk "Sudo find-file")
             "u" '(sudo-edit :wk "Sudo this file"))
  :config (sudo-edit-indicator-mode))

;;; Modes
;;;; Conf-mode
;; For Unix config files
(use-package conf-mode
  :elpaca nil
  :mode ("\\.rs\\'" . conf-mode)
  :gfhook 'outshine-mode)

;;;; Vimrc-mode
;; For editing vim/nvim config files
(use-package vimrc-mode)

;;; Other
;;;; Outshine
;; Outline-minor-mode but with better keybindings and more support
;; Demand to properly set outline-minor-mode-prefix
(use-package outshine
  :diminish (outshine-mode
             outline-minor-mode)
  :ghook 'LaTeX-mode-hook 'css-mode-hook 'prog-mode-hook
  :gfhook 'visual-line-mode
  :general
  (:keymaps 'outshine-mode-map
   "C-x n s" '(outshine-narrow-to-subtree :wk "Outshine narrow to subtree"))
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

;;;; Anzu
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

;;; programming-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-general-rcp)
