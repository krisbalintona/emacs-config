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
  :straight nil
  :config (global-prettify-symbols-mode)
  )

;;;; Hl-line
(use-package hl-line
  :straight nil
  :ghook
  'prog-mode-hook
  'conf-mode-hook
  )

;;;; Hl-todo
(use-package hl-todo
  :after kb-comment
  :hook (after-init . global-hl-todo-mode)
  :general
  (:keymaps 'hl-todo-mode-map
   :prefix "M-g"
   "n" '(hl-todo-next :wk "Hl-todo-next")
   "p" '(hl-todo-previous :wk "Hl-todo-previous")
   "o" '(hl-todo-occur :wk "Hl-todo-occur")
   )
  :custom
  (hl-todo-include-modes '(prog-mode text-mode))
  (hl-todo-text-modes '(text-mode org-mode))
  (hl-todo-exclude-modes '(org-mode))
  ;; Make sure to have all words in `kb/comment-keywords-coding' and
  ;; `kb/comment-keywords-writing' in this list, otherwise those words will not
  ;; appear in any calls to `kb/comment-dwim'.
  (hl-todo-keyword-faces kb/comment-keyword-faces)
  ;; TODO 2022-02-07: Change `kb-comment' such that I am able to leverage
  ;; hl-todo's punctuation highlighting.
  (hl-todo-require-punctuation nil)
  (hl-todo-highlight-punctuation "")
  )

;;;; Highlight-indent-guides
;; Show indicator for indentation levels (like in VS Code)
(use-package highlight-indent-guides
  :ghook
  'prog-mode-hook
  'conf-mode-hook
  :gfhook 'highlight-indent-guides-auto-set-faces ; Set faces based on theme
  :custom
  (highlight-indent-guides-method 'column)
  (highlight-indent-guides-character ?⏐)
  (highlight-indent-guides-suppress-auto-error t)
  )

;;;; Rainbow-mode
;; Colorify color codes
(use-package rainbow-mode
  :ghook 'text-mode-hook 'prog-mode-hook 'help-mode-hook
  )

;;;; Highlight-defined
;; Very useful for emacs configuration! Fontify symbols. Additionally, fontify
;; text which is the symbol of a face.
(use-package highlight-defined
  :ghook 'prog-mode-hook
  :custom
  (highlight-defined-face-use-itself t)
  )

;;;; Highlight-quoted
;; Make (lisp) quotes and quoted symbols easier to distinguish from free variables by highlighting
;; them
(use-package highlight-quoted
  :ghook 'emacs-lisp-mode-hook
  )

;;;; Paren
;; Highlight matching delimiters
(use-package paren
  :custom
  (show-paren-context-when-offscreen 'overlay)
  :init (show-paren-mode))

;;;; Display-fill-column-indicator
(use-package display-fill-column-indicator
  :straight nil
  :custom
  (display-fill-column-indicator-character ?│)
  :custom-face
  (fill-column-indicator ((t (:inherit line-number)))))

;;;; Adaptive-wrap
;; Wrap lines as if they were hard newlines (like `fill-paragraph'). In other
;; words, lines preserve indentation.
(use-package adaptive-wrap
  :hook (prog-mode . adaptive-wrap-prefix-mode)
  )

;;; General utility
;;;; Consult
;; Counsel equivalent for default Emacs completion. It provides many useful
;; commands.
(use-package consult
  :ensure-system-package ((fd . fd-find)
                          (rg . ripgrep))
  :general
  ([remap switch-to-buffer] '(consult-buffer :wk "Consult buffer")
   [remap switch-to-buffer-other-window] '(consult-buffer-other-window :wk "Consult buffer other window")
   [remap bookmark-jump] '(consult-bookmark :wk "Consult bookmark")
   [remap yank-pop] '(consult-yank-pop :wk "Consult yank-pop"))
  ([remap project-find-regexp] 'consult-ripgrep)
  (:keymaps 'help-map
   [remap apropos-command] '(consult-apropos :wk "Consult apropos"))
  (:keymaps 'minibuffer-local-map
   "M-r" 'consult-history)
  (:keymaps 'comint-mode-map
   [remap comint-history-isearch-backward-regexp] 'consult-history)
  (kb/nav-keys
    "o" '(consult-outline :wk "Consult outline")
    "j" '(consult-line :wk "Consult line"))
  ([remap imenu] '(kb/consult-imenu-versatile :wk "Consult imenu"))
  (:keymaps 'org-mode-map
   [remap consult-outline] '(consult-org-heading :wk "Consult outline"))
  ([remap recentf-open-files] '(consult-recent-file :wk "Consult recent file")
   [remap flymake-show-buffer-diagnostics] 'consult-flymake)
  :custom
  (consult-mode-histories   ; What variable consult-history looks at for history
   '((eshell-mode . eshell-history-ring)
     (comint-mode . comint-input-ring)
     (term-mode . term-input-ring)
     ))
  (consult-ripgrep-args
   "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number --hidden .")
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

  ;; Customize consult commands
  (consult-customize
   ;; For `consult-buffer'
   consult-buffer :prompt "Can use b, m, f, p..." :preview-key (kbd "C-M-;")
   ;; For `consult-ripgrep'
   consult-ripgrep :preview-key (kbd "C-M-;")
   ;; For `consult-fdfind'. Make sure this is after the definition of
   ;; `consult-recent-file'
   consult-recent-file :preview-key (kbd "C-M-;")
   ;; `consult-find'
   consult-find :preview-key (kbd "C-M-;")))

;;;; Embark
;; Allow an equivalent to ivy-actions to regular complete-read minibuffers (and
;; thus selectrum!)
(use-package embark
  :commands embark-act
  :general
  ("C-." 'embark-act)
  (:keymaps 'vertico-map
   "C-." 'embark-act
   "C->" 'embark-become)
  ([remap describe-bindings] '(embark-bindings :wk "Embark bindings"))
  (:keymaps 'embark-symbol-map
   "R" '(raise-sexp :wk "Raise sexp"))
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
  :demand t
  :requires (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;;; Hideshow
(use-package hideshow
  :hook ((prog-mode conf-mode) . hs-minor-mode)
  :general
  (:prefix "C-c h"
   "t" '(hs-hide-block :wk "Toggle hide")
   "l" '(hs-hide-level :wk "Hide level")
   "h" '(hs-hide-block :wk "Hide block")
   "s" '(hs-show-block :wk "show block")
   "H" '(hs-hide-all :wk "Hide all")
   "S" '(hs-show-all :wk "show all")
   )
  (kb/toggle-keys
    "t" '(hs-toggle-hiding :wk "Toggle hide")
    )
  )

;;; File or buffer utilities
;;;; Autorevert
;; Automatically update buffers as files are externally modified
(use-package autorevert
  :demand t
  :custom
  (auto-revert-interval 7)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose t)
  :config (global-auto-revert-mode)
  )

;;;; Super-save
;; Automatically save buffers when you do certain things
(use-package super-save
  :demand t
  :custom
  (super-save-auto-save-when-idle t) ; Save buffer if Emacs is idle
  (super-save-idle-duration 10) ; Wait 10 seconds for idle trigger
  (super-save-remote-files t) ; Turn on saving of remote files (those pulled from git repo?)
  (super-save-exclude nil) ; Don't exclude anything from being saved
  (super-save-predicates
   '((lambda ()
       (stringp (buffer-file-name (buffer-base-buffer))))
     (lambda ()
       (buffer-modified-p (current-buffer)))
     (lambda ()
       (file-writable-p (buffer-file-name (buffer-base-buffer))))
     (lambda ()
       (if (file-remote-p (buffer-file-name (buffer-base-buffer)))
           super-save-remote-files t))
     (lambda ()
       (super-save-include-p (buffer-file-name (buffer-base-buffer))))))
  :config
  (add-to-list 'super-save-hook-triggers 'eyebrowse-pre-window-switch-hook)
  (add-to-list 'super-save-triggers 'evil-window-mru)
  ;; Make sure this goes after adding hooks, since the hooks are manually added once `super-save-mode' is enable
  (super-save-mode))

;;;; Whitespace
;; Remove whitespace on save
(use-package whitespace
  :hook (before-save . whitespace-cleanup)
  :custom
  (whitespace-style '(face empty indentation::space tab))
  )

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
  :mode (".rc$" . conf-mode)
  :gfhook 'outshine-mode
  )

;;;; Vimrc-mode
;; For editing vim/nvim config files
(use-package vimrc-mode
  )

;;; Other
;;;; Outshine
;; Outline-minor-mode but with better keybindings and more support
;; Demand to properly set outline-minor-mode-prefix
(use-package outshine
  :ghook 'LaTeX-mode-hook 'css-mode-hook 'prog-mode-hook
  :gfhook 'display-line-numbers-mode 'visual-line-mode
  :general
  (:keymaps 'outshine-mode-map
   "C-x n s" '(outshine-narrow-to-subtree :wk "Outshine narrow to subtree"))
  :custom
  (outshine-use-speed-commands t) ; Use speedy commands on headlines (or other defined locations)
  :init
  ;; More convenient `outline-insert-heading'
  (defun kb/outline-insert-heading ()
    "Insert a new heading at same depth at point.

I've customized it such that it ensures there are newlines before
and after the heading that that insert mode is entered
afterward."
    (interactive)
    ;; Check for if previous line is empty
    (unless (sp--looking-back "[[:space:]]*$")
      (insert "\n"))
    (let ((head (save-excursion
                  (condition-case nil
                      (outline-back-to-heading)
                    (error (outline-next-heading)))
                  (if (eobp)
                      (or (caar outline-heading-alist) "")
                    (match-string 0)))))
      (unless (or (string-match "[ \t]\\'" head)
                  (not (string-match (concat "\\`\\(?:" outline-regexp "\\)")
                                     (concat head " "))))
        (setq head (concat head " ")))
      (unless (bolp) (end-of-line) (newline))
      (insert head)
      (unless (eolp)
        (save-excursion (newline-and-indent)))
      (run-hooks 'outline-insert-heading-hook))
    ;; Check for if next line is empty
    (unless (sp--looking-at-p "[[:space:]]*$")
      (save-excursion
        (end-of-line)
        (insert "\n")))
    (evil-insert-state))
  (when (bound-and-true-p evil-local-mode)
    (advice-add 'outline-insert-heading :override 'kb/outline-insert-heading)))

;;;; Anzu
;; Search Mode Info Display
(use-package anzu
  :demand t
  :general ("M-g r" '(anzu-query-replace-regexp :wk "Query replace")
            [remap query-replace] 'anzu-query-replace-regexp)
  :custom
  (anzu-cons-mode-line-p nil)
  :config (global-anzu-mode)
  )

;;; programming-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-general-rcp)
