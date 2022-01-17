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
  :hook (after-init . global-hl-todo-mode)
  :general
  (:keymaps 'hl-todo-mode-map
            :prefix "C-c"
            "p" 'hl-todo-previous
            "n" 'hl-todo-next
            "o" 'hl-todo-occur
            "i" 'hl-todo-insert
            )
  :custom
  (hl-todo-include-modes '(prog-mode text-mode))
  (hl-todo-text-modes '(text-mode org-mode))
  (hl-todo-exclude-modes nil)
  (hl-todo-keyword-faces
   '(("TODO" . "orange")
     ("FIXME" error bold)
     ("REVIEW" . "orchid")
     ("NOTE" success bold)
     ("BUG" error bold)
     ("DEPRECATED" font-lock-doc-face bold)
     ))
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
  :ghook 'text-mode-hook 'prog-mode-hook
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
  :demand t
  :custom
  (show-paren-style 'parenthesis)
  (show-paren-when-point-in-periphery t)
  (show-paren-when-point-inside-paren t)
  :config (show-paren-mode)
  )

;;;; Display-fill-column-indicator
(use-package display-fill-column-indicator
  :straight nil
  :custom
  (display-fill-column-indicator-character ?│)
  :custom-face
  (fill-column-indicator ((t (:inherit line-number))))
  )

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
  :demand t
  :ensure-system-package ((fd . fd-find)
                          (rg . ripgrep))
  :ryo
  ("P" consult-yank-pop)
  :general
  (:keymaps 'help-map
            [remap apropos-command] '(consult-apropos :which-key "Consult apropos"))
  (:keymaps 'global-map
            "C-x b" 'consult-buffer
            "C-x B" 'consult-buffer-other-window
            )
  (kb/nav-keys
    "h" '(consult-outline :which-key "Consult outline")
    "j" '(consult-line :which-key "Consult line")
    "i" '(consult-imenu :which-key "Consult imenu")
    "O" '(consult-multi-occur :which-key "Consult multi-occur")
    )
  (kb/nav-keys
    :keymaps 'org-mode-map
    :states '(normal visual insert motion)
    [remap consult-outline] '(consult-org-heading :which-key "Consult outline"))
  (kb/yank-kill-keys
    "y" '(consult-yank-pop :which-key "Consult yank-pop")
    )
  (kb/buffer-keys
    "b" 'consult-buffer
    "B" 'consult-buffer-other-window
    )
  (kb/file-keys
    "r" '(consult-recent-file :which-key "Consult recent file")
    )
  (kb/mark-keys
    "m" '(consult-bookmark :which-key "Consult bookmark")
    "r" '(consult-mark :which-key "Consult mark-ring")
    )
  :custom
  (consult-mode-histories ; What variable consult-history looks at for history
   '((eshell-mode . eshell-history-ring)
     (comint-mode . comint-input-ring)
     (term-mode . term-input-ring))
   )
  (consult-project-root-function #'vc-root-dir)
  :config
  ;; Enhanced multiple selection experience. Replaced the built-in method
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Customize consult commands
  (consult-customize
   ;; For `consult-buffer'
   consult-buffer :preview-key (kbd "M-'")
   consult-buffer :prompt "Can use b, m, f, p..."
   ;; For `consult-ripgrep'
   consult-ripgrep :preview-key (kbd "M-'")
   ;; For `consult-fdfind'. Make sure this is after the definition of
   ;; `consult-recent-file'
   consult-recent-file :preview-key (kbd "M-'")
   ;; `consult-find'
   consult-find :preview-key (kbd "M-'")
   )
  )

;;;; Embark
;; Allow an equivalent to ivy-actions to regular complete-read minibuffers (and
;; thus selectrum!)
(use-package embark
  :demand t               ; Other packages depend on this
  :after which-key        ; Because I replace its value of `prefix-help-command'
  :general
  (:states '(normal insert visual motion)
           "C-." 'embark-act
           )
  (:keymaps 'vertico-map
            "C-." 'embark-act
            "C->" 'embark-dwim
            )
  (kb/help-keys
    "B" '(embark-bindings :which-key "Embark-bindings")
    )
  :custom
  (prefix-help-command #'embark-prefix-help-command) ; Optionally replace the key help with a completing-read interface
  (embark-collect-live-update-delay 0.5)
  )

;;;; Embark-consult
;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  )

;;;; Hideshow
(use-package hideshow
  :hook ((prog-mode conf-mode text-mode) . hs-minor-mode)
  :general
  (:prefix "C-c h"
           "t" '(hs-hide-block :which-key "Toggle hide")
           "l" '(hs-hide-level :which-key "Hide level")
           "h" '(hs-hide-block :which-key "Hide block")
           "s" '(hs-show-block :which-key "show block")
           "H" '(hs-hide-all :which-key "Hide all")
           "S" '(hs-show-all :which-key "show all")
           )
  (kb/toggle-keys
    "t" '(hs-toggle-hiding :which-key "Toggle hide")
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
  :config
  (add-to-list 'super-save-triggers 'evil-window-next)
  (add-to-list 'super-save-hook-triggers 'eyebrowse-pre-window-switch-hook)
  :config (super-save-mode)
  )

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
             "U" '(sudo-edit-find-file :which-key "Sudo find-file")
             "u" '(sudo-edit :which-key "Sudo this file")
             )
  :config (sudo-edit-indicator-mode)
  )

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
            "C-x n s" '(outshine-narrow-to-subtree :which-key "Outshine narrow to subtree"))
  (:keymaps 'outshine-mode-map
            :states 'normal
            "<tab>" '(outshine-kbd-TAB :which-key "Outshine TAB")
            "C-<return>" 'outshine-insert-heading)
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
  (advice-add 'outline-insert-heading :override 'kb/outline-insert-heading)
  )

;;;; Anzu
;; Adds highlight face during replace and regexp
(use-package anzu
  :demand t
  :general ([remap query-replace] 'anzu-query-replace-regexp)
  :custom
  (anzu-cons-mode-line-p nil)
  :config (global-anzu-mode)
  )

;;; programming-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-general-rcp)
