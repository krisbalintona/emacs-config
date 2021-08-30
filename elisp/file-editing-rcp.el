;;; file-editing-rcp.el --- Summary
;;
;;; Commentary:
;;
;; All the things which are major mode and package agnostic which allow me to
;; navigate files and manipulate text in files more easily.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)
(require 'faces-rcp)

;;;; Manipulating text
;;;;; Paren
;; Highlight matching delimiters
(use-package paren
  :ghook ('after-init-hook 'show-paren-mode)
  )

;;;;; Smartparens
;; Autopairing parentheses
(use-package smartparens
  :commands (sp-local-pair sp-pair sp--looking-at-p)
  :ghook ('after-init-hook 'smartparens-global-mode)
  :gfhook 'show-smartparens-mode ; Subtlely highlight matching parentheses
  :general (:keymaps 'prog-mode-map
                     :states '(visual normal motion)
                     [remap evil-forward-sentence-begin] 'sp-forward-sexp
                     [remap evil-backward-sentence-begin] 'sp-backward-sexp)
  :custom
  (sp-show-pair-from-inside t)
  (sp-ignore-modes-list
   '(minibuffer-mode minibuffer-inactive-mode
                     ))
  (sp-autoskip-closing-pair 'always-end)
  ;; Read `sp-delete-pair' docstring
  (sp-autodelete-pair t)
  (sp-autodelete-opening-pair t)
  (sp-autodelete-closing-pair t)
  :init
  (defun kb/sp-point-before-letter-digit-p (_id action _context)
    "Return t if point is followed by any digit or alphanumeric character, nil
otherwise. This predicate is only tested on \"insert\" action."
    (when (eq action 'insert)
      (sp--looking-at-p "[a-z0-9A-Z]")))
  (defun kb/sp-point-adjacent-paren-p (_id action _context)
    "Return t if point is next to a parenthesis, nil otherwise. This predicate
is only tested on \"insert\" action."
    (when (eq action 'insert)
      (sp--looking-at-p "(\\|)")))
  (defun kb/sp-point-before-closing-paren-p (_id action _context)
    "Return t if point is before a closing parenthesis, nil otherwise. This predicate
is only tested on \"insert\" action."
    (when (eq action 'insert)
      (sp--looking-at-p "(")))
  :config
  ;; Global
  (sp-pair "(" ")" :actions '(insert autoskip navigate))
  (sp-pair "\"" "\""
           :actions '(insert autoskip navigate escape)
           :unless '(sp-in-string-quotes-p kb/sp-point-before-letter-digit-p sp-point-before-same-p)
           :post-handlers '(sp-escape-wrapped-region sp-escape-quotes-after-insert))

  ;; emacs-lisp-mode
  (sp-local-pair 'emacs-lisp-mode "(" ")"
                 :actions '(insert autoskip navigate)
                 :unless '(kb/sp-point-before-letter-digit-p kb/sp-point-before-closing-paren-p))
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "'" "'"
                 :actions '(insert autoskip navgiate)
                 :when '(sp-in-string-p))
  (sp-local-pair 'emacs-lisp-mode "`" "'"
                 :actions '(insert autoskip navigate)
                 :when '(sp-in-comment-p sp-in-string-p))
  )

;;;; Quick movement
;;;;; Ace-link
;; Open links easily
(use-package ace-link
  :general (:keymaps '(Info-mode-map helpful-mode-map help-mode-map woman-mode-map eww-mode-map compilation-mode-map mu4e-view-mode-map custom-mode-map org-mode-map)
                     "M-/" '(ace-link :which-key "Ace-link")
                     )
  )

;;;;; Ace-jump
;; Quickly jump to any character
(use-package ace-jump-mode
  :hook (org-mode . (lambda () (face-remap-add-relative 'ace-jump-face-foreground :font kb/variable-pitch-font)))
  :general ("M-a" '(ace-jump-mode :which-key "Ace-jump"))
  :config
  (setq ace-jump-mode-scope 'window
        ace-jump-mode-case-fold t ; Ignore case?
        ace-jump-mode-gray-background nil ; Don't make text's background gray
        ace-jump-mode-submode-list ; Priority of ace-jump selections
        '(ace-jump-char-mode ace-jump-word-mode ace-jump-line-mode))
  )

;;;;; Better-jumper
(use-package better-jumper
  :after evil
  :general (:states '(normal visual normal)
                    [remap evil-jump-backward] '(better-jumper-jump-backward :which-key "Jump backward")
                    [remap evil-jump-forward] '(better-jumper-jump-forward :which-key "Jump forward"))
  :custom
  (better-jumper-max-length 200)
  (better-jumper-use-evil-jump-advice t) ; Add evil-jump jumps
  (better-jumper-add-jump-behavior 'append)

  (better-jumper-context 'buffer)
  (better-jumper-use-savehist t)
  (better-jumper-buffer-savehist-size 50)
  :config
  ;; Add evil navigation commands to the better-jumper jump list
  (general-advice-add '(evil-forward-WORD-end evil-backward-WORD-begin evil-jump-item evil-first-non-blank evil-end-of-visual-line)
                      :after 'better-jumper-set-jump)
  (general-add-hook '(ace-jump-mode-before-jump-hook ace-jump-mode-end-hook) 'better-jumper-set-jump)
  )

;;;; Cleanup
;;;;; Whitespace
;; Remove whitespace on save
(use-package whitespace
  :hook (before-save . whitespace-cleanup)
  :custom
  (whitespace-style '(face empty indentation::space tab))
  )

;;;; Other
;;;;; Outshine
;; Outline-minor-mode but with better keybindings and more support
(use-package outshine
  :demand t ; Load immediately to properly set outline-minor-mode-prefix
  :straight (outshine :type git :host github :repo "alphapapa/outshine")
  :functions (sp--looking-at-p sp--looking-back outline-back-to-heading outline-next-heading)
  :commands evil-insert-state
  :ghook 'LaTeX-mode-hook 'css-mode-hook 'prog-mode-hook 'conf-mode-hook
  :gfhook 'display-line-numbers-mode 'visual-line-mode
  :general
  (:keymaps 'outshine-mode-map
            "C-x n s" '(outshine-narrow-to-subtree :which-key "Outshine narrow to subtree"))
  (:keymaps 'outshine-mode-map
            :states 'normal
            "<tab>" '(outshine-kbd-TAB :which-key "Outshine TAB"))
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
  :config
  ;; Outshine headline faces
  (set-face-attribute 'outshine-level-4 nil :inherit 'outline-5)
  (set-face-attribute 'outshine-level-5 nil :inherit 'outline-6)
  (set-face-attribute 'outshine-level-6 nil :inherit 'outline-8)
  (set-face-attribute 'outshine-level-8 nil :inherit 'outline-7)
  )

;;;;; Sudo-edit
;; Utilities to edit files as root
(use-package sudo-edit
  :hook (after-init . sudo-edit-indicator-mode)
  :general (kb/leader-keys
             "fU" '(sudo-edit-find-file :which-key "Sudo find-file")
             "fu" '(sudo-edit :which-key "Sudo this file")
             )
  )

;;;;; Anzu
;; Adds highlight face during replace and regexp
(use-package anzu
  :ghook ('after-init-hook 'global-anzu-mode)
  :general ([remap query-replace] 'anzu-query-replace-regexp)
  :custom
  (anzu-cons-mode-line-p nil)
  )

;;;;; Embark
;; Allow an equivalent to ivy-actions to regular complete-read minibuffers (and
;; thus selectrum!)
(use-package embark
  :after which-key ; Because I replace its value of `prefix-help-command'
  :general
  ("M-o" '(embark-act :which-key "Embark-act"))
  (kb/leader-keys
    "hB" '(embark-bindings :which-key "Embark-bindings")
    )
  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  )

;;; file-editing-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'file-editing-rcp)
