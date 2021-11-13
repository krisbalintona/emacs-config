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
  :gfhook
  ;; Custom font lock words
  ;; Mainly for `prot-comment-timestamp-keyword'. Faces taken from
  ;; `org-todo-keyword-faces' in org-agenda-general-rcp.el.
  '(lambda () ; All `prog-mode' derived major modes
     (font-lock-add-keywords nil
                             '(;; TODO wrapped between whitespace
                               ("\\s-TODO\\s-" 0 '(t :foreground "orange") t)
                               ;; NOTE wrapped between whitespace
                               ("\\s-NOTE\\s-" 0 '(t :foreground "turquoise") t)
                               ;; REVIEW wrapped between whitespace
                               ("\\s-REVIEW\\s-" 0 '(t :foreground "orchid") t)
                               ;; FIXME wrapped between whitespace
                               ("\\s-FIXME\\s-" 0 '(t :foreground "deep pink") t)
                               ))
     )
  :config (global-prettify-symbols-mode)
  )

;;;; Highlight-indent-guides
;; Show indicator for indentation levels (like in VS Code)
(use-package highlight-indent-guides
  :ghook 'prog-mode-hook
  :gfhook 'highlight-indent-guides-auto-set-faces ; Set faces based on theme
  :custom
  (highlight-indent-guides-method 'column)
  (highlight-indent-guides-character ?⏐)
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

;;; Quick movement
;;;; Ace-link
;; Open links easily
(use-package ace-link
  :general (:keymaps '(Info-mode-map helpful-mode-map help-mode-map woman-mode-map eww-mode-map compilation-mode-map mu4e-view-mode-map custom-mode-map org-mode-map)
                     "M-/" '(ace-link :which-key "Ace-link")
                     )
  )

;;;; Ace-jump
;; Quickly jump to any character
(use-package ace-jump-mode
  :general ("M-a" '(ace-jump-mode :which-key "Ace-jump"))
  :config
  (setq ace-jump-mode-scope 'window
        ace-jump-mode-case-fold t ; Ignore case?
        ace-jump-mode-gray-background nil ; Don't make text's background gray
        ace-jump-mode-submode-list ; Priority of ace-jump selections
        '(ace-jump-char-mode ace-jump-word-mode ace-jump-line-mode))
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

;;;; Consult
;; Counsel equivalent for default Emacs completion. It provides many useful
;; commands.
(use-package consult
  :ensure-system-package ((fd . fd-find)
                          (rg . ripgrep))
  :general
  (:keymaps 'help-map
            [remap apropos-command] '(consult-apropos :which-key "Consult apropos"))
  (:keymaps 'global-map
            "C-x b" 'consult-buffer
            "C-x B" 'consult-buffer-other-window
            )
  (kb/nav-keys
    "h" '(consult-outline :which-key "Consult outline")
    "l" '(consult-line :which-key "Consult line")
    "i" '(consult-imenu :which-key "Consult imenu")
    "o" '(consult-multi-occur :which-key "Consult multi-occur")
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
  :after which-key ; Because I replace its value of `prefix-help-command'
  :general
  ("M-o" '(embark-act :which-key "Embark-act"))
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
  :requires (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  )

;;;; Smartparens
;; Auto pairing parentheses
(use-package smartparens
  :demand t
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
  :config
  (smartparens-global-mode)

  ;; For python
  (require 'smartparens-python)

  ;; Helpter functions
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

  ;; Global
  (sp-pair "(" ")" :actions '(insert autoskip navigate))
  (sp-pair "\"" "\""
           :actions '(insert autoskip navigate escape)
           :unless '(sp-in-string-quotes-p kb/sp-point-before-letter-digit-p sp-point-before-same-p)
           :post-handlers '(sp-escape-wrapped-region sp-escape-quotes-after-insert))

  ;; Emacs-lisp-mode
  (sp-with-modes 'emacs-lisp-mode
    (sp-local-pair "<" ">" :actions '(insert autoskip navigate))
    (sp-local-pair "(" ")"
                   :actions '(insert autoskip navigate)
                   :unless '(kb/sp-point-before-letter-digit-p kb/sp-point-before-closing-paren-p))
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "'" "'"
                   :actions '(insert autoskip navgiate)
                   :when '(sp-in-string-p))
    (sp-local-pair "`" "'"
                   :actions '(insert autoskip navigate)
                   :when '(sp-in-comment-p sp-in-string-p))
    )
  )

;;;; Scratch.el
;; Easily create scratch buffers for different modes
(use-package scratch
  ;; :demand t ; For the initial scratch buffer at startup
  :hook (scratch-create-buffer . kb/scratch-buffer-setup)
  :general ("C-c s" '(scratch :which-key "Create scratch"))
  :preface
  (defun kb/scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly. Taken from https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore/"
    (let* ((mode (format "%s" major-mode))
           (string (concat "Scratch buffer for: " mode "\n\n")))
      (when scratch-buffer
        (save-excursion
          (insert string)
          (goto-char (point-min))
          (comment-region (point-at-bol) (point-at-eol)))
        (forward-line 2))
      (rename-buffer (concat "*Scratch for " mode "*") t))
    )
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

;;;; Goto-line-preview
;; Preview line before you jump to it with `goto-line'
(use-package goto-line-preview
  :general ([remap goto-line] 'goto-line-preview)
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

;;;; Tramp
(use-package tramp
  :custom
  (tramp-default-method "ssh")
  )

;;;; Whitespace
;; Remove whitespace on save
(use-package whitespace
  :hook (before-save . whitespace-cleanup)
  :custom
  (whitespace-style '(face empty indentation::space tab))
  )

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

;;;; Imenu
(use-package imenu
  :custom
  (org-imenu-depth 7)                   ; Show more than just 2 levels...
  )

;;;; Imenu-list
;; Side buffer with imenu items
(use-package imenu-list
  :general (kb/nav-keys
             "I" '(imenu-list :which-key "Imenu list"))
  )

;;; programming-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-general-rcp)
