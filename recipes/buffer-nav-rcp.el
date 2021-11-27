;;; buffer-nav-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Configuration of packages whose main functionality is to navigate buffers.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Ace-jump
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

;;; Ace-link
;; Open links easily
(use-package ace-link
  :general (:keymaps '(Info-mode-map helpful-mode-map help-mode-map woman-mode-map eww-mode-map compilation-mode-map mu4e-view-mode-map custom-mode-map org-mode-map)
                     "M-/" '(ace-link :which-key "Ace-link")
                     )
  )

;;; Better-jumper
;; Accompanies `evil-jumper' very well. Some of the smart stuff is taken from
;; https://www.reddit.com/r/emacs/comments/ntnhkc/how_i_jump_around_emacs_with_betterjumper/
(use-package better-jumper
  :demand t
  :after (evil consult)
  :general (:states '(normal visual normal insert)
                    [remap evil-jump-backward] 'better-jumper-jump-backward
                    [remap evil-jump-forward] 'better-jumper-jump-forward
                    "C-p" 'better-jumper-jump-toggle)
  :custom
  ;; This is THE key to avoiding conflict with evils' jumping functionality
  (better-jumper-use-evil-jump-advice nil)
  ;; (better-jumper-use-evil-jump-advice t) ; Add evil-jump jumps

  (better-jumper-max-length 200)
  (better-jumper-add-jump-behavior 'append)

  (better-jumper-context 'window)
  (better-jumper-use-savehist t)
  (better-jumper-buffer-savehist-size 50)
  :init
  ;; Toggle between two between current point and last better-jumper set point
  ;; (inspired by `evil-jump-backward-swap').
  (evil-define-motion better-jumper-jump-toggle (count)
    (better-jumper-jump-backward 1)
    (better-jumper-set-jump (point))
    )
  :config
  (better-jumper-mode)

  ;; Set a jump point using `better-jumper-set-jump'
  (general-advice-add '(evil-first-non-blank evil-end-of-visual-line
                                             evil-org-beginning-of-line evil-org-end-of-line
                                             org-beginning-of-line org-end-of-line
                                             evil-goto-first-line evil-goto-line evil-goto-mark evil-goto-definition
                                             evil-search-next evil-search-previous
                                             evilmi-jump-items
                                             consult-line consult-outline consult-ripgrep consult-imenu
                                             )
                      :before 'better-jumper-set-jump)

  ;; Specifically for ace-jump
  (general-add-hook '(ace-jump-mode-before-jump-hook ace-jump-mode-end-hook) 'better-jumper-set-jump)
  )

;;; Smartparens
;; Auto pairing parentheses
(use-package smartparens
  :demand t
  :gfhook 'show-smartparens-mode        ; Subtly highlight matching parentheses
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

  ;; Helper functions
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

;;; Imenu
(use-package imenu
  :custom
  (org-imenu-depth 7)                   ; Show more than just 2 levels...
  (imenu-auto-rescan t)
  )

;;; Imenu-list
;; Side buffer with imenu items
(use-package imenu-list
  :demand t
  :after imenu
  :general (kb/nav-keys
             "I" '(imenu-list :which-key "Imenu list"))
  :hook (imenu-list-major-mode . visual-line-mode)
  )

;;; Occur
;; Narrow current buffer to lines which match a regexp
(use-package occur
  :straight nil
  :general (kb/nav-keys
             "o" '(occur :which-key "Occur"))
  )

;;; Goto-line-preview
;; Preview line before you jump to it with `goto-line'
(use-package goto-line-preview
  :general ([remap goto-line] 'goto-line-preview)
  )

;;; buffer-nav-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'buffer-nav-rcp)
