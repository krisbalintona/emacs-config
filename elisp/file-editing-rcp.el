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
;;;;; Expand-region
;; Incrementally select a region outward
(use-package expand-region
  :hook (org-mode . er/add-text-mode-expansions)
  :general (:states '(normal visual motion)
                    "+" 'er/expand-region)
  :custom
  (expand-region-smart-cursor t)
  (expand-region-skip-whitespace nil)
  (expand-region-subword-enabled t)
  :init
  (defun er/add-text-mode-expansions ()
    (make-variable-buffer-local 'er/try-expand-list)
    (setq er/try-expand-list (append
                              er/try-expand-list
                              '(mark-paragraph
                                ))))
  )
;;;;; Paren
;; Highlight matching delimiters
(use-package paren
  :ghook ('after-init-hook 'show-paren-mode)
  )

;;;;; Smartparens
;; Autopairing parentheses
(use-package smartparens
  :commands sp-local-pair sp-pair
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
  ;; TODO 2021-08-21: Work still needs to be done for this mode
  (sp-local-pair 'emacs-lisp-mode "(" ")"
                 :actions '(insert autoskip navigate)
                 :unless '(kb/sp-point-before-letter-digit-p kb/sp-point-before-closing-paren-p))
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" "'"
                 :actions '(insert autoskip navigate)
                 :unless '(kb/sp-point-before-letter-digit-p))
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

;;;; Cleanup
;;;;; Whitespace
;; Remove whitespace on save
(use-package whitespace
  :hook (before-save . whitespace-cleanup)
  :custom
  (whitespace-style '(face empty indentation::space tab))
  )

;;;; Other
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
