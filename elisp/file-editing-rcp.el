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

;;;; Manipulating text
;;;;; Expand-region
;; Incrementally select a region outward
(use-package expand-region
  :general (:keymaps '(normal motion)
            "ge" 'er/expand-region)
  :custom
  (expand-region-smart-cursor t)
  (expand-region-skip-whitespace nil)
  (expand-region-subword-enabled t)
  )

;;;;; Smartparens
;; Autopairing parentheses
(use-package smartparens
  :functions sp-fair
  :ghook ('after-init-hook 'smartparens-global-mode)
  :gfhook 'show-smartparens-mode ; Subtlely highlight matching parentheses
  :custom
  (sp-show-pair-from-inside t)
  (sp-ignore-modes-list
   '(minibuffer-mode minibuffer-inactive-mode
                     ))
  ;; TODO 2021-08-19: Determine how to do what I want in emacs-lisp-mode and
  ;; org-mode and how it interacts based on deleting, location (e.g. in comment
  ;; or not)--for writing and programming
  ;; :config
  ;; (sp-pair "'" nil :actions :rem) ; Don't pair '
  ;; (sp-pair "'" :actions :rem) ; Don't pair '
  ;; (sp-pair "`" "'" :when comment)
  )

;;;; Quick movement
;;;;; Ace-link
;; Open links easily
(use-package ace-link
  :general (:keymaps '(Info-mode-map helpful-mode-map help-mode-map woman-mode-map eww-mode-map compilation-mode-map mu4e-view-mode-map custom-mode-map)
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
;;;;; Anzu
;; Adds highlight face during replace and regexp
(use-package anzu
  :ghook ('after-init-hook 'global-anzu-mode)
  :general ([remap query-replace] 'anzu-query-replace-regexp)
  :custom
  (anzu-cons-mode-line-p nil)
  )

;;;;; Sudo-edit
;; Utilities to edit files as root
(use-package sudo-edit
  :ghook ('after-init 'sudo-edit-indicator-mode)
  :general
  (kb/leader-keys
    "fU" '(sudo-edit-find-file :which-key "Sudo find-file")
    "fu" '(sudo-edit :which-key "Sudo this file")
    )
  )

;;; file-editing-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'file-editing-rcp)
