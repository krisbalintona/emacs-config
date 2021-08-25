;;; early-packages-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Load packages which need to be loaded at an early stage here.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'personal-variables-rcp)
(require 'keybinds-frameworks-rcp)

;;;; Load custom file
;; Set and load custom file.
(with-eval-after-load 'no-littering
  (setq custom-file (no-littering-expand-var-file-name "custom.el"))
  (load custom-file))

;;;; Exec-path-from-shell
;; Ensure eshell and system shell have same path
(use-package exec-path-from-shell
  :functions exec-path-from-shell-initialize
  :ghook ('after-init-hook 'exec-path-from-shell-initialize)
  )

;;;; System-packages
;; Install system packages within Emacs. Necessary for use-package's `:ensure-system-package' flag
(use-package system-packages
  :custom
  (system-packages-use-sudo t)
  (system-packages-noconfirm t) ; Just bypass this prompt
  :config
  (add-to-list 'system-packages-supported-package-managers
               '(yay . ; Add support for yay
                     ((default-sudo . t)
                      (install . "yay -S")
                      (search . "yay -Ss")
                      (uninstall . "yay -Rns")
                      (update . "yay -Syu")
                      (clean-cache . "yay -Sc")
                      (change-log . "yay -Qc")
                      (log . "cat /var/log/yay.log")
                      (get-info . "yay -Qi")
                      (get-info-remote . "yay -Si")
                      (list-files-provided-by . "yay -qQl")
                      (owning-file . "yay -Qo")
                      (owning-file-remote . "yay -F")
                      (verify-all-packages . "yay -Qkk")
                      (verify-all-dependencies . "yay -Dk")
                      (remove-orphaned . "yay -Rns $(yay -Qtdq)")
                      (list-installed-packages . "yay -Qe")
                      (list-installed-packages-all . "yay -Q")
                      (list-dependencies-of . "yay -Qi")
                      (noconfirm . "--noconfirm"))
                     ))
  (if kb/linux-arch
      (setq system-packages-package-manager 'yay
            system-packages-use-sudo nil))
  )

;;;; No-littering
;; Set default package paths
(use-package no-littering
  :custom
  (no-littering-etc-directory (expand-file-name "data/" user-emacs-directory)) ; Config files
  (no-littering-var-directory (expand-file-name "var/" user-emacs-directory)) ; Persistent files
  :preface (require 'recentf)
  )

;;;; Org
;; Use `org' from `straight.el'. This may or may not work?
(use-package org
  :straight (org :type git :repo "https://code.orgmode.org/bzg/org-mode.git" :local-repo "org" :depth full :pre-build (straight-recipes-org-elpa--build) :build (:not autoloads) :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*")))
  )

;;;; Outshine
;; Outline-minor-mode but with better keybindings and more support
(use-package outshine
  :demand t ; Load immediately to properly set outline-minor-mode-prefix
  :straight (outshine :type git :host github :repo "alphapapa/outshine")
  :functions (sp--looking-at-p sp--looking-back outline-back-to-heading outline-next-heading)
  :commands evil-insert-state
  :ghook 'LaTeX-mode-hook 'css-mode-hook 'prog-mode-hook
  :gfhook 'display-line-numbers-mode 'visual-line-mode
  :general (:keymaps 'outshine-mode-map
                     "C-x n s" '(outshine-narrow-to-subtree :which-key "Outshine narrow to subtree"))
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

  (general-define-key
   :keymaps 'outshine-mode-map
   :states 'normal
   "<tab>" '(outshine-kbd-TAB :which-key "Outshine TAB"))
  )

;;;; Helpful
;; Have more descriptive and helpful function and variable descriptions
(use-package helpful
  :gfhook 'visual-line-mode
  :general
  ;; NOTE 2021-08-20: Emacs' describe-function includes both functions and
  ;; macros
  ([remap describe-function] '(helpful-callable :which-key "Helpful function")
   [remap describe-command] '(helpful-command :which-key "Helpful command")
   [remap describe-variable] '(helpful-variable :which-key "Helpful variable")
   [remap describe-symbol] '(helpful-symbol :which-key "Helpful symbol")
   [remap describe-key] '(helpful-key :which-key "Helpful key")
   )
  (:states '(visual normal motion)
           "f" 'helpful-at-point
           )
  (kb/leader-keys
    "hk" '(helpful-key :which-key "Desc key")
    "hc" '(helpful-command :which-key "Helpful command"))
  )

;;; early-packages-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'early-packages-rcp)
