;;; early-packages-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Load packages which need to be loaded at an early state here.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Org-plus-contrib
;; Org but with more useful packages
(require 'straight)
(straight-use-package 'org-plus-contrib)

;;;; System-packages
;; Install system packages within Emacs. Necessary for use-package's `:ensure-system-package' flag
(use-package system-packages
  :custom
  (system-packages-use-sudo t)
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

  (if (string= linux-distribution "\"Arch Linux\"")
      (progn
        (setq system-packages-package-manager 'yay)
        (setq system-packages-use-sudo nil)))
  )

;;;; Exec-path-from-shell
;; Ensure eshell and system shell have same path
(use-package exec-path-from-shell
  :functions exec-path-from-shell-initialize
  :config (exec-path-from-shell-initialize)
  )

;;;; NoLittering
;; Set default package paths
(use-package no-littering
  :functions (no-littering-expand-var-file-name no-littering-expand-etc-file-name)
  :custom
  (no-littering-etc-directory (expand-file-name "data/" user-emacs-directory)) ; Config files
  (no-littering-var-directory (expand-file-name "var/" user-emacs-directory)) ; Persistent files

  (custom-file (no-littering-expand-etc-file-name "custom.el")) ; Set custom.el path
  (auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))) ; Store auto-saved files here
  :preface (require 'recentf)
  :config
  ;; Exclude these files from recent files list
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  )

;;;; Outshine
;; Outline-minor-mode but with better keybindings and more support
(use-package outshine
  :demand t ; Load immediately to properly set outline-minor-mode-prefix
  :straight (outshine :type git :host github :repo "alphapapa/outshine")
  :hook ((LaTeX-mode . outshine-mode)
         (prog-mode . outshine-mode)
         (outshine-mode . display-line-numbers-mode)
         (outshine-mode . visual-line-mode)
         )
  :preface
  (defvar outline-minor-mode-prefix (kbd "M-#"))
  :custom
  (outshine-use-speed-commands t) ; Use speedy commands on headlines (or other defined locations)
  :config
  ;; Outshine headline faces
  (set-face-attribute 'outshine-level-4 nil :inherit 'outline-5)
  (set-face-attribute 'outshine-level-5 nil :inherit 'outline-6)
  (set-face-attribute 'outshine-level-6 nil :inherit 'outline-8)
  (set-face-attribute 'outshine-level-8 nil :inherit 'outline-7)
  )

;;;; Diminish
;; Remove or rename modeline lighters
(use-package diminish)

;;; early-packages-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'early-packages-rcp)
