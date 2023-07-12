;;; programming-shell-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages related to developing in shell languages (e.g. bash).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'general)
(require 'keybinds-general-rcp)
(require 'keybinds-evil-rcp)

;;; Sh-script
;; Built-in for sh-mode
(use-package sh-script
  :elpaca nil
  :after flycheck
  :ensure-system-package shellcheck
  :mode (("\\.bats\\'" . sh-mode)
         ("\\.\\(?:zunit\\|env\\)\\'" . sh-mode)
         ("/bspwmrc\\'" . sh-mode))
  :custom
  (flycheck-sh-shellcheck-executable "shellcheck"))

;;; Ssh-config-mode
;; For ~/.ssh/config
(use-package ssh-config-mode
  :gfhook 'display-line-numbers-mode)

;;; programming-shell-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-shell-rcp)
