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
  :after (flycheck lsp)
  :ensure-system-package (shellcheck . ShellCheck)
  :mode (("\\.bats\\'" . sh-mode)
         ("\\.\\(?:zunit\\|env\\)\\'" . sh-mode)
         ("/bspwmrc\\'" . sh-mode))
  :hook (sh-mode . lsp-deferred)
  :custom
  (flycheck-sh-shellcheck-executable "shellcheck")
  )

;;; programming-shell-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-shell-rcp)
