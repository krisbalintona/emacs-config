;;; external-programs-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Make sure any external programs (e.g. pip and python) are -Sed here and
;; are available in my PATH.
;;
;; TODO 2021-08-20: These statements currently only work with Fedora. Change
;; to be compatible with other distributions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'personal-variables-rcp)

;;; Python
(unless (executable-find "python")
  (async-shell-command (concat "sudo " (kb/which-package-manager) " -S python")))
(unless (executable-find "pip")
  (async-shell-command "sudo " (kb/which-package-manager) " -S pip"))

;;; Javascript
(unless (executable-find "npm")
  (async-shell-command "sudo " (kb/which-package-manager) " -S npm"))
(unless (executable-find "yarn")
  (async-shell-command "sudo npm -S yarn -g"))

;;; Rust
(unless (executable-find "rustc")
  (async-shell-command (concat "sudo " (kb/which-package-manager) " -S rust")))
(unless (executable-find "cargo")
  (async-shell-command (concat "sudo " (kb/which-package-manager) " -S cargo")))
(unless (executable-find "watchexec")   ; For straight.el
  (async-shell-command (concat "cargo install watchexec-cli")))

;;; Java
(unless (executable-find "java")
  (async-shell-command (concat "sudo " (kb/which-package-manager) " -S jre-openjdk")))

;;; external-programs-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'external-programs-rcp)
