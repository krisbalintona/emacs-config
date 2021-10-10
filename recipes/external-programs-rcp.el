;;; external-programs-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Make sure any external programs (e.g. pip and python) are installed here and
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
  (async-shell-command (concat "sudo " (kb/which-package-manager) " install python")))
(unless (executable-find "pip")
  (async-shell-command "sudo " (kb/which-package-manager) " install pip"))

;;; Javascript
(unless (executable-find "npm")
  (async-shell-command "sudo " (kb/which-package-manager) " install npm"))
(unless (executable-find "yarn")
  (async-shell-command "sudo npm install yarn -g"))

;;; Rust
(unless (executable-find "rustc")
  (async-shell-command (concat "sudo " (kb/which-package-manager) " install rust")))
(unless (executable-find "cargo")
  (async-shell-command (concat "sudo " (kb/which-package-manager) " install cargo")))

;;; Java
(unless (executable-find "java")
  (async-shell-command (concat "sudo " (kb/which-package-manager) " install java-latest-openjdk")))

;;; external-programs-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'external-programs-rcp)
