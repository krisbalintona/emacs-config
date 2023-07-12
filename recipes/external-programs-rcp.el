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

;;; Python
(unless (executable-find "python")
  (system-packages-install "python"))
(unless (executable-find "pip")
  (system-packages-install "pip"))

;;; Javascript
(unless (executable-find "npm")
  (system-packages-install "npm"))
(unless (executable-find "yarn")
  (system-packages-install "yarn"))

;;; Rust
(unless (executable-find "rustc")
  (system-packages-install "rust"))
(unless (executable-find "cargo")
  (system-packages-install "cargo"))
(unless (executable-find "watchexec")
  (system-packages-install "watchexec"))

;;; Java
(unless (executable-find "java")
  (system-packages-install "jre-openjdk"))

;;; external-programs-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'external-programs-rcp)
