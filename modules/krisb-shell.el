;;; Pcmpl-args
;; Extend the build in `pcomplete'.  Includes flag and argument completion in
;; the shell.
(use-package pcmpl-args
  :after pcomplete)

;;; Eshell
;;;; Eshell-atuin
;; Use Atuin (https://github.com/atuinsh/atuin) with eshell
(use-package eshell-atuin
  :after eshell
  :custom
  (eshell-atuin-save-duration t)
  (eshell-atuin-filter-mode 'global)
  (eshell-atuin-search-options '("--exit" "0"))
  (eshell-atuin-search-fields '(time command duration directory))
  (eshell-atuin-history-format "%-110c (in %i)")
  :config
  (eshell-atuin-mode 1))

;;;; Eshell-syntax-highlighting
;; Zsh-esque syntax highlighting in eshell
(use-package eshell-syntax-highlighting
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode 1))

;;; Provide
(provide 'krisb-shell)
