;;; Pcmpl-args
;; Extend the build in `pcomplete'.  Includes flag and argument completion in
;; the shell.
(use-package pcmpl-args
  :after pcomplete)

;;; Eshell
;;;; Itself
(use-package eshell
  :ensure nil
  :hook ((eshell . visual-line-mode)
         (eshell . krisb-eshell-setup))
  :bind ( :map krisb-open-keymap
          ("e" . eshell)
          :map eshell-mode-map
          ([remap eshell-previous-matching-input] . consult-history))
  :config
  (defun krisb-eshell-setup ()
    "Buffer-local settings for eshell."
    (set-display-table-slot standard-display-table 0 ?\ )
    (setq-local scroll-margin 3
                line-spacing 0
                ;; `consult-outline' support for eshell prompts. See
                ;; https://github.com/minad/consult/wiki#consult-outline-support-for-eshell-prompts
                outline-regexp eshell-prompt-regexp
                ;; Imenu with eshell prompt history
                imenu-generic-expression `((nil ,eshell-prompt-regexp 0)))))

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

;;;; Eshell-z
;; Use z in Eshell
(use-package eshell-z
  :after eshell
  :demand
  :custom
  (eshell-z-freq-dir-hash-table-file-name (getenv "Z_DATA"))
  (eshell-z-exclude-dirs nil)
  :init
  (exec-path-from-shell-copy-env "Z_DATA"))

;;; Fish-mode
(use-package fish-mode
  :mode "\\.fish\\'")

;;; Provide
(provide 'krisb-shell)
