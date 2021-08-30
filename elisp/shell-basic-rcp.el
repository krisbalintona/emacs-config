;;; shell-basic-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Configuration and packages related to the basic `shell-mode'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Comint
;; Derived mode for all shells in Emacs
(use-package comint
  :straight nil
  :general
  (kb/leader-keys
    "os" '(shell :which-key "Shell-mode"))
  )

;;;; Shx
;; Extends `comint-mode' (e.g. `shell'). Compatible with other underlying REPLs
;; (e.g. zsh, bash, psql, ipython).
(use-package shx
  :ghook ('after-init-hook 'shx-global-mode) ; Run `shx-mode' in every `comint-mode' buffer
  :custom
  (shx-directory-tracker-regexp "^z ") ; Resync the shell's default-directory with Emacs on "z" commands:
  (shx-max-output 1024) ; Vastly improve display performance by breaking up long output lines
  (shx-max-input 1024) ; Prevent input longer than macOS's typeahead buffer from going through
  ;; (shx-img-height 250) ; Prefer inline images and plots to have a height of 250 pixels
  (shx-show-hints t)                    ; I need help!
  (shx-flash-prompt-time 1.0) ; Flash the previous comint prompt for a full second when using C-c C-p
  (shx-leader ":")            ; Use the default `:' to prefix sax commands
  )

;;; shell-basic-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'shell-basic-rcp)
