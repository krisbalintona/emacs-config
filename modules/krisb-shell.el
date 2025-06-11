;; -*- lexical-binding: t; -*-

;;; Shell
;; Built-in shell
(use-package shell
  :ensure nil
  :custom
  (shell-command-prompt-show-cwd t)     ; Emacs 27.1
  (shell-input-autoexpand 'input)
  (shell-highlight-undef-enable t)                   ; Emacs 29.1
  (shell-has-auto-cd nil)                            ; Emacs 29.1
  (shell-get-old-input-include-continuation-lines t) ; Emacs 30.1
  (shell-kill-buffer-on-exit t))                     ; Emacs 29.1

;;; Comint
(use-package comint
  :ensure nil
  :custom
  (comint-prompt-read-only t)
  (comint-buffer-maximum-size 9999)
  (comint-completion-autolist t)
  (comint-scroll-to-bottom-on-input 'this)
  (comint-scroll-to-bottom-on-output 'this)
  (comint-input-autoexpand 'input)
  (ansi-color-for-comint-mode t))

;;; Eshell
;;;; Eshell-z
;; Use z in Eshell
(use-package eshell-z
  :after eshell
  :demand
  :custom
  (eshell-z-freq-dir-hash-table-file-name (getenv "Z_DATA"))
  (eshell-z-exclude-dirs nil)
  :init
  ;; TODO 2025-04-25: In NixOS, I don't have a solution yet.  Firstly, the
  ;; Z_DATA environment variable seems to be inherited from the root user, not
  ;; mine.  Secondly, the data file created by Z doesn't have user write
  ;; permissions by default, I think.
  (exec-path-from-shell-copy-env "Z_DATA"))

;;; Provide
(provide 'krisb-shell)
