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

;;; Compile
(use-package compile
  :ensure nil
  :bind ("<f5>" . recompile)
  :custom
  (compilation-scroll-output 'first-error) ; Scroll with compile buffer
  (compilation-auto-jump-to-first-error 'if-location-known))

;;; Fancy-compilation
;; Make compilation outputs in compilation buffers more pleasant to see.
(use-package fancy-compilation
  :custom
  ;; The TERM environment variable to use (set to an empty string to leave
  ;; unset).  Set to \"ansi-term\" for the default of ansi-term
  (fancy-compilation-term "eterm-color")
  (fancy-compilation-override-colors nil)
  (fancy-compilation-quiet-prelude t)
  (fancy-compilation-quiet-prolog nil)
  :config
  (fancy-compilation-mode 1))

;;; Eshell
;;;; Itself
(use-package eshell
  :ensure nil
  :hook ((eshell-mode . visual-line-mode)
         (eshell-mode . krisb-eshell-setup))
  :bind ( :map krisb-open-keymap
          ("e" . eshell))
  :custom
  (eshell-banner-message "")
  (eshell-kill-processes-on-exit t)
  (eshell-scroll-to-bottom-on-input 'this)
  (eshell-scroll-to-bottom-on-output 'this)
  (eshell-glob-case-insensitive nil)
  (password-cache t)                    ; Cache password for tramp
  (password-cache-expiry 600)           ; Seconds passwords are cached
  :config
  ;; Set up `completion-at-point-functions'
  (defun krisb-eshell-setup ()
    "Buffer-local settings for eshell."
    (set-display-table-slot standard-display-table 0 ?\ )
    (setq-local scroll-margin 3
                line-spacing 0
                ;; TODO 2025-03-27: The `outline-regexp' and
                ;; `imenu-generic-expression' settings don't work anymore.  Not
                ;; sure why.
                ;; `consult-outline' support for eshell prompts. See
                ;; https://github.com/minad/consult/wiki#consult-outline-support-for-eshell-prompts
                outline-regexp eshell-prompt-regexp
                ;; Imenu with eshell prompt history
                imenu-generic-expression `((nil ,eshell-prompt-regexp 0))))

  ;; Eshell source in `consult-buffer'
  (with-eval-after-load 'consult
    ;; For showing eshell sources in `consult-buffer'. Taken from
    ;; https://github.com/minad/consult#multiple-sources
    (defvar kb/consult-buffer--eshell-source
      (list :name     "Eshell Buffers"
            :category 'buffer
            :narrow   ?e
            :face     'consult-buffer
            :history  'buffer-name-history
            :annotate '(lambda (cand)
                         (substring-no-properties
                          (car (ring-elements
                                (buffer-local-value 'eshell-history-ring (get-buffer cand))))))
            :state    'consult--buffer-state
            :action   'display-buffer
            :items (lambda ()
                     (mapcar #'buffer-name
                             (seq-filter
                              (lambda (x)
                                (eq (buffer-local-value 'major-mode x) 'eshell-mode))
                              (buffer-list))))))
    (add-to-list 'consult-buffer-sources #'kb/consult-buffer--eshell-source 'append)))

;;;; Esh-mode
(use-package esh-mode
  :ensure nil
  :bind ( :map eshell-mode-map
          ([remap eshell-previous-matching-input] . consult-history)))

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

;;; Fish-mode
(use-package fish-mode
  :mode "\\.fish\\'")

;;; Provide
(provide 'krisb-shell)
