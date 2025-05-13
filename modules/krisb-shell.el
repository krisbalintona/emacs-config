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

;;; Pcmpl-args
;; Extend the build in `pcomplete'.  Includes flag and argument completion in
;; the shell.
(use-package pcmpl-args
  :after pcomplete)

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

;;;; Em-hist
(use-package em-hist
  :ensure nil
  :custom
  (eshell-history-size 20000)
  (eshell-hist-ignoredups 'erase)       ; Only keep last duplicate
  (eshell-save-history-on-exit t)
  ;; Fix eshell overwriting history. From
  ;; https://emacs.stackexchange.com/a/18569/15023.
  (eshell-save-history-on-exit nil))

;;;; Eshell-atuin
;; Use Atuin (https://github.com/atuinsh/atuin) with eshell
(use-package eshell-atuin
  :after eshell
  :demand t
  :bind* ( :map eshell-mode-map
           ([remap eshell-isearch-backward-regexp] . eshell-atuin-history))
  :custom
  (eshell-atuin-save-duration t)
  (eshell-atuin-filter-mode 'global)
  (eshell-atuin-search-options nil)
  (eshell-atuin-search-fields '(time command duration directory))
  (eshell-atuin-history-format "%-110c (in %i)")
  :config
  (eshell-atuin-mode 1)

  (defun eshell-atuin--update-cache ()
    "Ensure the `eshell-atuin' cache is up-to-date.
I use this as :before advice for `cape-history', which I use as in
`completion-at-point-functions', instead of `eshell-atuin-history'."
    (when (derived-mode-p 'eshell-mode)
      ;; These two functions are called before the `completing-read' of
      ;; `eshell-atuin-history'
      (eshell-atuin--history-rotate-cache)
      (eshell-atuin--history-update)))
  (advice-add 'cape-history :before #'eshell-atuin--update-cache)

  ;; TODO 2025-05-08: Right now I've removed the function that used to use the
  ;; following function.  However, I keep it here just in case I decide to
  ;; create a PR/issue to upstream this missing functionlality (relative time).
  (defun krisb-eshell-atuin--relative-time (time-string)
    "Turn TIME-STRING into a relative time string.
TIME-STRING is a string that represents a time; it is in the format
returned by \"atuin history list\" CLI command.  For example:
\"2025-03-27 07:17:40\".

An example of a return value for this function is: \"9 minutes ago\"."
    ;; HACK 2025-03-27: We use `ignore-errors' to catch any malformed data
    ;; stored by upstream (which happened at least once for me...)
    (when-let* ((then-time (ignore-errors (date-to-time time-string)))
                (now-time (current-time))
                (diff-time (float-time (time-subtract then-time now-time)))
                (abs-diff (abs diff-time)))
      (cond ((< abs-diff 60)
             (format "%.0f seconds %s" abs-diff (if (< diff-time 0) "ago" "from now")))
            ((< abs-diff 3600)
             (format "%.0f minutes %s" (/ abs-diff 60) (if (< diff-time 0) "ago" "from now")))
            ((< abs-diff 86400)
             (format "%.0f hours %s" (/ abs-diff 3600) (if (< diff-time 0) "ago" "from now")))
            ((< abs-diff (* 30 86400))
             (format "%.0f days %s" (/ abs-diff 86400) (if (< diff-time 0) "ago" "from now")))
            (t (format "%.0f months %s" (/ abs-diff (* 30 86400)) (if (< diff-time 0) "ago" "from now")))))))

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
  ;; TODO 2025-04-25: In NixOS, I don't have a solution yet.  Firstly, the
  ;; Z_DATA environment variable seems to be inherited from the root user, not
  ;; mine.  Secondly, the data file created by Z doesn't have user write
  ;; permissions by default, I think.
  (exec-path-from-shell-copy-env "Z_DATA"))

;;; Fish-mode
(use-package fish-mode
  :mode "\\.fish\\'")

;;; EAT
(use-package eat
  ;; 2024-12-29: See https://codeberg.org/akib/emacs-eat/pulls/133 for why we
  ;; use this fork of eat.
  :vc ( :url "https://codeberg.org/vifon/emacs-eat.git"
        :branch "fish-integration"
        :rev :newest)
  :hook ((fontaine-set-preset . krisb-eat--setup)
         (eshell-load . eat-eshell-mode)
         (eshell-load . eat-eshell-visual-command-mode))
  :bind ( :map krisb-open-keymap
          ("s" . eat)
          :map project-prefix-map
          ("s" . eat-project))
  :config
  ;; 2025-04-05: This resolves the continuation lines issue in EAT terminal
  ;; (including eat-shell in `eat-eshell-visual-command-mode').  The
  ;; continuation line issue results in, I think, the default font being too
  ;; wide, causing the width of the characters to exceed the width of the
  ;; window, resulting in ugly continuation lines that ruin the wrapping of the
  ;; output.
  (defun krisb-eat--setup ()
    "Set up an EAT terminal shell."
    (when (featurep 'fontaine)
      (set-face-attribute 'eat-term-font-0 nil
                          ;; This returns the default-family of the current
                          ;; preset, whether explicitly or implicitly set
                          :family (fontaine--get-preset-property fontaine-current-preset :term-family)))))

;;; Provide
(provide 'krisb-shell)
