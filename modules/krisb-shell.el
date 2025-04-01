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
(use-package fancy-compilation
  :custom
  ;; The TERM environment variable to use (set to an empty string to leave
  ;; unset).  Set to \"ansi-term\" for the default of ansi-term
  (fancy-compilation-term nil)
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
                imenu-generic-expression `((nil ,eshell-prompt-regexp 0)))
    (when (featurep 'cape)
      (add-hook 'completion-at-point-functions #'cape-file nil t)))

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
  :hook (eshell-mode . krisb-eshell-atuin-setup-eshell-capf)
  :bind* ( :map eshell-mode-map
           ([remap eshell-isearch-backward-regexp] . eshell-atuin-history))
  :custom
  (eshell-atuin-save-duration t)
  (eshell-atuin-filter-mode 'global)
  (eshell-atuin-search-options '("--exit" "0"))
  (eshell-atuin-search-fields '(time command duration directory))
  (eshell-atuin-history-format "%-110c (in %i)")
  :config
  (eshell-atuin-mode 1)

  (defun krisb-eshell-atuin-setup-eshell-capf ()
    "Add `krisb-eshell-atium-capf' to the beginning of `completion-at-point-functions'."
    (add-hook 'completion-at-point-functions #'krisb-eshell-atium-capf -50 t))

  ;; Bespoke capf.  Especially useful with `completion-preview-mode'
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
            (t (format "%.0f months %s" (/ abs-diff (* 30 86400)) (if (< diff-time 0) "ago" "from now"))))))

  (defun krisb-eshell-atium-capf ()
    "Capf or `eshell-atuin' command history.
Meant for `completion-at-point-functions' in eshell buffers."
    (interactive)
    (when (bound-and-true-p eshell-atuin-mode)
      ;; Update cache first
      (eshell-atuin--history-rotate-cache)
      (eshell-atuin--history-update)
      (let* ((start (save-excursion (eshell-next-prompt 0) (point)))
             (end (save-excursion (goto-char (point-max))))
             (candidates
              (mapcar (lambda (e)
                        (let* ((command (alist-get 'command e))
                               (directory (alist-get 'directory e))
                               (time (alist-get 'time e))
                               ;; 2025-03-27: I manually parse the time string
                               ;; into relativetime string.  Upstream does not
                               ;; do it for us.
                               (relativetime (krisb-eshell-atuin--relative-time time)))
                          (put-text-property 0 (length command) 'relativetime relativetime command)
                          command))
                      eshell-atuin--history-cache)))
        (list start end candidates
              :exclulsive 'no                   ; Go to other capfs afterward
              :display-sort-function #'identity ; Keep in chronological order
              :annotation-function (lambda (s) (get-text-property 0 'relativetime s))))))) ; Add relative time annotation

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

;;; EAT
(use-package eat
  ;; 2024-12-29: See https://codeberg.org/akib/emacs-eat/pulls/133 for why we
  ;; use this fork of eat.
  :vc ( :url "https://codeberg.org/vifon/emacs-eat.git"
        :branch "fish-integration"
        :rev :newest)
  :hook ((eshell-load . eat-eshell-mode)
         (eshell-load . eat-eshell-visual-command-mode))
  :bind ( :map krisb-open-keymap
          ("E" . eat)
          :map project-prefix-map
          ("E" . eat-project)))

;;; Provide
(provide 'krisb-shell)
