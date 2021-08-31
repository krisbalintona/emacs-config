;;; shell-eshell-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Eshell-related packages and their configuration.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Eshell
(use-package esh-mode
  :straight nil
  :after shrink-path ; For a shorter directory name
  :gfhook
  ;; UI enhancements
  'visual-line-mode
  '(lambda () (set-display-table-slot standard-display-table 0 ?\ )
     ;; Text-wrap
     (face-remap-add-relative 'default :height 127) ; Change default face size
     (setq-local scroll-margin 3                    ; Scroll-margin
                 line-spacing 0)
     )
  'hide-mode-line-mode
  :general
  (:keymaps 'eshell-mode-map
            [remap eshell-previous-matching-input] '(consult-history :which-key "Command history")
            "<return>" 'eshell-send-input)
  (kb/leader-keys
    "oE" '(eshell :which-key "Open eshell")
    )
  :custom
  (eshell-kill-processes-on-exit t)
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all)
  (eshell-input-filter (lambda (input) (not (string-match-p "\\`\\s-+" input)))) ; Don't record command in history if prefixed with whitespace
  (eshell-glob-case-insensitive t)
  (eshell-error-if-no-glob t)
  (eshell-banner-message "Welcome to the shell, Onii-chan~ (◠﹏◠✿)\n")
  ;; Taken from https://protesilaos.com/dotemacs/#h:103a8795-c29c-474f-9ddf-ecafaa2f6775
  (eshell-modules-list
   '(eshell-alias
     eshell-basic
     eshell-cmpl
     eshell-dirs
     eshell-glob
     eshell-hist
     eshell-ls
     eshell-pred
     eshell-prompt
     eshell-script
     eshell-term
     eshell-tramp
     eshell-unix))
  :preface (defvaralias 'esh-mode-hook 'eshell-mode-hook) ; For more convenient `:gfhook'
  :init
  ;; Eshell modules should be loaded manually
  ;; Taken from
  ;; https://protesilaos.com/dotemacs/#h:103a8795-c29c-474f-9ddf-ecafaa2f6775
  (require 'em-cmpl)
  (require 'em-dirs)
  (setq eshell-cd-on-directory t)

  (require 'em-tramp)
  (setq password-cache t)               ; Cache password for tramp
  (setq password-cache-expiry 600)      ; Seconds passwords are cached

  (require 'em-hist)
  (setq eshell-hist-ignoredups 'erase)  ; Only keep last duplicate
  (setq eshell-save-history-on-exit t)
  :config
  (setenv "PAGER" "cat") ; solves issues, such as with 'git log' and the default 'less'

  ;; Save history on eshell command
  (setq eshell-save-history-on-exit nil) ; Useless since only saves upon exiting eshell session
  (defun eshell-append-history ()
    "Call `eshell-write-history' with the `append' parameter set to `t'."
    (when eshell-history-ring
      (let ((newest-cmd-ring (make-ring 1)))
        (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
        (let ((eshell-history-ring newest-cmd-ring))
          (eshell-write-history eshell-history-file-name t)))))
  (add-hook 'eshell-post-command-hook #'eshell-append-history)
  )

;;;; Eshell prompt
;; Entirely taken from http://www.modernemacs.com/post/custom-eshell/
(with-eval-after-load 'esh-mode
  (require 'dash)
  (require 's)

  (defmacro with-face (STR &rest PROPS)
    "Return STR propertized with PROPS."
    `(propertize ,STR 'face (list ,@PROPS)))

  (defmacro esh-section (NAME ICON FORM &rest PROPS)
    "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
    `(setq ,NAME
           (lambda () (when ,FORM
                   (-> ,ICON
                       (concat esh-section-delim ,FORM)
                       (with-face ,@PROPS))))))

  (defun esh-acc (acc x)
    "Accumulator for evaluating and concatenating esh-sections."
    (--if-let (funcall x)
        (if (s-blank? acc)
            it
          (concat acc esh-sep it))
      acc))

  (defun esh-prompt-func ()
    "Build `eshell-prompt-function'"
    (concat esh-header
            (-reduce-from 'esh-acc "" eshell-funcs)
            "\n"
            eshell-prompt-string))

  ;; Separator between esh-sections
  (setq esh-sep " | ")  ; or " | "

  ;; Separator between an esh-section icon and form
  (setq esh-section-delim " ")

  ;; Eshell prompt header
  (setq esh-header "\n┌─")  ; or "\n┌─"

  ;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
  ;; your login, these can be the same.
  (setq eshell-prompt-regexp "└─> λ ")   ; or "└─> "
  (setq eshell-prompt-string "└─> λ ")   ; or "└─> "

  (esh-section esh-dir
               " \xf07c "  ;  (favicon folder)
               (if (featurep 'shrink-path) ; This is where shrink-path is used
                   (concat (car (shrink-path-prompt (eshell/pwd)))
                           (cdr (shrink-path-prompt (eshell/pwd))))
                 (abbreviate-file-name (eshell/pwd))) ; Fallback to default if no shrink-path
               '(:foreground "gold" :weight bold))

  (require 'magit) ; Need `magit' to load `magit-get-current-branch'
  (esh-section esh-git
               "ᛦ"  ;  (git icon)
               (magit-get-current-branch)
               '(:foreground "pink"))

  ;; (esh-section esh-python
  ;;              "\xe928"  ;  (python icon)
  ;;              pyvenv-virtual-env-name)

  (esh-section esh-clock
               "\xf017 "  ;  (clock icon)
               (format-time-string "%H:%M" (current-time))
               '(:foreground "forest green"))

  ;; Below I implement a "prompt number" section
  (setq esh-prompt-num 0)
  (add-hook 'eshell-exit-hook (lambda () (setq esh-prompt-num 0)))
  (advice-add 'eshell-send-input :before
              (lambda (&rest args) (setq esh-prompt-num (incf esh-prompt-num))))

  (esh-section esh-num
               "\xf0c9 "  ;  (list icon)
               (number-to-string esh-prompt-num)
               '(:foreground "brown"))

  ;; Choose which eshell-funcs to enable
  (setq eshell-funcs (list esh-dir esh-git esh-clock esh-num))

  ;; Enable the new eshell prompt
  (setq eshell-prompt-function 'esh-prompt-func)
  )

;;;; Esh-opt
(use-package esh-opt ; An eshell module that needs to be loaded
  :straight nil
  :custom
  (eshell-history-buffer-when-process-dies t)
  (eshell-visual-commands '("htop" "vi" "vim" "nvim" "btm")) ; Commands to run in term buffer to properly display from eshell
  )

;;;; Eshell-toggle
;; Toggle eshell window in bottom of current buffer
(use-package eshell-toggle
  :general (kb/leader-keys
             "oe" '(eshell-toggle :which-key "Toggle eshell")
             )
  :custom
  (eshell-toggle-size-fraction 3) ; How big is the window?
  ;; (eshell-toggle-use-projectile-root t)
  (eshell-toggle-use-git-root t)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell) ; Terminal emulator to use
  (eshell-toggle-run-command nil) ; Command to run in new buffer
  (eshell-toggle-window-side 'above)
  )

;;;; Eshell-syntax-highlighting
;; Zsh-esque syntax highlighting in eshell
(use-package eshell-syntax-highlighting
  :ghook ('after-init-hook 'eshell-syntax-highlighting-global-mode)
  )

;;;; Esh-autosuggest
;; Has shadowed suggestions from shell history (like in zsh)
(use-package esh-autosuggest
  :ghook 'eshell-mode-hook
  :custom
  (esh-autosuggest-delay 0.25)
  :config
  (set-face-foreground 'company-preview-common "#4b5668")
  )

;;;; Fish-completion
;; Fall back on fish when Emacs does not find any completion candidate with its
;; native completion support (pcomplete).
(use-package fish-completion
  :ensure-system-package (fish)
  :ghook ('after-init-hook 'global-fish-completion-mode)
  )

;;;; Eshell-up
;; Go up directories easily with eshell-up (as well as eshell-up-peek)
;; Their aliases are up and pk,respectively
(use-package eshell-up
  :custom
  (eshell-up-ignore-case nil)
  (eshell-up-print-parent-dir t) ; Print the matching parent directory before changing to it:
  )

;;;; Eshell-z
;; Port of z cli command. Cd into most used and recent directories
;; Type z --help for usage
(use-package eshell-z)

;;;; Esh-help
;; See help doctrings/man pages for any function via esh-help-run-help. Also see
;; help strings if eldoc mode is enabled
(use-package esh-help
  :ghook ('after-init-hook 'setup-esh-help-eldoc)
  )

;;;; Shrink-path
;; Truncate eshell directory path (has to be configured in mycustom eshell
;; prompt)
(use-package shrink-path
  :after esh-mode
  )

;;; shell-eshell-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'shell-eshell-rcp)
