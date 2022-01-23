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

;;; Eshell
(use-package eshell
  :gfhook
  ;; UI enhancements
  'visual-line-mode
  '(lambda ()
     (set-display-table-slot standard-display-table 0 ?\ )
     ;; Text-wrap
     (face-remap-add-relative 'default :height 127) ; Change default face size
     (setq-local scroll-margin 3                    ; Scroll-margin
                 line-spacing 0)
     ;; `consult-outline' support for eshell prompts. See
     ;; https://github.com/minad/consult/wiki#consult-outline-support-for-eshell-prompts
     (setq outline-regexp eshell-prompt-regexp)
     )
  'hide-mode-line-mode
  :general
  (:keymaps 'eshell-mode-map
            [remap eshell-previous-matching-input] '(consult-history :wk "Command history")
            "<return>" 'eshell-send-input)
  (kb/open-keys
    "e" '((lambda ()            ; Unique vterm buffer with current directory appended
            (interactive)
            (eshell (concat "*eshell* "
                            (file-name-nondirectory (directory-file-name (file-name-directory default-directory)))
                            )))
          :wk "Eshell"))
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
  :init
  ;; NOTE 2022-01-19: I changed the functionality when passed a nonnumerical
  ;; prefix argument.
  (defun kb/eshell (&optional arg)
    "Create an interactive Eshell buffer. Start a new Eshell session, or switch
to an already active session. Return the buffer selected (or created).

With a string prefix arg, create a new session with arg as its name.

With a nonnumeric, nonstring prefix arg, create a new session.

With a numeric prefix arg (as in `\\[universal-argument] 42 \\[eshell]'), switch
to the session with that number, or create it if it doesn't already exist.

The buffer name used for Eshell sessions is determined by the value of
`eshell-buffer-name', which see.

Eshell is a shell-like command interpreter. For more information on Eshell, see
Info node `(eshell)Top'."
    (interactive "P")
    (cl-assert eshell-buffer-name)
    (let ((buf (cond ((numberp arg)
                      (get-buffer-create (format "%s<%d>"
                                                 eshell-buffer-name
                                                 arg)))
                     ((stringp arg)     ; If arg is string
                      (generate-new-buffer arg))
                     (arg
                      (generate-new-buffer eshell-buffer-name))
                     (t
                      (get-buffer-create eshell-buffer-name))
                     )))
      (cl-assert (and buf (buffer-live-p buf)))
      (pop-to-buffer-same-window buf)
      (unless (derived-mode-p 'eshell-mode)
        (eshell-mode))
      buf))
  :config
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

  ;; Eshell prompt
  ;; Entirely taken from http://www.modernemacs.com/post/custom-eshell/
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
              (lambda (&rest args) (setq esh-prompt-num (cl-incf esh-prompt-num))))

  (esh-section esh-num
               "\xf0c9 "  ;  (list icon)
               (number-to-string esh-prompt-num)
               '(:foreground "brown"))

  ;; Choose which eshell-funcs to enable
  (setq eshell-funcs (list esh-dir esh-git esh-clock esh-num))

  ;; Enable the new eshell prompt
  (setq eshell-prompt-function 'esh-prompt-func)
  )

;;; Esh-opt
(use-package esh-opt ; An eshell module that needs to be loaded
  :straight nil
  :after eshell
  :custom
  (eshell-history-buffer-when-process-dies t)
  (eshell-visual-commands '("htop" "vi" "vim" "nvim" "btm")) ; Commands to run in term buffer to properly display from eshell
  )

;;; Shrink-path
;; Truncate eshell directory path (has to be configured in mycustom eshell
;; prompt)
(use-package shrink-path
  :after eshell
  )

;;; Eshell-syntax-highlighting
;; Zsh-esque syntax highlighting in eshell
(use-package eshell-syntax-highlighting
  :ghook ('eshell-mode-hook 'eshell-syntax-highlighting-global-mode nil nil t)
  :config (eshell-syntax-highlighting-global-mode)
  )

;;; Esh-autosuggest
;; Has shadowed suggestions from shell history (like in zsh)
(use-package esh-autosuggest
  :ghook 'eshell-mode-hook
  :custom
  (esh-autosuggest-delay 0.25)
  )

;;; Fish-completion
;; Fall back on fish when Emacs does not find any completion candidate with its
;; native completion support (pcomplete).
(use-package fish-completion
  :ensure-system-package (fish)
  :defer 3
  :config (global-fish-completion-mode)
  )

;;; shell-eshell-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'shell-eshell-rcp)
