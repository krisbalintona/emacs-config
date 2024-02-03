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
;;;; Itself
(use-package eshell
  :elpaca nil
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
            [remap eshell-previous-matching-input] '(consult-history :wk "Command history"))
  (:keymaps 'eshell-mode-map
            :states 'insert
            [remap back-to-indentation] 'eshell-bol)
  (kb/open-keys
    "e" '((lambda ()
            "Create an eshell buffer per default directory."
            (interactive)
            (require 'eshell)
            (let ((buf-name (concat "*eshell* "
                                    (file-name-nondirectory (directory-file-name (file-name-directory default-directory))))))
              (if (member buf-name (mapcar #'buffer-name (buffer-list)))
                  (display-buffer buf-name)
                (kb/eshell buf-name))))
          :wk "My eshell"))
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
  (add-hook 'eshell-post-command-hook #'eshell-append-history))

;;;; `consult-outine' with eshell
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
  (add-to-list 'consult-buffer-sources #'kb/consult-buffer--eshell-source 'append)

  ;; `consult-outline' support for eshell prompts
  ;; Taken from https://github.com/minad/consult/wiki#consult-outline-support-for-eshell-prompts
  (add-hook 'eshell-mode-hook (lambda () (setq outline-regexp eshell-prompt-regexp))))

;;; Shrink-path
;; Truncate eshell directory path (has to be configured in my custom eshell
;; prompt)
(use-package shrink-path
  :after eshell)

;;; Eshell-syntax-highlighting
;; Zsh-esque syntax highlighting in eshell
(use-package eshell-syntax-highlighting
  :ghook ('eshell-mode-hook 'eshell-syntax-highlighting-global-mode nil nil t)
  :config (eshell-syntax-highlighting-global-mode))

;;; Pcmpl-args
;; Extend the build in `pcomplete' to another level.
(use-package pcmpl-args
  :demand
  :after eshell)

;;; Esh-autosuggest
;; Has shadowed suggestions from shell history (like in zsh)
(use-package esh-autosuggest
  :disabled                  ; FIXME 2023-07-12: Freezes eshell for some reason
  :ghook 'eshell-mode-hook
  :custom
  (esh-autosuggest-delay 0.25))

;;; shell-eshell-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'shell-eshell-rcp)
