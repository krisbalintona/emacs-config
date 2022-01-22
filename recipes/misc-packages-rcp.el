;;; misc-packages-rcp.el --- Summary
;;
;;; Commentary:
;;
;; This is all the configuration of the org-roam package
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Scratch.el
;; Easily create scratch buffers for different modes
(use-package scratch
  ;; :demand t ; For the initial scratch buffer at startup
  :hook (scratch-create-buffer . kb/scratch-buffer-setup)
  :general ("C-c s" '(scratch :which-key "Create scratch"))
  :preface
  (defun kb/scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly. Taken from https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore/"
    (let* ((mode (format "%s" major-mode))
           (string (concat "Scratch buffer for: " mode "\n\n")))
      (when scratch-buffer
        (save-excursion
          (insert string)
          (goto-char (point-min))
          (comment-region (point-at-bol) (point-at-eol)))
        (forward-line 2))
      (rename-buffer (concat "*Scratch for " mode "*") t))
    )
  )

;;; Keyfreq
;; See a heatmap of your keypresses.
;; Use =keyfreq-show= to see how many times you used a command. Use =keyfreq-html= to get the original rendered HTML page. Use =keyfreq-html-v2= to get the keyboard heat map.
(use-package keyfreq
  :straight (keyfreq :type git :host github :repo "KirmTwinty/keyfreq")
  :gfhook 'keyfreq-autosave-mode
  :custom
  (keyfreq-folder (concat no-littering-var-directory "keyfreq"))
  ;; Commands not to be logged
  (keyfreq-excluded-commands '(self-insert-command
                               org-self-insert-command
                               ;; forward-char
                               ;; backward-char
                               ;; previous-line
                               ;; next-line
                               ))
  :config (keyfreq-mode)
  )

;;; Disable-mouse
;; Disable mouse interaction within Emacs
(use-package disable-mouse
  :disabled t ; I actually want to use my mouse when on laptop
  :ghook ('window-setup-hook 'global-disable-mouse-mode)
  :config
  ;; For evil states
  (mapc #'disable-mouse-in-keymap
        (list evil-motion-state-map
              evil-normal-state-map
              evil-visual-state-map
              evil-insert-state-map))
  )

;;; Proced
;; Built in process monitor
(use-package proced
  :straight nil
  :gfhook 'evil-emacs-state
  :custom
  (proced-auto-update-flag t)      ; Update live
  (proced-auto-update-interval 1)
  (proced-descend t)                ; Descending order?
  (proced-filter 'all)      ; Which processes are shown?
  )

;;; Tmr
;; Timer package/library from Prot
(use-package tmr
  :straight (tmr :type git :host gitlab :repo "protesilaos/tmr.el")
  :general ("C-c T t" '(tmr :which-key "Tmr")
            "C-c T c" '(tmr-cancel :which-key "Tmr cancel"))
  )

;;; Emojify
(use-package emojify
  :hook (window-setup . global-emojify-mode)
  :custom
  (emojify-composed-text-p t)
  (emojify-emoji-styles '(ascii unicode github))
  :config
  ;; NOTE 2021-11-12: This isn't related to emojify but it does relate to how
  ;; emojis are shown in Emacs. Taken from
  ;; https://github.com/alphapapa/ement.el#displaying-symbols-and-emojis
  (setf use-default-font-for-symbols nil)
  (set-fontset-font t 'unicode "Noto Emoji" nil 'append)
  )

;;; Copy-as-format
(use-package copy-as-format
  :custom
  (copy-as-format-default "slack")
  )

;;; Restart-emacs
(use-package restart-emacs
  :general ("C-x Q" '(restart-emacs :which-key "Restart emacs"))
  )

;;; Tempel
;; Small and simple snippet/template system compatible with corfu.
(use-package tempel
  :general
  ("M-+" 'tempel-complete               ; List all available templates
   "M-*" 'tempel-insert                 ; Insert typed template
   )
  (:keymaps 'tempel-map
            "C-c C-c" 'tempel-done
            )
  :custom
  (tempel-file (no-littering-expand-var-file-name "tempel-templates"))
  )

;;; Ffap
;; Find file at point
(use-package ffap
  :general (:states '(normal motion)
                    "g F" '(ffap-menu :which-key "FFAP menu")
                    )
  :config
  (when (featurep 'vertico)
    ;; Use Vertico (and orderless) instead of a completions buffer
    (advice-add #'ffap-menu-ask :around #'(lambda (&rest args)
                                            (cl-letf (((symbol-function #'minibuffer-completion-help)
                                                       #'ignore))
                                              (apply args)))
                ))
  )

;;; Built-in Emacs modes/packages
(use-package emacs
  :straight nil
  :hook (messages-buffer-mode . visual-line-mode)
  :general
  ;; Info-mode
  (:keymaps 'Info-mode-map
            :states '(visual normal motion)
            "SPC" nil ; For my leader key
            [remap evil-ret] 'Info-follow-nearest-node)
  (kb/general-keys
    "u" '(universal-argument :which-key "Universal argument")

    "fF" '(find-file-other-window :which-key "Find file other window")
    "fS" '(save-some-buffers :which-key "Save most buffers")
    "ff" '(find-file :which-key "Find file")
    "fs" '(save-buffer :which-key "Save buffer")

    "bn" '(next-buffer :which-key "Next buffer")
    "bp" '(previous-buffer :which-key "Prev buffer")
    "br" '((lambda ()
             (interactive)
             (revert-buffer nil t))
           :which-key "Revert buffer")

    "eb" '(eval-buffer :which-key "Eval buffer")
    "ee" '(eval-last-sexp :which-key "Eval last sexp")
    "ed" '(eval-defun :which-key "Eval top-level form")
    "er" '(eval-region :which-key "Eval region")

    "hi" '(info :which-key "Info pages")

    "oc" '(calc :which-key "Open calculator")
    "om" '((lambda ()
             (interactive)
             (pop-to-buffer "*Messages*"))
           :which-key "Open *Messages*")
    )
  (:keymaps 'universal-argument-map     ; Multiple universal arguments
            "u" 'universal-argument-more
            )
  :config
  (general-unbind 'normal help-mode-map "SPC")
  (general-unbind 'normal custom-mode-map "SPC")
  )

;;; misc-packages-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'misc-packages-rcp)
