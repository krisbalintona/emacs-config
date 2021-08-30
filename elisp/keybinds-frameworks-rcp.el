;;; keybinds-frameworks-rcp.el --- Summary
;;
;;; Commentary:
;;
;; This is where I set up general.el and define the main categories of prefixes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)

;;;; General.el
;; Leader key capabilities
(use-package general
  :config
  (general-auto-unbind-keys)            ; Overwrite keybinds without error

  (general-unbind
    :keymaps '(Info-mode-map help-mode-map calc-mode-map Man-mode-map woman-mode-map custom-mode-map dired-mode-map pdf-view-mode-map debugger-mode-map
                             ;; Magit modes
                             magit-mode-map magit-log-mode-map magit-diff-mode-map magit-process-mode-map
                             )
    :states '(normal visual motion)
    "SPC")

  ;; My leader key definition and high-level keybindings
  (general-create-definer kb/leader-keys ; Use space as leader key
    :keymaps '(normal visual insert motion emacs)
    :prefix "SPC"
    :global-prefix "M-SPC"
    )
  (kb/leader-keys
    "t" '(:ignore t :which-key "Toggles")
    "o" '(:ignore t :which-key "Open...")
    "oc" '(calc :which-key "Open calculator")
    "b" '(:ignore t :which-key "Buffers")
    "bp" '(previous-buffer :which-key "Prev buffer")
    "bn" '(next-buffer :which-key "Next buffer")
    "f" '(:ignore t :which-key "Files")
    "ff" '(find-file :which-key "Find file")
    "fs" '(save-buffer :which-key "Save buffer")
    "h" '(:ignore t :which-key "Help")
    "hf" '(describe-function :which-key "Desc func")
    "hv" '(describe-variable :which-key "Desc var")
    "ho" '(describe-symbol :which-key "Desc sym")
    "q" '(:ignore t :which-key "Quit")
    "qs" '(org-save-all-org-buffers :which-key "Save all org buffers")
    "l" '(:ignore t :which-key "Langtool")
    "n" '(:ignore t :which-key "Org-roam")
    "i" '(:ignore t :which-key "Copying and pasting")
    "w" '(:ignore t :which-key "Manage windows")
    "g" '(:ignore t :which-key "git")
    "e" '(:ignore t :which-key "Eval stuff")
    "eb" '(eval-buffer :which-key "Eval buffer")
    "ee" '(eval-last-sexp :which-key "Eval last sexp")
    "er" '(eval-region :which-key "Eval region")
    "u" '(universal-argument :which-key "Universal argument")
    )

  ;; Multiple universal arguments
  (general-define-key
   :keymaps 'universal-argument-map
   :states '(normal visual motion)
   "u" 'universal-argument-more
   )
  ;; Make ESC quit everywhere
  (general-define-key "<escape>" 'keyboard-escape-quit)
  )

;;;; Native-Emacs keybinds
(general-define-key
 :keymaps 'minibuffer-mode-map
 ;; Text navigation
 "C-f" 'end-of-line
 "C-b" 'beginning-of-line
 "M-f" 'forward-word
 "M-F" 'forward-to-word
 "M-b" 'backward-word
 "M-B" 'backward-to-word
 "M-h" 'left-char
 "M-l" 'right-char
 "M-j" 'evil-next-visual-line
 "M-k" 'evil-previous-visual-line
 ;; Candidate navigation
 "M-k" 'previous-line
 "M-j" 'next-line
 )
(general-define-key
 :keymaps '(prog-mode-map text-mode-map)
 :states 'insert
 ;; Newline above
 [remap newline] '(lambda ()
                    (interactive)
                    (insert "\n")
                    (indent-according-to-mode))
 "M-RET" '(lambda ()
            (interactive)
            (move-beginning-of-line 1)
            (insert "\n")
            (forward-line -1)
            (indent-according-to-mode))
 ;; Beginning and end of line
 "C-f" 'end-of-line
 "C-b" 'beginning-of-line
 ;; Next and previous word
 "M-f" 'forward-word
 "M-F" 'forward-to-word
 "M-b" 'backward-word
 "M-B" 'backward-to-word
 ;; Navigation
 "M-h" 'left-char
 "M-l" 'right-char
 "M-j" 'next-line
 "M-k" 'previous-line
 )
(general-define-key
 :states '(normal motion visual insert)
 "S-<return>" '(lambda ()                    ; Go back a line
                 (interactive)
                 (forward-line -1)
                 (back-to-indentation))
 "C-M-;" '(eval-expression :which-key "Eval expression")
 )

;;; keybinds-frameworks-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'keybinds-frameworks-rcp)
