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
  (general-evil-setup)
  (general-auto-unbind-keys)

  ;; Make ESC quit everywhere
  (general-define-key "<escape>" 'keyboard-escape-quit)

  (general-unbind
    :keymaps '(Info-mode-map help-mode-map calc-mode-map Man-mode-map woman-mode-map custom-mode-map dired-mode-map pdf-view-mode-map debugger-mode-map
                             ;; Magit modes
                             magit-mode-map magit-log-mode-map magit-diff-mode-map magit-process-mode-map
                             )
    "SPC")

  (general-create-definer kb/leader-keys ; Use space as leader key
    :keymaps '(normal visual insert motion emacs)
    :prefix "SPC"
    :global-prefix "M-SPC"
    )

  (kb/leader-keys
    "t"  '(:ignore t :which-key "Toggles")
    "o"  '(:ignore t :which-key "Open")
    "oc" '(calendar :which-key "Open calendar")
    "b"  '(:ignore t :which-key "Buffers")
    "bp" '(previous-buffer :which-key "Prev buffer")
    "bn" '(next-buffer :which-key "Next buffer")
    "f"  '(:ignore t :which-key "Files")
    "ff" '(find-file :which-key "Find file")
    "fs" '(save-buffer :which-key "Save buffer")
    "h"  '(:ignore t :which-key "Help")
    "hf" '(describe-function :which-key "Desc func")
    "hv" '(describe-variable :which-key "Desc var")
    "ho" '(describe-symbol :which-key "Desc sym")
    "q"  '(:ignore t :which-key "Quit")
    "qs" '(org-save-all-org-buffers :which-key "Save all org buffers")
    "l"  '(:ignore t :which-key "Langtool")
    "n" '(:ignore t :which-key "Org-roam")
    "i" '(:ignore t :which-key "Copying and pasting")
    "w" '(:ignore t :which-key "Manage windows")
    "g"   '(:ignore t :which-key "git")
    "e"   '(:ignore t :which-key "Eval stuff")
    "eb"  '(eval-buffer :which-key "Eval buffer")
    "ee" '(eval-last-sexp :which-key "Eval last sexp")
    "er" '(eval-region :which-key "Eval region")
    "u" 'universal-argument
    )
  ;; Multiple universal arguments
  (general-define-key
   :keymaps 'universal-argument-map
   :states '(normal visual motion)
   "u" 'universal-argument-more
   )
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
 :states 'normal)
(general-define-key
 :states 'insert
 ;; Newline above
 "RET" '(lambda ()
          (interactive)
          (newline)
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
 "S-<return>" '(lambda ()
                 (interactive)
                 (forward-line -1)
                 (back-to-indentation))
 )

;;;; Pretty-hydra
;; Hydra but with prettier displays
(use-package pretty-hydra
  :general
  (kb/leader-keys
    "hp" '(hydra:straight-helper/body :which-key "Straight.el management")
    )
  :preface
  (use-package hydra)
  (use-package default-text-scale)
  :config
  (pretty-hydra-define hydra:straight-helper
    (:hint t :foreign-keys run :quit-key "q" :exit t)
    ("Package building"
     (("t" #'straight-use-package "Temporarily install package")
      ("c" #'straight-check-all "Check and build all modified")
      ("C" #'straight-check-package "Check and build particular modified")
      ("r" #'straight-rebuild-all "Rebuild all")
      ("R" #'straight-rebuild-package "Rebuild particular"))
     "Managing remote and local repos"
     (("f" #'straight-fetch-all "Fetch all")
      ("F" #'straight-fetch-package "Fetch particular")
      ("p" #'straight-pull-all "Pull all")
      ("P" #'straight-pull-package "Pull particular")
      ("u" #'straight-push-all "Push all")
      ("U" #'straight-push-package "Push particular")
      ("m" #'straight-merge-all "Merge all")
      ("M" #'straight-merge-package "Merge particular"))
     "Automatic and manual updates"
     (("n" #'straight-normalize-all "Normalize all")
      ("N" #'straight-normalize-package "Normalize particular")
      ("w" #'straight-watcher-start "Start file system watcher")
      ("W" #'straight-watcher-stop "Kill file system watcher")
      ("g" #'straight-get-recipe "Copy particular recipe")
      ("e" #'straight-prune-build "Prune: delete packages not currently loaded (?)")) ; Recommended occasionally to clean up really long cache file (straight-cache-autoloads t) over time
     "Suspending and restoring package versions"
     (("v" #'straight-freeze-versions "\"Freeze\" all installed packages (?)")
      ("V" #'straight-thaw-versions "\"Unfreeze\" all frozen packages (?)"))
     ("q" nil) ; Reserved for quit
     ))

  ;; Resize windows easily with hydra
  (pretty-hydra-define hydra:windows-and-font-size
    (:color pink :hint t :foreign-keys run :quit-key "q")
    ("Size"
     (("b" balance-windows :color blue)
      ("j" enlarge-window "Enlarge vertically")
      ("k" shrink-window "Shrink vertically")
      ("l" enlarge-window-horizontally "Enlarge horizontally")
      ("h" shrink-window-horizontally "Shrink horizontally"))
     "Zoom"
     (("-" default-text-scale-decrease)
      ("+" default-text-scale-increase)
      ("0" default-text-scale-reset :color blue))
     "Buffers"
     (("w" (revert-buffer nil t) :color blue))
     ))

  (kb/leader-keys
    "w TAB" '(hydra:windows-and-font-size/body :which-key "Augment window sizing")
    )
  )

;;; keybinds-frameworks-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'keybinds-frameworks-rcp)
