;;; keybinds-frameworks-rcp.el --- Summary
;;
;; This is where I set up general.el and define the main categories of prefixes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; General.el
;; Leader key capabilities
(use-package general
  :config
  (general-evil-setup)
  (general-auto-unbind-keys)

  (general-create-definer kb/leader-keys ; Use space as leader key
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "M-SPC"
    )

  (kb/leader-keys
    "t"  '(:ignore t :which-key "Toggles")
    "tl" '(display-line-numbers-mode :which-key "Line numbers")
    "o"  '(:ignore t :which-key "Open")
    "oc" '(calendar :which-key "Open calendar")
    "b"  '(:ignore t :which-key "Buffers")
    "bp" '(previous-buffer :which-key "Prev buffer")
    "bn" '(next-buffer :which-key "Next buffer")
    "f"  '(:ignore t :which-key "Files")
    "fs" '(save-buffer :which-key "Save buffer")
    "h"  '(:ignore t :which-key "Help")
    "q"  '(:ignore t :which-key "Quit")
    "qs" '(org-save-all-org-buffers :which-key "Save all org buffers")
    "l"  '(:ignore t :which-key "Langtool")
    "n" '(:ignore t :which-key "Org-roam")
    "i" '(:ignore t :which-key "Copying and pasting")
    "w" '(:ignore t :which-key "Manage windows")
    "g"   '(:ignore t :which-key "git")
    "e"   '(:ignore t :which-key "Eval stuff")
    "eb"  '(eval-buffer :which-key "Eval buffer")

    "u" 'universal-argument
    )
  )

;;;;; Misc keybinds
;; Package-agnostic keybinds
(kb/leader-keys
  :keymaps '(visual normal)
  "ee" '(eval-last-sexp :which-key "Eval last sexp")
  "er" '(eval-region :which-key "Eval region")
  )

;;;; Hydra
;;;;; Pretty-hydra
;; Hydra but with prettier displays
(use-package pretty-hydra
  :demand t
  :config
  ;; Straight.el integration
  (kb/leader-keys
    "hp" '(hydra:straight-helper/body :which-key "Straight.el management")
    )

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
      ("W" #'straight-watcher-quit "Kill file system watcher")
      ("g" #'straight-get-recipe "Copy particular recipe")
      ("e" #'straight-prune-build "Prune: delete packages not currently loaded (?)")) ; Recommended occasionally to clean up really long cache file (straight-cache-autoloads t) over time
     "Suspending and restoring package versions"
     (("v" #'straight-freeze-versions "\"Freeze\" all installed packages (?)")
      ("V" #'straight-thaw-versions "\"Unfreeze\" all frozen packages (?)"))
     ("q" nil) ; Reserved for quit
     ))
  )

;;;;;  Hydra-posframe
;; Prettify the hydra-frame my positioning it where you like
(use-package hydra-posframe
  :disabled t ; Too small sometimes
  :straight (hydra-posframe :type git :host github :repo "Ladicle/hydra-posframe")
  :config
  (hydra-posframe-enable)
  )

;;; Commentary:
;;
;;
;;; keybinds-frameworks-rcp.el ends here
