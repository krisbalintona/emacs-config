;;; keybinds-general-rcp.el --- Summary
;;
;;; Commentary:
;;
;; General.el setup and very broad keybindings.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)

;;;; General itself
;; Leader key capabilities and more convenient key definitions and bindings.
(use-package general
  :config
  (general-auto-unbind-keys)        ; Overwrite keybinds without returning error
  )

;;;; Leader key
;; My leader key definition and high-level keybindings
(general-create-definer kb/leader-keys  ; Use space as leader key
  :keymaps '(normal visual insert motion)
  :prefix "SPC"
  :global-prefix "M-SPC"
  )

;;;; Unbinding leader key prefix
(general-unbind
  :keymaps '(Info-mode-map help-mode-map calc-mode-map Man-mode-map woman-mode-map custom-mode-map dired-mode-map pdf-view-mode-map debugger-mode-map
                           ;; Magit modes
                           magit-mode-map magit-log-mode-map magit-diff-mode-map magit-process-mode-map
                           )
  :states '(normal visual motion)
  "SPC")

;;;; Keybinding labels and orphan bindings
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
  "fS" '(save-some-buffers :which-key "Save most buffers")
  "h" '(:ignore t :which-key "Help")
  "q" '(:ignore t :which-key "Quit")
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

;;;; Which-key
;; Show keybind tooltips
(use-package which-key
  :demand t
  :custom
  ;; These variables should be set before which-key-mode is activated
  (which-key-idle-delay 1.6)
  (which-key-idle-secondary-delay 1) ; Delay after which-key has already been shown
  (which-key-show-early-on-C-h t) ; Show which-key help immediately
  (which-key-add-column-padding 0)
  (which-key-max-display-columns nil)
  ;; (which-key-show-transient-maps t) ; Necessary so show embark keybinds with which-key
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'right)
  (which-key-side-window-max-width 0.23)
  :config
  (which-key-mode)
  
  ;; Don't display C-u, digit, and other numeric keybad bindings
  (push '(("^[0-9-]\\|kp-[0-9]\\|kp-subtract\\|C-u$" . nil) . ignore)
        which-key-replacement-alist)
  )

;;; keybinds-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'keybinds-general-rcp)
