;;; keybinds-general-rcp.el --- Summary
;;
;;; Commentary:
;;
;; General.el setup and very broad keybindings.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)

;;; General itself
;; Leader key capabilities and more convenient key definitions and bindings.
(use-package general
  :config
  (general-auto-unbind-keys)        ; Overwrite keybinds without returning error
  )

;;; Leader keys
(general-create-definer kb/general-keys ; General leader key
  :keymaps '(normal visual insert motion ryo-modal-mode-map)
  :prefix "SPC"
  :global-prefix "M-SPC"
  )
(general-create-definer kb/note-keys    ; For all lsp-related commands
  :keymaps '(normal visual insert motion ryo-modal-mode-map)
  :prefix "SPC n"
  :global-prefix "M-SPC n"
  )
(general-create-definer kb/magit-keys   ; Magit et al.
  :keymaps '(normal visual insert motion ryo-modal-mode-map)
  :prefix "SPC g"
  :global-prefix "M-SPC g"
  )
(general-create-definer kb/project-keys ; Projects
  :keymaps '(normal visual insert motion ryo-modal-mode-map)
  :prefix "SPC p"
  :global-prefix "M-SPC p"
  )
(general-create-definer kb/lsp-keys     ; For all lsp-related commands
  :keymaps '(normal visual insert motion ryo-modal-mode-map)
  :keymaps 'lsp-mode-map
  :prefix "\\"
  :global-prefix "M-SPC \\"
  )
(general-create-definer kb/buffer-keys  ; Buffers
  :keymaps '(normal visual insert motion ryo-modal-mode-map)
  :prefix "SPC b"
  :global-prefix "M-SPC b"
  )
(general-create-definer kb/file-keys    ; File-related
  :keymaps '(normal visual insert motion ryo-modal-mode-map)
  :prefix "SPC f"
  :global-prefix "M-SPC f"
  )
(general-create-definer kb/mark-keys    ; Marks (e.g. bookmarks)
  :keymaps '(normal visual insert motion ryo-modal-mode-map)
  :prefix "SPC m"
  :global-prefix "M-SPC m"
  )
(general-create-definer kb/help-keys    ; Help
  :keymaps '(normal visual insert motion ryo-modal-mode-map)
  :prefix "SPC h"
  :global-prefix "M-SPC h"
  )
(general-create-definer kb/nav-keys     ; Navigation in buffers
  :keymaps '(normal visual insert motion ryo-modal-mode-map)
  :prefix "SPC j"
  :global-prefix "M-SPC j"
  )
(general-create-definer kb/yank-kill-keys ; Killing, yanking, and popping
  :keymaps '(normal visual insert motion ryo-modal-mode-map)
  :prefix "SPC i"
  :global-prefix "M-SPC i"
  )
(general-create-definer kb/open-keys    ; Open certain things
  :keymaps '(normal visual insert motion ryo-modal-mode-map)
  :prefix "SPC o"
  :global-prefix "M-SPC o"
  )
(general-create-definer kb/toggle-keys  ; Toggles
  :keymaps '(normal visual insert motion ryo-modal-mode-map)
  :prefix "SPC t"
  :global-prefix "M-SPC t"
  )
(general-create-definer kb/window-keys  ; Windows
  :keymaps '(normal visual insert motion ryo-modal-mode-map)
  :prefix "SPC w"
  :global-prefix "M-SPC w"
  )

;;; Keybinding labels
(kb/general-keys
  "b" '(:ignore t :which-key "Buffers")
  "e" '(:ignore t :which-key "Evaluation")
  "f" '(:ignore t :which-key "Files")
  "g" '(:ignore t :which-key "Magit")
  "h" '(:ignore t :which-key "Help")
  "i" '(:ignore t :which-key "Yank and kill")
  "j" '(:ignore t :which-key "Navigation")
  "m" '(:ignore t :which-key "Marks")
  "n" '(:ignore t :which-key "Notes")
  "o" '(:ignore t :which-key "Open...")
  "p" '(:ignore t :which-key "Project")
  "t" '(:ignore t :which-key "Toggles")
  "w" '(:ignore t :which-key "Windows")
  )

;;; Unbinding leader key prefix
(general-define-key
 :keymaps '(Info-mode-map help-mode-map calc-mode-map Man-mode-map woman-mode-map custom-mode-map dired-mode-map pdf-view-mode-map debugger-mode-map
                          ;; Magit modes
                          magit-mode-map magit-log-mode-map magit-diff-mode-map magit-process-mode-map
                          )
 :keymaps '(normal visual motion)
 "SPC" '(:ignore t))

;;; Which-key
;; Show keybind tooltips
(use-package which-key
  :demand t
  :custom
  ;; These variables should be set before which-key-mode is activated
  (which-key-idle-delay 1.6)
  (which-key-idle-secondary-delay 1) ; Delay after which-key has already been shown
  (which-key-show-early-on-C-h t)    ; Show which-key help immediately
  (which-key-add-column-padding 0)
  (which-key-max-display-columns nil)
  ;; (which-key-show-transient-maps t) ; Necessary so show embark keybinds with which-key
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'right)
  (which-key-side-window-max-width 0.23)
  :config
  (which-key-mode)

  ;; Don't display C-u, digit, and other numeric keypad bindings
  (push '(("^[0-9-]\\|kp-[0-9]\\|kp-subtract\\|C-u$" . nil) . ignore)
        which-key-replacement-alist)
  )

;;; keybinds-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'keybinds-general-rcp)
