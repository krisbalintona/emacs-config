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
  (general-auto-unbind-keys))       ; Overwrite keybinds without returning error

;;; Leader keys
(general-create-definer kb/note-keys    ; For all lsp-related commands
  :prefix "C-c n")
(general-create-definer kb/lsp-keys     ; For all lsp-related commands
  :keymaps '(lsp-mode-map)
  :states '(normal visual insert motion)
  :prefix "\\"
  :global-prefix "M-SPC \\"
  )
(general-create-definer kb/dap-keys     ; For all dap commands
  :keymaps '(lsp-mode-map)
  :states '(normal visual insert motion)
  :prefix "|"
  :global-prefix "M-SPC |"
  )
(general-create-definer kb/file-keys    ; File-related
  :prefix "C-c f")
(general-create-definer kb/nav-keys     ; Navigation in buffers
  :prefix "C-c c")
(general-create-definer kb/yank-kill-keys ; Killing, yanking, and popping
  :prefix "C-c i")
(general-create-definer kb/open-keys    ; Open certain things
  :prefix "C-c o")
(general-create-definer kb/toggle-keys  ; Toggles
  :prefix "H-t")

;;; Use-package-chords
;; Use-package integration with `key-chord'
(use-package use-package-chords
  :demand t
  :requires key-chord)

;;; Which-key
;; Show keybind tooltips
(use-package which-key
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
  :init
  (which-key-mode)

  ;; Don't display C-u, digit, and other numeric keypad bindings
  (push '(("^[0-9-]\\|kp-[0-9]\\|kp-subtract\\|C-u$" . nil) . ignore)
        which-key-replacement-alist))

;;; keybinds-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'keybinds-general-rcp)
