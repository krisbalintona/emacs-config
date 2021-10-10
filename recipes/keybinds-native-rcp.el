;;; keybinds-native-rcp.el --- Summary
;;
;;; Commentary:
;;
;; General.el setup and very broad keybindings.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Everywhere
(general-define-key
 "<escape>" 'keyboard-escape-quit       ; Make ESC quit everywhere
 "C-x C-c" 'nil ; Unbinds `save-buffers-kill-emacs'. Prevents me from leaving Emacs.
 )

;;; Global-map
(general-define-key
 :keymaps 'global-map
 "C-M-;" 'eval-expression               ; Evaluate inputted expression
 "C-x K" 'kill-this-buffer
 "TAB" nil                              ; Remove `evil-jump-backward' keybind
 )
(general-define-key
 :keymaps 'global-map
 :states 'insert
 "S-<return>" '(lambda ()                    ; Go back a line
                 (interactive)
                 (forward-line -1)
                 (back-to-indentation))
 [remap newline] '(lambda ()            ; Newline with indent
                    (interactive)
                    (insert "\n")
                    (indent-according-to-mode))
 "M-<return>" '(lambda ()                    ; Insert newline and go to it
                 (interactive)
                 (move-beginning-of-line 1)
                 (insert "\n")
                 (forward-line -1)
                 (indent-according-to-mode))
 "C-a" 'back-to-indentation
 "C-e" 'move-end-of-line
 "C-k" 'kill-visual-line
 "C-y" 'yank
 )

;;; keybinds-native-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'keybinds-native-rcp)
