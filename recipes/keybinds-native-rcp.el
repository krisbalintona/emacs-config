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

;;;; Everywhere
(general-define-key
 "<escape>" 'keyboard-escape-quit       ; Make ESC quit everywhere
 )

;;;; Global-map
(general-define-key
 :keymaps 'global-map
 "C-x C-c" nil ; Unbinds `save-buffers-kill-emacs'. Prevents me from leaving Emacs.
 "C-M-;" 'eval-expression               ; Evaluate inputted expression
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
 "C-<return>" '(lambda ()          ; Insert rest of line after point in newline above
                 (interactive)
                 (kill-line)
                 (save-excursion (beginning-of-line) (open-line 1))
                 (forward-line -1)
                 (yank)
                 (indent-according-to-mode)
                 (back-to-indentation))
 "C-a" 'back-to-indentation
 "C-e" 'move-end-of-line
 "C-k" 'kill-visual-line
 "C-y" 'yank
 )

;;; keybinds-native-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'keybinds-native-rcp)
