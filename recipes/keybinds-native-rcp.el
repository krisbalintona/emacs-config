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

;;; Ctl-x
(general-define-key
 :keymaps 'ctl-x-map
 "K" '(kill-this-buffer :which-key "Kill this buffer")
 )

;;; Everywhere
(general-define-key
 "<escape>" 'keyboard-escape-quit       ; Make ESC quit everywhere
 "C-x C-c" 'nil ; Unbinds `save-buffers-kill-emacs'. Prevents me from leaving Emacs.
 )

;;; Global-map
(general-define-key
 :keymaps 'global-map
 :states 'insert
 "S-<return>" '(lambda ()                    ; Go back a line
                 (interactive)
                 (forward-line -1)
                 (back-to-indentation))
 [remap newline] '(lambda ()                 ; Newline with indent
                    (interactive)
                    (insert "\n")
                    (indent-according-to-mode))
 "M-<return>" '(lambda ()                    ; Insert newline above and go to it
                 (interactive)
                 (move-beginning-of-line 1)
                 (insert "\n")
                 (forward-line -1)
                 (indent-according-to-mode))
 "C-<return>" '(lambda ()                    ; Insert newline below and go to it
                 (interactive)
                 (move-end-of-line 1)
                 (insert "\n")
                 (indent-according-to-mode))
 "C-a" 'back-to-indentation
 "C-e" 'move-end-of-line
 "C-k" 'kill-visual-line
 "C-y" 'yank
 "C-S-k" 'join-line
 "C-S-j" '(lambda () (interactive) (join-line 1))
 )

(general-define-key
 :keymaps '(global-map general-override-mode-map)
 :states '(normal visual motion)
 "K" 'join-line
 "J" '(lambda () (interactive) (join-line 1))
 )

;;; keybinds-native-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'keybinds-native-rcp)
