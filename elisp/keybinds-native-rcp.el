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

;;;; Minibuffer
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
 ;; Candidate navigation
 "M-k" 'previous-line
 "M-j" 'next-line
 "<escape>" 'keyboard-escape-quit       ; Make ESC quit everywhere
 )

"M-j" 'next-line"M-j" 'next-line"M-j" 'next-line"M-j" 'next-line"M-j" 'next-line"M-j" 'next-line"M-j" 'next-line"M-j" 'next-line
;;;; Prog- and text-mode-map
(general-define-key
 :keymaps '(prog-mode-map text-mode-map)
 :states 'insert
 ;; Newline above
 [remap newline] '(lambda ()                 ; Newline with indent
                    (interactive)
                    (insert "\n")
                    (indent-according-to-mode))
 "M-RET" '(lambda ()                         ; Insert newline and go to it
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
                 )
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

;;;; Global
;; NOTE 2021-08-29: I'm not sure if "global" is an apt name since I don't know
;; if this corresponds to `global-map'
(general-define-key
 :states '(normal motion visual insert)
 "S-<return>" '(lambda ()                    ; Go back a line
                 (interactive)
                 (forward-line -1)
                 (back-to-indentation))
 "C-M-;" '(eval-expression :which-key "Eval expression")
 )

;;; keybinds-native-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'keybinds-native-rcp)
