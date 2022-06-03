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

;;; Global-map
(general-define-key
 "C-x C-c" 'nil ; Unbinds `save-buffers-kill-emacs'.
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
 "C-S-k" '(lambda ()                         ; Join line above
            (interactive)
            (save-excursion (join-line)))
 "C-S-j" '(lambda ()                         ; Join line below
            (interactive)
            (save-excursion (join-line 1))))

;;; keybinds-native-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'keybinds-native-rcp)
