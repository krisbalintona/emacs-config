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

(general-define-key
 "C-x C-c" nil                          ; Unbinds `save-buffers-kill-emacs'.
 (general-chord "xx") 'save-buffer)

;;; keybinds-native-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'keybinds-native-rcp)
