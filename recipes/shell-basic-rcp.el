;;; shell-basic-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Configuration and packages related to the basic `shell-mode'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Shell
;; Built-in shell
(use-package shell
  :straight nil
  :custom
  (async-shell-command-buffer 'new-buffer)) ; Don't ask, just do

;;; shell-basic-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'shell-basic-rcp)
