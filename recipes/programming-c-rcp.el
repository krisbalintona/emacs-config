;;; programming-c-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages related to developing in C.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'keybinds-general-rcp)

;;; C-mode
(use-package cc-mode
  :general (:keymaps 'c-mode-map
            "TAB" 'indent-for-tab-command))

;;; Gdb-mi
;; Built-in GDB
(use-package gdb-mi
  :custom
  (gdb-many-windows t)
  (gdb-show-main t)
  (gud-gdb-command-name "gdb -i=mi --quiet")
  (gdb-restore-window-configuration-after-quit 'if-gdb-many-windows)
  :config
  (defun kb/gdb-non-stop-handler ()
    "Version of the original that avoids the GDB startup error
regarding \"target-async\"."
    (goto-char (point-min))
    (if (re-search-forward "No symbol" nil t)
        (progn
          (message
           "This version of GDB doesn't support non-stop mode.  Turning it off.")
          (setq gdb-non-stop nil)
          (setq gdb-supports-non-stop nil))
      (setq gdb-supports-non-stop t)
      ;; (gdb-input "-gdb-set target-async 1" 'ignore)
      (gdb-input "-gdb-set mi-async 1" 'ignore) ; Change to this, as advised
      (gdb-input "-list-target-features" 'gdb-check-target-async)))
  (advice-add 'gdb-non-stop-handler :override #'kb/gdb-non-stop-handler))

;;; programming-c-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-c-rcp)
