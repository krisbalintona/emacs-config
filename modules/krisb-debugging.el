;; -*- lexical-binding: t; -*-

;;; Gud
(use-package gud
  :ensure nil
  :custom
  (gud-highlight-current-line t))

;;; Realgud
(use-package realgud
  :hook (realgud-srcbuf-mode . tool-bar-mode)
  :custom
  (realgud-window-split-orientation 'horizontal)
  (realgud-short-key-on-tracing? t))

;;; Gdb-mi
;; A graphical interface to gdb.
(use-package gdb-mi
  :ensure nil
  :custom
  (gdb-many-windows t)
  (gdb-show-main t)
  (gud-gdb-command-name "gdb -i=mi --quiet")
  (gdb-restore-window-configuration-after-quit 'if-gdb-many-windows)
  :config
  (defun krisb-gdb-non-stop-handler ()
    "Version of the original that avoids the GDB startup error regarding \"target-async\"."
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
  (advice-add 'gdb-non-stop-handler :override #'krisb-gdb-non-stop-handler))

;;; Provide
(provide 'krisb-debugging)
