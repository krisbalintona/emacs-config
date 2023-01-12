;;; programming-debugging-rcp.el --- Summary
;;
;;; Commentary:
;;
;; All configuration related to debugging.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Realgud
(use-package realgud
  :hook (realgud-srcbuf-mode . tool-bar-mode)
  :custom
  (realgud-window-split-orientation 'horizontal)
  (realgud-short-key-on-tracing? t))

;;; Dap-mode
(use-package dap-mode
  :after lsp-mode
  :commands dap-debug
  :general
  (:keymaps 'lsp-mode-map
   :prefix "<f4>"
   "d" '(dap-debug :wk "Debug")
   "l" '(dap-debug-last :wk "Debug last")
   "h" '(dap-hydra :wk "Hydra")
   "q" '(dap-disconnect :wk "Quit")
   "r" '(dap-ui-repl :wk "REPL")

   "b" '(:ignore t :wk "Breakpoints")
   "bt" '(dap-breakpoint-toggle :wk "Toggle breakpoint")
   "ba" '(dap-breakpoint-toggle :wk "Add breakpoint")
   "bd" '(dap-breakpoint-delete :wk "Delete breakpoint")
   "bD" '(dap-breakpoint-delete-all :wk "Delete all breakpoints")
   "bl" '(dap-breakpoint-log-message :wk "Breakpoint log")
   "bc" '(dap-breakpoint-condition :wk "Breakpoint condition")

   "e" '(:ignore t :wk "Expressions")
   "ea" '(dap-ui-expressions-add :wk "Expression add")
   "er" '(dap-ui-expressions-remove :wk "Expression add")
   )
  (:keymaps 'lsp-mode-map
   "H-c" 'dap-debug-last
   "H-C" 'dap-debug-recent
   )
  :custom
  (dap-debug-compilation-keep t)        ; Keep output window in success?
  (dap-debug-restart-keep-session nil)  ; Delete previous sessions

  (dap-auto-configure-features '(;; sessions
                                 locals
                                 breakpoints
                                 expressions
                                 controls
                                 tooltip
                                 ))

  ;; Dap-ui window configurations
  (dap-ui-buffer-configurations
   `((,dap-ui--sessions-buffer
      (side . right)
      (slot . 2)
      (window-width . 0.12))
     (,dap-ui--breakpoints-buffer
      (side . right)
      (slot . 3)
      (window-width . 0.12))
     (,dap-ui--locals-buffer
      (side . left)
      (slot . 1)
      (window-width . 0.15))
     (,dap-ui--expressions-buffer
      (side . left)
      (slot . 2)
      (window-width . 0.15))
     (,dap-ui--repl-buffer
      (side . bottom)
      (slot . 2)
      (window-height . 0.12)
      (window-parameters . ((mode-line-format . none))))
     )))

;;; programming-debugging-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-debugging-rcp)
