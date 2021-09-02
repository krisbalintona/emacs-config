;;; markdown-general-rcp.el --- Summary
;;
;;; Commentary:
;;
;; My configuration related to markdown-mode.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)

;;;; Markdown-mode
(use-package markdown-mode
  :straight (markdown-mode :type git :host github :repo "jrblevin/markdown-mode")
  :mode ("INSTALL\\'" "CONTRIBUTORS\\'" "LICENSE\\'" "README\\'")
  )

;;; markdown-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'markdown-general-rcp)
