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
  :mode ("INSTALL\\'" "CONTRIBUTORS\\'" "LICENSE\\'" "README\\'"))

;;; Markdown-xwidget
;; Similar to `grip-mode' but avoids sending many requests to GitHub's API and
;; more customization. However, `grip-mode' shows exactly what GitHub would show
(use-package markdown-xwidget
  :after markdown-mode
  :ensure-system-package pandoc
  :straight (:type git
             :host github
             :repo "cfclrk/markdown-xwidget"
             :files (:defaults "resources"))
  :general (:keymaps 'markdown-mode-command-map
            "x" 'markdown-xwidget-preview-mode)
  :custom
  (markdown-xwidget-command "pandoc")
  (markdown-xwidget-github-theme "dark")
  (markdown-xwidget-mermaid-theme "dark")
  (markdown-xwidget-code-block-theme "github-dark"))

;;; markdown-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'markdown-general-rcp)
