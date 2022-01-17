;;; programming-lua-rcp.el --- Summary
;;
;;; Commentary:
;;
;; These are packages that are helpful for programming in elisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'general)
(require 'keybinds-general-rcp)

;;; Lua-mode
;; Major-mode for the Lua language.
;; Install directions for system package described here:
;; https://github.com/sumneko/lua-language-server/wiki/Build-and-Run-(Standalone)
;; NOTE: For lsp-mode support, install lua-language-server, a separate system package
(use-package lua-mode
  :ensure-system-package (lua-language-server)
  :hook (lua-mode . lsp-deferred)
  :general
  (:keymaps 'lua-mode-map
            :states '(normal visual motion)
            "K" 'join-line)
  :custom
  (lua-indent-level 4)      ; This is the convention
  (lua-indent-string-contents t) ; Contents of a multiline string will be indented
  )

;;; Company-lua
;; Company backend for Lua
(use-package company-lua
  :after company
  :hook (lua-mode . (lambda ()
                      (add-to-list 'company-backends 'company-lua)))
  )

;;; programming-lua-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-lua-rcp)
