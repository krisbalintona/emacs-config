;;; programming-lua-rcp.el --- Summary
;;
;; These are packages that are helpful for programming in elisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Lua-mode
;; Major-mode for the Lua language.
;; NOTE:For lsp-mode support, install lua-language-server, a separate system package
(use-package lua-mode
  :custom
  ;; Lsp-mode settings
  (lsp-clients-lua-language-server-install-dir "/home/krisbalintona/Cloned_Repos/lua-language-server/") ; Path to where I manually cloned lua-language-server
  (lsp-clients-lua-language-server-bin (expand-file-name "bin/Linux/lua-language-server" lsp-clients-lua-language-server-install-dir))
  (lsp-clients-lua-language-server-main-location (expand-file-name "main.lua" lsp-clients-lua-language-server-install-dir))

  (lua-indent-level 2) ; lua-indent-level defaults to 3 otherwise. Madness.
  (lua-indent-string-contents t) ; Contents of a multiline string will be indented
  :config
  (general-define-key ; Lua-mode overwrites my eyebrowse-last-window-config binding
   :keymaps 'lua-mode-map
   :states '(motion normal visual)
   "gz" 'eyebrowse-last-window-config
   )
  )

;;;; Company-lua
;; Company backend for Lua
(use-package company-lua
  :after company
  :config
  (add-hook 'lua-mode-hook (lambda ()
                             (setq-local company-backends '((company-lua
                                                             company-etags
                                                             company-dabbrev-code
                                                             company-yasnippet)))
                             ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-lua-rcp.el)
;;; Commentary:
;;
;;; programming-lua-rcp.el ends here
