;;; web-browsing-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Configurations related to rendering web pages and browsing the web.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Shr
;; Simple HTML Renderer (Shr)
(use-package shr
  :custom
  ;; (shr-use-colors nil)                  ; t is bad for accessibility
  (shr-use-fonts nil)                   ; Proportional fonts?
  (shr-max-image-proportion 0.6)        ; How big are images?
  (shr-image-animate nil)               ; GIFs?
  (shr-width nil)
  ;; (shr-discard-aria-hidden t)
  ;; (shr-cookie-policy nil)               ; When to use cookies
  )

;;; Browse-url
(use-package browse-url
  :custom
  (browse-url-generic-program (executable-find (getenv "BROWSER")))
  (browse-url-browser-function 'browse-url-generic)       ; Primary browser
  (browse-url-secondary-browser-function 'eww-browse-url) ; Secondary browser
  (browse-url-handlers nil)

  (browse-url-new-window-flag t)        ; Always open new browser window
  )

;;; Eww
;; Emacs' web browser
(use-package eww
  :gfhook 'hl-line-mode
  :custom
  (eww-restore-desktop t)               ; Restore eww pages from `desktop-restore'
  (eww-desktop-remove-duplicates t)     ; Don't duplicate pages in history
  ;; (setq eww-header-line-format nil)
  (eww-search-prefix "https://duckduckgo.com/html/?q=") ; Use duckduckgo search engine
  (eww-download-directory (no-littering-expand-var-file-name "eww/downloads/")) ; Where to put downloads
  (eww-history-limit 150)
  (eww-browse-url-new-window-is-tab nil)
  (eww-form-checkbox-selected-symbol "[X]")
  (eww-form-checkbox-symbol "[ ]")
  (eww-auto-rename-buffer 'title))

;;; web-browsing-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'web-browsing-rcp)
