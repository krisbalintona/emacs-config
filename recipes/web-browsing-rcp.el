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

;;;; Shr
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

;;;; Browse-url
(use-package browse-url
  :custom
  (browse-url-browser-function 'eww-browse-url) ; Use `eww' for browser links
  (browse-url-secondary-browser-function 'browse-url-default-browser) ; Use default browser as secondary
  )

;;;; Eww
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
  :init
  (defun prot-eww--rename-buffer ()
    "Rename EWW buffer using page title or URL.
To be used by `eww-after-render-hook'."
    (let ((name (if (eq "" (plist-get eww-data :title))
                    (plist-get eww-data :url)
                  (plist-get eww-data :title))))
      (rename-buffer (format "*%s # eww*" name) t)))
  (add-hook 'eww-after-render-hook #'prot-eww--rename-buffer)
  (advice-add 'eww-back-url :after #'prot-eww--rename-buffer)
  (advice-add 'eww-forward-url :after #'prot-eww--rename-buffer)
  )

;;; web-browsing-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'web-browsing-rcp)
