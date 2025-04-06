;;; Shr
;; Emacs' built-in web renderer
(use-package shr
  :ensure nil
  :custom
  (shr-fill-text nil)                ; Prefer to use `visual-line-mode' instead
  (shr-use-fonts t)
  ;; (shr-use-colors nil)                  ; t is bad for accessibility
  (shr-width nil)
  (shr-discard-aria-hidden t)
  (shr-max-image-proportion 0.6)        ; How big are images?
  (shr-image-animate t)
  ;; (shr-cookie-policy nil)
  )

;;; Eww
;; Emacs' web browser
(use-package eww
  :ensure nil
  :hook (eww-after-render . visual-line-mode)
  :custom
  (eww-restore-desktop t)
  (eww-desktop-remove-duplicates t)     ; Don't duplicate pages in history
  (eww-search-prefix "https://duckduckgo.com/html/?q=") ; Use duckduckgo search engine
  (eww-download-directory (no-littering-expand-var-file-name "eww/downloads/")) ; Where to put downloads
  (eww-history-limit 150)
  (eww-readable-adds-to-history nil)
  (eww-browse-url-new-window-is-tab nil)
  (eww-form-checkbox-selected-symbol "[X]")
  (eww-form-checkbox-symbol "[ ]")
  (eww-auto-rename-buffer 'title))

;;; Engine-mode
;; Send arbitrary search engine queries to your browser from within Emacs
(use-package engine-mode
  :custom
  (engine/browser-function 'browse-url-default-browser)
  :config
  (engine-mode 1)

  ;; My searches
  (defengine amazon
    "https://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")

  (defengine google
    "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "G")

  (defengine wikipedia
    "https://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w")

  (defengine youtube
    "https://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y"))

;;; Wombag
(use-package wombag
  :disabled t                           ; 2025-04-06: Trying out wallabag.el
  :vc (:url "https://github.com/karthink/wombag.git"
            :rev :newest)
  :hook ((wombag-show-mode . org-remark-mode)
         (wombag-show-mode . krisb-wombag-entry-setup))
  :bind ( :map krisb-open-keymap
          ("w" . wombag))
  :custom
  (wombag-dir (no-littering-expand-var-file-name "wombag"))
  (wombag-db-file (no-littering-expand-var-file-name "wombag/wombag.sqlite"))
  (wombag-host "https://app.wallabag.it")
  (wombag-username "krisbalintona")
  (wombag-password (auth-source-pick-first-password :host "app.wallabag.it"))
  (wombag-client-id "23882_1jzdzdd09ikgw4k8o0cog4wggk48cgc0gwk8oos0gsc44gcsco")
  (wombag-client-secret (auth-source-pick-first-password :host "emacs-wombag.el"))
  (wombag-search-filter "")
  :config
  (defun krisb-wombag-entry-setup ()
    "Set up the visual for wombag-entry buffers."
    (setq-local line-spacing 0.08)
    (face-remap-add-relative 'default :height 1.1)
    (when (require 'olivetti nil t)
      (olivetti-mode 1)
      (olivetti-set-width 120))
    (when (require 'mixed-pitch nil t)
      (mixed-pitch-mode 1))
    (visual-line-mode 1)))

;;; Wallabag
(use-package wallabag
  :hook (wallabag-entry-mode . olivetti-mode)
  :bind ( :map krisb-open-keymap
          ("w" . wallabag)
          :map wallabag-search-mode-map
          ("n" . next-line)
          ("p" . previous-line)
          ("M-n" . wallabag-search-next-page)
          ("M-p" . wallabag-search-previous-page)
          :map wallabag-sidebar-mode-map
          ("RET" . wallabag-sidebar-find-tag)
          :map wallabag-entry-mode-map
          ("SPC" . scroll-up-command)
          ("DEL" . scroll-down-command))
  :custom
  (wallabag-host "https://app.wallabag.it")
  (wallabag-username "krisbalintona")
  (wallabag-password (auth-source-pick-first-password :host "app.wallabag.it"))
  (wallabag-clientid "23882_1jzdzdd09ikgw4k8o0cog4wggk48cgc0gwk8oos0gsc44gcsco")
  (wallabag-secret (auth-source-pick-first-password :host "emacs-wombag.el"))
  (wallabag-db-file (no-littering-expand-var-file-name "wallabag/wallabag.sqlite"))
  (wallabag-search-page-max-rows 50))

;;; Krisb-wombag-ext
(use-package krisb-wombag-ext
  :ensure nil
  :demand t
  :after wombag
  :config
  (krisb-org-remark-wombag-mode 1))

;;; Provide
(provide 'krisb-web)
