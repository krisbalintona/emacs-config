;;; application-framework-rcp.el --- Summary
;;
;;; Commentary:
;;
;; All packages which are reliant on or heavily utilize the EAF.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; EAF
;; The Emacs application framework.
(use-package eaf
  :disabled
  ;; HACK 2023-07-16: Easier to install this package as a submodule since
  ;; managing external scripts with elpaca is currently a pain...
  :load-path "./site-lisp/emacs-application-framework/"
  :ensure nil
  :ensure-system-package (("/usr/share/licenses/python-opencv/" . python-opencv)
                          (gdb))     ; For debugging purposes, if I ever need to
  :custom
  ;; Install desired modules
  ;; NOTE 2023-07-14: These names are the module names minus the "eaf-" prefix.
  ;; "eaf-airshare" is "airshare," for example
  (eaf-apps-to-install
   '(video-player pdf-viewer markdown-previewer image-viewer browser))
  (eaf-app-extensions-alist
   '(("video-player" . eaf-video-extension-list)
     ;; ("office" . eaf-office-extension-list)
     ("pdf-viewer" . eaf-pdf-extension-list)
     ;; ("music-player" . eaf-music-extension-list)
     ;; ("mindmap" . eaf-mindmap-extension-list)
     ("markdown-previewer" . eaf-markdown-extension-list)
     ("image-viewer" . eaf-image-extension-list)
     ;; ("browser" . eaf-browser-extension-list)
     ))
  (eaf-config-location (concat no-littering-var-directory "eaf"))
  (eaf-buffer-background-color (face-attribute 'default :background)) ; Set background color to theme's background's
  (eaf-preview-display-function-alist
   '(;; ("org-previewer" . eaf--org-preview-display)
     ("markdown-previewer" . eaf--markdown-preview-display)))

  ;; Modules config

  ;; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  ;; Browser
  (eaf-browser-continue-where-left-off t) ; Also note `eaf-browser-restore-buffers'
  (eaf-browser-enable-adblocker t)
  (eaf-browser-default-search-engine "duckduckgo")
  (eaf-browser-blank-page-url "https://duckduckgo.com")
  (eaf-browser-download-path "/tmp")
  (eaf-browser-default-zoom 1.25)

  ;; PDF
  (eaf-pdf-notify-file-changed nil)

  ;; Dark mode?
  (eaf-browser-dark-mode "follow")
  (eaf-terminal-dark-mode "follow")
  (eaf-mindmap-dark-mode "follow")
  (eaf-pdf-dark-mode "ignore")

  (eaf-enable-debug nil)                ; Enable if there is a seg-fault
  :init
  ;; HACK 2023-07-14: Don't demand since EAF can't be loaded while Emacs is
  ;; non-GUI. See, e.g.,
  ;; https://github.com/emacs-eaf/emacs-application-framework/issues/1042
  (add-hook (if (daemonp) 'server-after-make-frame-hook 'elpaca-after-init-hook)
            #'(lambda () (require 'eaf)))
  :config
  ;; All-the-icons integration
  (require 'eaf-all-the-icons)

  ;; All modules
  ;; (require 'eaf-airshare)
  (require 'eaf-browser)
  ;; (require 'eaf-camera)
  ;; (require 'eaf-demo)
  ;; (require 'eaf-file-browser)
  ;; (require 'eaf-file-manager)
  ;; (require 'eaf-file-sender)
  (require 'eaf-image-viewer)
  ;; (require 'eaf-jupyter)
  (require 'eaf-markdown-previewer)
  ;; (require 'eaf-mermaid) ; NOTE 2022-05-25: Dependency error with eslint currently
  ;; (require 'eaf-mindmap)
  ;; (require 'eaf-music-player)
  ;; (require 'eaf-org-previewer)
  (require 'eaf-pdf-viewer)
  ;; (require 'eaf-system-monitor)
  ;; (require 'eaf-terminal)
  (require 'eaf-video-player)
  ;; (require 'eaf-vue-demo)
  ;; (require 'eaf-netease-cloud-music)
  ;; (require 'eaf-rss-reader)
  ;; (require 'eaf-git)

  ;; Bindings
  (with-eval-after-load 'eaf-browser
    (eaf-bind-key clear_focus "<escape>" eaf-browser-keybinding)
    (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki
  (with-eval-after-load 'eaf-pdf-viewer
    (eaf-bind-key rotate_counterclockwise "C-<up>" eaf-pdf-viewer-keybinding)
    (eaf-bind-key rotate_clockwise "C-<down>" eaf-pdf-viewer-keybinding)
    (eaf-bind-key winner-undo "C-<left>" eaf-pdf-viewer-keybinding)
    (eaf-bind-key winner-redo "C-<right>" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_down_page "C-u" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_up_page "C-d" eaf-pdf-viewer-keybinding)
    (eaf-bind-key nil "SPC" eaf-pdf-viewer-keybinding)
    (eaf-bind-key nil "g" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_to_begin "gg" eaf-pdf-viewer-keybinding)
    (eaf-bind-key eyebrowse-last-window-config "gv" eaf-pdf-viewer-keybinding)
    (eaf-bind-key eyebrowse-prev-window-config "ga" eaf-pdf-viewer-keybinding)
    (eaf-bind-key eyebrowse-next-window-config "gt" eaf-pdf-viewer-keybinding))

  (with-eval-after-load 'dash-docs
    (setq dash-docs-browser-func 'eaf-open-browser)))

;;; application-framework-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'application-framework-rcp)
