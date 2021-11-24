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
  :demand t
  :straight (eaf :type git :host github :repo "emacs-eaf/emacs-application-framework")
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
  :custom
  ;; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :config
  ;; Require all modules
  (require 'eaf-airshare)
  (require 'eaf-browser)
  (require 'eaf-camera)
  (require 'eaf-demo)
  (require 'eaf-file-browser)
  (require 'eaf-file-manager)
  (require 'eaf-file-sender)
  (require 'eaf-image-viewer)
  (require 'eaf-jupyter)
  (require 'eaf-markdown-previewer)
  ;; (require 'eaf-mermaid)
  (require 'eaf-mindmap)
  (require 'eaf-music-player)
  (require 'eaf-org-previewer)
  (require 'eaf-pdf-viewer)
  (require 'eaf-system-monitor)
  (require 'eaf-terminal)
  (require 'eaf-video-player)
  (require 'eaf-vue-demo)
  (require 'eaf-netease-cloud-music)
  (require 'eaf-rss-reader)

  ;; Bindings
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding) ;; unbind, see more in the Wiki
  )

;;; Popweb
;; Use EAF to have popups for LaTeX math and bing/youdao Chinese translations
(use-package popweb
  :demand t
  :requires eaf
  :straight nil
  :load-path "~/.emacs.d/popweb"
  :custom
  (popweb-popup-pos "point-bottom-right")
  :config
  ;; LaTeX preview functionality
  (add-to-list 'load-path "/home/krisbalintona/.emacs.d/popweb/extension/latex")
  (require 'popweb-latex)
  (popweb-latex-mode)
  ;; Chinese-English translation popup
  (add-to-list 'load-path "/home/krisbalintona/.emacs.d/popweb/extension/dict")
  (require 'popweb-dict-bing)           ; Translation using Bing
  (require 'popweb-dict-youdao)         ; Translation using Youdao
  )

;;; application-framework-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'application-framework-rcp)
