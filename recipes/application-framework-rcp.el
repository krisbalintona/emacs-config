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
  :after browse-url
  :straight (eaf :type git :host github :repo "emacs-eaf/emacs-application-framework")
  :load-path (lambda () (list (concat user-emacs-directory "site-lisp/emacs-application-framework"))) ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
  :hook (eaf-pdf-viewer . hide-mode-line-mode)
  :custom
  (eaf-config-location (concat no-littering-var-directory "eaf"))
  (eaf-buffer-background-color "#282C34") ; Set background color to uninspiring-dark background's

  ;; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  ;; Browser
  (eaf-browser-continue-where-left-off t) ; Also note `eaf-browser-restore-buffers'
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser) ; Set EAF to open WWW links
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
  :init
  ;; Have `find-file' use `eaf-open'
  (defun adviser-find-file (orig-fn file &rest args)
    (let ((fn (if (commandp 'eaf-open) 'eaf-open orig-fn)))
      (pcase (file-name-extension file)
        ("pdf"  (apply fn file nil))
        ("epub" (apply fn file nil))
        (_      (apply orig-fn file args)))))
  (advice-add #'find-file :around #'adviser-find-file)
  :config
  ;; All-the-icons integration
  (require 'eaf-all-the-icons)

  ;; All modules
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
  ;; Compatibility with Evil's normal mode
  (require 'eaf-evil)
  ;; Browser
  (eaf-bind-key clear_focus "<escape>" eaf-browser-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding) ;; unbind, see more in the Wiki
  ;; PDF
  (eaf-bind-key rotate_counterclockwise "C-<up>" eaf-pdf-viewer-keybinding) ; Close buffer and window
  (eaf-bind-key rotate_clockwise "C-<down>" eaf-pdf-viewer-keybinding) ; Close buffer and window
  (eaf-bind-key winner-undo "C-<left>" eaf-pdf-viewer-keybinding) ; Close buffer and window
  (eaf-bind-key winner-redo "C-<right>" eaf-pdf-viewer-keybinding) ; Close buffer and window
  (eaf-bind-key evil-delete-buffer "x" eaf-pdf-viewer-keybinding) ; Close buffer and window
  (eaf-bind-key scroll_down_page "C-u" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_up_page "C-d" eaf-pdf-viewer-keybinding)
  (eaf-bind-key nil "SPC" eaf-pdf-viewer-keybinding)
  (eaf-bind-key evil-window-vsplit "SPC wv" eaf-pdf-viewer-keybinding)
  (eaf-bind-key evil-window-split "SPC ws" eaf-pdf-viewer-keybinding)
  (eaf-bind-key evil-window-next "SPC ww" eaf-pdf-viewer-keybinding)
  (eaf-bind-key evil-window-left "SPC wh" eaf-pdf-viewer-keybinding)
  (eaf-bind-key evil-window-right "SPC wl" eaf-pdf-viewer-keybinding)
  (eaf-bind-key evil-window-up "SPC wk" eaf-pdf-viewer-keybinding)
  (eaf-bind-key evil-window-down "SPC wj" eaf-pdf-viewer-keybinding)
  (eaf-bind-key evil-window-next "C-w C-w" eaf-pdf-viewer-keybinding)
  (eaf-bind-key evil-window-left "C-w h" eaf-pdf-viewer-keybinding)
  (eaf-bind-key evil-window-right "C-w l" eaf-pdf-viewer-keybinding)
  (eaf-bind-key evil-window-up "C-w k" eaf-pdf-viewer-keybinding)
  (eaf-bind-key evil-window-down "C-w j" eaf-pdf-viewer-keybinding)
  (eaf-bind-key nil "g" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_to_begin "gg" eaf-pdf-viewer-keybinding)
  (eaf-bind-key eyebrowse-last-window-config "gv" eaf-pdf-viewer-keybinding)
  (eaf-bind-key eyebrowse-prev-window-config "ga" eaf-pdf-viewer-keybinding)
  (eaf-bind-key eyebrowse-next-window-config "gt" eaf-pdf-viewer-keybinding)
  )

;;; Popweb
;; Use EAF to have popups for LaTeX math and bing/youdao Chinese translations
(use-package popweb
  :demand t
  :after eaf
  :load-path (lambda () (list (concat user-emacs-directory "popweb")))
  :straight nil
  :hook (latex-mode . popweb-latex-mode)
  :custom
  (popweb-popup-pos "top-left")
  :config
  ;; LaTeX preview functionality
  (add-to-list 'load-path (concat user-emacs-directory "popweb/extension/latex"))
  (require 'popweb-latex)
  (popweb-latex-mode)
  ;; Chinese-English translation popup
  (add-to-list 'load-path (concat user-emacs-directory "popweb/extension/dict"))
  (require 'popweb-dict-bing)           ; Translation using Bing
  (require 'popweb-dict-youdao)         ; Translation using Youdao
  )

;;; application-framework-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'application-framework-rcp)
