;;; org-roam-other-rcp.el --- Summary
;;
;;; Commentary:
;;
;; This is all the configuration for packages related to org-roam
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'custom-directories-rcp)
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Note-taking
;;;; Pdf-tools
;; View pdfs and interact with them. Has many dependencies
;; https://github.com/politza/pdf-tools#compiling-on-fedora
(use-package pdf-tools
  :straight (pdf-tools :type git :host github :repo "vedang/pdf-tools") ; Repo of current maintainer
  ;; Ensure it's installed without manually doing so
  :hook (after-init . pdf-tools-install)
  :custom
  (pdf-view-display-size 'fit-width)
  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick t)
  )

;;;; Org-noter
(use-package org-noter
  :defer 10                      ; Load so it doesn't defer to noter insert call
  :general
  (:keymaps 'org-noter-doc-mode-map
            "M-o" 'org-noter-insert-note)
  (kb/open-keys
    "n" '(org-noter :which-key "Org-noter")
    )
  :custom
  (org-noter-notes-search-path kb/roam-dir)
  (org-noter-separate-notes-from-heading t) ; Add blank line betwwen note heading and content
  (org-noter-notes-window-location 'horizontal-split) ; Horizontal split between notes and pdf
  (org-noter-always-create-frame nil) ; Don't open frame
  (org-noter-hide-other nil) ; Show notes that aren't synced with (you're on)
  (org-noter-auto-save-last-location t) ; Go to last location
  (org-noter-kill-frame-at-session-end nil) ; Don't close frame when killing pdf buffer
  )

;;; Org-transclusion
;; Enable transclusion of org files
(use-package org-transclusion
  :disabled t ; Issue with org-roam-node-capture
  :straight (org-transclusion :type git :host github :repo "nobiot/org-transclusion")
  :after org-roam
  :ghook ('org-mode-hook 'org-transclusion-activate)
  :general
  (kb/toggle-keys
    :keymaps 'org-mode-map
    "c" '(org-transclusion-mode :which-key "Toggle mode")
    "R" '(org-transclusion-refresh :which-key "Refresh")
    "m" '(org-transclusion-make-from-link :which-key "Make")
    "a" '(org-transclusion-add :which-key "Add")
    "r" '(org-transclusion-remove :which-key "Remove")
    "s" '(org-transclusion-live-sync-start :which-key "Edit start")
    "e" '(org-transclusion-live-sync-exit :which-key "Edit exit")
    )
  :custom
  (org-transclusion-include-first-section t)
  (org-transclusion-exclude-elements '(property-drawer keyword))
  :config
  ;; Currently need to look through Roam directory, not just agenda files
  (org-id-update-id-locations (org-roam--list-all-files))
  )

;;; Org-roam-ui
;; Newer `org-roam-server' for org-roam V2.
(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :custom
  (org-roam-ui-browser-function 'browse-url-firefox) ; Open in my actual browser, to avoid opening in EAF
  (org-roam-ui-open-on-start nil) ; Don't open graph on startup
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-custom-theme '(list
                              (bg . "#1E2029")
                              (bg-alt . "#282a36")
                              (fg . "#f8f8f2")
                              (fg-alt . "#6272a4")
                              (red . "#ff5555")
                              (orange . "#f1fa8c")
                              (yellow ."#ffb86c")
                              (green . "#50fa7b")
                              (cyan . "#8be9fd")
                              (blue . "#ff79c6")
                              (violet . "#8be9fd")
                              (magenta . "#bd93f9"))
                            )
  :preface
  (use-package websocket :demand t)
  (use-package simple-httpd :demand t)
  (use-package f :demand t)
  )

;;; org-roam-other-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-roam-other-rcp)
