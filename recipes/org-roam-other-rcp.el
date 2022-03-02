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
    "n" '(org-noter :wk "Org-noter")
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

;;; Org-remark
(use-package org-remark
  :after org
  :general (:keymaps 'org-remark-mode-map
                     :prefix "C-c r"
                     "m" #'org-remark-mark
                     "r" #'org-remark-remove
                     "c" #'org-remark-change
                     "t" #'org-remark-toggle
                     "o" #'org-remark-open
                     "v" #'org-remark-view
                     "n" #'org-remark-view-next
                     "p" #'org-remark-view-prev)
  :config
  (org-remark-mode)
  (require 'org-remark-global-tracking)
  (org-remark-global-tracking-mode))

;;; Org-transclusion
;; Enable transclusion of org files
(use-package org-transclusion
  :after org-roam
  :hook (org-mode . org-transclusion-activate)
  :general
  (kb/toggle-keys
    "c" '(org-transclusion-mode :wk "Toggle mode")
    "R" '(org-transclusion-refresh :wk "Refresh")
    "m" '(org-transclusion-make-from-link :wk "Make")
    "a" '(org-transclusion-add :wk "Add")
    "r" '(org-transclusion-remove :wk "Remove")
    "s" '(org-transclusion-live-sync-start :wk "Edit start")
    "e" '(org-transclusion-live-sync-exit :wk "Edit exit")
    )
  :custom
  (org-transclusion-include-first-section t)
  (org-transclusion-exclude-elements '(property-drawer keyword))
  )

;;; Org-roam-ui
;; Newer `org-roam-server' for org-roam V2.
(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :custom
  (org-roam-ui-browser-function 'browse-url-default-browser) ; Open in my actual browser, to avoid opening in EAF
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

;;; Zettelkdask
(use-package zetteldesk
  :straight (zetteldesk :type git :host github :repo "Vidianos-Giannitsis/zetteldesk.el")
  :after org-roam
  :general
  (:keymaps '(normal visual insert motion)
            :prefix "SPC n z"
            :global-prefix "M-SPC n z"
            "b" 'zetteldesk-switch-to-buffer
            "a" '(:ignore t :which-key "Add to Zetteldesk")
            "a b" 'zetteldesk-add-to-desktop
            "a n" 'zetteldesk-add-node-to-desktop
            "a i" 'zetteldesk-add-info-node-to-desktop
            "r" '(:ignore t :which-key "Remove from Zetteldesk")
            "r b" 'zetteldesk-remove-from-desktop
            "r n" 'zetteldesk-remove-node-from-desktop
            "r i" 'zetteldesk-remove-info-node-from-desktop
            "n" 'zetteldesk-node-find
            "s" 'zetteldesk-switch-to-scratch-buffer
            "i" '(:ignore t :which-key "Insert to Scratch Buffer")
            "i n" 'zetteldesk-insert-node-contents
            "i N" 'zetteldesk-insert-node-contents-without-link
            "i o" 'zetteldesk-insert-org-file-contents
            "i p" 'zetteldesk-insert-link-to-pdf
            "i i" 'zetteldesk-insert-info-contents
            )
  (:keymaps 'org-mode-map
            :states '(normal visual insert motion)
            :prefix ", z"
            "i" 'zetteldesk-node-insert
            "r" 'zetteldesk-remove-backlinks-from-desktop
            "b" 'zetteldesk-add-backlinks-to-desktop
            "p" 'zetteldesk-node-insert-if-poi-or-moc
            "s" 'zetteldesk-node-insert-sort-backlinks
            )
  :config
  (zetteldesk-mode))

;;; org-roam-other-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-roam-other-rcp)
