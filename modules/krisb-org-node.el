;;; Org-node
(use-package org-node
  :bind ( :map krisb-note-keymap
          ("l" . org-node-context-toggle))
  :custom
  (org-node-datestamp-format "%Y%m%d%H%M%S-")
  (org-node-context-persist-on-disk t)
  :config
  (org-node-cache-mode 1)
  (org-node-context-follow-mode 1))

;;; Org-node-fakeroam
(use-package org-node-fakeroam
  :after org-roam
  :bind ( :map krisb-note-keymap
          ([remap org-roam-buffer-toggle] . org-node-context-toggle))
  :custom
  (org-roam-db-update-on-save nil)      ; Don't update DB on save, not needed
  (org-roam-link-auto-replace nil)      ; Don't look for "roam:" links on save
  :config
  (org-roam-db-autosync-mode -1)
  (org-node-fakeroam-db-feed-mode 1))   ; Keep Roam DB up to date

;;; Provide
(provide 'krisb-org-node)
