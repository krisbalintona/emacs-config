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
  (org-transclusion-exclude-elements '(property-drawer keyword)))

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

;;; Delve
(use-package delve
  :straight (delve :type git :host github :repo "publicimageltd/delve")
  :after org-roam
  :gfhook #'delve-compact-view-mode
  :general
  ("<f12>" 'delve)
  (:keymaps 'delve-mode-map
            :states 'normal
            "dd" '(lambda ()
                    (interactive)
                    (kill-line)
                    (forward-line -1))
            "P" 'delve--key--yank       ; Paste above
            "p" '(lambda ()                  ; Paste below
                   (interactive)
                   (save-excursion
                     (next-line)
                     (delve--key--yank))
                   (next-line))
            "r" 'delve--key--sync
            "gr" 'delve--key--sync
            "h" 'delve--key--insert-heading
            "n" 'delve--node-transient-key
            "s" 'delve--key--sort
            "C-p" 'delve--key--collect-into-pile
            "I" 'delve--key--insert-query-or-pile
            "v" 'delve-compact-view-mode
            "f" 'delve--key--fromlinks
            "b" 'delve--key--backlinks
            "RET" 'delve--key--toggle-preview
            "C-o" 'delve--key--open-zettel
            "o" 'delve--key--open-zettel
            "+" 'delve--key--add-tags
            "-" 'delve--key--remove-tags
            )
  :custom
  (delve-storage-paths (concat kb/roam-dir "delve-storage/"))
  (delve-dashboard-tags '("working"))
  :init
  ;; Must be loaded before delve
  (setq delve-minor-mode-prefix-key (kbd "M-n"))
  :config
  (delve-global-minor-mode))

;;; Lister
;; Interactive list library for `delve'
(use-package lister
  :general
  (:keymaps 'lister-mode-map
            "M-k" 'kb/lister-mode-up
            "M-j" 'kb/lister-mode-down
            "M-h" 'lister-mode-left
            "M-l" 'kb/lister-mode-right
            ;; Use the initial versions of the functions for these
            "M-K" 'lister-mode-up
            "M-J" 'lister-mode-down
            "m" 'lister-mode-mark
            "u" 'lister-mode-unmark
            "U" 'lister-mode-unmark-all
            "gk" 'lister-mode-forward-same-level
            "gj" 'lister-mode-backward-same-level
            "zu" 'lister-mode-up-parent
            "gh" 'lister-mode-up-parent
            )
  :init
  (require 'lister-mode) ; Require since this proves the "core" definitions for the functions below
  :config
  ;; Helper functions
  (cl-defun kb/lister--exists-top-level-node (ewoc node)
    "Return the first matching top-level node in EWOC. If it does not exist,
return nil."
    (lister--next-node-matching ewoc node
                                #'(lambda (node)
                                    (and
                                     (lister-node-visible-p node)
                                     (= 0 (lister-node-get-level node)))
                                    )
                                #'ewoc-prev))

  ;; Movement vertically
  (defun kb/lister-move-item-up (ewoc pos)
    "Move item and its sublist one up, preserving `org-mode'-like indentation."
    (let* ((move-fn 'lister--prev-visible-node)
           (from-node (lister--parse-position ewoc pos)) ; Current node
           ;; Prefer `lister-sublist-below-p' over `lister-sublist-at-p' because
           ;; the latter, for some reason, does not consider the first and last
           ;; nodes to be a part of sublists...
           (to-node (if (lister-sublist-below-p ewoc from-node)
                        (cadr (lister--locate-sublist ewoc (ewoc-next ewoc from-node)))
                      from-node))
           (move-to-node (lister--next-node-same-level ewoc from-node move-fn)))
      (unless move-to-node
        (error "No movement possible"))
      ;; Move the range of nodes starting from the current node (from-node) and
      ;; ending with to-node to move-to-node's position. If from-node does not
      ;; have a sublist, then to-node will be from-node. If it does, then
      ;; to-node will be the last node in the sublist.
      (lister--move-list ewoc from-node to-node move-to-node nil)
      ))
  (defun kb/lister-move-item-down (ewoc pos &optional ignore-level)
    "Move item and its sublist one down, preserving `org-mode'-like indentation."
    (let* ((move-fn 'lister--next-visible-node)
           (current-node (lister--parse-position ewoc pos))
           (target-node (lister--next-node-same-level ewoc current-node move-fn)))
      (unless target-node
        (error "No movement possible"))
      ;; Move the next valid node (target-node), which takes its sublist if it
      ;; exists (see `kb/lister-move-item-up'), above the current-node.
      (kb/lister-move-item-up ewoc target-node)
      ))

  ;; Movement horizontally
  (defun kb/lister-move-item-right (ewoc pos)
    "In EWOC, increase indentation level of the item at POS.

But don't indent if indenting breaks the structure of the tree."
    ;; TODO 2022-05-26: Deal with edge case of being at the bottom of the buffer
    ;; already
    (let ((indentation-above (save-excursion
                               (let ((column (current-column)))
                                 (move-to-column column)
                                 (forward-line -1)
                                 (lister-get-level-at ewoc pos)
                                 )))
          (indentation-current (lister-get-level-at ewoc pos))
          (exists-top-level-node
           (kb/lister--exists-top-level-node ewoc (ewoc-locate ewoc))))
      ;; Only indent if the indentation level of the above node
      ;; (indentation-above) is at least one indentation level greater than
      ;; indentation-current. Also make sure there is at least one top-level
      ;; node somewhere above the current node.
      (when (and (>= indentation-above indentation-current) exists-top-level-node)
        (lister-set-level-at ewoc pos (1+ indentation-current)))
      ))
  ;; TODO 2022-05-27: Add keybinds for changing the indentation of an idem and
  ;; its entire sublist

  ;; New keybinds
  (lister-defkey kb/lister-mode-up (ewoc pos prefix node)
    "Move the item at point one up, preserving `org-mode'-like tree
structure."
    (kb/lister-move-item-up ewoc pos))
  (lister-defkey kb/lister-mode-down (ewoc pos prefix node)
    "Move the item at point one down, preserving `org-mode'-like tree
structure."
    (kb/lister-move-item-down ewoc pos prefix))
  (lister-defkey kb/lister-mode-right (ewoc pos prefix node)
    "Move the item at point to the right, preserving `org-mode'-like
tree structure."
    (kb/lister-move-item-right ewoc pos)))

;;; org-roam-other-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-roam-other-rcp)
