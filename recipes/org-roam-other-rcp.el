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
    "n" '(org-noter :wk "Org-noter"))
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
  :gfhook #'delve-compact-view-mode
  ;; FIXME 2022-05-27: `delve--zettel-preview' seems necessary to prevent cmacro
  ;; compiler error for `kb/delve--key--toggle-preview'.
  :commands delve delve--zettel-preview
  :general
  ("<f12>" 'delve)
  (:keymaps 'delve-mode-map
            "<backtab>" 'kb/delve--key-backtab
            )
  (:keymaps 'delve-mode-map
            :states 'visual
            "d" 'delve--key--multi-delete
            "DEL" 'delve--key--multi-delete
            "y" 'kb/delve-visual-copy-nodes
            )
  (:keymaps 'delve-mode-map
            :states 'normal
            "DEL" 'delve--key--multi-delete
            "q" 'bury-buffer
            "t" 'delve--key--insert-tagged
            "T" 'delve--key--insert-node-by-tags
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
            "RET" 'kb/delve--key--toggle-preview
            "C-o" 'delve--key--open-zettel
            "o" 'delve--key--open-zettel
            "go" 'delve--key--open-zettel
            "+" 'delve--key--add-tags
            "-" 'delve--key--remove-tags
            "M-d" 'kb/delve-mark-duplicates
            "Y" 'kb/delve-copy-zettel-title
            "yy" 'evil-yank-line
            )
  :custom
  (delve-storage-paths (concat kb/roam-dir "delve-storage/"))
  (delve-dashboard-tags '("working"))
  :init
  ;; Must be loaded before delve
  (setq delve-minor-mode-prefix-key (kbd "M-n"))
  :config
  (delve-global-minor-mode)

  ;; My own functions below
  (lister-defkey kb/delve--key--toggle-preview (ewoc pos prefix node)
    "Toggle the display of the preview of ZETTEL."
    (let ((zettel (delve--current-item-or-error 'delve--zettel ewoc pos)))
      ;; Unhide the sublist first
      (lister-with-sublist-below ewoc pos beg end
        (lister--outline-hide-show ewoc beg end nil))
      ;; Then show preview
      (let ((preview (and (not (delve--zettel-preview zettel))
                          (or (delve--get-preview-contents zettel)
                              "No preview available"))))
        (setf (delve--zettel-preview zettel) preview)
        (lister-refresh-at lister-local-ewoc :point))
      ))

  (defvar kb/delve-cycle-global-contents t
    "Are all the contents of the given delve buffer (EWOC)
        shown or hidden? nil if hidden, t if shown.")
  (defun kb/delve--key-backtab (ewoc &optional prefix)
    "In EWOC, toggle hiding or showing all sublists.

When called with PREFIX, hide all previews."
    (interactive (list lister-local-ewoc current-prefix-arg))
    (lister-walk-nodes ewoc
                       #'(lambda (ewoc new-node)
                           (let* ((current-item (delve--current-item nil ewoc new-node))
                                  (is-zettel (cl-typep current-item 'delve--zettel))
                                  (has-preview (when is-zettel (delve--zettel-preview current-item)))
                                  )
                             ;; Hide only when...
                             (when (and (not (lister-sublist-below-p ewoc new-node)) ; Non-parents
                                        (not has-preview) ; No preview
                                        (< 0 (lister-node-get-level new-node)) ; Not top-level
                                        )
                               (lister--outline-hide-show ewoc new-node new-node kb/delve-cycle-global-contents))
                             ;; Then hide if with prefix, hide all previews
                             (when (and prefix has-preview)
                               (setf (delve--zettel-preview current-item) nil)
                               (lister-refresh-at ewoc new-node)) ; Refresh to update visually
                             ))
                       :first :last)
    (if (lister--outline-invisible-p ewoc :point) ; End a non-invisible node
        (lister-goto ewoc (lister-parent-node ewoc :point)))
    (setq-local kb/delve-cycle-global-contents (not kb/delve-cycle-global-contents)))

  (lister-defkey kb/delve-mark-duplicates (ewoc pos prefix node)
    "Mark duplicate org-roam nodes in the current delve buffer."
    (let ((id-tracker (list)))
      (lister-save-current-node ewoc
          (lister-walk-nodes ewoc
                             #'(lambda (ewoc new-node)
                                 (let ((id (delve--zettel-id (delve--current-item nil ewoc new-node))))
                                   (if (member id id-tracker)
                                       ;; (lister-delete-at ewoc new-node)
                                       (lister-mode-mark ewoc new-node)
                                     (setq id-tracker (append id-tracker (list id))))))
                             :first :last
                             #'(lambda (new-node) (cl-typep (delve--current-item nil ewoc new-node) 'delve--zettel))))))
  (lister-defkey kb/delve-visual-copy-nodes (ewoc pos prefix node)
    "Copy current node(s) when region is active."
    (when (region-active-p)
      (copy-region-as-kill (mark) (point) 'region)))
  (lister-defkey kb/delve-copy-zettel-title (ewoc pos prefix node)
    "Copy current org-roam node's (delve--zettel) title."
    (let* ((item (delve--current-item-or-error 'delve--zettel ewoc node))
           (title (delve--zettel-title item))
           (file (delve--zettel-file item)))
      (kill-new title)
      (message (format "The node title \"%s\" from %s has been copied" title file)))))

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
            "M-K" '(lambda ()
                     (interactive) ; Ignore constraint of same indentation level
                     (funcall-interactively 'lister-mode-up lister-local-ewoc :point '(4)))
            "M-J" '(lambda ()
                     (interactive) ; Ignore constraint of same indentation level
                     (funcall-interactively 'lister-mode-down lister-local-ewoc :point '(4)))
            "M-H" 'kb/lister-mode-left-sublist
            "M-L" 'kb/lister-mode-right-sublist
            "gk" 'lister-mode-forward-same-level
            "gj" 'lister-mode-backward-same-level
            "zu" 'lister-mode-up-parent
            "gh" 'lister-mode-up-parent
            )
  (:keymaps 'lister-mode-map
            :states '(normal visual)
            "m" 'lister-mode-mark
            "u" 'lister-mode-unmark
            "U" 'lister-mode-unmark-all
            )
  :init
  (require 'lister-mode) ; Require since this proves the "core" definitions for the functions below
  :config
  ;; Helpers: movement vertically
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

  ;; Helpers: movement horizontally
  (defun kb/lister-move-item-right (ewoc pos node)
    "In EWOC, increase indentation level of the item at POS.

But don't indent if indenting breaks the structure of the tree."
    (let ((indentation-current (lister-get-level-at ewoc pos))
          (first-node (lister--parse-position ewoc :first)))
      ;; Don't indent if it's the first node
      (unless (equal node first-node)
        (lister-set-level-at ewoc pos (1+ indentation-current)))
      ))

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
    (kb/lister-move-item-right ewoc pos node))
  (lister-defkey kb/lister-mode-left-sublist (ewoc pos prefix node)
    "Move the node at point and its sublist, if any, to the left."
    (let ((indentation-current (lister-node-get-level node)))
      (if (and (< 0 indentation-current) (lister-sublist-below-p ewoc node))
          (progn
            (lister-set-node-level ewoc node (1- indentation-current))
            (lister-move-sublist-left ewoc (ewoc-next ewoc node)))
        (lister-move-item-left ewoc pos))))
  (lister-defkey kb/lister-mode-right-sublist (ewoc pos prefix node)
    "Move the node at point and its sublist, if any, to the right."
    (if (lister-sublist-below-p ewoc node)
        (progn
          (lister-move-sublist-right ewoc (ewoc-next ewoc node))
          ;; Move sublist before current node because the current node becomes
          ;; part of the sublist if indented first
          (lister-set-node-level ewoc node (1+ (lister-node-get-level node))))
      (kb/lister-move-item-right ewoc pos node))))

;;; org-roam-other-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-roam-other-rcp)
