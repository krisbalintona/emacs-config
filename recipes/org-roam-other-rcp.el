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
  (defun kb/lister--check-if-has-sublist (ewoc node)
    "Return `t' if NODE has a sublist (of proper indentation, that is, with a
level one greater than NODE) right below it. Otherwise, return `nil'."
    (let ((next (ewoc-next ewoc node)))
      (when next
        (= (1+ (lister-node-get-level node))
           (lister-node-get-level next)
           ))
      ))
  (cl-defun kb/lister--prev-visible-node-same-indentation-no-sublist (ewoc node)
    "In EWOC, move from NODE to previous visible node of the same indentation
    level as EWOC.

Return that node or nil if there is none."
    (let ((indentation-current (lister-node-get-level node)))
      (condition-case nil        ; Catch error if no movement is possible anyway
          (lister--next-node-matching ewoc node
                                      #'(lambda (new-node)
                                          (and (lister-node-visible-p new-node) ; Visible
                                               (= indentation-current ; Same indentation
                                                  (lister-node-get-level new-node))
                                               (not (kb/lister--check-if-has-sublist ewoc new-node)) ; Skip nodes with sublists

                                               ))
                                      #'ewoc-prev ; Check upwards
                                      (lister-parent-node ewoc pos)) ; Don't check beyond the parent, if existent
        (error nil))))
  (cl-defun kb/lister--next-visible-node-same-indentation-no-sublist (ewoc node &optional
                                                                           (move-fn #'ewoc-next))
    "In EWOC, move from NODE to next visible node that is of the same
    indentation level as EWOC via MOVE-FN.

Return that node or nil if there is none. To move backwards, set MOVE-FN to
`ewoc-prev'."
    (let ((indentation-current (lister-node-get-level node)))
      (condition-case nil        ; Catch error if no movement is possible anyway
          (lister--next-node-matching ewoc node
                                      #'(lambda (new-node)
                                          (and (lister-node-visible-p new-node) ; Visible
                                               (= indentation-current ; Same indentation
                                                  (lister-node-get-level new-node))
                                               (not (kb/lister--check-if-has-sublist ewoc new-node)) ; Skip nodes with sublists
                                               ))
                                      move-fn ; Check downwards
                                      (lister-parent-node ewoc pos)) ; Don't check beyond the parent, if existent
        (error nil))))
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
  (defun kb/lister-move-sublist-up (ewoc old-pos new-pos &optional after-target)
    "In EWOC, move sublist at OLD-POS up to right after the node at
NEW-POS."
    (when (lister-filter-active-p ewoc)
      (error "Cannot move sublists when filter is active"))
    (lister-with-sublist-at ewoc old-pos beg end
      (let ((target (ewoc-prev ewoc new-pos)))
        (if (or (not target)
                (eq target (ewoc-nth ewoc 0)))
            (error "Cannot move sublist further up")
          (lister--move-list ewoc beg end target after-target))
        )))
  (defun kb/lister-move-sublist-down (ewoc old-pos new-pos &optional after-target)
    "In EWOC, move sublist at OLD-POS down to right after the node at
NEW-POS."
    (when (lister-filter-active-p ewoc)
      (error "Cannot move sublists when filter is active"))

    ;; Handle special case when old-pos is the last node in EWOC by forcing it
    ;; to go after the target.
    (unless (equal old-pos (ewoc--last-node ewoc))
      (setq after-target t))

    (lister-with-sublist-at ewoc old-pos beg end
      ;; (let ((target (ewoc-next ewoc new-pos)))
      (let ((target new-pos))
        (if (not target)
            (error "Cannot move sublist further down")
          (lister--move-list ewoc beg end target after-target))
        )))

  ;; Movement vertically
  (defun kb/lister-move-item-up (ewoc pos &optional ignore-level)
    "Move item and its sublist one up.

Move the item at POS in EWOC up to the next visible item, swapping both. Only
move within the same indentation level unless IGNORE-LEVEL is non-nil."
    (let* ((move-fn 'kb/lister--prev-visible-node-same-indentation-no-sublist)
           (up nil)
           (keep-level (not ignore-level))
           ;; The above are the last three args that would be provided to
           ;; `lister--move-item'
           (from   (lister--parse-position ewoc pos))
           (next   (funcall move-fn ewoc from))
           (to     (if keep-level
                       (lister--next-node-same-level ewoc from move-fn)
                     next)))
      ;; Move current node up
      (unless to
        (error "No movement possible"))
      (if (eq next to)
          (lister--swap-item ewoc from to)
        (lister--move-list ewoc from from to up))
      ;; Move node's sublist, if any, up with it
      (if (kb/lister--check-if-has-sublist ewoc from)
          (kb/lister-move-sublist-up ewoc
                                     (ewoc-next ewoc from) ; From first node of sublist
                                     ;; Just put it the list right after the current
                                     ;; node, which has moved to the correct spot
                                     from t))
      ))
  (defun kb/lister-move-item-down (ewoc pos &optional ignore-level)
    "Move item and its sublist one down.

Move the item at POS in EWOC down to the next visible item, swapping both. Only
move within the same indentation level unless IGNORE-LEVEL is non-nil."
    ;; NOTE 2022-05-26: I initially changed `lister-move-item-down', but had to
    ;; resort to using most of the code from `lister--move-item' to move the
    ;; contained sublist with it (namely, I needed access to `to').
    (let* ((move-fn 'kb/lister--next-visible-node-same-indentation-no-sublist)
           (up nil)
           (keep-level (not ignore-level))
           ;; The above are the last three args that would be provided to
           ;; `lister--move-item'
           (from   (lister--parse-position ewoc pos))
           (next   (funcall move-fn ewoc from))
           (to     (if keep-level
                       (lister--next-node-same-level ewoc from move-fn)
                     next)))
      ;; Move current node down
      (unless to
        (error "No movement possible"))
      (if (eq next to)
          (lister--swap-item ewoc from to)
        (lister--move-list ewoc from from to up))
      ;; Move node's sublist, if any, down with it
      (if (kb/lister--check-if-has-sublist ewoc from)
          (kb/lister-move-sublist-down ewoc (ewoc-next ewoc from) to))
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

  ;; New keybinds
  (lister-defkey kb/lister-mode-up (ewoc pos prefix node)
    "Move the item at point one up, preserving `org-mode'-like tree
structure."
    (kb/lister-move-item-up ewoc pos prefix))
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
