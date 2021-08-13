;;; rss-feed-rcp.el --- Summary
;;
;;; Commentary:
;;
;; This is all the configuration of the org-roam package
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Elfeed
(use-package elfeed
  :custom
  ;; Give time for long updates to complete
  (url-queue-timeout 300)
  (elfeed-curl-timeout 300)
  (elfeed-curl-max-connections 300) ; Get a bunch at a time

  (elfeed-search-remain-on-entry t)
  (elfeed-search-clipboard-type 'clipboard) ; Paste to system clipboard

  (elfeed-feeds '())
  ;; (elfeed-search-filter "-archive @2-week-ago +unread")
  (elfeed-search-filter "")
  (elfeed-initial-tags nil)
  :init
  (defun prot-elfeed-kill-buffer-close-window-dwim ()
    "Do-what-I-mean way to handle `elfeed' windows and buffers.

When in an entry buffer, kill the buffer and return to the Elfeed
Search view.  If the entry is in its own window, delete it as
well.

When in the search view, close all other windows.  Else just kill
the buffer."
    (interactive)
    (let ((win (selected-window)))
      (cond ((eq major-mode 'elfeed-show-mode)
             (elfeed-kill-buffer)
             (unless (one-window-p) (delete-window win))
             (switch-to-buffer "*elfeed-search*"))
            ((eq major-mode 'elfeed-search-mode)
             (if (one-window-p)
                 (elfeed-search-quit-window)
               (delete-other-windows win))))))
  :config
  ;; Tag hooks
  (setq elfeed-new-entry-hook
   `(;; Status tags
     ,(elfeed-make-tagger :before "3 months ago" ; Archive very old entries
                          :remove 'aging
                          :add 'archive)
     ,(elfeed-make-tagger :before "1 month ago" ; Don't be distracted by old entries
                          :after "3 months ago"
                          :remove 'unread
                          :add 'aging)
     ))

  ;; Update elfeed every time it is opened
  (advice-add 'elfeed :after #'elfeed-update)
  ;; Apply the appropriate autotags to already existing entries
  (add-hook 'elfeed-search-update-hook #'elfeed-apply-autotags-now)

  (general-define-key
   :keymaps '(elfeed-show-mode-map elfeed-search-mode-map)
   :states 'normal
   [remap elfeed-search-quit-window] '(prot-elfeed-kill-buffer-close-window-dwim :which-key "Close elfeed")
   [remap elfeed-search-tag-all] '(prot-elfeed-toggle-tag :which-key "Add tag")
   "L" '((lambda ()
           (interactive)
           (elfeed-goodies/toggle-logs)
           (other-window 1)
           )
         :which-key "Elfeed logs")
   )

  (kb/leader-keys
    "or" '(elfeed :which-key "Elfeed")
    )
  )

;;;; QoL
;; From https://protesilaos.com/dotemacs/#h:0cd8ddab-55d1-40df-b3db-1234850792ba
;;;;; View in EWW
(defun prot-elfeed-show-eww (&optional link)
  "Browse current entry's link or optional LINK in `eww'.

Only show the readable part once the website loads.  This can
fail on poorly-designed websites."
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :ignore-region)))
         (link (or link (elfeed-entry-link entry))))
    (eww link)
    (add-hook 'eww-after-render-hook 'eww-readable nil t))
  )
(general-define-key
 :keymaps 'elfeed-show-mode-map
 :states 'normal
 "e" '(prot-elfeed-show-eww :which-key "Show in EWW")
 )

(add-hook 'eww-mode-hook '(lambda () (visual-fill-column-mode) (variable-pitch-mode)))

;;;;; Add archive tag
(defun prot-elfeed-toggle-tag (tag)
  "Toggle TAG for the current item.

When the region is active in the `elfeed-search-mode' buffer, all
entries encompassed by it are affected.  Otherwise the item at
point is the target.  For `elfeed-show-mode', the current entry
is always the target.

The list of tags is provided by `prot-elfeed-search-tags'."
  (interactive
   (list
    (intern
     ;; (prot-elfeed--character-prompt prot-elfeed-search-tags))))
     ;; (prot-elfeed--character-prompt prot-elfeed-search-tags)
     (car (completing-read-multiple
           "Apply one or more tags: "
           (delete-dups (append (mapcar (lambda (tag)
                                          (format "%s" tag))
                                        (elfeed-db-get-all-tags))
                                ))
           #'prot-common-crm-exclude-selected-p t)
          ;; (prot-elfeed-search-tag-filter)
          ))
    ))
  (if (derived-mode-p 'elfeed-show-mode)
      (if (elfeed-tagged-p tag elfeed-show-entry)
          (elfeed-show-untag tag)
        (elfeed-show-tag tag))
    (elfeed-search-toggle-all tag))
  )
(general-define-key ; Search keymap
 :keymaps '(elfeed-search-mode-map elfeed-show-mode-map)
 :states '(visual normal)
 "a" '((lambda ()
         (interactive)
         (prot-elfeed-toggle-tag 'archive)
         )
       :which-key "Add archive tag")
 )

;;;;; Search completion
(defun prot-common-crm-exclude-selected-p (input)
  "Filter out INPUT from `completing-read-multiple'.
Hide non-destructively the selected entries from the completion
table, thus avoiding the risk of inputting the same match twice.

To be used as the PREDICATE of `completing-read-multiple'."
  (if-let* ((pos (string-match-p crm-separator input))
            (rev-input (reverse input))
            (element (reverse
                      (substring rev-input 0
                                 (string-match-p crm-separator rev-input))))
            (flag t))
      (progn
        (while pos
          (if (string= (substring input 0 pos) element)
              (setq pos nil)
            (setq input (substring input (1+ pos))
                  pos (string-match-p crm-separator input)
                  flag (when pos t))))
        (not flag))
    t)
  )

(defun prot-elfeed-search-tag-filter ()
  "Filter Elfeed search buffer by tags using completion.

Completion accepts multiple inputs, delimited by `crm-separator'.
Arbitrary input is also possible, but you may have to exit the
minibuffer with something like `exit-minibuffer'."
  (interactive)
  (unwind-protect
      (elfeed-search-clear-filter)
    (let* ((elfeed-search-filter-active :live)
           (db-tags (elfeed-db-get-all-tags))
           (plus-tags (mapcar (lambda (tag)
                                (format "+%s" tag))
                              db-tags))
           (minus-tags (mapcar (lambda (tag)
                                 (format "-%s" tag))
                               db-tags))
           (all-tags (delete-dups (append plus-tags minus-tags)))
           (tags (completing-read-multiple
                  "Apply one or more tags: "
                  all-tags #'prot-common-crm-exclude-selected-p t))
           (input (string-join `(,elfeed-search-filter ,@tags) " ")))
      (setq elfeed-search-filter input))
    (elfeed-search-update :force))
  )

(general-define-key
 :keymaps 'elfeed-search-mode-map
 :states 'normal
 "C-s" '(prot-elfeed-search-tag-filter :which-key "Prot tag completion")
 )

;;;; Ancillary
;;;;; Elfeed-org
(use-package elfeed-org
  :after elfeed
  :custom
  (rmh-elfeed-org-files `(,(concat no-littering-var-directory "elfeed/elfeed-feeds.org")
                          ))
  (rmh-elfeed-org-auto-ignore-invalid-feeds nil) ; Appropriately tag failed entries
  :config
  (elfeed-org)
  )

;;;;; Elfeed-goodies
(use-package elfeed-goodies
  :after elfeed
  :custom
  (elfeed-goodies/feed-source-column-width 25)
  (elfeed-goodies/tag-column-width 40)

  (elfeed-goodies/entry-pane-position 'right)
  (elfeed-goodies/entry-pane-size 0.5)
  :config
  (elfeed-goodies/setup)

  (general-define-key
   :keymaps '(elfeed-show-mode-map elfeed-search-mode-map)
   :states 'normal
   "K" 'elfeed-goodies/split-show-prev
   "J" 'elfeed-goodies/split-show-next
   )
  )

;;;;; Elfeed-dashboard
(use-package elfeed-dashboard
  :disabled t ; Not really working
  :straight (elfeed-dashboard :type git :host github :repo "Manoj321/elfeed-dashboard")
  :custom
  (elfeed-dashboard-file (concat no-littering-var-directory "elfeed/elfeed-dashboard.org"))
  :config
  (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links) ; Update feed counts on elfeed-quit
  )

;;; rss-feed-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'rss-feed-rcp)
