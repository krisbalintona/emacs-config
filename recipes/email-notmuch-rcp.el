;;; email-notmuch-rcp.el --- Summary
;;
;;; Commentary:
;;
;; These are packages pertinent to using the `notmuch' email client. It is also
;; important to note that this is only a frontend for the `notmuch' command line
;; tool. Syncing from remote servers can be done via `mbsync' (from the `mu'
;; utility set) and `offlineimap', whose configuration files are `.mbsyncrc' and
;; `.offlineimaprc', respectively.
;;
;; Protesilaos provides a wonderful guide here:
;; https://protesilaos.com/emacs/dotemacs#h:5ad80664-3163-4d9d-be65-462637d77903
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'general)
(require 'keybinds-general-rcp)

;;; Notmuch
(use-package notmuch
  :ensure-system-package (notmuch
                          (mbsync . isync)
                          (gmi . "pip install lieer"))
  :hook ((notmuch-mua-send . notmuch-mua-attachment-check)
         (notmuch-show-mode . visual-fill-column-mode))
  :general
  (kb/open-keys
    "n" '(notmuch :wk "Notmuch"))
  ([remap compose-mail] #'notmuch-mua-new-mail)
  (:keymaps 'notmuch-search-mode-map
            "r" 'notmuch-search-reply-to-thread ; Easier to reply to all by default
            "R" 'notmuch-search-reply-to-thread-sender)
  (:keymaps 'notmuch-show-mode-map
            "r" 'notmuch-show-reply     ; Easier to reply to all by default
            "R" 'notmuch-show-reply-sender)
  :custom
  ;; Account settings
  (notmuch-identities nil)              ; Defer to notmuch-config's file data
  (notmuch-fcc-dirs                     ; Set sent mail directories
   '(("krisbalintona@gmail.com" . "personal/[Gmail].Sent Mail")
     ("kristoffer_balintona@brown.com" . "personal/[Gmail].Sent Mail")))

  ;; General UI
  (notmuch-show-logo nil)
  (notmuch-column-control t)
  (notmuch-hello-auto-refresh t)
  (notmuch-hello-recent-searches-max 20)
  (notmuch-hello-thousands-separator ",")
  (notmuch-hello-sections
   '(notmuch-hello-insert-saved-searches notmuch-hello-insert-alltags))
  (notmuch-show-all-tags-list t)

  ;; Search
  (notmuch-search-oldest-first nil)
  (notmuch-search-result-format
   '(("date" . "%12s  ")
     ("count" . "%-7s  ")
     ("authors" . "%-20s  ")
     ("subject" . "%-80s  ")
     ("tags" . "(%s)")))
  (notmuch-tree-result-format
   '(("date" . "%12s  ")
     ("authors" . "%-20s  ")
     ((("tree" . "%s")
       ("subject" . "%s"))
      . " %-80s  ")
     ("tags" . "(%s)")))
  (notmuch-search-line-faces
   '(("unread" . notmuch-search-unread-face)
     ("flagged" . notmuch-search-flagged-face)))
  (notmuch-show-empty-saved-searches t)
  ;; See `man' for mbsync and notmuch to see valid search terms. See
  ;; https://www.emacswiki.org/emacs/NotMuch#h5o-2 how to expunge local files
  ;; via cli
  (notmuch-saved-searches
   '((:name "[personal] inbox" :query "path:personal/** tag:inbox" :key "I")
     (:name "[uni] inbox" :query "path:uni/** tag:inbox" :key "i")
     (:name "[personal] flagged" :query "path:personal/** tag:flagged" :key "F")
     (:name "[uni] flagged" :query "path:uni/** tag:flagged" :key "f")
     (:name "[personal] sent" :query "from:krisbalintona@gmail.com* tag:sent" :key "S")
     (:name "[uni] sent" :query "from:kristoffer_balintona@brown.edu* tag:sent" :key "s")
     (:name "[personal] drafts" :query "from:krisbalintona@gmail.com* tag:draft -tag:trash -tag:deleted" :key "D")
     (:name "[uni] drafts" :query "from:kristoffer_balintona@brown.edu* tag:draft -tag:trash  -tag:deleted" :key "d")
     (:name "[personal] all mail" :query "path:personal/** -tag:trash" :key "A")
     (:name "[uni] all mail" :query "path:uni/** -tag:trash" :key "a")
     (:name "trash" :query "tag:trash" :key "T")))

  ;; Tree
  (notmuch-tree-show-out t)

  ;; Tags
  (notmuch-archive-tags '("-inbox"))
  (notmuch-message-replied-tags '("+replied"))
  (notmuch-message-forwarded-tags '("+forwarded"))
  (notmuch-show-mark-read-tags '("-unread"))
  (notmuch-draft-tags '("+draft"))
  (notmuch-draft-folder "drafts")
  (notmuch-draft-save-plaintext 'ask)
  (notmuch-tagging-keys
   '(("a" notmuch-archive-tags "Archive")
     ("r" notmuch-show-mark-read-tags "Mark read")
     ("f" ("+flagged") "Flag")
     ("s" ("+spam" "-inbox") "Mark as spam")
     ("d" ("+trash" "-inbox") "Trash")))
  (notmuch-tag-formats
   '(("unread" (propertize tag 'face 'notmuch-tag-unread))
     ("flagged"
      (propertize tag 'face 'notmuch-tag-flagged)
      (notmuch-tag-format-image-data tag (notmuch-tag-star-icon)))))
  (notmuch-tag-deleted-formats
   '(("unread" (notmuch-apply-face bare-tag `notmuch-tag-deleted))
     (".*" (notmuch-apply-face tag `notmuch-tag-deleted))))

  ;; Email composition
  (notmuch-mua-compose-in 'current-window)
  (notmuch-mua-hidden-headers nil)
  (notmuch-address-command 'internal)
  (notmuch-always-prompt-for-sender t)
  (notmuch-mua-cite-function 'message-cite-original-without-signature)
  (notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never)
  (notmuch-mua-user-agent-function nil)
  (notmuch-maildir-use-notmuch-insert t) ; Simple or notmuch fcc insert?
  (notmuch-crypto-process-mime t)
  (notmuch-crypto-get-keys-asynchronously t)
  (notmuch-mua-attachment-regexp        ; Also see `notmuch-mua-send-hook'
   "\\b\\(attache\?ment\\|attached\\|attach\\|pi[èe]ce\s+jointe?\\)\\b")

  ;; Reading messages
  (notmuch-show-relative-dates t)
  (notmuch-show-all-multipart/alternative-parts nil)
  ;; (notmuch-show-indent-messages-width 0)
  (notmuch-show-indent-multipart nil)
  (notmuch-show-part-button-default-action 'notmuch-show-save-part)
  ;; (notmuch-show-text/html-blocked-images ".") ; Block everything
  (notmuch-wash-citation-lines-prefix 6)
  (notmuch-wash-citation-lines-suffix 6)
  ;; (notmuch-wash-wrap-lines-length 100)
  (notmuch-unthreaded-show-out nil)
  (notmuch-message-headers '("To" "Cc" "Date" "Subject"))
  (notmuch-message-headers-visible t))

;;; Mu4e
(use-package mu4e
  :general (kb/open-keys
             "M" '(mu4e :wk "Mu4e"))
  ([remap compose-mail] 'mu4e-compose-new)
  :custom
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-drafts-folder "/personal/[Gmail].Drafts")
  (mu4e-sent-folder "/personal/[Gmail].Sent Mail")
  (mu4e-trash-folder "/personal/[Gmail].Trash")

  ;; Don't save message to Sent Messages, Gmail/IMAP takes care of this
  (mu4e-sent-messages-behavior 'delete)

  (mu4e-maildir-shortcuts
   '((:maildir "/personal/Index"              :key ?i)
     (:maildir "/personal/[Gmail].Sent Mail"  :key ?s)
     (:maildir "/personal/[Gmail].Trash"       :key ?t)
     (:maildir "/personal/[Gmail].All Mail"   :key ?a)))
  )

;;; email-notmuch-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'email-notmuch-rcp)
