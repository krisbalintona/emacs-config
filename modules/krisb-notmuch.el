;;;; Notmuch
(use-package notmuch
  :ensure-system-package (notmuch
                          (gmi . "lieer-git"))
  :hook ((notmuch-mua-send . notmuch-mua-attachment-check) ; Also see `notmuch-mua-attachment-regexp'
         (notmuch-show . olivetti-mode)
         (message-send . krisb-notmuch-set-sendmail-args))
  :bind (([remap compose-mail] . notmuch-mua-new-mail)
         :map krisb-open-keymap
         ("n" . notmuch)
         :map notmuch-search-mode-map
         ("a" . nil) ; The default is too easy to hit accidentally
         ("/" . notmuch-search-filter)
         ("r" . notmuch-search-reply-to-thread)
         ("R" . notmuch-search-reply-to-thread-sender)
         :map notmuch-show-mode-map
         ("a" . nil)
         ("r" . notmuch-show-reply)
         ("R" . notmuch-show-reply-sender)
         ("T" . krisb-notmuch-show-trash-thread-then-next)
         ([remap notmuch-show-advance-and-archive] . krisb-notmuch-show-advance-and-tag))
  :custom
  (mail-user-agent 'notmuch-user-agent)
  (notmuch-identities nil)              ; Defer to notmuch-config's file data

  ;; Hello UI
  (notmuch-hello-sections (list #'notmuch-hello-insert-saved-searches
                                #'notmuch-hello-insert-alltags
                                #'notmuch-hello-insert-recent-searches))
  (notmuch-hello-thousands-separator ",")
  (notmuch-show-all-tags-list t)

  ;; Searches
  ;; See `man' for mbsync and notmuch to see valid search terms. See
  ;; https://www.emacswiki.org/emacs/NotMuch#h5o-2 on how to expunge local files
  ;; via cli
  (notmuch-search-hide-excluded t)
  (notmuch-saved-searches
   '((:name "inbox"                 :query "tag:inbox and not tag:list" :sort-order oldest-first :key "i")
     (:name "to-read mailing lists" :query "tag:list and tag:inbox "    :sort-order oldest-first :key "l")
     (:name "all mailing lists"     :query "tag:list"                                            :key "L" )
     (:name "sent"                  :query "tag:sent"                                            :key "s")
     (:name "drafts"                :query "tag:draft or path:drafts/"  :search-type unthreaded  :key "d")
     (:name "archived"              :query "not tag:trash"                                       :key "a")
     (:name "all"                   :query "path:**"                                             :key "A")
     (:name "trash"                 :query "tag:trash"                                           :key "t")))
  (notmuch-show-empty-saved-searches t)
  (notmuch-search-oldest-first nil)
  (notmuch-search-result-format '(("date" . "%14s ")
                                  ("count" . "%-7s ")
                                  ("authors" . "%-30s ")
                                  ("subject" . "%-75.75s ")
                                  ("tags" . "(%s)")))

  ;; Tags
  (notmuch-archive-tags '("-inbox"))
  (notmuch-message-replied-tags '("+replied"))
  (notmuch-message-forwarded-tags '("+forwarded"))
  (notmuch-show-mark-read-tags '("-unread"))
  (notmuch-draft-tags '("+draft"))
  (notmuch-draft-folder "drafts")
  (notmuch-draft-save-plaintext 'ask)
  (notmuch-tagging-keys
   `(("a" notmuch-archive-tags "Archive")
     ("r" notmuch-show-mark-read-tags "Mark read")
     ("f" ("+flagged") "Flag")
     ("s" ("+spam" "-inbox") "Mark as spam")
     ("t" ("+trash" "-inbox") "Trash")))
  (notmuch-tag-formats
   '(("unread" (propertize tag 'face 'notmuch-tag-unread))
     ("flagged" (propertize tag 'face 'notmuch-tag-flagged))))
  (notmuch-tag-deleted-formats
   '(("unread" (notmuch-apply-face bare-tag `notmuch-tag-deleted))
     (".*" (notmuch-apply-face tag `notmuch-tag-deleted))))

  ;; Notmuch-show-mode (i.e. reading emails)
  (notmuch-show-relative-dates t)
  (notmuch-show-all-multipart/alternative-parts nil)
  (notmuch-show-indent-multipart nil)
  (notmuch-show-indent-messages-width 3)
  (notmuch-show-part-button-default-action 'notmuch-show-interactively-view-part)
  (notmuch-show-text/html-blocked-images ".") ; Block everything
  (notmuch-wash-wrap-lines-length nil)
  (notmuch-unthreaded-show-out t)
  (notmuch-message-headers-visible nil)
  (notmuch-message-headers '("To" "Cc" "Date" "Subject"))

  ;; Notmuch-tree-mode
  (notmuch-tree-show-out nil)
  (notmuch-tree-result-format '(("date" . "%12s  ")
                                ("authors" . "%-20s  ")
                                ((("tree" . "%s")
                                  ("subject" . "%s"))
                                 . " %-85.85s  ")
                                ("tags" . "(%s)")))
  (notmuch-tree-outline-enabled nil)

  ;; Email composition
  (notmuch-mua-compose-in 'current-window)
  (notmuch-mua-hidden-headers nil)
  (notmuch-address-command 'internal)
  (notmuch-address-internal-completion '(sent nil))
  (notmuch-always-prompt-for-sender t)
  (notmuch-mua-cite-function 'message-cite-original-without-signature)
  (notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never)
  (notmuch-mua-user-agent-function nil)
  (notmuch-maildir-use-notmuch-insert t)
  (notmuch-wash-citation-lines-prefix 0)
  (notmuch-wash-citation-lines-suffix 0)
  (notmuch-crypto-process-mime t)
  (notmuch-crypto-get-keys-asynchronously t)
  ;; See `notmuch-mua-send-hook'
  (notmuch-mua-attachment-regexp (concat "\\b\\("
                                         "attache\?ment\\|attached\\|attach\\|"
                                         "pi[èe]ce\s+jointe?"
                                         "\\)\\b"))

  ;; Sending emails.
  ;; Use Lieer to send emails. Also see `krisb-notmuch-set-sendmail-args'. Read
  ;; https://github.com/gauteh/lieer/wiki/Emacs-and-Lieer.
  (sendmail-program (executable-find "gmi"))
  (send-mail-function 'sendmail-send-it)
  (notmuch-fcc-dirs nil) ; Gmail already copies sent emails, so don't move them elsewhere locally
  :config
  ;; Don't buttonize citations
  ;; FIXME 2024-10-07: For some reason putting this in :custom and setting it to
  ;; a high value doesn't work, so I put it here
  (setq notmuch-wash-citation-lines-prefix most-positive-fixnum
        notmuch-wash-citation-lines-suffix most-positive-fixnum)

  ;; Set sendmail args appropriate to using lieer as `sendmail-program'
  (defun krisb-notmuch-set-sendmail-args ()
    "Set `message-sendmail-extra-arguments' arguments.
Set `message-sendmail-extra-arguments' accordingly (changing the
maildir) such that lieer can properly send the email. (This assumes
`sendmail-program' is set to the gmi executable.) Instruction from
https://github.com/gauteh/lieer/wiki/Emacs-and-Lieer."
    (when (string-match-p "gmi" sendmail-program)
      (let* ((from (downcase (message-fetch-field "from")))
             (root-maildir (expand-file-name "~/Documents/emails/"))
             (personal-maildir (expand-file-name "personal" root-maildir))
             (uni-maildir (expand-file-name "uni" root-maildir)))
        (cond
         ((string-match-p (rx (literal "krisbalintona@gmail.com")) from)
          (setq-local message-sendmail-extra-arguments `("send" "--quiet" "-t" "-C" ,personal-maildir)))
         ((string-match-p (rx (literal "kristoffer_balintona@alumni.brown.edu")) from)
          (setq-local message-sendmail-extra-arguments `("send" "--quiet" "-t" "-C" ,uni-maildir)))))))

  ;; REVIEW 2024-09-26: Prot's lin package apparently makes disabling this
  ;; better?
  (with-eval-after-load 'lin
    (remove-hook 'notmuch-search-hook #'notmuch-hl-line-mode))

  ;; Prefer not to have emails recentered as I readjust them
  (advice-add 'notmuch-show-message-adjust :override #'ignore)

  ;; Setup faces
  (defun krisb-notmuch--setup-faces (theme)
    "Set up faces in `notmuch-show-mode'."
    (when (string-match "^modus-" (symbol-name theme))
      (modus-themes-with-colors
        ;; More noticeable demarcation of emails in thread in notmuch-show-mode
        (set-face-attribute 'notmuch-message-summary-face nil
                            :foreground fg-alt
                            ;; NOTE 2024-09-26: We do it this way since changing
                            ;; faces will refresh the font to be 1.1 times the 1.1
                            ;; times height, and so on
                            :height (truncate (* (face-attribute 'default :height nil) 1.1))
                            :overline t
                            :extend nil
                            :inherit 'unspecified)
        (set-face-attribute 'notmuch-tag-added nil
                            :underline `(:color ,cyan-cooler :style double-line :position t))
        (add-to-list 'notmuch-tag-formats
                     `("correspondence" (propertize tag 'face '(:foreground ,green-faint))))
        (add-to-list 'notmuch-tag-formats
                     `("commitment" (propertize tag 'face '(:foreground ,yellow-faint)))))))
  (add-hook 'enable-theme-functions #'krisb-notmuch--setup-faces))

;;;; Krisb-notmuch-ext
(use-package krisb-notmuch-ext
  :after notmuch
  :hook (notmuch-show  . krisb-notmuch-show-expand-only-unread-h)
  :config
  (with-eval-after-load 'pulsar
    (dolist (func '(notmuch-show-rewind
                    notmuch-show-advance-and-archive
                    krisb-notmuch-show-advance-and-tag))
      (add-to-list 'pulsar-pulse-functions func))))

;;; Provide
(provide 'krisb-notmuch)
