;; -*- lexical-binding: t; -*-

;;;; Notmuch
(use-package notmuch
  ;; For AUR:
  ;; :ensure-system-package (notmuch
  ;;                         (gmi . lieer-git))
  :hook ((notmuch-mua-send . notmuch-mua-attachment-check) ; Also see `notmuch-mua-attachment-regexp'
         (notmuch-show . olivetti-mode)
         (notmuch-show . visual-line-mode)
         (notmuch-show . visual-wrap-prefix-mode)
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

  ;; Notmuch-searches
  (notmuch-saved-searches
   '((:name "inbox"                 :query "tag:inbox and (not tag:list or to:krisbalintona@gmail.com)" :sort-order oldest-first :key "i")
     (:name "to-read mailing lists" :query "tag:list and tag:inbox "                                    :sort-order oldest-first :key "l")
     (:name "all mailing lists"     :query "tag:list and not to:krisbalintona@gmail.com"                                         :key "L" )
     (:name "sent"                  :query "tag:sent"                                                   :sort-order newest-first :key "s")
     (:name "drafts"                :query "tag:draft or path:drafts/"                                  :sort-order newest-first :key "d" :search-type unthreaded)
     (:name "archived"              :query "not tag:trash"                                                                       :key "a")
     (:name "all"                   :query "path:**"                                                                             :key "A")
     (:name "trash"                 :query "tag:trash"                                                                           :key "t")))
  ;; See `man' for mbsync and notmuch to see valid search terms. See
  ;; https://www.emacswiki.org/emacs/NotMuch#h5o-2 on how to expunge local files
  ;; via cli
  (notmuch-search-hide-excluded t)
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
  (notmuch-multipart/alternative-discouraged
   '("text/html" "multipart/related" "text/x-patch"))

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
  (krisb-modus-themes-setup-faces
   "notmuch"
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
                `("commitment" (propertize tag 'face '(:foreground ,yellow-faint)))))

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
  (advice-add 'notmuch-show-message-adjust :override #'ignore))

;;;; Krisb-notmuch-ext
(use-package krisb-notmuch-ext
  :ensure nil
  :after notmuch
  :hook (notmuch-show  . krisb-notmuch-show-expand-only-unread-h)
  :config
  (with-eval-after-load 'pulsar
    (dolist (func '(notmuch-show-rewind
                    notmuch-show-advance-and-archive
                    krisb-notmuch-show-advance-and-tag))
      (add-to-list 'pulsar-pulse-functions func))))

;;;; Notmuch-transient
(use-package notmuch-transient
  :after notmuch
  :custom
  (notmuch-transient-add-bindings t)
  (notmuch-transient-prefix "C-d"))

;;;; Notmuch-addr
;; Better address completion for notmuch; replaces the built-in
;; `notmuch-address' completion system. Read
;; https://nmbug.notmuchmail.org/nmweb/show/20201108231150.5419-1-jonas%40bernoul.li
;; for more information
(use-package notmuch-addr
  :after notmuch-address
  :config
  (notmuch-addr-setup))

;;;; Ol-notmuch
;; Org-links for search queries (i.e. notmuch-search-mode, notmuch-tree-mode)
;; and messages (i.e. notmuch-show-mode).
(use-package ol-notmuch
  :config
  ;; Integration with `org-agenda'
  (with-eval-after-load 'ol
    (el-patch-defun org-notmuch-store-link ()
      (el-patch-swap
        "Store a link to one or more notmuch messages."
        "Store a link to one or more notmuch messages.
My version allows for linking to the first message in an email thread
from a `notmuch-search-mode' buffer.")
      ;; 2025-04-09: Not sure what the most elegant el-patch directives would
      ;; be, so I just remove then add.
      (el-patch-remove
        (when (memq major-mode '(notmuch-show-mode notmuch-tree-mode))
          ;; The value is passed around using variable `org-store-link-plist'.
          (org-link-store-props
           :type       "notmuch"
           :message-id (notmuch-show-get-message-id t)
           :subject    (notmuch-show-get-subject)
           :from       (notmuch-show-get-from)
           :to         (notmuch-show-get-to)
           :date       (org-trim (notmuch-show-get-date)))
          (org-link-add-props :link (org-link-email-description "notmuch:id:%m"))
          (org-link-add-props :description (org-link-email-description))
          org-store-link-plist))
      (el-patch-add
        (cond
         ((memq major-mode '(notmuch-show-mode notmuch-tree-mode))
          ;; The value is passed around using variable `org-store-link-plist'.
          (org-link-store-props
           :type       "notmuch"
           :message-id (notmuch-show-get-message-id t)
           :subject    (notmuch-show-get-subject)
           :from       (notmuch-show-get-from)
           :to         (notmuch-show-get-to)
           :date       (org-trim (notmuch-show-get-date)))
          (org-link-add-props :link (org-link-email-description "notmuch:id:%m"))
          (org-link-add-props :description (org-link-email-description))
          org-store-link-plist)
         ((equal major-mode 'notmuch-search-mode)
          (save-window-excursion
            (let ((buf (notmuch-show (notmuch-search-find-thread-id))))
              (with-current-buffer buf
                (org-link-store-props
                 :type       "notmuch"
                 :message-id (notmuch-show-get-message-id t)
                 :subject    (notmuch-show-get-subject)
                 :from       (notmuch-show-get-from)
                 :to         (notmuch-show-get-to)
                 :date       (org-trim (notmuch-show-get-date)))
                (org-link-add-props :link (org-link-email-description "notmuch:id:%m"))
                (org-link-add-props :description (org-link-email-description)))
              (kill-buffer buf)
              org-store-link-plist)))))))

  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 `("e" "Email" entry
                   (file ,(expand-file-name "todo.org" krisb-org-agenda-directory))
                   "* TODO %? [[%L][\"%:subject\"]] :email:\n\nFrom %:from\nTo: %:to\n"
                   :empty-lines 1)
                 'append)
    (add-to-list 'org-capture-templates
                 `("n" "Review newsletter/subscription email" entry
                   (file ,krisb-org-agenda-main-file)
                   "* TODO [#E] Review subscription/newsletter email: [[%L][\"%:subject\"]] %? :email:inbox:%^g

From %:from
To: %:to\n"
                   :immediate-finish t
                   :empty-lines 1)
                 'append)

    ;; Using `dolist' or `cl-loop' will not work as expected... you'll need to
    ;; (copy-sequence ...) the shared objects, making those forms not elegant
    (add-to-list 'org-capture-templates-contexts '("e" ((in-mode . "notmuch-tree-mode"))))
    (add-to-list 'org-capture-templates-contexts '("n" ((in-mode . "notmuch-tree-mode"))))
    (add-to-list 'org-capture-templates-contexts '("e" ((in-mode . "notmuch-search-mode"))))
    (add-to-list 'org-capture-templates-contexts '("n" ((in-mode . "notmuch-search-mode"))))
    (add-to-list 'org-capture-templates-contexts '("e" ((in-mode . "notmuch-show-mode"))))
    (add-to-list 'org-capture-templates-contexts '("n" ((in-mode . "notmuch-show-mode"))))))

;;;; Mode line indicator
;; Try using display-time's built-in email indicator --- less informative but
;; more visually subtle than `notmuch-indicator'.  Obviously the below applies
;; only when `display-time-mode' is non-nil.
(with-eval-after-load 'time
  (with-eval-after-load 'notmuch
    (setopt display-time-mail-face 'notmuch-search-flagged-face))

  (defvar krisb-display-time-mail-icon
    (cond
     ((featurep 'nerd-icons)
      (propertize (nerd-icons-mdicon "nf-md-email")
                  'face `(:family ,(nerd-icons-mdicon-family) :height 1.1)
                  'display '(raise 0.05)))
     ((featurep 'all-the-icons)
      (propertize (all-the-icons-material "mail_outline")
                  'face `(:family ,(all-the-icons-material-family) :height 1.1)
                  'display '(raise -0.1))))
    "Icon I use for displaying mail in `display-time-string-forms'.")

  (setopt display-time-use-mail-icon t
          display-time-mail-function
          (lambda ()
            (let* ((command (format "notmuch search tag:inbox and tag:unread and not tag:list and not tag:sub | wc -l"))
                   (count (string-to-number (shell-command-to-string command))))
              (< 0 count)))
          display-time-string-forms
          '((if (and (not display-time-format) display-time-day-and-date)
                (format-time-string "%a %b %e " now)
              "")
            (propertize
             (format-time-string (or display-time-format
                                     (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
                                 now)
             'face 'display-time-date-and-time
             'help-echo (format-time-string "%a %b %e, %Y" now))
            load
            (if mail
                (concat
                 " "
                 (propertize
                  (if (and display-time-use-mail-icon (display-graphic-p))
                      (symbol-value 'krisb-display-time-mail-icon)
                    display-time-mail-string)
                  'face display-time-mail-face
                  'help-echo "You have new mail; mouse-2: Read mail"
                  'mouse-face 'mode-line-highlight
                  'local-map (make-mode-line-mouse-map 'mouse-2
                                                       read-mail-command)))
              "")
            " "))
  (advice-add 'notmuch-bury-or-kill-this-buffer :around
              (lambda (&rest args)
                "Ensure mail icon is accurate.
Update right after closing the notmuch hello buffer so the mail icon
reflects the state of my maildirs accurate."
                (when (equal major-mode 'notmuch-hello-mode)
                  (display-time-update))
                (apply args))))

;;;; Mailcap
(use-package mailcap
  :ensure nil
  :custom
  ;; This affects the action called by `notmuch-show-view-part' and the listings
  ;; of `notmuch-show-interactively-view-part'
  (mailcap-user-mime-data
   '(("xdg-open %s" "text/html"))))

;;; Provide
(provide 'krisb-notmuch)
