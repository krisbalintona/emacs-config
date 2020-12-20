;;; mu4e-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Configuration of the `mu4e' email package and related packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'general)
;;;; Mu4e
;; Viewing emails with the best email client
(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :ensure-system-package ((isync)
                          (magick . imagemagick))
  :custom
  ;; Overall mu4e and mbsync settings
  (mu4e-get-mail-command "/usr/bin/mbsync -a") ; Use mbsync to sync mail
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-confirm-quit nil) ; Close mu4e without asking
  ;; (mu4e-completing-read-function 'ivy-completing-read) ; Use ivy for completion
  (mu4e-completing-read-function 'completing-read) ; Use selectrum for completion

  ;; Updating and indexing
  (mu4e-update-interval 300) ; Indexing interval
  (mu4e-index-cleanup t) ; Make sure nothing breaks
  (mu4e-index-update-in-background t) ; Update in background
  (mu4e-hide-index-messages t) ; Hide "indexing" message in minibuffer

  ;; Mail directories
  (mu4e-root-maildir "~/Documents/Emails/Personal")
  (mu4e-context-policy 'pick-first) ; Choose first context (Brown)
  ;; These are overwritten in by `mu4e-contexts'. They are defined here to allow
  ;; mu4e to open without error.
  (mu4e-sent-folder "/Personal/Sent/")
  (mu4e-trash-folder "/Personal/Trash/")
  (mu4e-drafts-folder "/Personal/Drafts/")
  (mu4e-attachment-dir "~/Documents/Emails/Personal/Attachments/")
  (mu4e-refile-folder "/Personal/All-Mail/")

  ;; Main mode
  (mu4e-maildir-shortcuts
   '((:maildir "/Personal/All-Mail" :key ?A)
     (:maildir "/Personal/Sent" :key ?S)
     (:maildir "/Personal/Trash" :key ?T)
     (:maildir "/Personal/Inbox" :key ?I)
     (:maildir "/Personal/Drafts" :key ?D)
     (:maildir "/Personal/Starred" :key ?L)
     (:maildir "/Brown/All-Mail" :key ?a)
     (:maildir "/Brown/Sent" :key ?s)
     (:maildir "/Brown/Trash" :key ?t)
     (:maildir "/Brown/Inbox" :key ?i)
     (:maildir "/Brown/Drafts" :key ?d)
     (:maildir "/Brown/Starred" :key ?l)
     ))

  ;; Header mode
  (mu4e-headers-fields '((:from . 25)
                         (:flags . 8)
                         (:subject . 83)
                         (:human-date . 13)
                         (:account . 13)
                         ))
  (mu4e-use-fancy-chars t) ; Use fancy icons
  (mu4e-headers-draft-mark '("D" . " "))
  (mu4e-headers-flagged-mark '("F" . " "))
  (mu4e-headers-new-mark '("N" . " "))
  (mu4e-headers-passed-mark '("P" . " "))
  (mu4e-headers-replied-mark '("R" . " "))
  (mu4e-headers-seen-mark '("S" . " "))
  (mu4e-headers-trashed-mark '("T" . " "))
  (mu4e-headers-attach-mark '("a" . " "))
  (mu4e-headers-encrypted-mark '("x" . " "))
  (mu4e-headers-signed-mark '("s" . " "))
  (mu4e-headers-unread-mark '("u" . " "))

  ;; View mode
  (mu4e-view-fields '(:from :to :cc :bcc :subject :flags :date :maildir :mailing-list :tags :attachments :signature))
  (mu4e-change-filenames-when-moving t) ; Prevent duplicate UUIDs of messages
  (mu4e-view-show-images t) ; Enable inline images
  (mu4e-view-prefer-html nil) ; Don't prefer html
  (mu4e-html2text-command 'mu4e-shr2text) ; Renders emails' html with eww engine

  ;; Compose mode
  (mu4e-compose-dont-reply-to-self t) ; Don't reply to myself
  (mu4e-compose-context-policy 'ask) ; Always ask which email I'm sending from
  (message-kill-buffer-on-exit t) ; Delete message buffer after sent
  (mu4e-view-show-addresses nil) ; Show names instead of addresses
  (mu4e-headers-include-related t) ; Show related messages on searching. Can be toggled with `mu4e-headers-toggle-include-related'
  ;; Some email clients ignore format=flowed (i.e. Outlook). Therefore,
  ;; we send very long lines, so that they auto-flow. 998 chars are the
  ;; actual maximum from the relevant RFC:
  ;; https://www.ietf.org/rfc/rfc2822.txt
  (fill-flowed-encode-column 998)
  (mu4e-compose-format-flowed t)
  (mu4e-sent-messages-behavior 'delete) ; Don't save messages to Sent Messages directory - Gmail/IMAP takes care of this
  :init (require 'org-mu4e) ; For mu4e stuff within org-mode
  :config
  ;; Mu4e account contexts
  (setq mu4e-contexts ; Can't be set in :custom for some reason
        `( ,(make-mu4e-context
             :name "Brown"
             :enter-func (lambda () (mu4e-message "Entering Brown context"))
             ;; :leave-func (lambda () (mu4e-message "Leaving Gmail context"))
             ;; we match based on the maildir of the message
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/Brown" (mu4e-message-field msg :maildir))))
             :vars '( ( user-mail-address . "kristoffer_balintona@brown.edu"  )
                      ( smtpmail-smtp-user . "kristoffer_balintona@brown.edu")
                      ( smtpmail-smtp-server . "smtp.gmail.com" )
                      ( user-full-name . "Kristoffer Balintona" )
                      ( mu4e-trash-folder . "/Brown/Trash/" )
                      ( mu4e-refile-folder . "/Brown/All-Mail/" )
                      ( mu4e-drafts-folder . "/Brown/Drafts/" )
                      ( mu4e-attachment-dir . "~/Documents/Emails/Brown/Attachments/" )
                      ( mu4e-compose-signature .
                        (concat
                         "⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼\n"
                         "Kind regards, \n"
                         "Kristoffer \n"))))
           ,(make-mu4e-context
             :name "Personal"
             :enter-func (lambda () (mu4e-message "Entering the personal context"))
             ;; :leave-func (lambda () (mu4e-message "Leaving Outlook context"))
             ;; we match based on the maildir of the message
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/Personal" (mu4e-message-field msg :maildir))))
             :vars '( ( user-mail-address . "krisbalintona@gmail.com" )
                      ( smtpmail-smtp-user . "krisbalintona@gmail.com" )
                      ( smtpmail-smtp-server . "smtp.gmail.com" )
                      ( user-full-name . "Kristoffer Balintona" )
                      ( mu4e-trash-folder . "/Personal/Trash/" )
                      ( mu4e-refile-folder . "/Personal/All-Mail/" )
                      ( mu4e-drafts-folder . "/Personal/Drafts/" )
                      ( mu4e-attachment-dir . "~/Documents/Emails/Personal/Attachments/" )
                      ( mu4e-compose-signature  .
                        (concat
                         "⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼\n"
                         "Kind regards, \n"
                         "Kristoffer \n"))))))

  ;; Main mode bookmarks
  (setq mu4e-bookmarks (append mu4e-bookmarks ; Can't be set in :custom for some reason
                               '(( :name "Brown inbox"
                                   :query "maildir:/Brown/Inbox"
                                   :key ?b)
                                 ( :name "Personal inbox"
                                   :query "maildir:/Personal/Inbox"
                                   :key ?h)
                                 )
                               ))
  ;; Mu4e actions
  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser)) ; View in browser

  ;; Add a column to display what email account the email belongs to.
  (add-to-list 'mu4e-header-info-custom
               '(:account
                 :name "Account"
                 :shortname "Account"
                 :help "Which account this email belongs to"
                 :function
                 (lambda (msg)
                   (let ((maildir (mu4e-message-field msg :maildir)))
                     (format "%s" (substring maildir 1 (string-match-p "/" maildir 1)))))))

  ;; Mode hooks
  (add-hook 'mu4e-view-mode-hook
            (lambda ()
              (visual-line-mode) ; Automatic line breaks when reading email
              ))
  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              (visual-line-mode)
              (visual-fill-column-mode) ; Otherwise it won't immediately center
              ))
  ;; This hook correctly modifies gmail flags on emails when they are marked.
  ;; Without it, refiling (archiving), trashing (deleting), and flagging
  ;; (starring) email won't properly result in the corresponding gmail action,
  ;; since the marks are ineffectual otherwise.
  (add-hook 'mu4e-mark-execute-pre-hook
            (defun +mu4e-gmail-fix-flags-h (mark msg)
              (pcase mark
                (`trash  (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
                (`refile (mu4e-action-retag-message msg "-\\Inbox"))
                (`flag   (mu4e-action-retag-message msg "+\\Starred"))
                (`unflag (mu4e-action-retag-message msg "-\\Starred")))))

  (kb/leader-keys
    "om" '(mu4e :which-key "Email")
    )
  )

;;;; Mu4e faces
;; Faces for mu4e
(set-face-attribute 'mu4e-header-face nil :height 140 :font "FiraCode Nerd Font") ; Non-colored header items
(set-face-attribute 'mu4e-header-highlight-face nil :height 140 :font "FiraCode Nerd Font") ; Current item
(set-face-attribute 'mu4e-flagged-face nil :height 140 :font "FiraCode Nerd Font")
(set-face-attribute 'mu4e-unread-face nil :height 140 :font "FiraCode Nerd Font") ; Unread (pink) items

;; Compose-mode - same height and font as org-mode to work well with org-mu4e-compose-org-mode
(set-face-attribute 'message-header-name nil :height 158 :font kb/variable-pitch-font)
(set-face-attribute 'message-header-cc nil :height 158 :font kb/variable-pitch-font)
(set-face-attribute 'message-header-to nil :height 158 :font kb/variable-pitch-font)
(set-face-attribute 'message-header-other nil :height 158 :font kb/variable-pitch-font)
(set-face-attribute 'message-header-xheader nil :height 158 :font kb/variable-pitch-font)
(set-face-attribute 'message-header-subject nil :height 158 :font kb/variable-pitch-font)
(set-face-attribute 'message-header-newsgroups nil :height 158 :font kb/variable-pitch-font)

;;;; Mu4e-alert
;; Email notifications for desktop and modeline
(use-package mu4e-alert
  :after (doom-modeline mu4e)
  :hook (doom-modeline-mode . (lambda ()
                                (mu4e-alert-enable-mode-line-display)
                                (mu4e-alert-enable-notifications)
                                ))
  :custom
  (mu4e-alert-interesting-mail-query ; Emails I get notified of
   (concat
    "flag:unread"
    " AND NOT flag:trashed"
    " AND NOT maildir:\"/Personal/All-Mail\""
    " AND NOT maildir:\"/Brown/All-Mail\""
    " AND NOT maildir:\"/Queue/\""))
  (mu4e-alert-email-notification-types '(subjects)) ; What is shown in the notification
  (display-time-use-mail-icon t) ; Use icon in modeline
  (display-time-mail-icon "📬") ; Icon in modeline
  :config
  ;; Format how it appears in the modeline
  (defun mu4e-alert-default-mode-line-formatter (mail-count)
    "Default formatter used to get the string to be displayed in the mode-line.
MAIL-COUNT is the count of mails for which the string is to displayed"
    (when (not (zerop mail-count))
      (concat ""
              (if (zerop mail-count)
                  ""
                (format "%d " mail-count))
              (propertize
               "Mail"
               'display (when (display-graphic-p)
                          display-time-mail-icon)
               'face display-time-mail-face
               'help-echo (concat (if (= mail-count 1)
                                      "You have an unread email"
                                    (format "You have %s unread emails" mail-count))
                                  "\nClick here to view "
                                  (if (= mail-count 1) "it" "them"))
               'mouse-face 'mode-line-highlight
               'keymap '(mode-line keymap
                                   (mouse-1 . mu4e-alert-view-unread-mails)
                                   (mouse-2 . mu4e-alert-view-unread-mails)
                                   (mouse-3 . mu4e-alert-view-unread-mails)))
              " " ; Padding
              )))
  
  (mu4e-alert-set-default-style 'libnotify) ; Library to show notifications
  )

;;;; Mu4e-views
;; Use xwidgets-webkit to view HTML emails. Emacs has to be compiled with
;; xwidget support. Check by evaluating: (xwidget-webkit-browse-url
;; "https://www.gnu.org/")
(use-package mu4e-views
  :disabled t ; Needs Emacs compiled with xwidget support
  :after mu4e
  :functions mu4e-views-mu4e-use-view-msg-method
  :custom
  (mu4e-views-completion-method 'default) ; Use selectrum for completion
  (mu4e-views-default-view-method "html") ; Make xwidgets default for viewing html
  (mu4e-views-next-previous-message-behaviour 'stick-to-current-window) ; When pressing n and p stay in the current window
  :config
  (mu4e-views-mu4e-use-view-msg-method "html") ; Select the default viewing method

  (general-define-key
   :keymaps 'mu4e-headers-mode-map
   "v" 'mu4e-views-mu4e-select-view-msg-method ; Select viewing method
   "M-n" 'mu4e-views-cursor-msg-view-window-down ; From headers window scroll the email view
   "M-p" 'mu4e-views-cursor-msg-view-window-up ; From headers window scroll the email view
   )
  )

;;;; Smtpmail
;; Sending emails with msmtp
(use-package smtpmail
  :after mu4e
  :custom
  ;; Use smtpmail to send emails
  (send-mail-function 'smtpmail-send-it)
  (message-send-mail-function 'smtpmail-send-it)
  (smtpmail-stream-type  'starttls)
  (sendmail-program "/usr/bin/msmtp")
  (smtpmail-smtp-service '587)

  ;; I'm using gmail
  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-local-domain "gmail.com")

  ;; Queuing mail
  (smtpmail-queue-mail 't) ; Queue by default
  (smtpmail-queue-dir  "~/Documents/Emails/Queue/cur")

  ;; Other
  (message-sendmail-extra-arguments '("--read-envelope-from")) ; Tell msmtp to choose the SMTP server according to the from field in the outgoing email
  (message-sendmail-f-is-evil 't)
  )

;;;; Org-msg
;; Using org-mode to compose HTML-friendly emails
(use-package org-msg
  :after mu4e
  :custom
  (org-msg-startup "inlineimages")
  (org-msg-greeting-name-limit 3)
  (org-msg-text-plain-alternative t)
  )

;;; mu4e-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'mu4e-rcp)
