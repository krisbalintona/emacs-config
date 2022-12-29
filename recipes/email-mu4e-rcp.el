;;; email-mu4e-rcp.el --- Summary
;;
;;; Commentary:
;;
;; These are packages pertinent to using the `mu4e' email client which leverages
;; the `mu' backend. Syncing from remote servers can be done via `mbsync' (from
;; the `mu' utility set).
;;
;; Protesilaos provides a wonderful guide here:
;; https://protesilaos.com/emacs/dotemacs#h:5ad80664-3163-4d9d-be65-462637d77903
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'general)
(require 'keybinds-general-rcp)

;;; Mu4e
(use-package mu4e
  :general
  (kb/open-keys
    "M" '(mu4e :wk "Mu4e"))
  ([remap compose-mail] 'mu4e-compose-new)
  :custom
  (mail-user-agent 'mu4e-user-agent)

  ;; Contexts
  (mu4e-contexts
   `(,(make-mu4e-context
       :name "Personal"
       :enter-func (lambda () (mu4e-message "Entering Personal context"))
       :leave-func (lambda () (mu4e-message "Leaving Personal context"))
       :match-func (lambda (msg)
                     (when msg
                       (mu4e-message-contact-field-matches msg :to "krisbalintona@gmail.com")))
       :vars '((user-mail-address . "krisbalintona@gmail.com")
               ;; Directories
               (mu4e-drafts-folder . "/personal/[Gmail].Drafts")
               (mu4e-sent-folder . "/personal/[Gmail].Sent Mail")
               (mu4e-refile-folder . "/personal/[Gmail].All Mail")
               (mu4e-trash-folder . "/personal/[Gmail].Trash")
               ;; Bookmarks
               (mu4e-bookmarks . ((:name "Inbox unread" :query "flag:unread AND maildir:/personal/Inbox" :key ?u)
                                  (:name "Sent" :query "maildir:\"/personal/[Gmail].Sent Mail\"" :key ?s)
                                  (:name "Drafts" :query "maildir:/personal/[Gmail].Drafts" :key ?d)
                                  (:name "All mail" :query "maildir:\"/personal/[Gmail].All Mail\"" :key ?a)
                                  ))
               ;; Maildirs
               (mu4e-maildir-shortcuts . ((:maildir "/personal/Inbox" :key ?i)
                                          (:maildir "/personal/[Gmail].Sent Mail" :key ?s)
                                          (:maildir "/personal/[Gmail].Trash" :key ?t)
                                          (:maildir "/personal/[Gmail].All Mail" :key ?a)
                                          ))
               (mu4e-compose-signature . "⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼\nKind regards,\nKristoffer\n")))
     ,(make-mu4e-context
       :name "Uni"
       :enter-func (lambda () (mu4e-message "Entering Uni context"))
       :leave-func (lambda () (mu4e-message "Leaving Uni context"))
       :match-func (lambda (msg)
                     (when msg
                       (mu4e-message-contact-field-matches msg :to "kristoffer_balintona@brown.edu")))
       :vars '((user-mail-address . "kristoffer_balintona@brown.edu")
               ;; Directories
               (mu4e-drafts-folder . "/uni/[Gmail].Drafts")
               (mu4e-sent-folder . "/uni/[Gmail].Sent Mail")
               (mu4e-refile-folder . "/uni/[Gmail].All Mail")
               (mu4e-trash-folder . "/uni/[Gmail].Trash")
               ;; Bookmarks
               (mu4e-bookmarks . ((:name "Inbox unread" :query "flag:unread AND maildir:/uni/Inbox" :key ?u)
                                  (:name "Sent" :query "maildir:\"/uni/[Gmail].Sent Mail\"" :key ?s)
                                  (:name "Drafts" :query "maildir:/uni/[Gmail].Drafts" :key ?d)
                                  (:name "All mail" :query "maildir:\"/uni/[Gmail].All Mail\"" :key ?a)
                                  ))
               ;; Maildirs
               (mu4e-maildir-shortcuts . ((:maildir "/uni/Inbox" :key ?i)
                                          (:maildir "/uni/[Gmail].Sent Mail" :key ?s)
                                          (:maildir "/uni/[Gmail].Trash" :key ?t)
                                          (:maildir "/uni/[Gmail].All Mail" :key ?a)
                                          ))
               (mu4e-compose-signature . "⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼\nKind regards,\nKristoffer\n")))))

  ;; Indexing
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-update-interval 300)
  (mu4e-index-update-error-continue t)
  ;; Speed up indexing. See 2.9.4 Speeding up indexing of the Mu4e info manual.
  ;; Also note `mu4e-update-index-nonlazy'
  ;; (mu4e-index-cleanup nil)              ; Don't do a full cleanup check
  ;; (mu4e-index-lazy-check t)             ; Don't consider up-to-date dirs

  ;; Headers
  (mu4e-split-view 'horizontal)
  (mu4e-headers-visible-lines 13)
  (mu4e-use-fancy-chars t)
  (mu4e-headers-personal-mark  '("p" . " "))
  (mu4e-headers-unread-mark    '("u" . "📩 "))
  (mu4e-headers-draft-mark     '("D" . "🚧 "))
  (mu4e-headers-flagged-mark   '("F" . "🚩 "))
  (mu4e-headers-new-mark       '("N" . "✨ "))
  (mu4e-headers-passed-mark    '("P" . "↪ "))
  (mu4e-headers-replied-mark   '("R" . "↩ "))
  (mu4e-headers-seen-mark      '("S" . " "))
  (mu4e-headers-trashed-mark   '("T" . "🗑️"))
  (mu4e-headers-attach-mark    '("a" . "📎 "))
  (mu4e-headers-encrypted-mark '("x" . "🔑 "))
  (mu4e-headers-signed-mark    '("s" . "🖊 "))

  ;; View
  ;; Also see 5.3 of the mu4e info manual
  (mu4e-view-scroll-to-next t)
  (gnus-unbuttonized-mime-types nil) ; Visible buttons for email's type (e.g. plain, html)
  (shr-color-visible-luminance-min 80)  ; Better viewing for dark theme
  ;; Show inline images and inhibit loading of external ones. See
  ;; https://github.com/djcb/mu/issues/1434#issuecomment-579412427
  (gnus-blocked-images "http")

  ;; Sending
  ;; Don't save message to Sent Messages, Gmail/IMAP takes care of this
  (mu4e-sent-messages-behavior 'delete)
  ;; (mu4e-compose-format-flowed t)        ; Not sure if needed yet
  (mu4e-compose-signature-auto-include nil)

  ;; Other
  (mu4e-completing-read-function 'completing-read)
  (mu4e-change-filenames-when-moving t) ; Prevent duplication
  :config
  (org-msg-mode)

  ;; View
  ;; Discourage viewing messages in html or richtext. See 5.3 of the mu4e
  ;; info manual
  (with-eval-after-load 'mm-decode
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext")))

;;; Mu4e-column-faces
(use-package mu4e-column-faces
  :after mu4e
  :init
  (mu4e-column-faces-mode))

;;; Mu4e-views
(use-package mu4e-views
  :after mu4e
  :ensure-system-package wkhtmltopdf    ; HTML to PDF CLI command
  ;; This branch for support of new version of `mu'
  :straight (mu4e-views :type git :host github :repo "lordpretzel/mu4e-views" :branch "mu-1.8-support")
  :general
  (:keymaps 'mu4e-headers-mode-map
            "v" 'mu4e-views-mu4e-select-view-msg-method ; Select viewing method
            "M-n" 'mu4e-views-cursor-msg-view-window-down ; From headers window scroll the email view
            "M-p" 'mu4e-views-cursor-msg-view-window-up ; From headers window scroll the email view
            "f" 'mu4e-views-toggle-auto-view-selected-message ; Toggle opening messages automatically when moving in the headers view
            "i" 'mu4e-views-mu4e-view-as-nonblocked-html) ; Show currently selected email with all remote content
  :custom
  (mu4e-views-auto-view-selected-message nil)
  (mu4e-views-next-previous-message-behaviour 'stick-to-current-window)
  (mu4e-views-default-view-method "html-nonblock")
  (mu4e-views-html-to-pdf-command "wkhtmltopdf %h %p"))

;;; email-mu4e-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'email-mu4e-rcp)
