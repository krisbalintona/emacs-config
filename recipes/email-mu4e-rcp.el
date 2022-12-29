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
  :hook (window-setup . (lambda ()        ; Won't take effect until window is loaded
                          (setq mu4e-headers-personal-mark  '("p" . " ")
                                mu4e-headers-unread-mark    '("u" . "📩 ")
                                mu4e-headers-draft-mark     '("D" . "🖎 ")
                                mu4e-headers-flagged-mark   '("F" . "🚩 ")
                                mu4e-headers-new-mark       '("N" . "✨ ")
                                mu4e-headers-passed-mark    '("P" . "↪ ")
                                mu4e-headers-replied-mark   '("R" . "↩ ")
                                mu4e-headers-seen-mark      '("S" . " ")
                                mu4e-headers-trashed-mark   '("T" . "🗑️")
                                mu4e-headers-attach-mark    '("a" . "📎 ")
                                mu4e-headers-encrypted-mark '("x" . "🔑 ")
                                mu4e-headers-signed-mark    '("s" . "🖊 "))))
  :general
  (kb/open-keys
    "M" '(mu4e :wk "Mu4e"))
  ([remap compose-mail] 'mu4e-compose-new)
  :custom
  (mail-user-agent 'mu4e-user-agent)

  ;; Contexts
  (mu4e-context-policy 'ask-if-none)
  (mu4e-compose-context-policy 'ask)
  (mu4e-contexts
   `(,(make-mu4e-context
       :name "Personal"
       :enter-func (lambda () (mu4e-message "Entering Personal context"))
       :leave-func (lambda () (mu4e-message "Leaving Personal context"))
       :match-func (lambda (msg)
                     (when msg
                       (string-prefix-p "/personal" (mu4e-message-field msg :maildir))))
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
                                          ))))
     ,(make-mu4e-context
       :name "Uni"
       :enter-func (lambda () (mu4e-message "Entering Uni context"))
       :leave-func (lambda () (mu4e-message "Leaving Uni context"))
       :match-func (lambda (msg)
                     (when msg
                       (string-prefix-p "/uni" (mu4e-message-field msg :maildir))))
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
                                          ))))))

  ;; Indexing
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-update-interval 300)
  (mu4e-index-update-error-continue t)
  ;; Speed up indexing. See 2.9.4 Speeding up indexing of the Mu4e info manual.
  ;; Also note `mu4e-update-index-nonlazy'
  ;; (mu4e-index-cleanup nil)              ; Don't do a full cleanup check
  ;; (mu4e-index-lazy-check t)             ; Don't consider up-to-date dirs
  (mu4e-index-update-in-background t)
  (mu4e-hide-index-messages nil)        ; Hide "indexing" message in minibuffer?

  ;; Headers
  (mu4e-split-view 'horizontal)
  (mu4e-headers-visible-lines 13)
  (mu4e-use-fancy-chars t)
  (mu4e-headers-fields
   '((:from . 25)
     (:flags . 8)
     (:subject . 83)
     (:human-date . 13)
     (:account . 13)))
  (mu4e-marks
   ;; Refile is identical to delete now, since GMail "archives" by removing from
   ;; the maildir (all mail is already in the "All Mail" maildir)
   '((refile :char
             ("a" . "▶")
             :prompt "archive"
             :show-target (lambda (target)
                            "archive")
             :action (lambda (docid msg target)
                       (mu4e--server-remove docid)))
     (delete :char
             ("D" . "x")
             :prompt "Delete" :show-target
             (lambda
               (target)
               "delete")
             :action
             (lambda
               (docid msg target)
               (mu4e--server-remove docid)))
     (flag :char
           ("+" . "✚")
           :prompt "+flag" :show-target
           (lambda
             (target)
             "flag")
           :action
           (lambda
             (docid msg target)
             (mu4e--server-move docid nil "+F-u-N")))
     (move :char
           ("m" . "▷")
           :prompt "move" :ask-target mu4e--mark-get-move-target :action
           (lambda
             (docid msg target)
             (mu4e--server-move docid
                                (mu4e--mark-check-target target)
                                "-N")))
     (read :char
           ("!" . "◼")
           :prompt "!read" :show-target
           (lambda
             (target)
             "read")
           :action
           (lambda
             (docid msg target)
             (mu4e--server-move docid nil "+S-u-N")))
     ;; Prevent mu4e from permanently deleting trashed items This snippet was
     ;; taken from the following article:
     ;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
     (trash :char ("d" . "▼")
            :prompt "dtrash"
            :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
            :action (lambda (docid msg target)
                      (mu4e--server-move docid
                                         (mu4e--mark-check-target target) "-N")))
     (unflag :char
             ("-" . "➖")
             :prompt "-unflag" :show-target
             (lambda
               (target)
               "unflag")
             :action
             (lambda
               (docid msg target)
               (mu4e--server-move docid nil "-F-N")))
     (untrash :char
              ("=" . "▲")
              :prompt "=untrash" :show-target
              (lambda
                (target)
                "untrash")
              :action
              (lambda
                (docid msg target)
                (mu4e--server-move docid nil "-T")))
     (unread :char
             ("?" . "◻")
             :prompt "?unread" :show-target
             (lambda
               (target)
               "unread")
             :action
         (lambda
               (docid msg target)
               (mu4e--server-move docid nil "-S+u-N")))
     (unmark :char " " :prompt "unmark" :action
             (mu4e-error "No action for unmarking"))
     (action :char
             ("A" . "◯")
             :prompt "action" :ask-target
             (lambda nil
               (mu4e-read-option "Action: " mu4e-headers-actions))
             :action
             (lambda
               (docid msg actionfunc)
               (save-excursion
                 (when
                     (mu4e~headers-goto-docid docid)
                   (mu4e-headers-action actionfunc)))))
     (something :char
                ("*" . "✱")
                :prompt "*something" :action
                (mu4e-error "No action for deferred mark"))))

  ;; View
  (mu4e-view-fields
   '(:from :to :cc :bcc
           :subject :flags :date
           :maildir :mailing-list
           :tags :attachments :signature))
  (mu4e-view-scroll-to-next t)
  ;; Also see 5.3 of the mu4e info manual
  (gnus-unbuttonized-mime-types nil) ; Visible buttons for email's type (e.g. plain, html)
  (shr-color-visible-luminance-min 80)  ; Better viewing for dark theme
  ;; Show inline images and inhibit loading of external ones. See
  ;; https://github.com/djcb/mu/issues/1434#issuecomment-579412427
  (gnus-blocked-images "http")

  ;; Composing and sending
  ;; Don't save message to Sent Messages, Gmail/IMAP takes care of this
  (mu4e-sent-messages-behavior 'delete)
  ;; (mu4e-compose-format-flowed t)        ; Not sure if needed yet
  (mu4e-compose-signature-auto-include nil)
  (mu4e-attachment-dir (expand-file-name "attachments" (expand-file-name "attachments" message-directory)))

  ;; Other
  (mu4e-completing-read-function 'completing-read)
  (mu4e-change-filenames-when-moving t) ; Prevent duplication
  (mu4e-confirm-quit nil)

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
  (mu4e-views-default-view-method "html")
  (mu4e-views-html-to-pdf-command "wkhtmltopdf %h %p")
  :config
  (mu4e-views-mu4e-use-view-msg-method "html"))

;;; email-mu4e-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'email-mu4e-rcp)
