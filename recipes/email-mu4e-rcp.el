;;; email-mu4e-rcp.el --- Summary
;;
;;; Commentary:
;;
;; These are packages pertinent to using the `mu4e' email client which leverages
;; the `mu' backend. Syncing from remote servers can be done via `mbsync' (from
;; the `mu' utility set).
;;
;; I also have configuration taken from Doom Emacs that pertain to Gmail
;; integration. I have marked those code snippets accordingly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'general)
(require 'keybinds-general-rcp)

;;; Mu4e
;;;; Itself
(use-package mu4e
  :elpaca nil
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
  :hook ((elpaca-after-init . (lambda () (when (daemonp) (mu4e t))))
         (window-setup . (lambda ()
                           (setq mu4e-headers-personal-mark  '("p" . " ")) ; Always set this
                           (unless (bound-and-true-p kb/mu4e-initialise-icons)
                             (setq mu4e-headers-personal-mark  '("p" . "  ")
                                   mu4e-headers-unread-mark    '("u" . "📩 ")
                                   mu4e-headers-draft-mark     '("D" . "✏️ ")
                                   mu4e-headers-flagged-mark   '("F" . "🚩 ")
                                   mu4e-headers-new-mark       '("N" . "✨ ")
                                   mu4e-headers-passed-mark    '("P" . "⮡ ")
                                   mu4e-headers-replied-mark   '("R" . "↵ ")
                                   mu4e-headers-seen-mark      '("S" . "  ")
                                   mu4e-headers-list-mark      '("s" . "🔉 ")
                                   mu4e-headers-trashed-mark   '("T" . "🗑️")
                                   mu4e-headers-attach-mark    '("a" . "📎 ")
                                   mu4e-headers-encrypted-mark '("x" . "🔑 ")
                                   mu4e-headers-signed-mark    '("s" . "🖊 ")))
                           ;; Taken from Doom Emacs
                           (setq mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶")
                                 mu4e-headers-thread-orphan-prefix '("┬>" . "┬▶ ")
                                 mu4e-headers-thread-last-child-prefix '("└>" . "╰▶")
                                 mu4e-headers-thread-child-prefix '("├>" . "├▶")
                                 mu4e-headers-thread-connection-prefix '("│" . "│ "))))
         (mu4e-mark-execute-pre . kb/mu4e-gmail-fix-flags-h)
         (dired-mode . turn-on-gnus-dired-mode) ; Attachment integration with dired
         (mu4e-view-mode . visual-fill-column-mode))
  :general
  (:keymaps 'mu4e-main-mode-map
   "q" 'bury-buffer
   "Q" 'mu4e-quit)
  (kb/open-keys
    "m" '(mu4e :wk "Mu4e"))
  ([remap compose-mail] 'mu4e-compose-new)
  (:keymaps '(mu4e-main-mode-map mu4e-headers-mode-map mu4e-view-mode-map)
   "M-U" 'mu4e-update-index-nonlazy)
  :custom
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-bookmarks
   '((:name "All inboxes" :query "maildir:/personal/Inbox OR maildir:/uni/Inbox" :key ?u)
     (:name "All drafts" :query "maildir:/personal/[Gmail].Drafts OR maildir:/uni/[Gmail].Drafts" :key ?d)
     (:name "All sent" :query "maildir:\"/personal/[Gmail].Sent Mail\" OR maildir:\"/uni/[Gmail].Sent Mail\"" :key ?s)))

  ;; Modeline and notifications
  (mu4e-modeline-support t)
  (mu4e-notification-support t)

  ;; Contexts
  (mu4e-context-policy 'pick-first)
  (mu4e-compose-context-policy 'ask-if-none)

  ;; Indexing
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-update-interval 300)
  (mu4e-index-update-error-continue t)
  ;; Speed up indexing. See 2.9.4 Speeding up indexing of the Mu4e info manual.
  ;; Also note `mu4e-update-index-nonlazy'
  (mu4e-index-cleanup t)                ; Don't do a full cleanup check?
  (mu4e-index-lazy-check nil)           ; Don't consider up-to-date dirs?
  (mu4e-index-update-in-background t)
  (mu4e-hide-index-messages t)          ; Hide "indexing" message in echo area?

  ;; Headers
  (mu4e-split-view 'horizontal)
  (mu4e-headers-visible-lines 13)
  (mu4e-use-fancy-chars t)
  (mu4e-headers-precise-alignment t) ; Reduces performance but aligns everything
  (mu4e-headers-fields
   '((:maildir . 35)
     (:from-or-to . 25)
     (:human-date . 12)
     (:flags . 10)                      ; The max width of 3 icon flags
     (:subject)))
  (mu4e-headers-actions
   '(("capture message" . mu4e-action-capture-message)
     ("browse online archive" . mu4e-action-browse-list-archive)
     ("show this thread" . mu4e-action-show-thread)
     ("kill-ring-save file path" . mu4e-action-copy-message-file-path)))
  (mu4e-maildir-initial-input "")       ; Annoying "/" by default

  ;; View
  (mu4e-view-fields
   '(:from :to :cc :bcc
     :subject :flags :date
     :maildir :mailing-list
     :tags :attachments :signature))
  (mu4e-view-scroll-to-next t)
  (shr-color-visible-luminance-min 80)   ; Better viewing for dark theme

  ;; Composing and sending
  ;; Don't save message to Sent Messages, Gmail/IMAP takes care of this
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-attachment-dir (expand-file-name ".attachments/" message-directory))
  (mu4e-compose-signature-auto-include t)
  (mu4e-compose-hidden-headers nil)
  (mu4e-compose-format-flowed t) ; Not sure, but I think this setting helps
  (mu4e-compose-dont-reply-to-self t)

  ;; Other
  (mu4e-change-filenames-when-moving t) ; Prevent duplication
  (mu4e-confirm-quit nil)
  (mu4e-headers-eldoc-format "In %m with flags %F")
  ;; The following two options have mu4e use vanilla completion (e.g. Vertico)
  (mu4e-read-option-use-builtin t)
  (mu4e-completing-read-function 'completing-read)
  :init
  ;; Gmail integration is taken from Doom
  ;; Check if msg is being called from a gmail account
  (defun kb/mu4e-msg-gmail-p (msg)
    t)                     ; Just return t for now since all my emails are gmail
  (defun kb/mu4e--mark-seen (docid _msg target)
    (mu4e--server-move docid (mu4e--mark-check-target target) "+S-u-N"))
  (defvar kb/mu4e--last-invalid-gmail-action 0)
  ;; This hook correctly modifies gmail flags on emails when they are marked.
  ;; Without it, refiling (archiving), trashing, and flagging (starring) email
  ;; won't properly result in the corresponding gmail action, since the marks
  ;; are ineffectual otherwise.
  (defun kb/mu4e-gmail-fix-flags-h (mark msg)
    (when (kb/mu4e-msg-gmail-p msg)
      (pcase mark
        (`trash  (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
        (`delete (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
        (`refile (mu4e-action-retag-message msg "-\\Inbox"))
        (`flag   (mu4e-action-retag-message msg "+\\Starred"))
        (`unflag (mu4e-action-retag-message msg "-\\Starred")))))
  :config
  ;; Modeline
  ;; Force using regular characters rather than the fancy ones
  (advice-add 'mu4e--bookmarks-modeline-item :around
                                             (lambda (orig-fun &rest args)
                                               (let ((mu4e-use-fancy-chars nil))
                                                 (apply orig-fun args))))

  ;; Headers
  ;; Taken from Doom
  (plist-put (cdr (assoc :flags mu4e-header-info)) :shortname " Flags") ; default=Flgs
  (setq mu4e-marks
        ;; Refile is identical to delete now, since GMail "archives" by removing
        ;; from the maildir (all mail is already in the "All Mail" maildir)
        '(
          ;; Refile will be my "archive" function.
          (refile :char ("r" . "▶")
                  :prompt "archive"
                  :show-target
                  (lambda (target) "archive")
                  :dyn-target
                  (lambda (_target msg) (mu4e-get-refile-folder msg))
                  :action
                  (lambda (docid msg target)
                    (if (kb/mu4e-msg-gmail-p msg)
                        (kb/mu4e--mark-seen docid msg target)
                      (mu4e--server-move docid (mu4e--mark-check-target target) "-N")))
                  #'kb/mu4e--mark-seen)
          ;; In my workflow, emails won't be moved at all. Only their
          ;; flags/labels are changed. So we redefine the trash and refile marks
          ;; not to do any moving. However, the real magic happens in
          ;; `kb/mu4e-gmail-fix-flags-h'.
          ;;
          ;; Gmail will handle the rest.
          (delete :char ("D" . "x")
                  :prompt "Delete"
                  :show-target
                  (lambda (target) "delete")
                  :action
                  (lambda (docid msg target)
                    (if (and (kb/mu4e-msg-gmail-p msg)
                             (not (file-in-directory-p (mu4e-message-field msg :path) mail-default-directory)))
                        (progn
                          (message "The delete operation is invalid for Gmail accounts. Trashing instead.")
                          (kb/mu4e--mark-seen docid msg target)
                          (when (< 2 (- (float-time) kb/mu4e--last-invalid-gmail-action))
                            (sit-for 1))
                          (setq kb/mu4e--last-invalid-gmail-action (float-time)))
                      (mu4e--server-remove docid))))
          (trash :char ("d" . "▼")
                 :prompt "trash"
                 :show-target
                 (lambda (target) "trash")
                 :dyn-target
                 (lambda (_target msg) (mu4e-get-trash-folder msg))
                 :action
                 (lambda (docid msg target)
                   (if (kb/mu4e-msg-gmail-p msg)
                       (kb/mu4e--mark-seen docid msg target)
                     (mu4e--server-move docid (mu4e--mark-check-target target) "+T-N"))))
          (label :char ("l" . "↗")
                 :prompt "label"
                 :show-target
                 (lambda (target) "labeled")
                 :ask-target mu4e--mark-get-move-target
                 :action
                 (lambda (docid msg target)
                   (if (kb/mu4e-msg-gmail-p msg)
                       (mu4e-action-retag-message msg (format "+\\Inbox,+\\%s" (f-filename target)))
                     (mu4e--server-move docid (mu4e--mark-check-target target) "-N"))))
          ;; Default commands
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
                  ("a" . "◯")
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
  ;; NOTE 2023-01-01: Have to do this for any new marks (i.e. marks with new
  ;; names, not redefinitions of extant marks) I create
  (mu4e~headers-defun-mark-for label)
  (mu4e--view-defun-mark-for label)
  (general-define-key :keymaps 'mu4e-headers-mode-map "l" 'mu4e-headers-mark-for-label)
  (general-define-key :keymaps 'mu4e-view-mode-map "l" 'mu4e-view-mark-for-label)

  ;; See 5.3 of the mu4e info manual. Also see 1.5 Display Customization of the
  ;; emacs-mime info entry
  (with-eval-after-load 'mm-decode
    (setq gnus-blocked-images (rx unmatchable) ; Don't block images
          ;; gnus-inhibit-mime-unbuttonizing t    ; Show all MIME buttons?
          mm-discouraged-alternatives '("text/html" "text/richtext" "image/.*")
          mm-automatic-display (remove "text/html" mm-automatic-display))) ; If I really don't want to see HTML

  ;; Sending and composition
  (org-msg-mode)
  (setq mu4e-compose-signature
        (unless (bound-and-true-p org-msg-mode)
          "⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼\nKind regards,\nKristoffer\n")))

;;;; Mu4e-contexts
(with-eval-after-load 'mu4e
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Uni"
            :enter-func (lambda () (mu4e-message "Entering Uni context"))
            :leave-func (lambda () (mu4e-message "Leaving Uni context"))
            :vars `((user-mail-address . "kristoffer_balintona@brown.edu")
                    ;; Directories
                    (mu4e-drafts-folder . "/drafts/uni/")
                    (mu4e-sent-folder . "/uni/[Gmail].Sent Mail")
                    (mu4e-refile-folder . "/uni/[Gmail].All Mail")
                    (mu4e-trash-folder . "/uni/[Gmail].Trash")
                    (mu4e-maildir-initial-input . "/uni/")
                    ;; Maildirs
                    (mu4e-maildir-shortcuts . ((:maildir "/uni/Inbox" :key ?i)
                                               (:maildir "/uni/[Gmail].Sent Mail" :key ?s)
                                               (:maildir "/drafts/uni/" :key ?d)
                                               (:maildir "/uni/[Gmail].Drafts" :key ?D)
                                               (:maildir "/uni/[Gmail].Trash" :key ?t)
                                               (:maildir "/uni/[Gmail].All Mail" :key ?a)))
                    (kb/signature-alist .
                                        (("Take care" . ,(format "%s\nTake care,\nKristoffer" kb/signature-separator))
                                         ("In gratitude" . ,(format "%s\nIn gratitude,\nKristoffer" kb/signature-separator))
                                         ("Best" . ,(format "%s\nBest,\nKristoffer" kb/signature-separator))
                                         ("With appreciation" . ,(format "%s\nWith appreciation,\nKristoffer" kb/signature-separator))
                                         ("Brown banner" . ,(concat kb/signature-separator "\n"
                                                                    "With appreciation,\nKristoffer\n\n"
                                                                    "#+begin_export html
<br />
<table
  style='color: rgb(136, 136, 136); border: none; border-collapse: collapse; font-family: garamond'
>
  <tbody>
    <tr style='height: 81.25pt'>
      <td
        style='
          border-right: 0.75pt dotted rgb(135, 127, 116);
          vertical-align: top;
          padding: 5pt 11pt 5pt 5pt;
        '
        title=''
      >
        <img
          src='https://clipground.com/images/brown-university-logo-png-1.png'
          alt='Brown logo'
          style='border: none'
          height='100'
        />
      </td>
      <td
        style='
          border-left: 0.75pt dotted rgb(135, 127, 116);
          vertical-align: top;
          padding: 5pt 5pt 5pt 11pt;
        '
      >
        <p
          dir='ltr'
          style='line-height: 1.38; margin-top: 6pt; margin-bottom: 0pt'
        >
          <span
            style='
              font-size: 11pt;
              font-weight: 700;
              white-space: pre-wrap;
            '
            >Kristoffer Balintona</span
          >
          <br />
        </p>
        <p
          dir='ltr'
          style='line-height: 1.38; margin-top: 0pt; margin-bottom: 0pt'
        >
          <span
            style='
              font-size: 10pt;
              vertical-align: baseline;
              white-space: pre-wrap;
            '
            >B.A. Philosophy</span
          >
          <br />
        </p>
        <p
          dir='ltr'
          style='line-height: 1.38; margin-top: 0pt; margin-bottom: 0pt'
        >
          <span
            style='
              font-size: 10pt;
              vertical-align: baseline;
              white-space: pre-wrap;
            '
            >Class of 2024</span
          >
        </p>
        <p
          dir='ltr'
          style='line-height: 1.38; margin-top: 0pt; margin-bottom: 0pt'
        >
          <span
            style='
              font-size: 10pt;
              white-space: pre-wrap;
            '
            >Tel: (773) 677-9699</span
          >
          <br />
        </p>
        <p
          dir='ltr'
          style='
            font-size: 10pt;
            line-height: 1.2;
            margin-top: 0pt;
            margin-bottom: 0pt;
          '
        >
          <span
            style='
              font-size: 10pt;
              vertical-align: baseline;
              white-space: pre-wrap;
            '
            >Box: 6327</span
          >
        </p>
        <br />
      </td>
    </tr>
  </tbody>
</table>
#+end_export"))
                                         ("BUI banner" . ,(concat kb/signature-separator "\n\n"
                                                                  "Warmly,\nBrown University Interviews Executive Committee\n\n"
                                                                  "#+begin_export html
<br />
<table
  style='
    color: rgb(136, 136, 136);
    border: none;
    border-collapse: collapse;
    font-family: garamond;
  '
>
  <tbody>
    <tr style='height: 81.25pt'>
      <td
        style='
          border-right: 0.75pt dotted rgb(135, 127, 116);
          vertical-align: top;
          padding: 5pt 11pt 5pt 5pt;
        '
        title=''
      >
        <img
          src='https://browninterviews.org/wp-content/uploads/2020/06/bu-small-logo.png'
          alt='Brown logo'
          style='border: none'
          height='70'
        />
      </td>
      <td
        style='
          border-left: 0.75pt dotted rgb(135, 127, 116);
          vertical-align: top;
          padding: 5pt 5pt 5pt 11pt;
        '
      >
        <p
          dir='ltr'
          style='margin-top: 6pt; margin-bottom: 0pt; font-size: 11pt'
        >
          <span style='font-weight: 700'>Kristoffer Balintona ('24)</span>
          <span> | Editor in Chief</span>
          <br />
        </p>
        <p
          dir='ltr'
          style='margin-top: 6pt; margin-bottom: 0pt; font-size: 11pt'
        >
          <span style='font-weight: 700'>Charles Alaimo ('25)</span>
          <span> | Senior Interviews Coordinator</span>
          <br />
        </p>
        <br />
      </td>
    </tr>
  </tbody>
</table>
#+end_export"))))
                    ;; Smtpmail
                    (smtpmail-smtp-user "kristoffer_balintona@brown.edu") ; Send from this address
                    (smtpmail-mail-address "kristoffer_balintona@brown.edu")))
          ,(make-mu4e-context
            :name "Personal"
            :enter-func (lambda () (mu4e-message "Entering Personal context"))
            :leave-func (lambda () (mu4e-message "Leaving Personal context"))
            :vars `((user-mail-address . "krisbalintona@gmail.com")
                    ;; Directories
                    (mu4e-drafts-folder . "/drafts/personal")
                    (mu4e-sent-folder . "/personal/[Gmail].Sent Mail")
                    (mu4e-refile-folder . "/personal/[Gmail].All Mail")
                    (mu4e-trash-folder . "/personal/[Gmail].Trash")
                    (mu4e-maildir-initial-input . "/personal/")
                    ;; Maildirs
                    (mu4e-maildir-shortcuts . ((:maildir "/personal/Inbox" :key ?i)
                                               (:maildir "/personal/[Gmail].Sent Mail" :key ?s)
                                               (:maildir "/drafts/personal/" :key ?d)
                                               (:maildir "/personal[Gmail].Drafts/[Gmail].Drafts" :key ?D)
                                               (:maildir "/personal/[Gmail].Trash" :key ?t)
                                               (:maildir "/personal/[Gmail].All Mail" :key ?a)))
                    (kb/signature-alist .
                                        (("Take care" . ,(format "%s\nTake care,\nKristoffer" kb/signature-separator))
                                         ("In gratitude" . ,(format "%s\nIn gratitude,\nKristoffer" kb/signature-separator))
                                         ("Best" . ,(format "%s\nBest,\nKristoffer" kb/signature-separator))
                                         ("With appreciation" . ,(format "%s\nWith appreciation,\nKristoffer" kb/signature-separator))))
                    ;; Smtpmail
                    (smtpmail-smtp-user "krisbalintona@gmail.com") ; Send from this address
                    (smtpmail-mail-address "krisbalintona@gmail.com"))))))

;;; Mu4e header icons (from Doom Emacs)
(with-eval-after-load 'mu4e
  (defun kb/mu4e--get-string-width (str)
    "Return the width in pixels of a string in the current
window's default font. If the font is mono-spaced, this
will also be the width of all other printable characters."
    (let ((window (selected-window))
          (remapping face-remapping-alist))
      (with-temp-buffer
        (make-local-variable 'face-remapping-alist)
        (setq face-remapping-alist remapping)
        (set-window-buffer window (current-buffer))
        (insert str)
        (car (window-text-pixel-size)))))

  (cl-defun kb/mu4e-normalised-icon (name &key set color height v-adjust)
    "Convert :icon declaration to icon"
    (let* ((icon-set (intern (concat "all-the-icons-" (or set "faicon"))))
           (v-adjust (or v-adjust 0.02))
           (height (or height 0.8))
           (icon (if color
                     (apply icon-set `(,name :face ,(intern (concat "all-the-icons-" color)) :height ,height :v-adjust ,v-adjust))
                   (apply icon-set `(,name  :height ,height :v-adjust ,v-adjust))))
           (icon-width (kb/mu4e--get-string-width icon))
           (space-width (kb/mu4e--get-string-width " "))
           (space-factor (- 2 (/ (float icon-width) space-width))))
      (concat (propertize " " 'display `(space . (:width ,space-factor))) icon)))

  ;; FIXME 2022-12-29: Icons are too large...
  (defun kb/mu4e-initialise-icons ()
    (setq mu4e-headers-draft-mark      (cons "D" (kb/mu4e-normalised-icon "pencil"))
          mu4e-headers-flagged-mark    (cons "F" (kb/mu4e-normalised-icon "flag"))
          ;; mu4e-headers-new-mark        (cons "N" (kb/mu4e-normalised-icon "sync" :set "material" :height 0.8 :v-adjust -0.10))
          mu4e-headers-passed-mark     (cons "P" (kb/mu4e-normalised-icon "arrow-right"))
          mu4e-headers-replied-mark    (cons "R" (kb/mu4e-normalised-icon "arrow-right"))
          mu4e-headers-seen-mark       (cons "S" "") ;(kb/mu4e-normalised-icon "eye" :height 0.6 :v-adjust 0.07 :color "dsilver"))
          mu4e-headers-trashed-mark    (cons "T" (kb/mu4e-normalised-icon "trash"))
          mu4e-headers-attach-mark     (cons "a" (kb/mu4e-normalised-icon "file-text-o" :color "silver"))
          mu4e-headers-encrypted-mark  (cons "x" (kb/mu4e-normalised-icon "lock"))
          mu4e-headers-signed-mark     (cons "s" (kb/mu4e-normalised-icon "certificate" :height 0.7 :color "dpurple"))
          mu4e-headers-unread-mark     (cons "u" (kb/mu4e-normalised-icon "eye-slash" :v-adjust 0.05))
          ;; Mine
          mu4e-headers-new-mark        (cons "N" (kb/mu4e-normalised-icon "plus" :height 0.4))
          mu4e-headers-list-mark       (cons "s" (kb/mu4e-normalised-icon "repeat" :height 0.5))))

  ;; (add-hook 'window-setup-hook #'kb/mu4e-initialise-icons)
  )

;;; Mu4e-column-faces
(use-package mu4e-column-faces
  :after mu4e
  :init
  (mu4e-column-faces-mode))

;;; Mu4e-views
(use-package mu4e-views
  :disabled
  :after mu4e
  :ensure-system-package wkhtmltopdf    ; HTML to PDF CLI command
  ;; This branch for support of new version of `mu'
  :elpaca (mu4e-views :type git :host github :repo "lordpretzel/mu4e-views" :branch "mu-1.8-support")
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
