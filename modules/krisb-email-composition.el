;;; Composition
;;;; Message
(use-package message
  :ensure nil
  :commands compose-mail
  :hook ((message-setup . message-sort-headers)
         ;; I like to use prose linters. See my flymake and flymake-collection
         ;; configurations that leverage vale
         (message-mode . flymake-mode)
         (message-mode . olivetti-mode)
         (message-mode . mixed-pitch-mode)
         (message-send . krisb-message-check-subject)
         (message-send . krisb-message-check-from))
  :custom
  (message-directory krisb-email-directory)
  (message-mail-user-agent t)           ; Use `mail-user-agent'
  (compose-mail-user-agent-warnings t)
  (message-kill-buffer-on-exit t)
  (message-elide-ellipsis "> [... %l lines elided]\n")
  (message-confirm-send nil)

  ;; Headers
  (message-hidden-headers nil)
  (message-ignored-cited-headers ".") ; Don't include any headers when citing emails
  ;; Generates all headers in the variables `message-required-news-headers' or
  ;; `message-required-mail-headers'. Otherwise, unless another package manually
  ;; adds headers (e.g. mu4e), those headers won't be inserted into a message
  ;; draft buffer. I enable this to make sure that the date header is inserted
  ;; in a draft. (No date header means the date is set to time 0, which is
  ;; annoying for querying emails via e.g. notmuch.)
  (message-generate-headers-first t)

  ;; Signatures
  (message-signature-insert-empty-line t)
  (message-signature "Kind regards,\nKristoffer\n")
  (message-signature-separator "^-- *$")

  ;; Citations. See e.g. `message-cite-style-gmail' for the options relevant to
  ;; citations. Importantly, I can set these options buffer locally.
  (message-cite-function 'message-cite-original-without-signature)
  (message-citation-line-function 'message-insert-formatted-citation-line)
  (message-citation-line-format "On %a, %b %d %Y, %N wrote:\n")
  (message-cite-reply-position 'below)

  ;; Replying
  (message-wide-reply-confirm-recipients t)

  ;; Forwarding
  (message-forward-as-mime t)           ; NOTE 2024-09-27: Experimental
  (message-forward-before-signature nil)

  ;; Attachments
  (mml-attach-file-at-the-end t)
  (mml-dnd-attach-options t)
  :config
  (krisb-modus-themes-setup-faces
   "message"
   (set-face-attribute 'message-mml nil :weight 'bold :background bg-sage))

  (with-eval-after-load 'mu4e
    (setq mu4e-attachment-dir (expand-file-name ".attachments/" message-directory)))

  ;; Taken from Doom. Detect empty subjects, and give users an opportunity to
  ;; fill something in
  (defun krisb-message-check-subject ()
    "Check that a subject is present, and prompt for a subject if not."
    (save-excursion
      (goto-char (point-min))
      (search-forward "--text follows this line--")
      (re-search-backward "^Subject:")
      (let ((subject (string-trim (substring (thing-at-point 'line) 8))))
        (when (string-empty-p subject)
          (end-of-line)
          (insert (read-string "Subject (optional): "))))))

  (defun krisb-message-check-from ()
    "Prompt user to confirm sending from this email."
    (save-excursion
      (goto-char (point-min))
      (search-forward "--text follows this line--")
      (re-search-backward "^From:")
      (let ((from (string-trim (substring (thing-at-point 'line) 5))))
        (when (and (not (string-match-p (rx (literal user-mail-address)) from))
                   (not (yes-or-no-p (concat
                                      "Are you sure you want to send from "
                                      (propertize from 'face 'highlight)
                                      "?"))))
          (cl--set-buffer-substring (pos-bol) (pos-eol)
                                    (concat
                                     "From: "
                                     (read-string "Set FROM to: " user-mail-address))))))))

;;;; Krisb-email-composition-ext
(use-package krisb-email-composition-ext
  :ensure nil
  :after message
  :custom
  (message-signature-separator (format "^%s *$" (read krisb-signature-separator)))
  (message-signature #'krisb-signature-select)
  :config
  (with-eval-after-load 'mu4e
    (setq mu4e-contexts
          `(,(make-mu4e-context
              :name "Uni"
              :enter-func (lambda () (mu4e-message "Entering Uni context"))
              :leave-func (lambda () (mu4e-message "Leaving Uni context"))
              :vars `((user-mail-address . "kristoffer_balintona@alumni.brown.edu")
                      ;; Directories
                      (mu4e-drafts-folder . "/drafts/uni/")
                      (mu4e-sent-folder . "/uni/[Gmail].Sent Mail")
                      (mu4e-refile-folder . "/uni/[Gmail].All Mail")
                      (mu4e-trash-folder . "/uni/[Gmail].Trash")
                      (mu4e-maildir-initial-input . "/uni/ ")
                      ;; Maildirs
                      (mu4e-maildir-shortcuts . ((:maildir "/uni/Inbox" :key ?i)
                                                 (:maildir "/uni/[Gmail].Sent Mail" :key ?s)
                                                 (:maildir "/drafts/uni/" :key ?d)
                                                 (:maildir "/uni/[Gmail].Drafts" :key ?D)
                                                 (:maildir "/uni/[Gmail].Trash" :key ?t)
                                                 (:maildir "/uni/[Gmail].All Mail" :key ?a)))
                      (krisb-signature-alist .
                                             (("Take care" . "Take care,\nKristoffer")
                                              ("In gratitude" . "In gratitude,\nKristoffer")
                                              ("Best" . "Best,\nKristoffer")
                                              ("With appreciation" . "With appreciation,\nKristoffer")
                                              ("Brown banner" . "\nWith appreciation,\nKristoffer\n\n#+begin_export html
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
#+end_export")
                                              ("BUI banner" . "\n\nWarmly,\nBrown University Interviews Executive Committee\n\n#+begin_export html
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
        <p
          dir='ltr'
          style='margin-top: 6pt; margin-bottom: 0pt; font-size: 11pt'
        >
          <span style='font-weight: 700'>Dana Toneva ('24)</span>
          <span> | Senior Editor</span>
          <br />
        </p>
        <p
          dir='ltr'
          style='margin-top: 6pt; margin-bottom: 0pt; font-size: 11pt'
        >
          <span style='font-weight: 700'>Riley Stevenson ('27)</span>
          <span> | Senior Editor</span>
          <br />
        </p>
        <br />
      </td>
    </tr>
  </tbody>
</table>
#+end_export")))
                      ;; Smtpmail
                      (smtpmail-smtp-user "kristoffer_balintona@alumni.brown.edu") ; Send from this address
                      (smtpmail-mail-address "kristoffer_balintona@alumni.brown.edu")))
            ,(make-mu4e-context

              :enter-func (lambda () (mu4e-message "Entering Personal context"))
              :leave-func (lambda () (mu4e-message "Leaving Personal context"))
              :vars `((user-mail-address . "krisbalintona@gmail.com")
                      ;; Directories
                      (mu4e-drafts-folder . "/drafts/personal")
                      (mu4e-sent-folder . "/personal/[Gmail].Sent Mail")
                      (mu4e-refile-folder . "/personal/[Gmail].All Mail")
                      (mu4e-trash-folder . "/personal/[Gmail].Trash")
                      (mu4e-maildir-initial-input . "/personal/ ")
                      ;; Maildirs
                      (mu4e-maildir-shortcuts . ((:maildir "/personal/Inbox" :key ?i)
                                                 (:maildir "/personal/[Gmail].Sent Mail" :key ?s)
                                                 (:maildir "/drafts/personal/" :key ?d)
                                                 (:maildir "/personal[Gmail].Drafts/[Gmail].Drafts" :key ?D)
                                                 (:maildir "/personal/[Gmail].Trash" :key ?t)
                                                 (:maildir "/personal/[Gmail].All Mail" :key ?a)))
                      (krisb-signature-alist .
                                             (("Take care" . "Take care,\nKristoffer")
                                              ("In gratitude" . "In gratitude,\nKristoffer")
                                              ("Best" . "Best,\nKristoffer")
                                              ("With appreciation" . "With appreciation,\nKristoffer")))
                      ;; Smtpmail
                      (smtpmail-smtp-user "krisbalintona@gmail.com") ; Send from this address
                      (smtpmail-mail-address "krisbalintona@gmail.com")))))))

;;;; Footnote
;; Footnotes for `message-mode'
(use-package footnote
  :ensure nil
  :hook (message-mode . footnote-mode)
  :custom
  (footnote-mode-line-string "")
  (footnote-section-tag "Footnotes:")
  (footnote-spaced-footnotes nil)
  (footnote-prompt-before-deletion nil))

;;;; Org-mime
(use-package org-mime
  :pin melpa
  :after message
  :hook ((message-send . org-mime-confirm-when-no-multipart)
         (org-mime-html . (lambda ()
                            "Nicely offset block quotes in email bodies.
Taken from
https://github.com/org-mime/org-mime?tab=readme-ov-file#css-style-customization."
                            (org-mime-change-element-style
                             "blockquote" "border-left: 2px solid gray; padding-left: 4px;"))))
  :bind ( :map message-mode-map
          ("C-c M-o" . org-mime-htmlize)
          ("C-c '" . org-mime-edit-mail-in-org-mode))
  :custom
  (org-mime-library 'mml)               ; For gnus
  (org-mime-export-ascii 'ascii)
  (org-mime-preserve-breaks nil)
  ;; Keep GPG signatures outside of multipart. Modified version of
  ;; https://github.com/org-mime/org-mime?tab=readme-ov-file#keep-gpg-signatures-outside-of-multipart
  (org-mime-find-html-start
   (lambda (start)
     (save-excursion
       (goto-char start)
       (if (search-forward "<#secure method=pgpmime mode=sign>" nil t)
           (1+ (point))
         start))))
  (org-mime-debug nil)
  :config
  (defun krisb-org-mime--remove-spacer ()
    "Remove the \"spacer\" above the line at point.
A spacer is two newlines inserted after portions inserted by
`org-mime-htmlize'."
    (save-excursion
      (previous-logical-line)
      (delete-blank-lines)))
  (advice-add 'org-mime-htmlize :after #'krisb-org-mime--remove-spacer)
  :config
  ;; FIXME 2024-10-07: For some reason, setting these in :custom doesn't work...
  (setq org-mime-src--hint "# org-mime hint: Press C-c C-c to commit change.\n" ; Start with a single # to font-lock as comment
        org-mime-export-options '( :with-latex t
                                   :section-numbers nil
                                   :with-author nil
                                   :with-toc nil))

  ;; Pop buffer according to `display-buffer-alist'
  (defun krisb-org-mime-edit-mail-in-org-mode ()
    "Call a special editor to edit the mail body in `org-mode'."
    (interactive)
    ;; see `org-src--edit-element'
    (cond
     ((eq major-mode 'org-mode)
      (message "This command is not for `org-mode'."))
     (t
      (setq org-mime--saved-temp-window-config (current-window-configuration))
      (let* ((beg (copy-marker (org-mime-mail-body-begin)))
             (end (copy-marker (or (org-mime-mail-signature-begin) (point-max))))
             (bufname "OrgMimeMailBody")
             (buffer (generate-new-buffer bufname))
             (overlay (org-mime-src--make-source-overlay beg end))
             (text (buffer-substring-no-properties beg end)))

        (setq org-mime-src--beg-marker beg)
        (setq org-mime-src--end-marker end)
        ;; don't use local-variable because only user can't edit multiple emails
        ;; or multiple embedded org code in one mail
        (setq org-mime-src--overlay overlay)

        (with-current-buffer buffer
          (erase-buffer)
          (insert org-mime-src--hint)
          (insert text)
          (goto-char (point-min))
          (org-mode)
          (org-mime-src-mode)
          (while (org-at-comment-p)
            (forward-line 1)))
        (display-buffer buffer)))))
  (advice-add 'org-mime-edit-mail-in-org-mode :override #'krisb-org-mime-edit-mail-in-org-mode))

;;; Mail Transfer Agent (email sending)

;;;; Sendmail
;; Use `sendmail' program to send emails? If yes, send the value of
;; `send-mail-function' to `sendmail-send-it'
(use-package sendmail
  :ensure nil
  :after message
  :custom
  (mail-default-directory (expand-file-name "drafts/" message-directory))
  ;; These two messages make sure that emails are sent from the email address
  ;; specified in the "from" header field! Taken from
  ;; https://jonathanchu.is/posts/emacs-notmuch-isync-msmtp-setup/
  (mail-specify-envelope-from t)
  (message-sendmail-envelope-from 'header)
  (mail-envelope-from 'header))

;;;; Smtpmail
;; Use `msmtp' program to send emails? If yes, set the value of
;; `send-mail-function' to `smtpmail-send-it'
(use-package smtpmail
  :ensure nil
  :ensure-system-package msmtp
  :after message
  :custom
  (smtpmail-queue-mail nil)
  ;; Below are settings for Gmail. See
  ;; https://support.google.com/mail/answer/7126229?hl=en#zippy=%2Cstep-change-smtp-other-settings-in-your-email-client
  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  (smtpmail-stream-type 'starttls)
  ;; Make sure email details that are used are not the current (when flushing)
  ;; variables, but the variables used when writing the email
  (smtpmail-store-queue-variables t)
  (smtpmail-queue-dir (expand-file-name "drafts/.smtp-queue" message-directory))
  (smtpmail-servers-requiring-authorization "gmail")) ; NOTE 2024-08-25: Fixes Gmail's 530 error on sending

;;; Provide
(provide 'krisb-email-composition)
