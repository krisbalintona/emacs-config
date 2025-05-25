;; -*- lexical-binding: t; -*-

;;; Composition

;;;; Krisb-email-composition-ext
(use-package krisb-email-composition-ext
  :ensure nil
  :after message
  :custom
  (message-signature #'krisb-signature-select)
  :config
  ;; TODO 2025-04-03: For some reason using `krisb-signature-separator' in
  ;; :custom causes a startup warning about the variable not being
  ;; defined... I'm not sure why, so a workaround is to set
  ;; `message-signature-separator' in :config.
  (setopt message-signature-separator (format "^%s *$" (read krisb-signature-separator)))

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
  (el-patch-defun org-mime-edit-mail-in-org-mode ()
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

        (el-patch-swap
          (save-excursion
            (delete-other-windows)
            (org-switch-to-buffer-other-window buffer)
            (erase-buffer)
            (insert org-mime-src--hint)
            (insert text)
            (goto-char (point-min))
            (org-mode)
            (org-mime-src-mode))
          (with-current-buffer buffer
            (erase-buffer)
            (insert org-mime-src--hint)
            (insert text)
            (goto-char (point-min))
            (org-mode)
            (org-mime-src-mode)
            (while (org-at-comment-p)
              (forward-line 1))))
        (el-patch-add (display-buffer buffer)))))))

;;; Provide
(provide 'krisb-email-composition)
