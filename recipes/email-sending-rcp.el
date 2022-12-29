;;; email-sending-rcp.el --- Summary
;;
;;; Commentary:
;;
;; This is configuration pertinent to sending emails.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package)
(require 'keybinds-general-rcp)

;;; Message
(use-package message
  :straight nil
  :hook ((message-setup . message-sort-headers)
         (message-mode . visual-fill-column-mode))
  :custom
  (message-directory "~/Documents/emails/")
  (message-mail-user-agent t)           ; Use `mail-user-agent'
  (compose-mail-user-agent-warnings t)

  (message-elide-ellipsis "\n> [... %l lines elided]\n")
  (message-signature "⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼\nKind regards,\nKristoffer\n")
  (message-signature-insert-empty-line t)
  (message-citation-line-function #'message-insert-formatted-citation-line)
  (message-citation-line-format (concat "> From: %f\n"
                                        "> Date: %a, %e %b %Y %T %z\n"
                                        ">"))
  (message-ignored-cited-headers nil)   ; Default is "." for all headers
  (message-confirm-send nil)
  (message-kill-buffer-on-exit t)
  (message-wide-reply-confirm-recipients t)
  (message-sendmail-envelope-from 'header)

  ;; TODO 2022-12-26: Revisit these two variables
  ;; (message-sendmail-extra-arguments '("--read-envelope-from")) ; Tell msmtp to choose the SMTP server according to the from field in the outgoing email
  ;; (message-sendmail-f-is-evil 't)
  )

;;; Sendmail
;; Use `sendmail' program to send emails?
(use-package sendmail
  :custom
  ;; If I want to use `sendmail' over `msmtp'/`smtpmail'
  (send-mail-function 'sendmail-send-it)
  (sendmail-program (executable-find "sendmail"))

  (mail-specify-envelope-from t)
  (mail-header-separator (purecopy "*****")))

;;; Smtpmail
;; Use `msmtp' program to send emails?
(use-package smtpmail
  :ensure-system-package (msmtp)
  :custom
  (send-mail-function 'smtpmail-send-it)

  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  (smtpmail-stream-type 'starttls)
  (smtpmail-queue-mail nil)
  (smtpmail-queue-dir  "~/Documents/emails/smtp-queue/"))

;;; Org-msg
;;;; Itself
;; Using org-mode to compose HTML-friendly emails
(use-package org-msg
  :straight (org-msg :type git :host github :repo "jeremy-compostella/org-msg")
  :hook (org-msg-edit-mode . (lambda ()
                               (setq-local org-download-method 'directory
                                           org-download-image-dir (expand-file-name "attachments" message-directory))))
  :custom
  (org-msg-options "html-postamble:nil toc:nil author:nil email:nil")
  (org-msg-startup "hidestars indent inlineimages hideblocks")
  (org-msg-greeting-fmt nil)
  (org-msg-greeting-name-limit 1)
  (org-msg-default-alternatives
   '((new . (text html))
     (reply-to-html . (text html))
     (reply-to-text . (text))))
  (org-msg-convert-citation t))

;;;; Signature
(with-eval-after-load 'org-msg
  (setq message-signature nil
        message-signature-separator "^⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼ *$"
        org-msg-signature "
#+begin_signature
⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼

In gratitude,

Kristoffer

#+begin_export html
   <br>
      <table style='color:rgb(136,136,136);border:none;border-collapse:collapse'>
        <tbody>
          <tr style='height:81.25pt'>
            <td style='border-right: 0.75pt dotted rgb(135, 127, 116); vertical-align: top; padding: 5pt 11pt 5pt 5pt;' title=''>
              <img src='https://clipground.com/images/brown-university-logo-png-1.png' alt='Brown logo' style='border:none;' height='100'>
            </td>
            <td style='border-left:0.75pt dotted rgb(135,127,116);vertical-align:top;padding:5pt 5pt 5pt 11pt'>
              <p dir='ltr' style='line-height:1.38;margin-top:6pt;margin-bottom:0pt'>
                <span style='color:rgb(0,0,0);font-family:&quot;Times New Roman&quot;;font-size:11pt;font-weight:700;white-space:pre-wrap'>Kristoffer Balintona</span>
                <br>
              </p>
              <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt'>
                <span style='font-size:10pt;font-family:&quot;Times New Roman&quot;;color:rgb(0,0,0);vertical-align:baseline;white-space:pre-wrap'>B.A. Philosophy</span>
                <br>
              </p>
              <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt'>
                <span style='font-size:10pt;font-family:&quot;Times New Roman&quot;;color:rgb(0,0,0);vertical-align:baseline;white-space:pre-wrap'>Class of 2024</span>
              </p>
              <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt'>
                <span style='color:rgb(153,153,153);font-family:garamond;font-size:8pt;white-space:pre-wrap'>Tel: (773) 677-9699</span>
                <br>
              </p>
              <p dir='ltr' style='color:rgb(34,34,34);font-size:12.8px;line-height:1.2;margin-top:0pt;margin-bottom:0pt'>
                <span style='font-size:8pt;font-family:garamond;color:rgb(153,153,153);vertical-align:baseline;white-space:pre-wrap'>Box: 6327</span>
                <span style='font-size:8pt;font-family:garamond;color:rgb(136,136,136);vertical-align:baseline;white-space:pre-wrap'> &nbsp;
                </span>
              </p>
            </td>
          </tr>
        </tbody>
      </table>
    </div>
  </div>
</div>
#+end_export
#+end_signature"))

;;; email-sending-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'email-sending-rcp)
