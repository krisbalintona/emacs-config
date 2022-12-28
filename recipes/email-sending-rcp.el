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
  :hook (message-setup . message-sort-headers)
  :custom
  (message-directory "~/Documents/emails/")
  (message-mail-user-agent t)           ; Use `mail-user-agent'
  (mail-user-agent 'message-user-agent)
  (message-send-mail-function 'message-use-send-mail-function)
  (compose-mail-user-agent-warnings t)


  (mail-specify-envelope-from t)
  (mail-header-separator (purecopy "*****"))
  (message-elide-ellipsis "\n> [... %l lines elided]\n")
  (mail-signature nil)
  (message-signature "Kind regards,\nKristoffer\n")
  (message-signature-insert-empty-line t)
  (message-citation-line-function #'message-insert-formatted-citation-line)
  (message-citation-line-format (concat "> From: %f\n"
                                        "> Date: %a, %e %b %Y %T %z\n"
                                        ">"))
  (message-ignored-cited-headers nil)    ; Default is "." for all headers
  (message-confirm-send nil)
  (message-kill-buffer-on-exit t)
  (message-wide-reply-confirm-recipients t)
  (message-sendmail-envelope-from 'header)

  ;; TODO 2022-12-26: Revisit these two variables
  ;; (message-sendmail-extra-arguments '("--read-envelope-from")) ; Tell msmtp to choose the SMTP server according to the from field in the outgoing email
  ;; (message-sendmail-f-is-evil 't)
  :config
  (org-msg-mode)
  )

;;; Sendmail
;; Use `sendmail' program to send emails?
(use-package sendmail
  :disabled
  :custom
  (send-mail-function 'sendmail-send-it)
  (sendmail-program (executable-find "sendmail")))

;;; Smtpmail
;; Use `msmtp' program to send emails?
(use-package smtpmail
  ;; :disabled
  :ensure-system-package (msmtp)
  :custom
  (send-mail-function 'smtpmail-send-it)

  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  (smtpmail-stream-type 'starttls)
  (smtpmail-queue-mail t)
  (smtpmail-queue-dir  "~/Documents/emails/queue/"))

;;; Org-msg
;; Using org-mode to compose HTML-friendly emails
(use-package org-msg
  :custom
  (org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t")
  (org-msg-startup "hidestars indent inlineimages")
  (org-msg-greeting-fmt nil)
  (org-msg-greeting-name-limit 3)
  (org-msg-default-alternatives
   '((new     . (text html))
     (reply-to-html   . (text html))
     (reply-to-text   . (text))))
  (org-msg-convert-citation t)
  (org-msg-signature "
----
#+begin_signature
Kind regards,
Kristoffer
#+end_signature"))

;;; email-sending-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'email-sending-rcp)
