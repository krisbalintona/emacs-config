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

;;; Smtpmail
;; Sending emails with msmtp
(use-package smtpmail
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

;;; Org-msg
;; Using org-mode to compose HTML-friendly emails
(use-package org-msg
  :custom
  (org-msg-startup "inlineimages")
  (org-msg-greeting-name-limit 3)
  (org-msg-text-plain-alternative t)
  )

;;; email-sending-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'email-sending-rcp)
