;;; org-agenda-other-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Other org-agenda packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:


;;;; Org-wild-notifier
(use-package org-wild-notifier
  :disabled t ; Error on timer
  :custom
  (alert-default-style 'libnotify) ; Set default alert (global) style
  (org-wild-notifier-alert-time '(10 45 120))
  (org-wild-notifier-notification-title "Org-agenda")
  (org-wild-notifier-keyword-whitelist nil)
  (org-wild-notifier-keyword-blacklist nil)
  (org-wild-notifier-tags-whitelist nil)
  (org-wild-notifier-tags-blacklist nil)
  (org-wild-notifier-alert-times-property "wild_notifier_notify_before")
  :config
  (org-wild-notifier-mode)
  )

;;; org-agenda-other-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-agenda-other-rcp)
