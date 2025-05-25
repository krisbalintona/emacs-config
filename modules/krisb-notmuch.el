;; -*- lexical-binding: t; -*-

;;;; Krisb-notmuch-ext
(use-package krisb-notmuch-ext
  :ensure nil
  :after notmuch
  :hook (notmuch-show  . krisb-notmuch-show-expand-only-unread-h)
  :config
  (with-eval-after-load 'pulsar
    (dolist (func '(notmuch-show-rewind
                    notmuch-show-advance-and-archive
                    krisb-notmuch-show-advance-and-tag))
      (add-to-list 'pulsar-pulse-functions func))))

;;;; Mailcap
(use-package mailcap
  :ensure nil
  :custom
  ;; This affects the action called by `notmuch-show-view-part' and the listings
  ;; of `notmuch-show-interactively-view-part'
  (mailcap-user-mime-data
   '(("xdg-open %s" "text/html"))))

;;; Provide
(provide 'krisb-notmuch)
