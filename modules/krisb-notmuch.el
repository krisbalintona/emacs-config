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

;;;; Notmuch-transient
(use-package notmuch-transient
  :after notmuch
  :custom
  (notmuch-transient-add-bindings t)
  (notmuch-transient-prefix "C-d"))

;;;; Notmuch-addr
;; Better address completion for notmuch; replaces the built-in
;; `notmuch-address' completion system. Read
;; https://nmbug.notmuchmail.org/nmweb/show/20201108231150.5419-1-jonas%40bernoul.li
;; for more information
(use-package notmuch-addr
  :after notmuch-address
  :config
  (notmuch-addr-setup))

;;;; Mode line indicator
;; Try using display-time's built-in email indicator --- less informative but
;; more visually subtle than `notmuch-indicator'.  Obviously the below applies
;; only when `display-time-mode' is non-nil.
(with-eval-after-load 'time
  (with-eval-after-load 'notmuch
    (setopt display-time-mail-face 'notmuch-search-flagged-face))

  (defvar krisb-display-time-mail-icon
    (cond
     ((featurep 'nerd-icons)
      (propertize (nerd-icons-mdicon "nf-md-email")
                  'face `(:family ,(nerd-icons-mdicon-family) :height 1.1)
                  'display '(raise 0.05)))
     ((featurep 'all-the-icons)
      (propertize (all-the-icons-material "mail_outline")
                  'face `(:family ,(all-the-icons-material-family) :height 1.1)
                  'display '(raise -0.1))))
    "Icon I use for displaying mail in `display-time-string-forms'.")

  (setopt display-time-use-mail-icon t
          display-time-mail-function
          (lambda ()
            (let* ((command (format "notmuch search tag:inbox and tag:unread and not tag:list and not tag:sub | wc -l"))
                   (count (string-to-number (shell-command-to-string command))))
              (< 0 count)))
          display-time-string-forms
          '((if (and (not display-time-format) display-time-day-and-date)
                (format-time-string "%a %b %e " now)
              "")
            (propertize
             (format-time-string (or display-time-format
                                     (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
                                 now)
             'face 'display-time-date-and-time
             'help-echo (format-time-string "%a %b %e, %Y" now))
            load
            (if mail
                (concat
                 " "
                 (propertize
                  (if (and display-time-use-mail-icon (display-graphic-p))
                      (symbol-value 'krisb-display-time-mail-icon)
                    display-time-mail-string)
                  'face display-time-mail-face
                  'help-echo "You have new mail; mouse-2: Read mail"
                  'mouse-face 'mode-line-highlight
                  'local-map (make-mode-line-mouse-map 'mouse-2
                                                       read-mail-command)))
              "")
            " "))
  (advice-add 'notmuch-bury-or-kill-this-buffer :around
              (lambda (&rest args)
                "Ensure mail icon is accurate.
Update right after closing the notmuch hello buffer so the mail icon
reflects the state of my maildirs accurate."
                (when (equal major-mode 'notmuch-hello-mode)
                  (display-time-update))
                (apply args))))

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
