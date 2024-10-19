;;; email-notmuch-rcp.el --- Notmuch email client    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; These are packages pertinent to using the `notmuch' email client. It is also
;; important to note that this is only a frontend for the `notmuch' command line
;; tool. Syncing from remote servers can be done via `mbsync' (from the `mu'
;; utility set) and `offlineimap', whose configuration files are `.mbsyncrc' and
;; `.offlineimaprc', respectively.
;;
;; Protesilaos provides a wonderful guide here:
;; https://protesilaos.com/emacs/dotemacs#h:5ad80664-3163-4d9d-be65-462637d77903

;;; Code:
(require 'keybinds-general-rcp)

;;;; Email mode line indicator
;; Try using display-time's built-in email indicator --- less informative but
;; more visually subtle than `notmuch-indicator'.
(with-eval-after-load 'time
  ;; Obviously the below applies only when `display-time-mode' is non-nil.
  (with-eval-after-load 'notmuch
    (setopt display-time-mail-face 'notmuch-search-flagged-face))
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
                  (if (and display-time-use-mail-icon (display-graphic-p) (featurep 'all-the-icons))
                      (propertize (all-the-icons-material "mail_outline")
                                  'face `(:family ,(all-the-icons-material-family) :height 1.1)
                                  'display '(raise -0.1))
                    display-time-mail-string)
                  'face display-time-mail-face
                  'help-echo "You have new mail; mouse-2: Read mail"
                  'mouse-face 'mode-line-highlight
                  'local-map (make-mode-line-mouse-map 'mouse-2
                                                       read-mail-command)))
              "")
            " "))
  ;; Update right after closing the notmuch hello buffer so the mail icon
  ;; reflects the state of my maildirs accurate
  (advice-add 'notmuch-bury-or-kill-this-buffer :after #'display-time-update))

;;;; Notmuch-indicator
(use-package notmuch-indicator
  :disabled   ; REVIEW 2024-09-29: Trying out simple display-time mail indicator
  :after notmuch
  :demand
  :custom
  (notmuch-indicator-add-to-mode-line-misc-info nil) ; I add it to the modeline myself
  (notmuch-indicator-counter-format "%s%s")
  (notmuch-indicator-args '(( :terms "tag:unread and tag:inbox"
                              :label "M:"
                              :label-face modus-themes-fg-green)))
  (notmuch-indicator-refresh-count (* 60 3))
  (notmuch-indicator-hide-empty-counters t)
  (notmuch-indicator-force-refresh-commands '(notmuch notmuch-refresh-this-buffer))
  :config
  (notmuch-indicator-mode 1)

  ;; Override default mode line construct
  (setq-default notmuch-indicator-mode-line-construct
                '(notmuch-indicator-mode ((:eval notmuch-indicator--counters) " ")))
  ;; Add to mode line myself
  (add-to-list 'global-mode-string 'notmuch-indicator-mode-line-construct))

;;;; Notmuch-transient
(use-package notmuch-transient
  :after notmuch
  :demand
  :custom
  (notmuch-transient-add-bindings t)
  (notmuch-transient-prefix "C-d"))

;;;; Ol-notmuch
;; Org-links for search queries (i.e. notmuch-search-mode, notmuch-tree-mode)
;; and messages (i.e. notmuch-show-mode).
(use-package ol-notmuch
  :autoload kb/org-notmuch-store-link
  :config
  ;; Integration with `org-agenda'
  (defun kb/org-notmuch-store-link ()
    "Store a link to one or more notmuch messages.
My version allows for linking to the first message in an email thread
from a `notmuch-search-mode' buffer."
    (cond
     ((memq major-mode '(notmuch-show-mode notmuch-tree-mode))
      ;; The value is passed around using variable `org-store-link-plist'.
      (org-link-store-props
       :type       "notmuch"
       :message-id (notmuch-show-get-message-id t)
       :subject    (notmuch-show-get-subject)
       :from       (notmuch-show-get-from)
       :to         (notmuch-show-get-to)
       :date       (org-trim (notmuch-show-get-date)))
      (org-link-add-props :link (org-link-email-description "notmuch:id:%m"))
      (org-link-add-props :description (org-link-email-description))
      org-store-link-plist)
     ((equal major-mode 'notmuch-search-mode)
      (save-window-excursion
        (let ((buf (notmuch-show (notmuch-search-find-thread-id))))
          (with-current-buffer buf
            (org-link-store-props
             :type       "notmuch"
             :message-id (notmuch-show-get-message-id t)
             :subject    (notmuch-show-get-subject)
             :from       (notmuch-show-get-from)
             :to         (notmuch-show-get-to)
             :date       (org-trim (notmuch-show-get-date)))
            (org-link-add-props :link (org-link-email-description "notmuch:id:%m"))
            (org-link-add-props :description (org-link-email-description)))
          (kill-buffer buf)
          org-store-link-plist)))))
  (advice-add 'org-notmuch-store-link :override #'kb/org-notmuch-store-link)

  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 `("e" "Email" entry
                   (file ,(expand-file-name "todo.org" krisb-org-agenda-directory))
                   "* TODO %? [[%L][\"%:subject\"]] :email:\n\nFrom %:from\nTo: %:to\n"
                   :empty-lines 1)
                 'append)

    (add-to-list 'org-capture-templates-contexts '("e" ((in-mode . "notmuch-tree-mode"))))
    (add-to-list 'org-capture-templates-contexts '("e" ((in-mode . "notmuch-search-mode"))))
    (add-to-list 'org-capture-templates-contexts '("e" ((in-mode . "notmuch-show-mode"))))))

;;;; Notmuch-addr
;; Better address completion for notmuch; replaces the built-in
;; `notmuch-address' completion system. Read
;; https://nmbug.notmuchmail.org/nmweb/show/20201108231150.5419-1-jonas%40bernoul.li
;; for more information
(use-package notmuch-addr
  :autoload notmuch-addr-setup
  :init
  (with-eval-after-load 'notmuch-address
    (notmuch-addr-setup)))

(provide 'email-notmuch-rcp)
;;; email-notmuch-rcp.el ends here
