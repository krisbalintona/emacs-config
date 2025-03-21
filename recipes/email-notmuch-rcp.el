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

;;;; Notmuch
(use-package notmuch
  :ensure-system-package (notmuch
                          (gmi . "sudo paru -S lieer-git"))
  :hook
  ((notmuch-mua-send . notmuch-mua-attachment-check) ; Also see `notmuch-mua-attachment-regexp'
   (notmuch-show . olivetti-mode)
   (notmuch-show  . kb/notmuch-show-expand-only-unread-h)
   (message-send . kb/notmuch-set-sendmail-args))
  :bind
  (([remap compose-mail] . notmuch-mua-new-mail)
   :map kb/open-keys
   ("n" . notmuch)
   :map notmuch-search-mode-map
   ("a" . nil) ; The default is too easy to hit accidentally
   ("/" . notmuch-search-filter)
   ("r" . notmuch-search-reply-to-thread)
   ("R" . notmuch-search-reply-to-thread-sender)
   :map notmuch-show-mode-map
   ("a" . nil)
   ("r" . notmuch-show-reply)
   ("R" . notmuch-show-reply-sender)
   ("T" . kb/notmuch-show-trash-thread-then-next)
   ([remap notmuch-show-advance-and-archive] . kb/notmuch-show-advance-and-tag))
  :custom
  (mail-user-agent 'notmuch-user-agent)
  (notmuch-identities nil)              ; Defer to notmuch-config's file data

  ;; Hello UI
  (notmuch-hello-sections (list #'notmuch-hello-insert-saved-searches
                                #'notmuch-hello-insert-alltags
                                #'notmuch-hello-insert-recent-searches))
  (notmuch-hello-thousands-separator ",")
  (notmuch-show-all-tags-list t)

  ;; Searches
  ;; See `man' for mbsync and notmuch to see valid search terms. See
  ;; https://www.emacswiki.org/emacs/NotMuch#h5o-2 on how to expunge local files
  ;; via cli
  (notmuch-search-hide-excluded t)
  (notmuch-saved-searches
   '((:name "inbox"                 :query "tag:inbox and not tag:list" :sort-order oldest-first :key "i")
     (:name "to-read mailing lists" :query "tag:list and tag:inbox "    :sort-order oldest-first :key "l")
     (:name "all mailing lists"     :query "tag:list"                                            :key "L" )
     (:name "sent"                  :query "tag:sent"                                            :key "s")
     (:name "drafts"                :query "tag:draft or path:drafts/"  :search-type unthreaded  :key "d")
     (:name "archived"              :query "not tag:trash"                                       :key "a")
     (:name "all"                   :query "path:**"                                             :key "A")
     (:name "trash"                 :query "tag:trash"                                           :key "t")))
  (notmuch-show-empty-saved-searches t)
  (notmuch-search-oldest-first nil)
  (notmuch-search-result-format '(("date" . "%14s ")
                                  ("count" . "%-7s ")
                                  ("authors" . "%-30s ")
                                  ("subject" . "%-75.75s ")
                                  ("tags" . "(%s)")))

  ;; Tags
  (notmuch-archive-tags '("-inbox"))
  (notmuch-message-replied-tags '("+replied"))
  (notmuch-message-forwarded-tags '("+forwarded"))
  (notmuch-show-mark-read-tags '("-unread"))
  (notmuch-draft-tags '("+draft"))
  (notmuch-draft-folder "drafts")
  (notmuch-draft-save-plaintext 'ask)
  (notmuch-tagging-keys
   `(("a" notmuch-archive-tags "Archive")
     ("r" notmuch-show-mark-read-tags "Mark read")
     ("f" ("+flagged") "Flag")
     ("s" ("+spam" "-inbox") "Mark as spam")
     ("t" ("+trash" "-inbox") "Trash")))
  (notmuch-tag-formats
   '(("unread" (propertize tag 'face 'notmuch-tag-unread))
     ("flagged" (propertize tag 'face 'notmuch-tag-flagged))))
  (notmuch-tag-deleted-formats
   '(("unread" (notmuch-apply-face bare-tag `notmuch-tag-deleted))
     (".*" (notmuch-apply-face tag `notmuch-tag-deleted))))

  ;; Notmuch-show-mode (i.e. reading emails)
  (notmuch-show-relative-dates t)
  (notmuch-show-all-multipart/alternative-parts nil)
  (notmuch-show-indent-multipart nil)
  (notmuch-show-indent-messages-width 3)
  (notmuch-show-part-button-default-action 'notmuch-show-interactively-view-part)
  (notmuch-show-text/html-blocked-images ".") ; Block everything
  (notmuch-wash-wrap-lines-length nil)
  (notmuch-unthreaded-show-out t)
  (notmuch-message-headers-visible nil)
  (notmuch-message-headers '("To" "Cc" "Date" "Subject"))

  ;; Notmuch-tree-mode
  (notmuch-tree-show-out nil)
  (notmuch-tree-result-format '(("date" . "%12s  ")
                                ("authors" . "%-20s  ")
                                ((("tree" . "%s")
                                  ("subject" . "%s"))
                                 . " %-85.85s  ")
                                ("tags" . "(%s)")))
  (notmuch-tree-outline-enabled nil)

  ;; Email composition
  (notmuch-mua-compose-in 'current-window)
  (notmuch-mua-hidden-headers nil)
  (notmuch-address-command 'internal)
  (notmuch-address-internal-completion '(sent nil))
  (notmuch-always-prompt-for-sender t)
  (notmuch-mua-cite-function 'message-cite-original-without-signature)
  (notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never)
  (notmuch-mua-user-agent-function nil)
  (notmuch-maildir-use-notmuch-insert t)
  (notmuch-wash-citation-lines-prefix 0)
  (notmuch-wash-citation-lines-suffix 0)
  (notmuch-crypto-process-mime t)
  (notmuch-crypto-get-keys-asynchronously t)
  ;; See `notmuch-mua-send-hook'
  (notmuch-mua-attachment-regexp (concat "\\b\\("
                                         "attache\?ment\\|attached\\|attach\\|"
                                         "pi[èe]ce\s+jointe?"
                                         "\\)\\b"))

  ;; Sending emails.
  ;; Use Lieer to send emails. Also see `kb/notmuch-set-sendmail-args'. Read
  ;; https://github.com/gauteh/lieer/wiki/Emacs-and-Lieer.
  (sendmail-program (executable-find "gmi"))
  (send-mail-function 'sendmail-send-it)
  (notmuch-fcc-dirs nil) ; Gmail already copies sent emails, so don't move them elsewhere locally
  :config
  ;; Don't buttonize citations
  ;; FIXME 2024-10-07: For some reason putting this in :custom and setting it to
  ;; a high value doesn't work, so I put it here
  (setq notmuch-wash-citation-lines-prefix most-positive-fixnum
        notmuch-wash-citation-lines-suffix most-positive-fixnum)


  ;; Restore window configuration when closing notmuch-hello window
  (defvar kb/notmuch-hello-pre-window-conf nil)

  (defun kb/notmuch-hello-set-window-conf ()
    "Set the value of `kb/notmuch-hello-pre-window-conf'."
    (unless (memq major-mode '(notmuch-show-mode
                               notmuch-tree-mode
                               notmuch-hello-mode
                               notmuch-search-mode
                               notmuch-message-mode))
      (setq kb/notmuch-hello-pre-window-conf (current-window-configuration))))

  (defun kb/notmuch--around (fn &rest args)
    "Set pre-window-configuration also."
    (interactive)
    (kb/notmuch-hello-set-window-conf)
    (apply fn args)
    ;; We delete other windows afterward just in case `notmuch' is called with
    ;; e.g. `other-frame-prefix'
    (delete-other-windows))
  (advice-add 'notmuch :around #'kb/notmuch--around)

  (defun kb/notmuch-bury-or-kill-this-buffer--around (fn &rest args)
    "Restore window configuration if appropriate."
    (interactive)
    (if (and (equal major-mode 'notmuch-hello-mode)
             kb/notmuch-hello-pre-window-conf
             (equal (selected-frame) (window-configuration-frame kb/notmuch-hello-pre-window-conf)))
        (progn
          (apply fn args)
          (set-window-configuration kb/notmuch-hello-pre-window-conf)
          (setq kb/notmuch-hello-pre-window-conf nil))
      (apply fn args)))
  (advice-add 'notmuch-bury-or-kill-this-buffer :around #'kb/notmuch-bury-or-kill-this-buffer--around)

  ;; REVIEW 2024-09-26: Prot's lin package apparently makes disabling this
  ;; better?
  (with-eval-after-load 'lin
    (remove-hook 'notmuch-search-hook #'notmuch-hl-line-mode))

  (defun kb/notmuch-set-sendmail-args ()
    "Set `message-sendmail-extra-arguments' arguments.
Set `message-sendmail-extra-arguments' accordingly (changing the
maildir) such that lieer can properly send the email. (This assumes
`sendmail-program' is set to the gmi executable.) Instruction from
https://github.com/gauteh/lieer/wiki/Emacs-and-Lieer."
    (when (string-match-p "gmi" sendmail-program)
      (let* ((from (downcase (message-fetch-field "from")))
             (root-maildir (expand-file-name "~/Documents/emails/"))
             (personal-maildir (expand-file-name "personal" root-maildir))
             (uni-maildir (expand-file-name "uni" root-maildir)))
        (cond
         ((string-match-p (rx (literal "krisbalintona@gmail.com")) from)
          (setq-local message-sendmail-extra-arguments `("send" "--quiet" "-t" "-C" ,personal-maildir)))
         ((string-match-p (rx (literal "kristoffer_balintona@alumni.brown.edu")) from)
          (setq-local message-sendmail-extra-arguments `("send" "--quiet" "-t" "-C" ,uni-maildir)))))))

  (defun kb/notmuch--setup-faces (theme)
    "Set up faces in `notmuch-show-mode'."
    (when (string-match "^modus-" (symbol-name theme))
      (modus-themes-with-colors
        ;; More noticeable demarcation of emails in thread in notmuch-show-mode
        (set-face-attribute 'notmuch-message-summary-face nil
                            :foreground fg-alt
                            ;; NOTE 2024-09-26: We do it this way since changing
                            ;; faces will refresh the font to be 1.1 times the 1.1
                            ;; times height, and so on
                            :height (truncate (* (face-attribute 'default :height nil) 1.1))
                            :overline t
                            :extend nil
                            :inherit 'unspecified)
        (set-face-attribute 'notmuch-tag-added nil
                            :underline `(:color ,cyan-cooler :style double-line :position t))
        (add-to-list 'notmuch-tag-formats
                     `("correspondence" (propertize tag 'face '(:foreground ,green-faint))))
        (add-to-list 'notmuch-tag-formats
                     `("commitment" (propertize tag 'face '(:foreground ,yellow-faint)))))))
  (add-hook 'enable-theme-functions #'kb/notmuch--setup-faces)

  ;; Prefer not to have emails recentered as I readjust them
  (advice-add 'notmuch-show-message-adjust :override #'ignore)

  (defun kb/notmuch-show-expand-only-unread-h ()
    "The `+notmuch-show-expand-only-unread-h' taken from Doom Emacs.
In `notmuch-show-mode', when showing a thread, keep read messages
folded."
    (interactive)
    (let ((unread nil)
          (open (notmuch-show-get-message-ids-for-open-messages)))
      (notmuch-show-mapc (lambda ()
                           (when (member "unread" (notmuch-show-get-tags))
                             (setq unread t))))
      (when unread
        (let ((notmuch-show-hook (remove 'kb/notmuch-show-expand-only-unread-h notmuch-show-hook)))
          (notmuch-show-filter-thread "tag:unread")))))

  ;; FIXME 2024-09-26: This is a workaround. For some reason
  ;; `notmuch-show-view-part' opens a non-existent HTML file in the browser...
  (defun kb/notmuch-show-view-part ()
    "View part in browser."
    (notmuch-show-apply-to-current-part-handle
     (lambda (handle &optional mime-type)
       (let ((file (make-temp-file "kb-notmuch-part-" nil (when (string= mime-type "text/html") ".html")))
             (browse-url-generic-args (remove "--new-window" browse-url-generic-args))) ; This is ad-hoc: I prefer not to open in a new window
         (mm-save-part-to-file handle file)
         (browse-url file)))))
  (advice-add 'notmuch-show-view-part :override #'kb/notmuch-show-view-part)

  ;; Bespoke `notmuch-show-mode' commands
  (defun kb/notmuch-show-trash-thread-then-next (&optional show)
    "\"Trash\" all messages in the current buffer, then exit thread.
If SHOW is provided (interactively by prefix-arg), then also open that
thread."
    (interactive "P")
    (notmuch-show-tag-all
     (notmuch-tag-change-list (append notmuch-archive-tags '("+trash"))))
    (notmuch-show-next-thread t show))

  (defun kb/notmuch-show-tag-thread (&optional reverse)
    "Like `notmuch-show-archive-thread' put prompt "
    (interactive "P")
    (let (current-tags)
      (notmuch-show-mapc
       (lambda () (setq current-tags (append (notmuch-show-get-tags) current-tags))))
      (notmuch-show-tag-all
       (notmuch-tag-change-list
        (notmuch-read-tag-changes current-tags)
        reverse))))

  (defun kb/notmuch-show-advance-and-tag ()
    "Like `notmuch-show-advance-and-archive' but prompt for tag instead.
Tagging is done by `kb/notmuch-show-tag-thread'."
    (interactive)
    (when (notmuch-show-advance)
      (kb/notmuch-show-tag-thread)
      (notmuch-show-next-thread t)))
  (with-eval-after-load 'pulsar
    (dolist (func '(notmuch-show-rewind
                    notmuch-show-advance-and-archive
                    kb/notmuch-show-advance-and-tag))
      (add-to-list 'pulsar-pulse-functions func)))

  ;; Sets the style of `message's citations before sending a reply
  ;; TODO 2024-10-07: Try setting this automatically. If I can't, then clean up
  ;; the prompting of this function
  (defun kb/notmuch--set-message-citation-style (orig-func &rest args)
    "Prompt for which style of citations should be used for message reply."
    (let ((selection
           (completing-read "Citation style: "
                            '("default" "traditional" "gmail"))))
      (cond
       ((equal selection "traditional")
        (message "Setting citation style to \"traditional\"")
        (let ((message-cite-function 'message-cite-original)
              (message-citation-line-function 'message-insert-formatted-citation-line)
              (message-citation-line-format "On %a, %b %d %Y, %N wrote:\n")
              (message-cite-reply-position 'below)
              (message-yank-prefix "> ")
              (message-yank-cited-prefix ">")
              (message-yank-empty-prefix ">"))
          (apply orig-func args)))
       ((equal selection "gmail")
        (message "Setting citation style to \"gmail\"")
        ;; These settings set what is specified by `message-cite-style-gmail'. I
        ;; do this manually since not all packages seem to be affected by
        ;; `message-cite-style'
        (let ((message-cite-function 'message-cite-original)
              (message-citation-line-function 'message-insert-formatted-citation-line)
              (message-citation-line-format "On %a, %b %d, %Y at %-I:%M %p %f wrote:\n")
              (message-cite-reply-position 'above)
              (message-yank-prefix "    ")
              (message-yank-cited-prefix "    ")
              (message-yank-empty-prefix "    "))
          (apply orig-func args)))
       ((equal selection "default")
        (message "Using default citation style")
        (apply orig-func args)))))
  (advice-add 'notmuch-mua-new-reply :around #'kb/notmuch--set-message-citation-style))

;;;; `notmuch-mua-reply' overide to obey `message-cite-reply-position'
(with-eval-after-load 'notmuch-mua
  (defun kb/notmuch-mua-reply (query-string &optional sender reply-all duplicate)
    "Like `notmuch-mua-reply' but positions citation based on `message-cite-reply-position'."
    (let* ((duparg (and duplicate (list (format "--duplicate=%d" duplicate))))
           (args `("reply" "--format=sexp" "--format-version=5" ,@duparg))
           (process-crypto notmuch-show-process-crypto)
           reply
           original)
      (when process-crypto
        (setq args (append args '("--decrypt=true"))))
      (if reply-all
          (setq args (append args '("--reply-to=all")))
        (setq args (append args '("--reply-to=sender"))))
      (setq args (append args (list query-string)))
      ;; Get the reply object as SEXP, and parse it into an elisp object.
      (setq reply (apply #'notmuch-call-notmuch-sexp args))
      ;; Extract the original message to simplify the following code.
      (setq original (plist-get reply :original))
      ;; Extract the headers of both the reply and the original message.
      (let* ((original-headers (plist-get original :headers))
             (reply-headers (plist-get reply :reply-headers)))
        ;; If sender is non-nil, set the From: header to its value.
        (when sender
          (plist-put reply-headers :From sender))
        (let
            ;; Overlay the composition window on that being used to read
            ;; the original message.
            ((same-window-regexps '("\\*mail .*")))
          ;; We modify message-header-format-alist to get around
          ;; a bug in message.el.  See the comment above on
          ;; notmuch-mua-insert-references.
          (let ((message-header-format-alist
                 (cl-loop for pair in message-header-format-alist
                          if (eq (car pair) 'References)
                          collect (cons 'References
                                        (apply-partially
                                         'notmuch-mua-insert-references
                                         (cdr pair)))
                          else
                          collect pair)))
            (notmuch-mua-mail (plist-get reply-headers :To)
                              (notmuch-sanitize (plist-get reply-headers :Subject))
                              (notmuch-headers-plist-to-alist reply-headers)
                              nil (notmuch-mua-get-switch-function))))
        ;; Create a buffer-local queue for tag changes triggered when
        ;; sending the reply.
        (when notmuch-message-replied-tags
          (setq notmuch-message-queued-tag-changes
                (list (cons query-string notmuch-message-replied-tags))))
        ;; Insert the message body - but put it in front of the signature
        ;; if one is present, and after any other content
        ;; message*setup-hooks may have added to the message body already.
        (save-restriction
          (message-goto-body)
          (narrow-to-region (point) (point-max))
          (goto-char (point-max))
          (if (re-search-backward message-signature-separator nil t)
              (when message-signature-insert-empty-line
                (forward-line -1))
            (goto-char (point-max))))
        ;; If `message-cite-reply-position' is `above', e.g., for Gmail-like
        ;; email replies, then before inserting the citation, put the point
        ;; after the signature and insert a newline for spacing. Also respects
        ;; if `message-cite-reply-position' is set via `message-cite-style'.
        (when (or (equal message-cite-reply-position 'above)
                  (and message-cite-style
                       (eq (eval (cadr
                                  (assoc 'message-cite-reply-position
                                         (if (symbolp message-cite-style)
                                             (eval message-cite-style)
                                           message-cite-style))))
                           'above)))
          (goto-char (point-max))
          (insert "\n"))
        (let ((from (plist-get original-headers :From))
              (date (plist-get original-headers :Date))
              (start (point)))
          ;; notmuch-mua-cite-function constructs a citation line based
          ;; on the From and Date headers of the original message, which
          ;; are assumed to be in the buffer.
          (insert "From: " from "\n")
          (insert "Date: " date "\n\n")
          (insert
           (with-temp-buffer
             (let
                 ;; Don't attempt to clean up messages, excerpt
                 ;; citations, etc. in the original message before
                 ;; quoting.
                 ((notmuch-show-insert-text/plain-hook nil)
                  ;; Don't omit long parts.
                  (notmuch-show-max-text-part-size 0)
                  ;; Insert headers for parts as appropriate for replying.
                  (notmuch-show-insert-header-p-function
                   notmuch-mua-reply-insert-header-p-function)
                  ;; Ensure that any encrypted parts are
                  ;; decrypted during the generation of the reply
                  ;; text.
                  (notmuch-show-process-crypto process-crypto)
                  ;; Don't indent multipart sub-parts.
                  (notmuch-show-indent-multipart nil)
                  ;; Stop certain mime types from being inlined
                  (mm-inline-override-types (notmuch--inline-override-types)))
               ;; We don't want sigstatus buttons (an information leak and usually wrong anyway).
               (cl-letf (((symbol-function 'notmuch-crypto-insert-sigstatus-button) #'ignore)
                         ((symbol-function 'notmuch-crypto-insert-encstatus-button) #'ignore))
                 (notmuch-show-insert-body original (plist-get original :body) 0)
                 (buffer-substring-no-properties (point-min) (point-max))))))
          (set-mark (point))
          (goto-char start)
          ;; Quote the original message according to the user's configured style.
          (funcall notmuch-mua-cite-function)))
      ;; Crypto processing based crypto content of the original message
      (when process-crypto
        (notmuch-mua-reply-crypto (plist-get original :body))))
    ;; Push mark right before signature, if any.
    (message-goto-signature)
    (unless (eobp)
      (end-of-line -1))
    (push-mark)
    (message-goto-body)
    (set-buffer-modified-p nil))
  (advice-add 'notmuch-mua-reply :override #'kb/notmuch-mua-reply))

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
                   (file ,(expand-file-name "todo.org" kb/agenda-dir))
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
