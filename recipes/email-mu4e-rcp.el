;;; email-mu4e-rcp.el --- Mu4e email client          -*- lexical-binding: t; -*-

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

;; These are packages pertinent to using the `mu4e' email client which leverages
;; the `mu' backend. Syncing from remote servers can be done via `mbsync' (from
;; the `mu' utility set).
;;
;; I also have configuration taken from Doom Emacs that pertain to Gmail
;; integration. I have marked those code snippets accordingly.

;;; Code:
(require 'keybinds-general-rcp)

;;;; Mu4e
;;;;; Itself
(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :commands (kb/mu4e mu4e)
  :hook (;; (after-init . (lambda () (when (daemonp) (mu4e t))))
         (server-mode . (lambda () (mu4e t)))
         ;; (on-first-file . (lambda () (mu4e t)))
         (window-setup . (lambda ()
                           (setq mu4e-headers-personal-mark  '("p" . " ")) ; Always set this
                           (unless (bound-and-true-p kb/mu4e-initialise-icons)
                             (setq mu4e-headers-personal-mark  '("p" . "  ")
                                   mu4e-headers-unread-mark    '("u" . "📩 ")
                                   mu4e-headers-draft-mark     '("D" . "✏️ ")
                                   mu4e-headers-flagged-mark   '("F" . "🚩 ")
                                   mu4e-headers-new-mark       '("N" . "✨ ")
                                   mu4e-headers-passed-mark    '("P" . "⮡ ")
                                   mu4e-headers-replied-mark   '("R" . "↵ ")
                                   mu4e-headers-seen-mark      '("S" . "  ")
                                   mu4e-headers-list-mark      '("s" . "🔉 ")
                                   mu4e-headers-trashed-mark   '("T" . "🗑️")
                                   mu4e-headers-attach-mark    '("a" . "📎 ")
                                   mu4e-headers-encrypted-mark '("x" . "🔑 ")
                                   mu4e-headers-signed-mark    '("s" . "🖊 ")))
                           ;; Taken from Doom Emacs
                           (setq mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶")
                                 mu4e-headers-thread-orphan-prefix '("┬>" . "┬▶ ")
                                 mu4e-headers-thread-last-child-prefix '("└>" . "╰▶")
                                 mu4e-headers-thread-child-prefix '("├>" . "├▶")
                                 mu4e-headers-thread-connection-prefix '("│" . "│ "))))
         (mu4e-mark-execute-pre . kb/mu4e-gmail-fix-flags-h)
         (mu4e-view-mode . olivetti-mode)
         (mu4e-compose-mode . fraolt-mu4e-mark-deletable-headers))
  :bind
  ( :map kb/open-keys
    ("m" . kb/mu4e)
    :map mu4e-main-mode-map
    ("q" . kb/mu4e-main-bury-buffer)
    ("Q" . mu4e-quit)
    ([remap compose-mail] . mu4e-compose-new)
    :map mu4e-main-mode-map
    ("M-U" . mu4e-update-index-nonlazy)
    :map mu4e-headers-mode-map
    ("M-U" . mu4e-update-index-nonlazy)
    :map mu4e-view-mode-map
    ("M-U" . mu4e-update-index-nonlazy))
  :custom
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-bookmarks
   '((:name "All inboxes" :query "maildir:/personal/Inbox OR maildir:/uni/Inbox" :key ?u)
     (:name "All drafts" :query "maildir:/personal/[Gmail].Drafts OR maildir:/uni/[Gmail].Drafts" :key ?d)
     (:name "All sent" :query "maildir:\"/personal/[Gmail].Sent Mail\" OR maildir:\"/uni/[Gmail].Sent Mail\"" :key ?s)))

  ;; Modeline and notifications
  (mu4e-modeline-support t)
  (mu4e-notification-support t)

  ;; Contexts
  (mu4e-context-policy 'pick-first)
  (mu4e-compose-context-policy 'ask-if-none)

  ;; Indexing
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-update-interval 300)
  (mu4e-index-update-error-continue t)
  ;; Speed up indexing. See 2.9.4 Speeding up indexing of the Mu4e info manual.
  ;; Also note `mu4e-update-index-nonlazy'
  (mu4e-index-cleanup t)                ; Don't do a full cleanup check?
  (mu4e-index-lazy-check nil)           ; Don't consider up-to-date dirs?
  (mu4e-index-update-in-background t)
  (mu4e-hide-index-messages t)          ; Hide "indexing" message in echo area?

  ;; Headers
  (mu4e-split-view 'horizontal)
  (mu4e-headers-visible-lines 13)
  (mu4e-use-fancy-chars t)
  (mu4e-headers-precise-alignment t) ; Reduces performance but aligns everything
  (mu4e-headers-fields
   '((:maildir . 35)
     (:from-or-to . 25)
     (:human-date . 12)
     (:flags . 10)                      ; The max width of 3 icon flags
     (:subject)))
  (mu4e-headers-actions
   '(("view in browser" . mu4e-action-view-in-browser)
     ("capture message" . mu4e-action-capture-message)
     ("browse online archive" . mu4e-action-browse-list-archive)
     ("show this thread" . mu4e-action-show-thread)
     ("kill-ring-save file path" . mu4e-action-copy-message-file-path)))
  (mu4e-maildir-initial-input "")       ; Annoying "/" by default

  ;; View
  (mu4e-view-fields
   '(:from :to :cc :bcc
           :subject :flags :date
           :maildir :mailing-list
           :tags :attachments :signature))
  (mu4e-view-scroll-to-next nil)
  (shr-color-visible-luminance-min 80)   ; Better viewing for dark theme

  ;; Composing and sending
  (send-mail-function 'smtpmail-send-it) ; Use smtpmail
  (mu4e-sent-messages-behavior 'delete) ; Don't save message to Sent Messages, Gmail/IMAP takes care of this
  (mu4e-compose-signature-auto-include t)
  (mu4e-compose-hidden-headers nil)   ; See all headers
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-compose-complete-only-personal t)
  (mu4e-compose-switch 'window)

  ;; Other
  (mu4e-change-filenames-when-moving t) ; Prevent duplication
  (mu4e-confirm-quit nil)
  (mu4e-headers-eldoc-format "In %m with flags %F")
  (mu4e-read-option-use-builtin t)      ; Builtin is unobtrusive
  (mu4e-completing-read-function 'completing-read)
  :config
  ;; Restore window configuration when closing Mu4e main window like you can
  ;; with org-agenda via the `org-agenda-restore-windows-after-quit' user option
  (defvar kb/mu4e-main-pre-window-conf nil)

  (defun kb/mu4e-main-set-window-conf (&rest r)
    "Set the value of `kb/mu4e-main-pre-window-conf'."
    (unless (derived-mode-p '(mu4e-main-mode mu4e-view-mode mu4e-headers-mode))
      (setq kb/mu4e-main-pre-window-conf (current-window-configuration))))

  (defun kb/mu4e (&optional background)
    "Wrapper for `mu4e' command.
Opens mu4e to take up the entire frame. When
BACKGROUND (prefix-argument) is non-nil, don't show the window."
    (interactive "P")
    (require 'mu4e)
    (unless background (kb/mu4e-main-set-window-conf))
    (delete-other-windows)
    (mu4e background))

  (defun kb/mu4e-main-bury-buffer ()
    "Restore window configuration."
    (interactive)
    (if kb/mu4e-main-pre-window-conf
        (progn
          (set-window-configuration kb/mu4e-main-pre-window-conf)
          (setq kb/mu4e-main-pre-window-conf nil))
      (bury-buffer)))

  ;; Gmail integration is taken from Doom
  ;; Check if msg is being called from a gmail account
  (defun kb/mu4e-msg-gmail-p (msg)
    t)                     ; Just return t for now since all my emails are gmail
  (defun kb/mu4e--mark-seen (docid _msg target)
    (mu4e--server-move docid (mu4e--mark-check-target target) "+S-u-N"))
  (defvar kb/mu4e--last-invalid-gmail-action 0)
  ;; This hook correctly modifies gmail flags on emails when they are marked.
  ;; Without it, refiling (archiving), trashing, and flagging (starring) email
  ;; won't properly result in the corresponding gmail action, since the marks
  ;; are ineffectual otherwise.
  (defun kb/mu4e-gmail-fix-flags-h (mark msg)
    (when (kb/mu4e-msg-gmail-p msg)
      (pcase mark
        (`trash  (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
        (`delete (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
        (`refile (mu4e-action-retag-message msg "-\\Inbox"))
        (`flag   (mu4e-action-retag-message msg "+\\Starred"))
        (`unflag (mu4e-action-retag-message msg "-\\Starred")))))

  ;; Respect the value of `message-deletable-headers'; mu4e doesn't do so by
  ;; default. See https://github.com/djcb/mu/issues/2502
  (defun fraolt-mu4e-mark-deletable-headers ()
    "Set deletable headers as deletable."
    (save-restriction
      (message-narrow-to-headers)
      (let ((headers message-deletable-headers))
        (unless (buffer-modified-p)
          (setq headers (delq 'Message-ID (copy-sequence headers))))
        (while headers
          (goto-char (point-min))
          (if (re-search-forward
               (concat "^" (symbol-name (car headers)) ": *") nil t)
              (add-text-properties
               (match-beginning 0) (match-end 0)
               '(message-deletable t face italic) (current-buffer)))
          (pop headers)))))

  ;; Modeline
  ;; Force using regular characters rather than the fancy ones
  (advice-add 'mu4e--bookmarks-modeline-item :around
              (lambda (orig-fun &rest args)
                (let ((mu4e-use-fancy-chars nil))
                  (apply orig-fun args))))

  ;; Headers
  ;; Taken from Doom
  (plist-put (cdr (assoc :flags mu4e-header-info)) :shortname " Flags") ; default=Flgs
  (setq mu4e-marks
        ;; Refile is identical to delete now, since GMail "archives" by removing
        ;; from the maildir (all mail is already in the "All Mail" maildir)
        '(
          ;; Refile will be my "archive" function.
          (refile :char ("r" . "▶")
                  :prompt "archive"
                  :show-target
                  (lambda (target) "archive")
                  :dyn-target
                  (lambda (_target msg) (mu4e-get-refile-folder msg))
                  :action
                  (lambda (docid msg target)
                    (if (kb/mu4e-msg-gmail-p msg)
                        (kb/mu4e--mark-seen docid msg target)
                      (mu4e--server-move docid (mu4e--mark-check-target target) "-N")))
                  #'kb/mu4e--mark-seen)
          ;; In my workflow, emails won't be moved at all. Only their
          ;; flags/labels are changed. So we redefine the trash and refile marks
          ;; not to do any moving. However, the real magic happens in
          ;; `kb/mu4e-gmail-fix-flags-h'.
          ;;
          ;; Gmail will handle the rest.
          (delete :char ("D" . "x")
                  :prompt "Delete"
                  :show-target
                  (lambda (target) "delete")
                  :action
                  (lambda (docid msg target)
                    (if (and (kb/mu4e-msg-gmail-p msg)
                             (not (file-in-directory-p (mu4e-message-field msg :path) mail-default-directory)))
                        (progn
                          (message "The delete operation is invalid for Gmail accounts. Trashing instead.")
                          (kb/mu4e--mark-seen docid msg target)
                          (when (< 2 (- (float-time) kb/mu4e--last-invalid-gmail-action))
                            (sit-for 1))
                          (setq kb/mu4e--last-invalid-gmail-action (float-time)))
                      (mu4e--server-remove docid))))
          (trash :char ("d" . "▼")
                 :prompt "trash"
                 :show-target
                 (lambda (target) "trash")
                 :dyn-target
                 (lambda (_target msg) (mu4e-get-trash-folder msg))
                 :action
                 (lambda (docid msg target)
                   (if (kb/mu4e-msg-gmail-p msg)
                       (kb/mu4e--mark-seen docid msg target)
                     (mu4e--server-move docid (mu4e--mark-check-target target) "+T-N"))))
          (label :char ("l" . "↗")
                 :prompt "label"
                 :show-target
                 (lambda (target) "labeled")
                 :ask-target mu4e--mark-get-move-target
                 :action
                 (lambda (docid msg target)
                   (if (kb/mu4e-msg-gmail-p msg)
                       (mu4e-action-retag-message msg (format "+\\Inbox,+\\%s" (f-filename target)))
                     (mu4e--server-move docid (mu4e--mark-check-target target) "-N"))))
          ;; Default commands
          (flag :char
                ("+" . "✚")
                :prompt "+flag" :show-target
                (lambda
                  (target)
                  "flag")
                :action
                (lambda
                  (docid msg target)
                  (mu4e--server-move docid nil "+F-u-N")))
          (move :char
                ("m" . "▷")
                :prompt "move" :ask-target mu4e--mark-get-move-target :action
                (lambda
                  (docid msg target)
                  (mu4e--server-move docid
                                     (mu4e--mark-check-target target)
                                     "-N")))
          (read :char
                ("!" . "◼")
                :prompt "!read" :show-target
                (lambda
                  (target)
                  "read")
                :action
                (lambda
                  (docid msg target)
                  (mu4e--server-move docid nil "+S-u-N")))
          (unflag :char
                  ("-" . "➖")
                  :prompt "-unflag" :show-target
                  (lambda
                    (target)
                    "unflag")
                  :action
                  (lambda
                    (docid msg target)
                    (mu4e--server-move docid nil "-F-N")))
          (untrash :char
                   ("=" . "▲")
                   :prompt "=untrash" :show-target
                   (lambda
                     (target)
                     "untrash")
                   :action
                   (lambda
                     (docid msg target)
                     (mu4e--server-move docid nil "-T")))
          (unread :char
                  ("?" . "◻")
                  :prompt "?unread" :show-target
                  (lambda
                    (target)
                    "unread")
                  :action
                  (lambda
                    (docid msg target)
                    (mu4e--server-move docid nil "-S+u-N")))
          (unmark :char " " :prompt "unmark" :action
                  (mu4e-error "No action for unmarking"))
          (action :char
                  ("a" . "◯")
                  :prompt "action" :ask-target
                  (lambda nil
                    (mu4e-read-option "Action: " mu4e-headers-actions))
                  :action
                  (lambda
                    (docid msg actionfunc)
                    (save-excursion
                      (when
                          (mu4e~headers-goto-docid docid)
                        (mu4e-headers-action actionfunc)))))
          (something :char
                     ("*" . "✱")
                     :prompt "*something" :action
                     (mu4e-error "No action for deferred mark"))))
  ;; NOTE 2023-01-01: Have to do this for any new marks (i.e. marks with new
  ;; names, not redefinitions of extant marks) I create
  (mu4e~headers-defun-mark-for label)
  (mu4e--view-defun-mark-for label)
  (bind-key "l" #'mu4e-headers-mark-for-label mu4e-headers-mode-map)
  (bind-key "l" #'mu4e-view-mark-for-label mu4e-view-mode-map)

  ;; See 5.3 of the mu4e info manual. Also see 1.5 Display Customization of the
  ;; emacs-mime info entry
  (with-eval-after-load 'mm-decode
    (setq gnus-blocked-images (rx unmatchable) ; Don't block images
          ;; gnus-inhibit-mime-unbuttonizing t    ; Show all MIME buttons?
          mm-discouraged-alternatives '("text/html" "text/richtext" "image/.*")
          mm-automatic-display (remove "text/html" mm-automatic-display))) ; If I really don't want to see HTML

  (with-eval-after-load 'org-agenda
    (add-to-list 'org-capture-templates
                 `("e" "Email" entry
                   (file ,(expand-file-name "todo.org" kb/agenda-dir))
                   "* TODO Respond to%? [[%L][\"%:subject\"]] :email:\n\nFrom %:from\nTo: %:to\n"
                   :empty-lines 1))
    ;; NOTE 2023-01-01: Also see `mu4e--org-store-link-message' from mu4e-org
    (add-to-list 'org-capture-templates
                 `("E" "Mu4e-captured email" entry
                   (file ,(expand-file-name "todo.org" kb/agenda-dir))
                   ,(concat "* TODO Respond to%? "
                            "[[mu4e:msgid:%(plist-get mu4e-captured-message :message-id)]"
                            "[\"%(plist-get mu4e-captured-message :subject)\" "
                            "from %(plist-get (car (plist-get mu4e-captured-message :from)) :name) "
                            "on %(format-time-string \"%F\" (plist-get mu4e-captured-message :date))]]\n")
                   :empty-lines 1))

    (add-to-list 'org-capture-templates-contexts '("e" ((in-mode . "mu4e-headers-mode"))))
    (add-to-list 'org-capture-templates-contexts '("e" ((in-mode . "mu4e-view-mode"))))
    (add-to-list 'org-capture-templates-contexts '("E" ((lambda () (bound-and-true-p mu4e-captured-message)))))))

;;;;; Integration with `logos.el'
(defun kb/mu4e-view-set-outline-regexp ()
  "Set `outline-regexp' according to FROM field.
Useful for `consult-outline' and `logos.el'."
  (let* ((from (message-field-value "FROM"))
         (regexp
          ;; According to FROM field
          (pcase from
            ((or "emacs-orgmode-request@gnu.org" ; Org-mode devel
                 "emacs-devel-request@gnu.org")  ; Emacs devel
             (rx bol
                 (literal "------------------------------")
                 (zero-or-more "-")
                 eol)))))
    (when regexp
      (setq-local outline-regexp regexp
                  page-delimiter regexp))))
(add-hook 'mu4e-view-rendered-hook #'kb/mu4e-view-set-outline-regexp)

;;;; Mu4e header icons (from Doom Emacs)
(with-eval-after-load 'mu4e
  (defun kb/mu4e--get-string-width (str)
    "Return the width in pixels of a string in the current
window's default font. If the font is mono-spaced, this
will also be the width of all other printable characters."
    (let ((window (selected-window))
          (remapping face-remapping-alist))
      (with-temp-buffer
        (make-local-variable 'face-remapping-alist)
        (setq face-remapping-alist remapping)
        (set-window-buffer window (current-buffer))
        (insert str)
        (car (window-text-pixel-size)))))

  (cl-defun kb/mu4e-normalised-icon (name &key set color height v-adjust)
    "Convert :icon declaration to icon"
    (let* ((icon-set (intern (concat "all-the-icons-" (or set "faicon"))))
           (v-adjust (or v-adjust 0.02))
           (height (or height 0.8))
           (icon (if color
                     (apply icon-set `(,name :face ,(intern (concat "all-the-icons-" color)) :height ,height :v-adjust ,v-adjust))
                   (apply icon-set `(,name  :height ,height :v-adjust ,v-adjust))))
           (icon-width (kb/mu4e--get-string-width icon))
           (space-width (kb/mu4e--get-string-width " "))
           (space-factor (- 2 (/ (float icon-width) space-width))))
      (concat (propertize " " 'display `(space . (:width ,space-factor))) icon)))

  ;; FIXME 2022-12-29: Icons are too large...
  (defun kb/mu4e-initialise-icons ()
    (setq mu4e-headers-draft-mark      (cons "D" (kb/mu4e-normalised-icon "pencil"))
          mu4e-headers-flagged-mark    (cons "F" (kb/mu4e-normalised-icon "flag"))
          ;; mu4e-headers-new-mark        (cons "N" (kb/mu4e-normalised-icon "sync" :set "material" :height 0.8 :v-adjust -0.10))
          mu4e-headers-passed-mark     (cons "P" (kb/mu4e-normalised-icon "arrow-right"))
          mu4e-headers-replied-mark    (cons "R" (kb/mu4e-normalised-icon "arrow-right"))
          mu4e-headers-seen-mark       (cons "S" "") ;(kb/mu4e-normalised-icon "eye" :height 0.6 :v-adjust 0.07 :color "dsilver"))
          mu4e-headers-trashed-mark    (cons "T" (kb/mu4e-normalised-icon "trash"))
          mu4e-headers-attach-mark     (cons "a" (kb/mu4e-normalised-icon "file-text-o" :color "silver"))
          mu4e-headers-encrypted-mark  (cons "x" (kb/mu4e-normalised-icon "lock"))
          mu4e-headers-signed-mark     (cons "s" (kb/mu4e-normalised-icon "certificate" :height 0.7 :color "dpurple"))
          mu4e-headers-unread-mark     (cons "u" (kb/mu4e-normalised-icon "eye-slash" :v-adjust 0.05))
          ;; Mine
          mu4e-headers-new-mark        (cons "N" (kb/mu4e-normalised-icon "plus" :height 0.4))
          mu4e-headers-list-mark       (cons "s" (kb/mu4e-normalised-icon "repeat" :height 0.5))))

  ;; (add-hook 'window-setup-hook #'kb/mu4e-initialise-icons)
  )

;;;; Mu4e-column-faces
(use-package mu4e-column-faces
  :after mu4e
  :demand
  :config
  (mu4e-column-faces-mode 1))

;;;; Mu4e-views
(use-package mu4e-views
  :disabled
  :after mu4e
  :ensure-system-package wkhtmltopdf    ; HTML to PDF CLI command
  ;; This branch for support of new version of `mu'
  ;; :ensure (mu4e-views :type git :host github :repo "lordpretzel/mu4e-views" :branch "mu-1.8-support")
  :vc (:url "https://github.com/lordpretzel/mu4e-views.git"
            :rev :newest
            :branch "mu-1.8-support")
  :bind
  ( :map mu4e-headers-mode-map
    ("v" . mu4e-views-mu4e-select-view-msg-method) ; Select viewing method
    ("M-n" . mu4e-views-cursor-msg-view-window-down) ; From headers window scroll the email view
    ("M-p" . mu4e-views-cursor-msg-view-window-up) ; From headers window scroll the email view
    ("f" . mu4e-views-toggle-auto-view-selected-message) ; Toggle opening messages automatically when moving in the headers view
    ("i" . mu4e-views-mu4e-view-as-nonblocked-html)) ; Show currently selected email with all remote content
  :custom
  (mu4e-views-auto-view-selected-message nil)
  (mu4e-views-next-previous-message-behaviour 'stick-to-current-window)
  (mu4e-views-default-view-method "html")
  (mu4e-views-html-to-pdf-command "wkhtmltopdf %h %p")
  :config
  (mu4e-views-mu4e-use-view-msg-method "html"))

;;;; Message-view-patch
;; Fontify patches in message buffers
(use-package message-view-patch
  :disabled t
  :hook (gnus-part-display . message-view-patch-highlight))

(provide 'email-mu4e-rcp)
;;; email-mu4e-rcp.el ends here
