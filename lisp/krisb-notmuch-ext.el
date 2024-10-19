;;; krisb-notmuch-ext.el --- Notmuch extensions      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords: lisp, mail

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

;; Bespoke extensions to Notmuch.

;;; Code:
(require 'notmuch)

;;; Restore window configuration when closing notmuch-hello window
(defvar krisb-notmuch-hello-pre-window-conf nil)

(defun krisb-notmuch-hello-set-window-conf ()
  "Set the value of `krisb-notmuch-hello-pre-window-conf'."
  (unless (memq major-mode '(notmuch-show-mode
                             notmuch-tree-mode
                             notmuch-hello-mode
                             notmuch-search-mode
                             notmuch-message-mode))
    (setq krisb-notmuch-hello-pre-window-conf (current-window-configuration))))

;;;###autoload
(defun krisb-notmuch--around (fn &rest args)
  "Set pre-window-configuration also."
  (interactive)
  (krisb-notmuch-hello-set-window-conf)
  (apply fn args)
  ;; We delete other windows afterward just in case `notmuch' is called with
  ;; e.g. `other-frame-prefix'
  (delete-other-windows))
(advice-add 'notmuch :around #'krisb-notmuch--around)

;;;###autoload
(defun krisb-notmuch-bury-or-kill-this-buffer--around (fn &rest args)
  "Restore window configuration if appropriate."
  (interactive)
  (if (and (equal major-mode 'notmuch-hello-mode)
           krisb-notmuch-hello-pre-window-conf
           (equal (selected-frame) (window-configuration-frame krisb-notmuch-hello-pre-window-conf)))
      (progn
        (apply fn args)
        (set-window-configuration krisb-notmuch-hello-pre-window-conf)
        (setq krisb-notmuch-hello-pre-window-conf nil))
    (apply fn args)))
(advice-add 'notmuch-bury-or-kill-this-buffer :around #'krisb-notmuch-bury-or-kill-this-buffer--around)

;;; Show only unread emails in thread opened via in notmuch-show-mode
;;;###autoload
(defun krisb-notmuch-show-expand-only-unread-h ()
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
      (let ((notmuch-show-hook (remove 'krisb-notmuch-show-expand-only-unread-h notmuch-show-hook)))
        (notmuch-show-filter-thread "tag:unread")))))

;;; Open MML part in browser
;; FIXME 2024-09-26: This is a workaround. For some reason
;; `notmuch-show-view-part' opens a non-existent HTML file in the browser...
;;;###autoload
(defun krisb-notmuch-show-view-part ()
  "View part in browser."
  (notmuch-show-apply-to-current-part-handle
   (lambda (handle &optional mime-type)
     (let ((file (make-temp-file "kb-notmuch-part-" nil (when (string= mime-type "text/html") ".html")))
           (browse-url-generic-args (remove "--new-window" browse-url-generic-args))) ; This is ad-hoc: I prefer not to open in a new window
       (mm-save-part-to-file handle file)
       (browse-url file)))))
(advice-add 'notmuch-show-view-part :override #'krisb-notmuch-show-view-part)

;;; Bespoke `notmuch-show-mode' commands
;;;###autoload
(defun krisb-notmuch-show-trash-thread-then-next (&optional show)
  "\"Trash\" all messages in the current buffer, then exit thread.
If SHOW is provided (interactively by prefix-arg), then also open that
thread."
  (interactive "P")
  (notmuch-show-tag-all
   (notmuch-tag-change-list (append notmuch-archive-tags '("+trash"))))
  (notmuch-show-next-thread t show))

;;;###autoload
(defun krisb-notmuch-show-tag-thread (&optional reverse)
  "Like `notmuch-show-archive-thread' put prompt "
  (interactive "P")
  (let (current-tags)
    (notmuch-show-mapc
     (lambda () (setq current-tags (append (notmuch-show-get-tags) current-tags))))
    (notmuch-show-tag-all
     (notmuch-tag-change-list
      (notmuch-read-tag-changes current-tags)
      reverse))))

;;;###autoload
(defun krisb-notmuch-show-advance-and-tag ()
  "Like `notmuch-show-advance-and-archive' but prompt for tag instead.
Tagging is done by `krisb-notmuch-show-tag-thread'."
  (interactive)
  (when (notmuch-show-advance)
    (krisb-notmuch-show-tag-thread)
    (notmuch-show-next-thread t)))

;; Sets the style of `message's citations before sending a reply
;; TODO 2024-10-07: Try setting this automatically. If I can't, then clean up
;; the prompting of this function
(defun krisb-notmuch--set-message-citation-style (orig-func &rest args)
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
(advice-add 'notmuch-mua-new-reply :around #'krisb-notmuch--set-message-citation-style)

;;;; `notmuch-mua-reply' overide to obey `message-cite-reply-position'
(defun krisb-notmuch-mua-reply (query-string &optional sender reply-all duplicate)
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
(advice-add 'notmuch-mua-reply :override #'krisb-notmuch-mua-reply)

;;; Provide
(provide 'krisb-notmuch-ext)
;;; krisb-notmuch-ext.el ends here
