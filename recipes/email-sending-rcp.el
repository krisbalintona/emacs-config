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
  :elpaca nil
  :hook ((message-setup . message-sort-headers)
         (message-mode . visual-fill-column-mode)
         (message-send . kb/message-check-for-subject)
         (message-send . kb/message-check-correct-from))
  :custom
  (message-directory "~/Documents/emails/")
  (message-mail-user-agent t)           ; Use `mail-user-agent'
  (compose-mail-user-agent-warnings t)
  (message-hidden-headers nil)          ; Show everything!

  (message-elide-ellipsis "\n> [... %l lines elided]\n")
  (message-signature "Kind regards,\nKristoffer\n")
  (message-signature-separator "^-- $")
  (message-signature-insert-empty-line t)
  (message-citation-line-function 'message-insert-formatted-citation-line)
  (message-ignored-cited-headers "") ; Don't include any headers when citing emails
  (message-confirm-send nil)
  (message-kill-buffer-on-exit t)
  (message-wide-reply-confirm-recipients t)
  :init
  ;; Taken from Doom. Detect empty subjects, and give users an opportunity to
  ;; fill something in
  (defun kb/message-check-for-subject ()
    "Check that a subject is present, and prompt for a subject if not."
    (save-excursion
      (goto-char (point-min))
      (search-forward "--text follows this line--")
      (re-search-backward "^Subject:")
      (let ((subject (string-trim (substring (thing-at-point 'line) 8))))
        (when (string-empty-p subject)
          (end-of-line)
          (insert (read-string "Subject (optional): "))))))

  (defun kb/message-check-correct-from ()
    "Prompt user to confirm to send from this email."
    (save-excursion
      (goto-char (point-min))
      (search-forward "--text follows this line--")
      (re-search-backward "^From:")
      (let ((from (string-trim (substring (thing-at-point 'line) 5))))
        (when (and (not (string-match-p (rx (literal user-mail-address)) from))
                   (not (yes-or-no-p (concat
                                      "Are you sure you want to send from "
                                      (propertize from 'face 'highlight)
                                      "?"))))
          (cl--set-buffer-substring (pos-bol) (pos-eol)
                                    (concat
                                     "From: "
                                     (read-string "Set FROM to: " user-mail-address))))))))

;;; Sendmail
;; Use `sendmail' program to send emails?
(use-package sendmail
  :elpaca nil
  :custom
  ;; If I want to use `sendmail' over `msmtp'/`smtpmail'
  (send-mail-function 'sendmail-send-it)
  (sendmail-program (executable-find "sendmail"))
  (mail-default-directory (progn (require 'message)
                                 (expand-file-name "drafts/" message-directory)))
  ;; These two messages make sure that emails are sent from the email address
  ;; specified in the "from" header field! Taken from
  ;; https://jonathanchu.is/posts/emacs-notmuch-isync-msmtp-setup/
  (mail-specify-envelope-from t)
  (message-sendmail-envelope-from 'header)
  (mail-envelope-from 'header))

;;; Smtpmail
;; Use `msmtp' program to send emails?
(use-package smtpmail
  :elpaca nil
  :ensure-system-package msmtp
  :custom
  ;; I set this in my sendmail configuration too so that if smtpmail isn't use,
  ;; the above configuration works still
  (send-mail-function 'smtpmail-send-it)
  (smtpmail-queue-mail nil)
  ;; Below are settings for Gmail. See
  ;; https://support.google.com/mail/answer/7126229?hl=en#zippy=%2Cstep-change-smtp-other-settings-in-your-email-client
  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  (smtpmail-stream-type 'starttls)
  ;; Make sure email details that are used are not the current (when flushing)
  ;; variables, but the variables used when writing the email
  (smtpmail-store-queue-variables t)
  (smtpmail-queue-dir (expand-file-name "drafts/.smtp-queue" message-directory)))

;;; Org-msg
;;;; Itself
;; Using org-mode to compose HTML-friendly emails
(use-package org-msg
  :elpaca (org-msg :type git :host github :repo "jeremy-compostella/org-msg")
  :hook ((org-msg-edit-mode . (lambda ()
                                (setq-local org-download-method 'directory
                                            org-download-image-dir mu4e-attachment-dir)))
         ;; Don't show exported buffers after sending emails. Inspired by
         ;; https://github.com/jeremy-compostella/org-msg/issues/169#issuecomment-1627375688
         (message-sent . (lambda ()
                           (when (bound-and-true-p org-msg-mode)
                             (switch-to-buffer "*Org ASCII Export*")
                             (kill-buffer-and-window)))))
  :general
  ;; Get access to the `message' header editing commands in `org-msg-edit-mode'
  (:keymaps 'org-msg-edit-mode-map
   :prefix "C-c C-m"
   "C-t" 'message-goto-to
   "C-s" 'message-goto-subject
   "C-c" 'message-goto-cc
   "C-b" 'message-goto-bcc
   "C-r" 'message-goto-reply-to
   "C-f" 'message-goto-followup-to
   "C-w" 'message-goto-fcc)
  (:keymaps 'org-msg-edit-mode-map
   ;; This keybinding is fine since we wouldn't want to call `org-refile' anyway
   "C-c C-w" 'kb/mu4e-insert-signature)
  :custom
  (mu4e-compose-signature-auto-include nil)
  (org-msg-options "html-postamble:nil toc:nil author:nil email:nil \\n:t")
  (org-msg-startup "hidestars indent inlineimages hideblocks")
  (org-msg-greeting-fmt nil)
  (org-msg-greeting-name-limit 1)
  (org-msg-default-alternatives
   '((new . (text html))
     (reply-to-html . (text html))
     (reply-to-text . (text))))
  (org-msg-convert-citation t)
  (org-msg-attached-file-reference      ; Taken from Doom
   "see[ \t\n]\\(?:the[ \t\n]\\)?\\(?:\\w+[ \t\n]\\)\\{0,3\\}\\(?:attached\\|enclosed\\)\\|\
(\\(?:attached\\|enclosed\\))\\|\
\\(?:attached\\|enclosed\\)[ \t\n]\\(?:for\\|is\\)[ \t\n]")
  ;; Settings for Gmail-formatted HTML citations
  (org-msg-posting-style 'gmail) ; My own value which I leverage in `kb/org-msg-post-setup'
  (message-cite-function 'message-cite-original-without-signature)
  (message-citation-line-function 'message-insert-formatted-citation-line)
  (message-citation-line-format "On %a, %b %d, %Y at %-I:%M %p %f wrote:\n")
  ;; CSS for emails. Taken initially from Doom Emacs then modified.
  (org-msg-enforce-css
   ;; Avoid styling that applies to all blockquotes (i.e. (blockquotes nil ...))
   ;; and blockquotes whose class is gmail_quote since this overrides the
   ;; styling we do in `kb/org-msg--html-special-block' which sets the class to
   ;; gmail_quote and adds styling. We style there rather than here since we
   ;; cannot add both a class and style property; the class property is
   ;; overwritten if we use `org-msg-enforce-css'.
   (let* ((font-family '(font-family . "-apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Oxygen, Ubuntu, Cantarell,\
        \"Fira Sans\", \"Droid Sans\", \"Helvetica Neue\", Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\";"))
          (monospace-font '(font-family . "SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace;"))
          (font-size '(font-size . "10pt"))
          (font `(,font-family ,font-size))
          (line-height '(line-height . "1.1"))
          (theme-color "#000")
          (bold '(font-weight . "bold"))
          (color `(color . ,theme-color))
          (table `((margin-top . "6px") (margin-bottom . "6px")
                   (border-left . "none") (border-right . "none")
                   (border-top . "2px solid #222222")
                   (border-bottom . "2px solid #222222")))
          (ftl-number `(,color ,bold (text-align . "left")))
          (inline-modes '(asl c c++ conf cpp csv diff ditaa emacs-lisp
                              fundamental ini json makefile man org plantuml
                              python sh xml))
          (inline-src `((background-color . "rgba(27,31,35,.05)")
                        (border-radius . "3px")
                        (padding . ".2em .4em")
                        (font-size . "90%") ,monospace-font
                        (margin . 0)))
          (code-src
           (mapcar (lambda (mode)
                     `(code ,(intern (concat "src src-" (symbol-name mode)))
                            ,inline-src))
                   inline-modes))
          (base-quote '((padding-left . "5px") (margin-left . "10px")
                        (margin-top . "20px") (margin-bottom . "0")))
          (quote-palette '("#6A8FBF" "#bf8f6a" "#6abf8a" "#906abf"
                           "#6aaebf" "#bf736a" "#bfb66a" "#bf6a94"
                           "#6abf9b" "#bf6a7d" "#acbf6a" "#6a74bf"))
          (quotes                   ; Styles divs with class quote1, quote2, ...
           (mapcar (lambda (x)
                     (let ((c (nth x quote-palette)))
                       `(div ,(intern (format "quote%d" (1+ x)))
                             (,@base-quote
                              (color . ,c)
                              (border-left . ,(concat "3px solid "
                                                      (org-msg-lighten c)))))))
                   ;; Begin at 1 since I set quote0 manually below, which is the
                   ;; class for all quote blocks. See
                   ;; `org-msg--html-quote-block'
                   (number-sequence 1 (1- (length quote-palette))))))
     `((del nil ((color . "grey") (border-left . "none")
                 (text-decoration . "line-through") (margin-bottom . "0px")
                 (margin-top . "10px") (line-height . "11pt")))
       (a nil (,color))
       (a reply-header ((color . "black") (text-decoration . "none")))
       (div reply-header ((padding . "3.0pt 0in 0in 0in")
                          (border-top . "solid #e1e1e1 1.0pt")
                          (margin-bottom . "20px")))
       (span underline ((text-decoration . "underline")))
       (li nil (,line-height (margin-bottom . "0px")
                             (margin-top . "2px")
                             (max-width . "47em")))
       (nil org-ul ((list-style-type . "disc")))
       (nil org-ol (,@font ,line-height (margin-bottom . "0px")
                           (margin-top . "0px") (margin-left . "30px")
                           (padding-top . "0px") (padding-left . "5px")))
       (nil signature (,@font (margin-bottom . "20px")))
       (blockquote quote0 ,(append base-quote '((border-left . "3px solid #ccc")
                                                (padding-bottom . "2px"))))
       ,@quotes
       (p blockquote  ((margin . "0") (padding . "4px 0")))
       (code nil (,font-size ,monospace-font (background . "#f9f9f9")))
       ,@code-src
       (nil linenr ((padding-right . "1em")
                    (color . "black")
                    (background-color . "#aaaaaa")))
       (pre nil ((line-height . "1.2")
                 (color . ,(face-foreground 'default))
                 (background-color . ,(face-background 'default))
                 (margin . "4px 0px 8px 0px")
                 (padding . "8px 12px")
                 (width . "max-content")
                 (min-width . "50em")
                 (border-radius . "5px")
                 (font-size . "0.9em")
                 (font-weight . "500")
                 ,monospace-font))
       (div org-src-container ((margin-top . "10px")))
       (nil figure-number ,ftl-number)
       (nil table-number)
       (caption nil ((text-align . "left")
                     (background . ,theme-color)
                     (color . "white")
                     ,bold))
       (nil t-above ((caption-side . "top")))
       (nil t-bottom ((caption-side . "bottom")))
       (nil listing-number ,ftl-number)
       (nil figure ,ftl-number)
       (nil org-src-name ,ftl-number)
       (img nil ((vertical-align . "middle")
                 (max-width . "100%")))
       (img latex-fragment-inline ((margin . "0 0.1em")))
       (table nil (,@table ,line-height (border-collapse . "collapse")))
       (th nil ((border . "none") (border-bottom . "1px solid #222222")
                (background-color . "#EDEDED") (font-weight . "500")
                (padding . "3px 10px")))
       (td nil (,@table (padding . "1px 10px")
                        (background-color . "#f9f9f9") (border . "none")))
       (td org-left ((text-align . "left")))

       (td org-center ((text-align . "center")))
       (kbd nil ((border . "1px solid #d1d5da") (border-radius . "3px")
                 (box-shadow . "inset 0 -1px 0 #d1d5da")
                 (background-color . "#fafbfc") (color . "#444d56")
                 (font-size . "0.85em")
                 (padding . "1px 4px") (display . "inline-block")))
       (div outline-text-4 ((margin-left . "15px")))
       (div outline-4 ((margin-left . "10px")))
       (h4 nil ((margin-bottom . "0px") (font-size . "11pt")))
       (h3 nil ((margin-bottom . "0px")
                (font-size . "14pt")))
       (h2 nil ((margin-top . "20px") (margin-bottom . "20px")
                (font-size . "18pt")))
       (h1 nil ((margin-top . "20px") (margin-bottom . "0px")
                (font-size . "24pt")))
       (p nil ((text-decoration . "none") (line-height . "1.4")
               (margin-top . "10px") (margin-bottom . "0px")
               ,font-size))
       ;; Applies to entire body
       (div ,(intern org-html-content-class) (,@font (line-height . "12pt")))))))

;;;; Custom signatures
(with-eval-after-load 'org-msg
  (defvar kb/signature-separator "⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼"
    "Separator between email body and its signature.")
  (defvar kb/signature-open "\n\n#+begin_signature\n"
    "String meant to begin email signatures.")
  (defvar kb/signature-close "\n#+end_signature"
    "String meant to end email signatures.")
  (setq message-signature-separator (format "^%s *" (read kb/signature-separator)))
  (defvar kb/signature-alist nil
    "Alist of aliases and their corresponding email signatures.")

  (defun kb/mu4e-select-signature (alias)
    "Select one of the signatures from `kb/signature-alist'."
    (interactive (list (completing-read
                        "Insert signature: "
                        (cl-loop for (key . value) in kb/signature-alist
                                 collect key))))
    (or (alist-get alias kb/signature-alist nil nil #'string=)
        ;; If a signature was manually typed rather than an alias chosen.
        ;; Example: if "Test" is typed, the result will be:
        ;; "#+begin_signature (kb/signature-open)
        ;; ⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼  (kb/signature-separator)
        ;; Test,
        ;; Kristoffer
        ;; #+end_signature    (kb/signature-close)"
        (format "%s%s\n%s%s%s"
                kb/signature-open
                kb/signature-separator
                alias
                ",\nKristoffer"
                kb/signature-close)))
  (defun kb/mu4e-insert-signature ()
    "Insert a selection from `kb/signature-alist'.

Replaces existing signature if present in buffer. Relies on
signatures being wrapped in `kb/signature-open' and
`kb/signature-close'."
    (interactive)
    (save-excursion
      (let ((sig (call-interactively 'kb/mu4e-select-signature))
            (existing-sig-beg
             (save-excursion
               (save-match-data
                 (goto-char (point-min))
                 (when (search-forward kb/signature-open nil t)
                   (match-beginning 0)))))
            (existing-sig-end
             (save-excursion
               (save-match-data
                 (goto-char (point-min))
                 (search-forward kb/signature-close nil t)))))
        (if (and existing-sig-beg existing-sig-end)
            ;; Replace existing signature
            (progn
              (goto-char existing-sig-beg)
              (delete-region existing-sig-beg existing-sig-end)
              (insert sig))
          ;; Remove leading whitespace from sig if inserting
          (insert (string-trim-left sig)))))
    ;; Change email signature separator to the conventional "--" for text-only
    ;; emails
    (when (and (equal major-mode 'org-msg-edit-mode)
               (equal (org-msg-get-prop "alternatives")
                      '(text)))
      (save-excursion
        (goto-char (point-min))
        (when (search-forward kb/signature-separator nil t)
          (replace-match "--" 1))))))

;;;; Custom creation of `org-msg' buffer
(with-eval-after-load 'org-msg
  (defun kb/org-msg--html-special-block (special-block contents info)
    "Similar to `org-html-special-block' but treat specially the
blocks of type \"quote...\" generated by `org-msg-ascii-blockquote'."
    (let ((block-type (org-element-property :type special-block)))
      (cond
       ((string-match "quote[0-9]+" block-type)
        (let* ((contents (or contents ""))
               (a (org-html--make-attribute-string
                   '(:class "gmail_quote"
                     :style "margin:0 0 0.8ex;border-left:1px #ccc solid;padding-left:1ex"))))
          (format "<blockquote %s>\n%s\n</blockquote>" a contents)))
       (t (org-html-special-block special-block contents info)))))
  (advice-add 'org-msg--html-special-block :override 'kb/org-msg--html-special-block)

  (defun kb/org-msg-composition-parameters (type alternatives)
    "Return the posting-style, greeting format and signature.
TYPE is a one of the keys of `org-msg-default-alternatives'.
ALTERNATIVES is a list of alternative symbols included as defined
in `org-msg-alternative-exporters'.

This function returns the value of the `org-msg-posting-style',
`org-msg-greeting-fmt' and `org-msg-posting-style' customization
variables as an association list with `style', `greeting-fmt' and
`signature' as their respective keys. The goal of this function
is to offer a anchor point for advanced configuration: it can be
advised to implement more complex behaviors such as change the
signature and posting style when replying to a particular mail
address or tweak the signature when replying with plain text
email."
    `((style . ,(when (and (eq type 'reply-to-html)
                           (memq 'html alternatives)
                           (not (= (point) (point-max)))
                           ;; NOTE 2023-08-18: I have commented the line below
                           ;; in order to allow attachments to be forwarded.
                           ;; However, I haven't tested this well so I'm not
                           ;; 100% sure if this is okay, especially since OrgMsg
                           ;; says that MML tags are not supported, supposedly.
                           ;; (not (org-msg-has-mml-tags))
                           )
                  org-msg-posting-style))
      (greeting-fmt . ,org-msg-greeting-fmt)
      (signature . ,(unless (save-excursion ; Don't interactively insert signature if one already present
                              (save-match-data
                                (goto-char (point-min))
                                (re-search-forward message-signature-separator nil t)))
                      (call-interactively 'kb/mu4e-select-signature)))))
  (advice-add 'org-msg-composition-parameters :override 'kb/org-msg-composition-parameters)

  (defun kb/org-msg-post-setup (&rest _args)
    "Transform the current `message' buffer into a OrgMsg buffer.
If the current `message' buffer is a reply, the
`org-msg-separator' string is inserted at the end of the editing
area. If the current buffer contains MML tags,
`org-msg-edit-mode' is not activated as OrgMsg does not support
MML tags."
    (unless (eq major-mode 'org-msg-edit-mode)
      (message-goto-body)
      (let* ((type (cond ((not (org-msg-message-fetch-field "subject")) 'new)
                         ((org-msg-mua-call 'article-htmlp) 'reply-to-html)
                         ('reply-to-text)))
             (alternatives (org-msg-get-alternatives type))
             ;; Insert the value of `message-citation-line-format' into a
             ;; "gmail_attr" org block
             (message-citation-line-format (concat "#+begin_gmail_attr\n"
                                                   message-citation-line-format
                                                   "#+end_gmail_attr\n")))
        (when alternatives
          (let-alist (org-msg-composition-parameters type alternatives)
            (unless (search-forward org-msg-options nil t)
              (insert (org-msg-header (when (eq .style 'top-posting)
                                        (org-msg-mua-call 'save-article-for-reply))
                                      alternatives))
              (when .greeting-fmt
                (insert (format .greeting-fmt
                                (if (eq type 'new)
                                    ""
                                  (concat " " (org-msg-get-to-name))))))
              ;; Get a chance to modify the inserted citation according to
              ;; `org-msg-posting-style'
              (save-excursion
                (pcase .style
                  ('top-posting
                   (insert "\n\n" org-msg-separator "\n")
                   (delete-region (line-beginning-position) (1+ (line-end-position)))
                   (dolist (rep '(("^>+ *" . "") ("___+" . "---")))
                     (save-excursion
                       (while (re-search-forward (car rep) nil t)
                         (replace-match (cdr rep)))))
                   (org-escape-code-in-region (point) (point-max)))
                  ('gmail
                   (insert"#+begin_gmail_quote")
                   (goto-char (point-max))
                   (insert"#+end_gmail_quote"))))
              ;; Insert signature at the proper place according to
              ;; `org-msg-posting-style'
              (when .signature
                (pcase .style
                  ('top-posting
                   (insert .signature))
                  ('gmail
                   (insert .signature "\n\n"))
                  (t
                   (goto-char (point-max))
                   (save-excursion (insert .signature))
                   ;; Convenient formatting: Ensure only one blank line
                   ;; separates the email signature from the email citation
                   (delete-blank-lines)))))
            (if (org-msg-message-fetch-field "to")
                (org-msg-goto-body)
              (message-goto-to))
            (org-msg-edit-mode))
          (set-buffer-modified-p nil)))))
  (advice-add 'org-msg-post-setup :override 'kb/org-msg-post-setup))

;;; Mu4e-send-delay
(use-package mu4e-send-delay
  :demand ; So that we aren't waiting on loading `mu4e' to send scheduled messages
  :elpaca (:type git
           :host github
           :protocol ssh
           :repo "krisbalintona/mu4e-send-delay"
           :depth nil)
  :hook (mu4e-main-mode . mu4e-send-delay-setup)
  :general ([remap message-send-and-exit] 'mu4e-send-delay-send-and-exit)
  :custom
  (mu4e-send-delay-default-delay "10m")
  (mu4e-send-delay-default-hour "8")
  (mu4e-send-delay-timer 60)
  (mu4e-send-delay-enable-org-msg t))

;;; email-sending-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'email-sending-rcp)
