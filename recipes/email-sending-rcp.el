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
  :hook ((message-setup . message-sort-headers)
         (message-mode . visual-fill-column-mode)
         (message-send . kb/message-check-for-subject))
  :custom
  (message-directory "~/Documents/emails/")
  (message-mail-user-agent t)           ; Use `mail-user-agent'
  (compose-mail-user-agent-warnings t)
  (message-hidden-headers nil)          ; Show everything!

  (message-elide-ellipsis "\n> [... %l lines elided]\n")
  (message-signature "⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼\nKind regards,\nKristoffer\n")
  (message-signature-insert-empty-line t)
  (message-citation-line-function #'message-insert-formatted-citation-line)
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
      (re-search-backward "^Subject:") ; this should be present no matter what
      (let ((subject (string-trim (substring (thing-at-point 'line) 8))))
        (when (string-empty-p subject)
          (end-of-line)
          (insert (read-string "Subject (optional): "))
          (message "Sending..."))))))

;;; Sendmail
;; Use `sendmail' program to send emails?
(use-package sendmail
  :custom
  ;; If I want to use `sendmail' over `msmtp'/`smtpmail'
  (send-mail-function 'sendmail-send-it)
  (sendmail-program (executable-find "sendmail"))
  (mail-default-directory (expand-file-name "drafts/" message-directory))
  ;; These two messages make sure that emails are sent from the email address
  ;; specified in the "from" header field!
  ;; (mail-specify-envelope-from t)
  (mail-specify-envelope-from nil)
  (mail-envelope-from 'header))

;;; Smtpmail
;; Use `msmtp' program to send emails?
(use-package smtpmail
  :ensure-system-package (msmtp)
  :custom
  ;; (send-mail-function 'smtpmail-send-it)
  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  (smtpmail-stream-type 'starttls)
  (smtpmail-queue-mail nil)
  :config
  (if (eq send-mail-function 'smtpmail-send-it)
      (setq smtpmail-queue-dir
            (file-name-as-directory (expand-file-name "/drafts/.smtp-queue" message-directory)))
    (setq smtpmail-queue-dir "")))

;;; Org-msg
;;;; Itself
;; Using org-mode to compose HTML-friendly emails
(use-package org-msg
  :straight (org-msg :type git :host github :repo "jeremy-compostella/org-msg")
  :hook (org-msg-edit-mode . (lambda ()
                               (setq-local org-download-method 'directory
                                           org-download-image-dir mu4e-attachment-dir)))
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
   "C-c C-w" 'kb/mu4e-insert-signature)
  :custom
  (org-msg-options "html-postamble:nil toc:nil author:nil email:nil")
  (org-msg-startup "hidestars indent inlineimages hideblocks")
  (org-msg-greeting-fmt nil)
  (org-msg-greeting-name-limit 1)
  (org-msg-default-alternatives
   '((new . (text html))
     (reply-to-html . (text html))
     (reply-to-text . (text html))))
  (org-msg-convert-citation t)
  ;; Taken from Doom
  (org-msg-attached-file-reference
   "see[ \t\n]\\(?:the[ \t\n]\\)?\\(?:\\w+[ \t\n]\\)\\{0,3\\}\\(?:attached\\|enclosed\\)\\|\
(\\(?:attached\\|enclosed\\))\\|\
\\(?:attached\\|enclosed\\)[ \t\n]\\(?:for\\|is\\)[ \t\n]")
  :config
  ;; Copied from Doom. Influences the foreground color of hyperlinks (used to
  ;; also be applied to headline foregrounds).
  (defvar kb/org-msg-accent-color (face-attribute 'link :foreground nil t)
    "Accent color to use in org-msg's generated CSS.
Must be set before org-msg is loaded to take effect.")
  (setq org-msg-enforce-css
        (let* ((font-family '(font-family . "-apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Oxygen, Ubuntu, Cantarell,\
        \"Fira Sans\", \"Droid Sans\", \"Helvetica Neue\", Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\";"))
               (monospace-font '(font-family . "SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace;"))
               (font-size '(font-size . "11pt"))
               (font `(,font-family ,font-size))
               (line-height '(line-height . "1.2"))
               (theme-color kb/org-msg-accent-color)
               (bold '(font-weight . "bold"))
               (color `(color . ,theme-color))
               (table `((margin-top . "6px") (margin-bottom . "6px")
                        (border-left . "none") (border-right . "none")
                        (border-top . "2px solid #222222")
                        (border-bottom . "2px solid #222222")
                        ))
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
                             (margin-top . "20px") (margin-bottom . "0")
                             (font-style . "italic") (background . "#f9f9f9")))
               (quote-palette '("#6A8FBF" "#bf8f6a" "#6abf8a" "#906abf"
                                "#6aaebf" "#bf736a" "#bfb66a" "#bf6a94"
                                "#6abf9b" "#bf6a7d" "#acbf6a" "#6a74bf"))
               (quotes
                (mapcar (lambda (x)
                          (let ((c (nth x quote-palette)))
                            `(div ,(intern (format "quote%d" (1+ x)))
                                  (,@base-quote
                                   (color . ,c)
                                   (border-left . ,(concat "3px solid "
                                                           (org-msg-lighten c)))))))
                        (number-sequence 0 (1- (length quote-palette))))))
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
            (blockquote nil ((padding . "2px 12px") (margin-left . "10px")
                             (margin-top . "10px") (margin-bottom . "0")
                             (border-left . "3px solid #ccc")
                             (font-style . "italic")
                             (background . "#f9f9f9")))
            (p blockquote  ((margin . "0") (padding . "4px 0")))
            ,@quotes
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
                    ,font-size (max-width . "50em")))
            (b nil ((font-weight . "500") (color . ,theme-color)))
            (div nil (,@font (line-height . "12pt")))))))

;;;; Custom signatures
(with-eval-after-load 'org-msg
  (defvar kb/signature-separator "⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼"
    "Separator between email body and its signature.")
  (setq message-signature nil
        message-signature-separator (format "^%s *$" kb/signature-separator))
  (defvar kb/signature-alist
    `(("Take care" . ,(format "%s\n\nTake care,\n\nKristoffer" kb/signature-separator))
      ("In gratitude" . ,(format "%s\n\nIn gratitude,\n\nKristoffer" kb/signature-separator))
      ("With appreciation" . ,(format "%s\n\nWith appreciation,\n\nKristoffer" kb/signature-separator))
      ("Brown banner" . ,(concat kb/signature-separator "\n\n"
                                 "With appreciation,\n\nKristoffer\n\n"
                                 "#+begin_export html
<br />
<table
  style='color: rgb(136, 136, 136); border: none; border-collapse: collapse'
>
  <tbody>
    <tr style='height: 81.25pt'>
      <td
        style='
          border-right: 0.75pt dotted rgb(135, 127, 116);
          vertical-align: top;
          padding: 5pt 11pt 5pt 5pt;
        '
        title=''
      >
        <img
          src='https://clipground.com/images/brown-university-logo-png-1.png'
          alt='Brown logo'
          style='border: none'
          height='100'
        />
      </td>
      <td
        style='
          border-left: 0.75pt dotted rgb(135, 127, 116);
          vertical-align: top;
          padding: 5pt 5pt 5pt 11pt;
        '
      >
        <p
          dir='ltr'
          style='line-height: 1.38; margin-top: 6pt; margin-bottom: 0pt'
        >
          <span
            style='
              font-family: garamond;
              font-size: 11pt;
              font-weight: 700;
              white-space: pre-wrap;
            '
            >Kristoffer Balintona</span
          >
          <br />
        </p>
        <p
          dir='ltr'
          style='line-height: 1.38; margin-top: 0pt; margin-bottom: 0pt'
        >
          <span
            style='
              font-size: 10pt;
              font-family: garamond;
              vertical-align: baseline;
              white-space: pre-wrap;
            '
            >B.A. Philosophy</span
          >
          <br />
        </p>
        <p
          dir='ltr'
          style='line-height: 1.38; margin-top: 0pt; margin-bottom: 0pt'
        >
          <span
            style='
              font-size: 10pt;
              font-family: garamond;
              vertical-align: baseline;
              white-space: pre-wrap;
            '
            >Class of 2024</span
          >
        </p>
        <p
          dir='ltr'
          style='line-height: 1.38; margin-top: 0pt; margin-bottom: 0pt'
        >
          <span
            style='
              font-family: garamond;
              font-size: 10pt;
              white-space: pre-wrap;
            '
            >Tel: (773) 677-9699</span
          >
          <br />
        </p>
        <p
          dir='ltr'
          style='
            font-size: 10pt;
            line-height: 1.2;
            margin-top: 0pt;
            margin-bottom: 0pt;
          '
        >
          <span
            style='
              font-size: 10pt;
              font-family: garamond;
              vertical-align: baseline;
              white-space: pre-wrap;
            '
            >Box: 6327</span
          >
        </p>
        <br />
      </td>
    </tr>
  </tbody>
</table>
#+end_export")))
    "Alist of aliases their corresponding email signatures.")

  (defun kb/mu4e-select-signature (alias)
    "Select one of the signatures from `kb/signature-alist'."
    (interactive (list (completing-read
                        "Insert signature:"
                        (cl-loop for (key . value) in kb/signature-alist
                                 collect key))))
    (let ((sig (concat "#+begin_signature\n"
                       (or (alist-get alias kb/signature-alist nil nil #'string=) "")
                       "\n#+end_signature")))
      (setq-local org-msg-signature sig)
      sig))
  (defun kb/mu4e-insert-signature ()
    "Insert a selection from `kb/signature-alist'."
    (interactive)
    (insert (call-interactively 'kb/mu4e-select-signature))))

;;;; Custom creation of `org-msg' buffer
(with-eval-after-load 'org-msg
  ;; This makes email citations buttonized in the Gmail interface
  (setq org-msg-posting-style 'gmail
        message-cite-style message-cite-style-gmail
        message-cite-function 'message-cite-original-without-signature
        message-citation-line-function 'message-insert-formatted-citation-line
        message-citation-line-format "On %e %B %Y %R, %f wrote:\n"
        message-cite-reply-position 'above
        ;; These spaces are the magic!
        message-yank-prefix  "    "
        message-yank-cited-prefix  "    "
        message-yank-empty-prefix  "    ")

  (defun kb/org-msg-export-as-html (str)
    "Transform the Org STR into html.

Specially exports verse org-blocks as processed plain text."
    (prog2
        (org-export-define-derived-backend 'org-msg-html 'html
          :translate-alist `((special-block . org-msg--html-special-block)
                             (quote-block . org-msg--html-quote-block)
                             (verse-block . (lambda (verse-block contents info)
                                              contents)) ; Export contents raw
                             ,@(org-export-get-all-transcoders 'html)))
        (org-msg-xml-to-str (org-msg-build str))
      (setq org-export-registered-backends
            (cl-delete-if (apply-partially 'eq 'org-msg-html)
                          org-export-registered-backends
                          :key 'org-export-backend-name))))
  (advice-add 'org-msg-export-as-html :override 'kb/org-msg-export-as-html)

  (defun kb/org-msg-composition-parameters (type alternatives)
    "My won composition parameter settings.

Always use return `style' as `org-msg-posting-style' if its value
is `gmail'. See `kb/org-msg-export-as-html' for why this is.

Interactively select signature via `kb/mu4e-select-signature'."
    `((style . ,(if (equal org-msg-posting-style 'gmail)
                    'gmail
                  (when (and (eq type 'reply-to-html)
                             (memq 'html alternatives)
                             (not (= (point) (point-max)))
                             (not (org-msg-has-mml-tags)))
                    org-msg-posting-style)))
      (greeting-fmt . ,org-msg-greeting-fmt)
      (signature . ,(unless (save-excursion ; Don't interactively select signature if one already present
                              (save-match-data
                                (goto-char (point-min))
                                (search-forward "#+begin_signature" nil t)))
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
             (alternatives (org-msg-get-alternatives type)))
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
              ;; Where I customize how and when a signature is inserted
              (when message-signature-insert-empty-line
                (insert "\n\n"))
              (when .signature
                (insert .signature))
              ;; For Gmail-formatted citations
              (when (and (not (eq type 'new)) (eq .style 'gmail))
                (insert "\n")
                (insert"\n#+begin_verse\n")
                (delete-char 1)         ; Remove following (empty) line
                (save-excursion
                  (goto-char (point-max))
                  (delete-char -2)      ; Delete two empty lines
                  (insert"\n#+end_verse")))
              ;; The default top-posting org-msg citation style
              (when (eq .style 'top-posting)
                (save-excursion
                  (insert "\n\n" org-msg-separator "\n")
                  (delete-region (line-beginning-position) (1+ (line-end-position)))
                  (dolist (rep '(("^>+ *" . "") ("___+" . "---")))
                    (save-excursion
                      (while (re-search-forward (car rep) nil t)
                        (replace-match (cdr rep)))))
                  (org-escape-code-in-region (point) (point-max))))
              (unless (eq .style 'top-posting)
                (goto-char (point-max))))
            (if (org-msg-message-fetch-field "to")
                (org-msg-goto-body)
              (message-goto-to))
            (org-msg-edit-mode))
          (set-buffer-modified-p nil)))))
  (advice-add 'org-msg-post-setup :override 'kb/org-msg-post-setup))

;;; email-sending-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'email-sending-rcp)
