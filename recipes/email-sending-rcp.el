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
          (insert (read-string "Subject (optional): ")))))))

;;; Sendmail
;; Use `sendmail' program to send emails?
(use-package sendmail
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
  :ensure-system-package msmtp
  :custom
  (send-mail-function 'smtpmail-send-it)
  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  (smtpmail-stream-type 'starttls)
  (smtpmail-queue-mail nil)
  ;; Make sure email details that are used are not the current (when flushing)
  ;; variables, but the variables used when writing the email
  (smtpmail-store-queue-variables t)
  (smtpmail-queue-dir (expand-file-name "drafts/.smtp-queue" message-directory)))

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
  (defun kb/org-msg-set-faces ()
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
  (add-hook 'kb/themes-hooks 'kb/org-msg-set-faces)

  ;; Don't show exported buffers after sending emails. Inspired by
  ;; https://github.com/jeremy-compostella/org-msg/issues/169#issuecomment-1627375688
  (add-hook 'message-sent-hook
            #'(lambda ()
                 (when (bound-and-true-p org-msg-mode)
                   (switch-to-buffer "*Org ASCII Export*")
                   (kill-buffer-and-window)))))

;;;; Custom signatures
(with-eval-after-load 'org-msg
  (defvar kb/signature-separator "⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼"
    "Separator between email body and its signature.")
  (setq message-signature nil
        message-signature-separator (format "^%s *$" kb/signature-separator))
  (defvar kb/signature-alist
    `(("Take care" . ,(format "%s\n\nTake care,\\\\\nKristoffer" kb/signature-separator))
      ("In gratitude" . ,(format "%s\n\nIn gratitude,\\\\\nKristoffer" kb/signature-separator))
      ("Best" . ,(format "%s\n\nBest,\\\\\nKristoffer" kb/signature-separator))
      ("With appreciation" . ,(format "%s\n\nWith appreciation,\\\\\nKristoffer" kb/signature-separator))
      ("Brown banner" . ,(concat kb/signature-separator "\n\n"
                                 "With appreciation,\\\\\nKristoffer\n\n"
                                 "#+begin_export html
<br />
<table
  style='color: rgb(136, 136, 136); border: none; border-collapse: collapse; font-family: garamond'
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
#+end_export"))
      ("BUI banner" . ,(concat kb/signature-separator "\n\n"
                               "Warmly,\\\\\nBrown University Interviews Executive Committee\n\n"
                               "#+begin_export html
<br />
<table
  style='
    color: rgb(136, 136, 136);
    border: none;
    border-collapse: collapse;
    font-family: garamond;
  '
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
          src='https://browninterviews.org/wp-content/uploads/2020/06/bu-small-logo.png'
          alt='Brown logo'
          style='border: none'
          height='70'
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
          style='margin-top: 6pt; margin-bottom: 0pt; font-size: 11pt'
        >
          <span style='font-weight: 700'>Kristoffer Balintona ('24)</span>
          <span> | Editor in Chief</span>
          <br />
        </p>
        <p
          dir='ltr'
          style='margin-top: 6pt; margin-bottom: 0pt; font-size: 11pt'
        >
          <span style='font-weight: 700'>Charles Alaimo ('25)</span>
          <span> | Senior Interviews Coordinator</span>
          <br />
        </p>
        <br />
      </td>
    </tr>
  </tbody>
</table>
#+end_export")))
    "Alist of aliases and their corresponding email signatures.")

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
        message-cite-function 'message-cite-original
        message-citation-line-function 'message-insert-formatted-citation-line
        message-citation-line-format "On %d %b %Y at %R, %f wrote:\n"
        message-cite-reply-position 'above
        ;; These spaces are the magic!
        message-yank-prefix  "    "
        message-yank-cited-prefix  "    "
        message-yank-empty-prefix  "    ")

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
    "Add citations last in the email."
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

;;;; Mu4e-send-delay
(use-package mu4e-send-delay
  :demand ; So that we aren't waiting on loading `mu4e' to send scheduled messages
  :straight (:type git :host github :protocol ssh :repo "krisbalintona/mu4e-send-delay")
  :hook (mu4e-main-mode . mu4e-send-delay-setup)
  :general ([remap message-send-and-exit] 'mu4e-send-delay-send-and-exit)
  :custom
  (mu4e-send-delay-default-delay "10m")
  (mu4e-send-delay-default-hour "8")
  (mu4e-send-delay-timer 60)
  :config
  (with-eval-after-load 'org-msg
    ;; Use `mu4e-send-delay-send-and-exit' instead. Also pass prefix arg to it
    (defun kb/org-msg-ctrl-c-ctrl-c ()
      "Send message like `message-send-and-exit'.

If the current buffer is an OrgMsg buffer and OrgMsg is
enabled (see `org-msg-toggle'), it calls `message-send-and-exit'.
With the universal prefix argument, it calls `message-send'."
      (when (eq major-mode 'org-msg-edit-mode)
        (org-msg-sanity-check)
        (if current-prefix-arg
            (org-msg-mua-call 'send 'mu4e-send-delay-send-and-exit current-prefix-arg)
          (org-msg-mua-call 'send-and-exit 'mu4e-send-delay-send-and-exit current-prefix-arg))))
    (advice-add 'org-msg-ctrl-c-ctrl-c :override 'kb/org-msg-ctrl-c-ctrl-c)))

;;; email-sending-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'email-sending-rcp)
