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
         (message-send . kb/mu4e-check-for-subject))
  :custom
  (message-directory "~/Documents/emails/")
  (message-mail-user-agent t)           ; Use `mail-user-agent'
  (compose-mail-user-agent-warnings t)

  (message-elide-ellipsis "\n> [... %l lines elided]\n")
  (message-signature "⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼\nKind regards,\nKristoffer\n")
  (message-signature-insert-empty-line t)
  (message-citation-line-function #'message-insert-formatted-citation-line)
  (message-citation-line-format (concat "> From: %f\n"
                                        "> Date: %a, %e %b %Y %T %z\n"
                                        ">"))
  (message-ignored-cited-headers nil)   ; Default is "." for all headers
  (message-confirm-send nil)
  (message-kill-buffer-on-exit t)
  (message-wide-reply-confirm-recipients t)
  (message-sendmail-envelope-from 'header)

  ;; TODO 2022-12-26: Revisit these two variables
  ;; (message-sendmail-extra-arguments '("--read-envelope-from")) ; Tell msmtp to choose the SMTP server according to the from field in the outgoing email
  ;; (message-sendmail-f-is-evil 't)
  :init
  ;; Taken from Doom. Detect empty subjects, and give users an opotunity to fill
  ;; something in
  (defun kb/mu4e-check-for-subject ()
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

  (mail-specify-envelope-from t))

;;; Smtpmail
;; Use `msmtp' program to send emails?
(use-package smtpmail
  :ensure-system-package (msmtp)
  :custom
  (send-mail-function 'smtpmail-send-it)

  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  (smtpmail-stream-type 'starttls)
  (smtpmail-queue-mail nil)
  (smtpmail-queue-dir  "~/Documents/emails/.smtp-queue/"))

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
  :custom
  (org-msg-options "html-postamble:nil toc:nil author:nil email:nil")
  (org-msg-startup "hidestars indent inlineimages hideblocks")
  (org-msg-greeting-fmt nil)
  (org-msg-greeting-name-limit 1)
  (org-msg-default-alternatives
   '((new . (text html))
     (reply-to-html . (text html))
     (reply-to-text . (text))))
  (org-msg-convert-citation t)
  ;; Taken from Doom
  (org-msg-attached-file-reference
   "see[ \t\n]\\(?:the[ \t\n]\\)?\\(?:\\w+[ \t\n]\\)\\{0,3\\}\\(?:attached\\|enclosed\\)\\|\
(\\(?:attached\\|enclosed\\))\\|\
\\(?:attached\\|enclosed\\)[ \t\n]\\(?:for\\|is\\)[ \t\n]")
  :config
  ;; Copied from Doom. I know this influences at least the foreground color of
  ;; hyperlinks.
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
                     ,color (font-size . "14pt")))
            (h2 nil ((margin-top . "20px") (margin-bottom . "20px")
                     ,color (font-size . "18pt")))
            (h1 nil ((margin-top . "20px") (margin-bottom . "0px")
                     ,color (font-size . "24pt")))
            (p nil ((text-decoration . "none") (line-height . "1.4")
                    (margin-top . "10px") (margin-bottom . "0px")
                    ,font-size (max-width . "50em")))
            (b nil ((font-weight . "500") (color . ,theme-color)))
            (div nil (,@font (line-height . "12pt")))))))

;;;; Signature
(with-eval-after-load 'org-msg
  (setq message-signature nil
        message-signature-separator "^⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼ *$"
        org-msg-signature "
#+begin_signature
⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼

In gratitude,

Kristoffer

#+begin_export html
   <br>
      <table style='color:rgb(136,136,136);border:none;border-collapse:collapse'>
        <tbody>
          <tr style='height:81.25pt'>
            <td style='border-right: 0.75pt dotted rgb(135, 127, 116); vertical-align: top; padding: 5pt 11pt 5pt 5pt;' title=''>
              <img src='https://clipground.com/images/brown-university-logo-png-1.png' alt='Brown logo' style='border:none;' height='100'>
            </td>
            <td style='border-left:0.75pt dotted rgb(135,127,116);vertical-align:top;padding:5pt 5pt 5pt 11pt'>
              <p dir='ltr' style='line-height:1.38;margin-top:6pt;margin-bottom:0pt'>
                <span style='color:rgb(0,0,0);font-family:&quot;Times New Roman&quot;;font-size:11pt;font-weight:700;white-space:pre-wrap'>Kristoffer Balintona</span>
                <br>
              </p>
              <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt'>
                <span style='font-size:10pt;font-family:&quot;Times New Roman&quot;;color:rgb(0,0,0);vertical-align:baseline;white-space:pre-wrap'>B.A. Philosophy</span>
                <br>
              </p>
              <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt'>
                <span style='font-size:10pt;font-family:&quot;Times New Roman&quot;;color:rgb(0,0,0);vertical-align:baseline;white-space:pre-wrap'>Class of 2024</span>
              </p>
              <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt'>
                <span style='color:rgb(153,153,153);font-family:garamond;font-size:8pt;white-space:pre-wrap'>Tel: (773) 677-9699</span>
                <br>
              </p>
              <p dir='ltr' style='color:rgb(34,34,34);font-size:12.8px;line-height:1.2;margin-top:0pt;margin-bottom:0pt'>
                <span style='font-size:8pt;font-family:garamond;color:rgb(153,153,153);vertical-align:baseline;white-space:pre-wrap'>Box: 6327</span>
                <span style='font-size:8pt;font-family:garamond;color:rgb(136,136,136);vertical-align:baseline;white-space:pre-wrap'> &nbsp;
                </span>
              </p>
            </td>
          </tr>
        </tbody>
      </table>
    </div>
  </div>
</div>
#+end_export
#+end_signature"))

;;; email-sending-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'email-sending-rcp)
