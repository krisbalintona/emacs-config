;;; email-sending-rcp.el --- Sending and composing emails  -*- lexical-binding: t; -*-

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

;; This is configuration pertinent to composing then sending emails.

;;; Code:
(require 'use-package)
(require 'keybinds-general-rcp)

;;;; Org-msg
;;;;; Itself
;; Using org-mode to compose HTML-friendly emails
(use-package org-msg
  :disabled ; FIXME 2024-03-12: For now since mu4e 1.12 refactored email composition
  ;; :ensure (org-msg :type git :host github :repo "jeremy-compostella/org-msg")
  :vc (:url "https://github.com/jeremy-compostella/org-msg.git"
            :rev :newest)
  :demand
  :after mu4e
  :hook ((org-msg-edit-mode . (lambda ()
                                "Set `org-download' directories."
                                (setq-local org-download-method 'directory
                                            org-download-image-dir mu4e-attachment-dir)))
         ;; Don't show exported buffers after sending emails. Inspired by
         ;; https://github.com/jeremy-compostella/org-msg/issues/169#issuecomment-1627375688
         (message-sent . (lambda ()
                           (when (bound-and-true-p org-msg-mode)
                             (switch-to-buffer "*Org ASCII Export*")
                             (kill-buffer-and-window)))))
  :bind
  ;; Get access to the `message' header editing commands in `org-msg-edit-mode'
  ( :map org-msg-edit-mode-map
    ("C-c C-m C-t". message-goto-to)
    ("C-c C-m C-s". message-goto-subject)
    ("C-c C-m C-c". message-goto-cc)
    ("C-c C-m C-b". message-goto-bcc)
    ("C-c C-m C-r". message-goto-reply-to)
    ("C-c C-m C-f". message-goto-followup-to)
    ("C-c C-m C-w". message-goto-fcc))
  (:keymaps 'org-msg-edit-mode-map
            ;; This keybinding is fine since we wouldn't want to call `org-refile' anyway
            "C-c C-w" 'kb/signature-insert-mu4e)
  :custom
  (mu4e-compose-signature-auto-include nil)
  (org-msg-options "html-postamble:nil toc:nil author:nil email:nil \\n:t")
  ;; NOTE 2024-01-05: Don't include "indent" with org-margin
  (org-msg-startup "hidestars inlineimages hideblocks customtime")
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
  ;; CSS for emails. Taken initially from Doom Emacs then modified.
  (org-msg-enforce-css
   ;; Avoid styling that applies to all blockquotes (i.e. (blockquotes nil ...))
   ;; and blockquotes whose class is gmail_quote since this overrides the
   ;; styling done in `kb/org-msg--html-special-block', which sets the class to
   ;; gmail_quote and adds styling. We style there rather than here since we
   ;; cannot add both a class and style property; the class property is
   ;; overwritten if we use `org-msg-enforce-css'.
   (let* ((font-family '(font-family . "-apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Oxygen, Ubuntu, Cantarell,\
        \"Fira Sans\", \"Droid Sans\", \"Helvetica Neue\", Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\";"))
          (monospace-font '(font-family . "SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace;"))
          (font-size '(font-size . "10pt"))
          (font `(,font-family ,font-size))
          (line-height '(line-height . "1.5"))
          (theme-color "#0071c5")
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
       (li nil ((line-height . "1.7")
                (margin-bottom . "5px")
                (margin-top . "7px")
                (max-width . "47em")))
       (nil org-ul ((list-style-type . "disc")))
       (nil org-ol (,@font ,line-height (margin-bottom . "0px")
                           (margin-top . "0x") (margin-left . "30px")
                           (padding-top . "2px") (padding-bottom . "2px")
                           (padding-left . "5px")))
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
       (p nil ((text-decoration . "none") ,line-height
               (margin-top . "10px") (margin-bottom . "0px")
               ,font-size))
       ;; Applies to entire body
       (div ,(intern org-html-content-class) (,@font (line-height . "12pt"))))))
  :config
  (org-msg-mode 1))

;;;;; Custom creation of `org-msg' buffer
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
                                ;; We don't use `message-signature-separator'
                                ;; because the separator may be replaced
                                ;; conditionally. See
                                ;; `kb/signature-insert-mu4e'.
                                (re-search-forward (rx (literal kb/signature-open)
                                                       (+ anychar)
                                                       (literal kb/signature-close))
                                                   nil t)))
                      (call-interactively 'kb/signature-select)))))
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
              ;; Get a chance to modify the inserted citation according to
              ;; `org-msg-posting-style'. Point begins at the start of the
              ;; citation
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
                   (insert "#+begin_gmail_quote\n#+begin_gmail_attr")
                   (save-match-data
                     (re-search-forward (rx "\n" bol (literal message-yank-prefix)))
                     (goto-char (match-beginning 0)))
                   (insert "#+end_gmail_attr\n")
                   (goto-char (point-max))
                   (insert "#+end_gmail_quote"))))
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

;;;; Mu4e-send-delay
(use-package mu4e-send-delay
  :after mu4e
  :commands mu4e
  ;; :ensure (:type git
  ;;                :host github
  ;;                :protocol ssh
  ;;                :repo "krisbalintona/mu4e-send-delay"
  ;;                :depth nil)
  :vc (:url "https://github.com/krisbalintona/mu4e-send-delay.git"
            :rev :newest)
  :hook
  (mu4e-main-mode . mu4e-send-delay-setup)
  :bind
  ([remap message-send-and-exit] . mu4e-send-delay-send-and-exit)
  :custom
  (mu4e-send-delay-default-delay "10m")
  (mu4e-send-delay-default-hour "8")
  (mu4e-send-delay-timer 60)
  (mu4e-send-delay-enable-org-msg t))

(provide 'email-sending-rcp)
;;; email-sending-rcp.el ends here
