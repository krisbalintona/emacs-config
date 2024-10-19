;;; krisb-email-composition-ext.el --- Email composition extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona(require 'message) <krisbalintona@gmail.com>
;; Keywords: mail, lisp

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

;; Extensions to composing email.

;;; Code:

;;; Custom signatures
(defcustom krisb-signature-separator "--"
  "Separator between email body and its signature."
  :type 'string
  :group 'message)

(defcustom krisb-signature-open
  (concat (when message-signature-insert-empty-line "\n")
          "\n#+begin_signature\n")
  "String meant to begin email signatures."
  :type 'string
  :group 'message)

(defcustom krisb-signature-close "\n#+end_signature"
  "String meant to end email signatures."
  :type 'string
  :group 'message)

(defcustom krisb-signature-alist
  '(("Take care" . "Take care,\nKristoffer")
    ("In gratitude" . "In gratitude,\nKristoffer")
    ("Best" . "Best,\nKristoffer")
    ("With appreciation" . "With appreciation,\nKristoffer")
    ("Professional" . "Best regards,\nKristoffer Balintona\nPhone: (773) 677-9699")
    ("Website" . "In gratitude,\nKristoffer Balintona\nhttps://kristofferbalintona.me"))
  "Alist of aliases and their corresponding email signatures."
  :type '(alist :key-type (string :tag "Signature name")
                :value-type (string :tag "Signature content"))
  :group 'message)

;;;###autoload
(defun krisb-signature-select (&optional alias)
  "Select one of the signatures from `krisb-signature-alist'.
Meant to be the value of `message-signature'.

If ALIAS is a key in `krisb-signature-alist', then the corresponding
value will be returned. If it is not, then it will be treated as the
content of a properly formatted signature.

If no ALIAS is supplied, then the keys from `krisb-signature-alist' will
 be shown via the `completing-read' interface."
  (let* ((alias (or alias
                    (completing-read
                     "Insert signature: "
                     (cl-loop for (key . value) in krisb-signature-alist
                              collect key))))
         (content (or (alist-get alias krisb-signature-alist nil nil #'string=) alias)))
    (if (bound-and-true-p org-msg-mode)
        ;; If using `org-msg-mode' and a signature was manually typed rather
        ;; than an alias chosen, then format that manually-typed-signature.
        ;; Example: if "Test" is typed, the result will be:
        ;; "#+begin_signature  (`krisb-signature-open')
        ;; ⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼  (`krisb-signature-separator')
        ;; Test,
        ;; Kristoffer
        ;; #+end_signature  (`krisb-signature-close')"
        (format "%s%s\n%s%s%s"
                krisb-signature-open
                krisb-signature-separator
                content
                ",\nKristoffer"
                krisb-signature-close)
      content)))

;;;###autoload
(defun krisb-signature-insert-mu4e ()
  "Insert a selection from `krisb-signature-alist'.

Replaces existing signature if present in buffer. Relies on
signatures being wrapped in `krisb-signature-open' and
`krisb-signature-close'."
  (interactive)
  (save-excursion
    (let ((sig (funcall 'krisb-signature-select))
          (existing-sig-beg
           (save-excursion
             (save-match-data
               (goto-char (point-min))
               (when (search-forward krisb-signature-open nil t)
                 (match-beginning 0)))))
          (existing-sig-end
           (save-excursion
             (save-match-data
               (goto-char (point-min))
               (search-forward krisb-signature-close nil t)))))
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
  (when (and (derived-mode-p 'org-msg-edit-mode)
             (equal (org-msg-get-prop "alternatives")
                    '(text)))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward krisb-signature-separator nil t)
        (replace-match "--" 1)))))

;;; Provide
(provide 'krisb-email-composition-ext)
;;; krisb-email-composition-ext.el ends here
