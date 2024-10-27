;;; checking-words-rcp.el --- Dictionaries and thesauruses  -*- lexical-binding: t; -*-

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

;; Packages relevant to the dictionaries and thesauruses.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Offline
;;;;; Wordnut
;; Offline dictionary
(use-package wordnut
  :disabled                             ; Use dictionary localhost now
  :after define-word
  ;; TODO 2021-08-20: Have this changed depending on Linux distribution
  ;; Make sure the install a dictnary, in case it is a separate package
  :ensure-system-package (wn . wordnet-cli))

;;;;; Synosaurus
;; Offline thesaurus
(use-package synosaurus
  :after powerthesaurus
  ;; TODO 2021-08-20: Have this changed depending on Linux distribution
  :ensure-system-package (wn . wordnet-cli) ; Make sure English dictionary is also installed
  :custom
  (synosaurus-backend 'synosaurus-backend-wordnet) ; Offline thesaurus that relies on `wordnet'
  (synosaurus-choose-method 'default))

;;;;; kb/{dictionary,thesaurus}-at-point kb/{dictionary,thesaurus}-lookup
;; Change which packages are used depending on internet connection
(defun kb/internet-up-p (&optional host)
  "Return `t' if the device has internet access, and `nil'
otherwise. Credit to https://emacs.stackexchange.com/a/18516"
  (equal 70 (dbus-get-property
             :system "org.freedesktop.NetworkManager" "/org/freedesktop/NetworkManager"
             "org.freedesktop.NetworkManager" "State")))

;; TODO 2022-06-04: Better integrate bindings with other packages
(defun kb/dictionary-at-point ()
  "Call a dictionary command or `helpful-at-point'.

If the major-mode is derived from text-mode, then call a
dictionary command for the word at point depending on the
internet connection.

If not (i.e. in a prog-mode derived major mode), then call
`helpful-at-point' instead. If the sexp at point does not
exist,then throw an error message.

If in `lsp-mode' and `dash-docs-completing-read' is a feature,
then call `dash-docs-completing-read-at-point'."
  (interactive)
  (let ((dict-function 'dictionary-lookup-definition)
        (in-comment-p (nth 4 (syntax-ppss)))
        (in-string-p (nth 3 (syntax-ppss)))
        (valid-symbol-p (or (fboundp (symbol-at-point))
                            (boundp (symbol-at-point)))))
    (cond
     ((and (bound-and-true-p lsp-mode) (featurep 'dash-docs-completing-read))
      (require 'dash-docs)
      (dash-docs-completing-read-at-point))
     (valid-symbol-p
      (if (featurep 'helpful)
          (helpful-at-point)
        (describe-symbol (symbol-at-point))))
     ((or in-comment-p in-string-p)
      (funcall dict-function))
     ((derived-mode-p 'text-mode)
      (funcall dict-function))
     (t
      (error "I can't do anything here!")))))

(defun kb/thesaurus-at-point ()
  "Use `powerthesaurus' if online, and `synosaurus' if offline."
  (interactive)
  (if (kb/internet-up-p)
      (powerthesaurus-lookup-dwim 'action-insert :synonyms)
    (synosaurus-choose-and-replace)))

(defun kb/dictionary-lookup ()
  "Use `dictionary' if online, and `wordnet' if offline."
  (interactive)
  (if (kb/internet-up-p)
      (call-interactively 'dictionary-search)
    (call-interactively 'wordnut-search)))

(defun kb/thesuarus-lookup ()
  "Use `powerthesaurus' if online, and `synosaurus' if offline."
  (interactive)
  (if (kb/internet-up-p)
      (powerthesaurus-lookup-dwim)
    (synosaurus-choose-and-insert)))

(if (or (bound-and-true-p evil-local-mode)
        (bound-and-true-p meow-mode))
    (bind-keys
     ("C-c d" . kb/dictionary-at-point)
     ("C-c D" . kb/dictionary-lookup)
     ("C-c l" . kb/thesaurus-at-point)
     ("C-c L" . kb/thesuarus-lookup))
  (bind-chords
   ("jj" . kb/dictionary-at-point)
   ("JJ" . kb/dictionary-lookup)
   ("kk" . kb/thesaurus-at-point)
   ("KK" . kb/thesuarus-lookup)))

;;;; Other
;;;;; Reverso
;; Use Reverso to check grammar, translate, find synonyms, conjugations, etc.
(use-package reverso
  :config
  (reverso-history-mode)
  (diminish 'reverso-history-mode))

(provide 'checking-words-rcp)
;;; checking-words-rcp.el ends here
