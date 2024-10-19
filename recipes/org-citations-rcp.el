;;; org-citations-rcp.el --- Citations in org-mode   -*- lexical-binding: t; -*-

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

;; Config for using citations in org-mode.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)
(require 'org-general-rcp)

;;;; Oc-csl-activate

;;;;; Itself
(use-package oc-csl-activate
  :vc (:url "https://github.com/andras-simonyi/org-cite-csl-activate.git"
            :rev :newest)
  :after oc
  :demand)

;;;;; Custom eldoc backend that formats citations into CSL form
(with-eval-after-load 'oc-csl-activate ; REVIEW 2024-09-22: Currently depends on its functions
  (with-eval-after-load 'eldoc
    (defun kb/org-cite-eldoc (callback &rest _ignored)
      "Show a CSL-formatted citation at point by calling CALLBACK.
Intended for `eldoc-documentation-functions'."
      (when-let ((citation (org-cite-csl-activate--get-citation (point))))
        (let* ((proc (org-cite-csl-activate--processor))
               (info (list :cite-citeproc-processor proc))
               (cit-struct (org-cite-csl--create-structure citation info)))
          (citeproc-clear proc)
          (citeproc-append-citations (list cit-struct) proc)
          (funcall callback (car (citeproc-render-citations proc 'plain nil))))))

    (add-hook 'org-mode-hook #'(lambda ()
                                 (add-hook 'eldoc-documentation-functions #'kb/org-cite-eldoc nil t)
                                 (setq-local eldoc-idle-delay 1)))))

;;;; Citar
;; Alternative to `ivy-bibtex' and `helm-bibtex'

;;;;; Itself
(use-package citar
  :hook (org-mode . citar-capf-setup)
  :bind
  :bind (("C-c b b" . citar-insert-citation)
         ("C-c b o" . citar-open)
         ("C-c b f" . citar-open-files)
         ("C-c b n" . citar-open-notes)
         :map org-mode-map
         ([remap org-cite-insert] . citar-insert-citation))
  :custom
  (citar-bibliography krisb-bibliography-files)
  (citar-notes-paths (list krisb-notes-directory))
  (citar-open-entry-function #'citar-open-entry-in-file)
  (citar-default-action #'citar-open-files)
  :config
  ;; Immediately set citation prefix and suffix and enable `typo-mode'
  ;; temporarily while inserting
  (advice-add 'citar-org-update-prefix-suffix
              :around (lambda (orig-fun &rest args)
                        (when (and (fboundp 'typo-mode) (not org-export-with-smart-quotes))
                          (add-hook 'minibuffer-mode-hook 'typo-mode))
                        (condition-case err
                            (apply orig-fun args)
                          (quit
                           ;; Remove from minibuffer-mode-hook when user
                           ;; interrupts with keyboard-quit (C-g)
                           (when (member 'typo-mode minibuffer-mode-hook)
                             (remove-hook 'minibuffer-mode-hook 'typo-mode))))
                        (when (fboundp 'typo-mode)
                          (remove-hook 'minibuffer-mode-hook 'typo-mode))))

  (with-eval-after-load 'all-the-icons
    ;; Taken from https://github.com/emacs-citar/citar/wiki/Indicators
    (defvar citar-indicator-files-icons
      (citar-indicator-create
       :symbol (all-the-icons-faicon
                "file-o"
                :face 'all-the-icons-green
                :v-adjust -0.1)
       :function #'citar-has-files
       :padding "  " ; Need this because the default padding is too low for these icons
       :tag "has:files"))
    (defvar citar-indicator-links-icons
      (citar-indicator-create
       :symbol (all-the-icons-octicon
                "link"
                :face 'all-the-icons-orange
                :v-adjust 0.01)
       :function #'citar-has-links
       :padding "  "
       :tag "has:links"))
    (defvar citar-indicator-notes-icons
      (citar-indicator-create
       :symbol (all-the-icons-material
                "speaker_notes"
                :face 'all-the-icons-blue
                :v-adjust -0.3)
       :function #'citar-has-notes
       :padding "  "
       :tag "has:notes"))
    (defvar citar-indicator-cited-icons
      (citar-indicator-create
       :symbol (all-the-icons-faicon
                "circle-o"
                :face 'all-the-icon-green)
       :function #'citar-is-cited
       :padding "  "
       :tag "is:cited"))
    (setq citar-indicators
          (list citar-indicator-files-icons
                citar-indicator-links-icons
                citar-indicator-notes-icons
                citar-indicator-cited-icons))))

;;;;; Faster renderer
;; Replaces the *VERY SLOW* `'org-cite-basic-activate' (which `citar' relies on
;; in `citar-org-activate') with a faster version. Practically necessary if I
;; want to edit a line with a citation in Org without having to wait several
;; seconds for it to render. See for more information on the matter:
;; 1. https://www.reddit.com/r/orgmode/comments/td76wz/comment/i0lpg7k/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
;; 2. https://list.orgmode.org/87ils5sz8x.fsf@localhost/t/#u
(with-eval-after-load 'citar
  (defun kb/citar-basic-activate (citation)
    "Set various text properties on CITATION object.

Fontify whole citation with `org-cite' face.  Fontify key with `error' face
when it does not belong to known keys.  Otherwise, use `org-cite-key' face.

Moreover, when mouse is on a known key, display the corresponding bibliography.
On a wrong key, suggest a list of possible keys, and offer to substitute one of
them with a mouse click.

My version of this function uses the speed of `citar' (and its cache) to
eliminate the extraordinary slow down of the default function's
rendering. I believe the bottlenecks are `org-cite-basic--all-keys' and
`org-cite-basic--get-entry' so I have replaced them with equivalent
functions from `citar'."
    (pcase-let ((`(,beg . ,end) (org-cite-boundaries citation))
                ;; NOTE 2024-09-05: Use `citar' (and its cache) to get all keys
                (keys (let (keys)
                        (maphash (lambda (key value) (push key keys))
                                 (citar-get-entries))
                        keys)))
      (put-text-property beg end 'font-lock-multiline t)
      (add-face-text-property beg end 'org-cite)
      (dolist (reference (org-cite-get-references citation))
        (pcase-let* ((`(,beg . ,end) (org-cite-key-boundaries reference))
                     (key (org-element-property :key reference)))
          ;; Highlight key on mouse over.
          (put-text-property beg end
                             'mouse-face
                             org-cite-basic-mouse-over-key-face)
          (if (member key keys)
              ;; Activate a correct key. Face is `org-cite-key' and `help-echo'
              ;; displays bibliography entry, for reference. <mouse-1> calls
              ;; `org-open-at-point'.
              ;; NOTE 2024-09-05: Use `citar' (and its cache) to create the
              ;; bibliographic entry text used in the help echo
              (let* ((entry (string-trim (citar-format-reference (list key))))
                     (bibliography-entry
                      (org-element-interpret-data entry)))
                (add-face-text-property beg end 'org-cite-key)
                (put-text-property beg end 'help-echo bibliography-entry)
                (org-cite-basic--set-keymap beg end nil))
            ;; Activate a wrong key. Face is `error', `help-echo' displays
            ;; possible suggestions.
            (add-face-text-property beg end 'error)
            (let ((close-keys (org-cite-basic--close-keys key keys)))
              (when close-keys
                (put-text-property beg end 'help-echo
                                   (concat "Suggestions (mouse-1 to substitute): "
                                           (mapconcat #'identity close-keys " "))))
              ;; When the are close know keys, <mouse-1> provides completion to
              ;; fix the current one. Otherwise, call `org-cite-insert'.
              (org-cite-basic--set-keymap beg end (or close-keys 'all))))))))
  (setq citar-org-activation-functions '(kb/citar-basic-activate citar-org-activate-keymap)))

;;;; Citar-org
;; Use `citar' with `org-cite'
(use-package citar-org
  :ensure nil
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-org-styles-format 'long))

;;;; Citar-embark
(use-package citar-embark
  :diminish
  :bind (("C-c b z" . kb/citar-open-pdfs-in-zotero)
         :map citar-embark-citation-map
         ("z" . kb/citar-open-pdfs-in-zotero))
  :custom
  (citar-at-point-function 'embark-act)
  :config
  (citar-embark-mode 1)

  ;; Original function. Was able to discover the appropriate link here:
  ;; https://forums.zotero.org/discussion/90858/pdf-reader-and-zotero-open-pdf-links.
  ;; Also see https://github.com/emacs-citar/citar/issues/685 with potentially
  ;; https://forums.zotero.org/discussion/101535/betterbibtex-export-itemids-to-bib-file
  ;; for a different solution
  (defun kb/citar-open-pdf-in-zotero (citekey)
    "Open PDF associated with CITEKEY in Zotero."
    (if-let* ((files-hash (hash-table-values (citar-get-files citekey)))
              (files-list (delete-dups (apply #'append files-hash)))
              ;; OPTIMIZE 2023-07-16: The following line of code only works if
              ;; there is only one PDF attached to the item, and that PDF is
              ;; the document. For progress on differentiating mere
              ;; attachments to PDF documents, see the issue linked above
              (pdf (car (-filter
                         (lambda (file) (string= (file-name-extension file) "pdf")) files-list)))
              (zotero-key (f-base (f-parent pdf))))
        (citar-file-open-external
         (concat "zotero://open-pdf/library/items/" zotero-key))
      (message "No PDF for %s!" citekey)))
  (defun kb/citar-open-pdfs-in-zotero (citekeys)
    "Open PDFs associated with CITEKEYS in Zotero."
    (interactive (list (citar-select-refs)))
    (dolist (citekey citekeys)
      (kb/citar-open-pdf-in-zotero citekey))))

(provide 'org-citations-rcp)
;;; org-citations-rcp.el ends here
