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
