;;; org-citations-rcp.el --- Summary
;;
;;; Commentary:
;;
;; This is all the configuration of the org-roam package
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)
(require 'org-general-rcp)

;;; Oc (org-cite)
;; Built-in citations in org-mode
(use-package oc
  :elpaca nil
  :general (:keymaps 'org-mode-map [remap citar-insert-citation] 'org-cite-insert)
  :custom
  (org-cite-global-bibliography kb/bib-files)
  (org-cite-csl-locales-dir (file-name-concat user-emacs-directory "locales/"))
  (org-cite-csl-styles-dir (expand-file-name "~/Zotero/styles/"))
  (org-cite-export-processors
   '((md . (csl "chicago-fullnote-bibliography.csl"))   ; Footnote reliant
     (latex biblatex)                                   ; For humanities
     (odt . (csl "chicago-fullnote-bibliography.csl"))  ; Footnote reliant
     (docx . (csl "chicago-fullnote-bibliography.csl")) ; Footnote reliant
     (t . (csl "modern-language-association.csl"))))    ; Fallback
  :custom-face
  ;; Have citation link faces look closer to as they were for `org-ref'
  (org-cite ((t (:foreground "DarkSeaGreen4"))))
  (org-cite-key ((t (:foreground "forest green" :slant italic))))
  :config
  ;; NOTE 2023-07-14: Require all `oc-*' packages so that I don't run into the
  ;; issue where the package associated with a style (e.g. `oc-biblatex' for the
  ;; biblatex style) in `org-cite-export-processors' is used prior to its
  ;; loading
  (require 'oc-natbib)
  (require 'oc-csl)
  (require 'oc-basic)
  (require 'oc-bibtex)
  (require 'oc-biblatex))

;;; Citar
;; Alternative to `ivy-bibtex' and `helm-bibtex'
(use-package citar
  :after all-the-icons
  :general (:keymaps 'org-mode-map
                     :prefix "C-c b"
                     "b" 'citar-insert-citation
                     "r" 'citar-insert-reference
                     "o" 'citar-open
                     "f" 'citar-open-files
                     "n" 'citar-open-notes
                     "z" 'kb/citar-open-pdfs-in-zotero)
  :custom
  (citar-bibliography kb/bib-files)
  (citar-notes-paths (list kb/notes-dir))
  (citar-open-entry-function #'citar-open-entry-in-file)
  (citar-default-action 'kb/citar-open-pdf-in-zotero)
  :init
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
      (kb/citar-open-pdf-in-zotero citekey)))
  :config
  ;; Immediately set citation prefix and suffix and enable `typo-mode'
  ;; temporarily while inserting
  (advice-add 'org-cite-insert :after
              #'(lambda (&rest _)
                  (when (eq org-cite-insert-processor 'citar)
                    (citar-org-update-prefix-suffix))))
  (advice-add 'citar-org-update-prefix-suffix
              :around (lambda (orig-fun &rest args)
                        (when (featurep 'typo)
                          (add-hook 'minibuffer-mode-hook 'typo-mode))
                        (condition-case err
                            (apply orig-fun args)
                          (quit
                           ;; Remove from minibuffer-mode-hook when user
                           ;; interrupts with keyboard-quit (C-g)
                           (when (featurep 'typo)
                             (remove-hook 'minibuffer-mode-hook 'typo-mode))))
                        (when (featurep 'typo)
                          (remove-hook 'minibuffer-mode-hook 'typo-mode))))

  ;; Taken from https://github.com/emacs-citar/citar/wiki/Indicators
  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (all-the-icons-faicon
              "file-o"
              :face 'all-the-icons-green
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  " ; need this because the default padding is too low for these icons
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
              citar-indicator-cited-icons)))

;;; Citar-embark
(use-package citar-embark
  :after citar
  :diminish
  :general (:keymaps 'citar-embark-citation-map
                     "z" 'kb/citar-open-pdfs-in-zotero)
  :custom
  (citar-at-point-function 'embark-act)
  :init
  (citar-embark-mode))

;;; Citar-org
;; Use `citar' with `org-cite'
(use-package citar-org
  :elpaca nil
  :hook (org-mode . citar-capf-setup)
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-org-styles-format 'long))

;;; org-citations-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-citations-rcp)
