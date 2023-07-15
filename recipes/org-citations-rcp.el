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
  :general (:keymaps 'org-mode-map
            [remap citar-insert-citation] '(org-cite-insert :wk "Insert citation"))
  :custom
  (org-cite-global-bibliography kb/bib-files)
  (org-cite-csl-locales-dir (file-name-concat user-emacs-directory "locales/"))
  (org-cite-csl-styles-dir (expand-file-name "~/Documents/Zotero/styles/"))
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
  ;; biblatex style) in `org-cite-export-processors' is used
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
            "f" 'citar-open-files
            "o" 'citar-open-notes
            "p" 'kb/citar-open-pdf-in-zotero)
  :custom
  (citar-bibliography kb/bib-files)
  (citar-notes-paths (list kb/notes-dir))
  :init
  ;; Minor customizations to `citar-org--update-prefix-suffix'; just changed
  ;; prompts
  (defun kb/citar-org--update-prefix-suffix ()
    "Change the pre/suffix text of the reference at point."
    (let* ((ref (org-element-context))
           (key (org-element-property :key ref))
           (citekey-str (propertize key 'face 'mode-line-emphasis))
           (pre (read-string (concat "Prefix for " citekey-str ": ")
                             (org-element-property :prefix ref)))
           (post (string-trim-left      ; Always want this I think
                  (read-string (concat "Suffix for " citekey-str ": ")
                               (org-element-property :suffix ref))))
           ;; Change post to automatically have one space prior to any
           ;; user-inputted suffix, unless post is already blank or whitespace
           (post-processed (concat (when (string-empty-p post) " ") post))
           (v1 (org-element-property :begin ref))
           (v2 (org-element-property :end ref)))
      (cl--set-buffer-substring v1 v2
                                (org-element-interpret-data
                                 `(citation-reference
                                   (:key ,key :prefix ,pre :suffix ,post-processed))))))
  ;; Gave `kb/citar-org-update-prefix-suffix' the ability to set the
  ;; pre/suffixes for all references in reference
  (defun kb/citar-org-update-prefix-suffix (&optional arg)
    "Change the pre/suffix text of the reference at point.
If given ARG, change the prefix and suffix for every reference in
the citation at point."
    (interactive "P")
    ;; Enable `typo' typographic character cycling in minibuffer. Particularly
    ;; useful in adding en- and em-dashes in citation suffixes (e.g. for page
    ;; ranges)
    (when (featurep 'typo)
      (add-hook 'minibuffer-mode-hook 'typo-mode)) ; Enable dashes

    (let* ((datum (org-element-context))
           (citation-p (eq 'citation (org-element-type datum)))
           (current-citation (if citation-p datum (org-element-property :parent datum)))
           (refs (org-cite-get-references current-citation)))
      (save-excursion
        (if (or arg citation-p)
            (dotimes (ref-index (length refs))
              (goto-char (org-element-property :begin (nth ref-index refs)))
              (kb/citar-org--update-prefix-suffix)
              ;; Update refs since the begins and ends for the following
              ;; reference could have changed
              (setq refs (org-cite-get-references
                          (org-element-property :parent (org-element-context)))))
          (kb/citar-org--update-prefix-suffix))))

    ;; Remove hook if it was added earlier
    (remove-hook 'minibuffer-mode-hook 'typo-mode))
  (advice-add 'citar-org-update-pre-suffix :override #'kb/citar-org-update-prefix-suffix)

  ;; Run `citar-org-update-pre-suffix' (which is overridden by my
  ;; `kb/citar-org-update-pre-suffix') right after `org-cite-insert' to
  ;; immediately set its prefix and suffix
  (advice-add 'org-cite-insert :after '(lambda (_) (citar-org-update-pre-suffix)))
  :config
  ;; Original function by me. Was able to discover the appropriate link here:
  ;; https://forums.zotero.org/discussion/90858/pdf-reader-and-zotero-open-pdf-links.
  ;; Also see https://github.com/emacs-citar/citar/issues/685 with potentially
  ;; https://forums.zotero.org/discussion/101535/betterbibtex-export-itemids-to-bib-file
  ;; for a different solution
  (defun kb/citar-open-pdf-in-zotero (citekey)
    "Open the PDF of an item with CITEKEY in Zotero."
    (interactive (list (citar-select-ref)))
    (if-let* ((files-hash (hash-table-values (citar-get-files citekey)))
              (files-list (delete-dups (apply #'append files-hash)))
              (pdf (car (-filter
                         (lambda (file) (string= (file-name-extension file) "pdf")) files-list)))
              (zotero-key (f-base (f-parent pdf))))
        (citar-file-open-external
         (concat "zotero://open-pdf/library/items/" zotero-key))
      (user-error "No PDF for %s!" citekey)))

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
  :demand
  :diminish
  :general (:keymaps 'citar-embark-citation-map
            "p" 'kb/citar-open-pdf-in-zotero)
  :config
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
