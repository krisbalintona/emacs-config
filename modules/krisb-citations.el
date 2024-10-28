;;; Oc (org-cite)
;; Built-in citations in org-mode
(use-package oc
  :ensure nil
  :after org
  :custom
  (org-cite-global-bibliography krisb-bibliography-files)
  (org-cite-csl-locales-dir nil)
  (org-cite-csl-styles-dir (expand-file-name "styles/" krisb-zotero-directory))
  (org-cite-export-processors
   '((md . (csl "chicago-fullnote-bibliography.csl"))   ; Footnote reliant
     (latex biblatex)                                   ; For humanities
     (odt . (csl "chicago-fullnote-bibliography.csl"))  ; Footnote reliant
     (docx . (csl "chicago-fullnote-bibliography.csl")) ; Footnote reliant
     (t . (csl "modern-language-association.csl"))))    ; Fallback
  :custom-face
  ;; Have citation link faces look closer to as they were for `org-ref'
  (org-cite ((t (:foreground "DarkSeaGreen4"))))
  (org-cite-key ((t (:foreground "forest green" :slant italic)))))

;;; Citar
(use-package citar
  :hook (org-mode . citar-capf-setup)
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

;;; Citar-org
;; Use `citar' with `org-cite'
(use-package citar-org
  :after oc
  :ensure nil
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-org-styles-format 'short))

;;; Provide
(provide 'krisb-citations)
