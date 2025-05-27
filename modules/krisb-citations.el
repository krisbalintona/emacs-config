;; -*- lexical-binding: t; -*-

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
  (org-cite-key ((t (:foreground "forest green" :slant italic))))
  :config
  ;; 2025-03-30: For the biblatex cite export processor.  Otherwise,
  ;; `org-cite-supported-styles' errors because (org-cite-get-processor
  ;; 'biblatex) returns nil.
  (require 'oc-biblatex))

;;; Provide
(provide 'krisb-citations)
