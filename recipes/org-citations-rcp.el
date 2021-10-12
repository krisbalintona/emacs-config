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

;;; Bibtex-completion
(use-package bibtex-completion
  :demand t                             ; Other citation packages depend on this
  :custom
  (bibtex-completion-notes-path kb/roam-dir) ; Irrelevant since I use org-roam-bibtex instead
  (bibtex-completion-library-path (expand-file-name (concat kb/roam-dir "bibliographic/bib-pdfs")))
  (bibtex-completion-bibliography
   (list (expand-file-name (concat kb/roam-dir "bibliographic/master-lib.bib"))))
  (bibtex-completion-additional-search-fields '(doi url))
  (bibtex-completion-pdf-field "file") ; Zotero stores pdfs in a field called file - this settings allows bibtex to find the pdf
  (bibtex-completion-pdf-open-function ; Use okular to open a pdf
   (lambda (fpath)
     (call-process "okular" nil 0 nil fpath)))
  (bibtex-completion-browser-function 'browse-url-default-browser) ; Use default browser to open

  ;; Display formats
  (bibtex-completion-display-formats
   '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
     (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
     (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
     (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
     (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
   )

  ;; Template for new note (but I use orb for this)
  ;; Taken from `org-roam-capture-templates'; copied from "Reference without pdf
  ;; notes". Doesn't matter though since `org-roam-bibtex' forces a template
  ;; selection
  (bibtex-completion-notes-template-multiple-files "#+filetags: %(kb/insert-lit-category)\n#+title: ${citekey} ${title}\nSource: ${author-or-editor}\nDate: %<%b %d, %Y>")

  ;; Symbols used for indicating the availability of notes and PDF files
  (bibtex-completion-pdf-symbol "🖇")
  (bibtex-completion-notes-symbol "🖋")
  )

;;; Org-cite
;; Built-in citations in org-mode
(use-package oc
  :straight nil
  :after (bibtex-completion bibtex-actions)
  :general
  (:keymaps 'org-mode-map
            [remap bibtex-actions-insert-citation] '(org-cite-insert :which-key "Insert citation"))
  :custom
  (org-cite-global-bibliography bibtex-completion-bibliography)
  (org-cite-csl-locales-dir (expand-file-name (concat user-emacs-directory "locales/")))
  (org-cite-csl-styles-dir (expand-file-name "~/Documents/Zotero/styles"))
  (org-cite-export-processors
   '((md csl "chicago-fullnote-bibliography.csl")   ; Footnote reliant
     (latex csl "chicago-author-date.csl")          ; For philosophy
     (odt csl "chicago-fullnote-bibliography.csl")  ; Footnote reliant
     (docx csl "chicago-fullnote-bibliography.csl") ; Footnote reliant
     (t csl "modern-language-association.csl")
     ))
  :config
  ;; NOTE 2021-10-11: Need org-export to be loaded for org files to properly be
  ;; loaded without calling `org-export-dispatch'.
  (require 'ox)
  ;; NOTE 2021-10-11: For some reason these aren't being loaded?
  (require 'oc-csl)
  (require 'oc-basic)
  (require 'oc-bibtex)
  (require 'oc-biblatex)

  ;; Have citation links be as they were for `org-ref'
  (set-face-attribute 'org-cite nil :foreground "DarkSeaGreen4")
  (set-face-attribute 'org-cite-key nil :foreground "forest green")
  )

;;; Org-roam-bibtex
;; Ivy/helm-bibtex (which integrates with bibtex-completion) integration with
;; org-roam (provides templates and modifies edit notes action)
(use-package org-roam-bibtex
  :after (org-roam oc embark)
  :ghook 'org-mode-hook ; FIXME 2021-09-14: Make so that I don't need to call in this way
  :custom
  (orb-preformat-keywords '("citekey" "author" "date"))
  (bibtex-actions-file-open-note-function 'orb-bibtex-actions-edit-note)
  )

;;; Bibtex-actions
;; Alternative to `ivy-bibtex' and `helm-bibtex'
(use-package bibtex-actions
  :demand t
  :after (bibtex-completion embark)
  :general
  ("C-c bb" '(bibtex-actions-insert-citation :which-key "Insert citation")
   "C-c br" '(bibtex-actions-insert-reference :which-key "Insert reference")
   "C-c bo" '(bibtex-actions-open-notes :which-key "Open note")
   )
  :custom
  (bibtex-actions-bibliography bibtex-completion-bibliography)
  (bibtex-actions-presets '("has:note")) ; A list of predefined searches
  (bibtex-actions-templates
   '((main . "${author editor:40}   ${date year issued:4}    ${title:115}")
     (suffix . "     ${=type=:20}    ${tags keywords keywords:*}")
     (note . "#+title: Notes on ${author editor}, ${title}") ; For new notes
     ))
  (bibtex-actions-at-point-function 'embark-act)
  :config
  ;; Make the 'bibtex-actions' bindings and targets available to `embark'.
  (add-to-list 'embark-target-finders 'bibtex-actions-citation-key-at-point)
  (add-to-list 'embark-keymap-alist '(bibtex . bibtex-actions-map))
  (add-to-list 'embark-keymap-alist '(citation-key . bibtex-actions-buffer-map))

  ;; Configuring all-the-icons. From
  ;; https://github.com/bdarcus/bibtex-actions#rich-ui
  (setq bibtex-actions-symbols
        `((pdf . (,(all-the-icons-icon-for-file "foo.pdf" :face 'all-the-icons-dred) .
                  ,(all-the-icons-icon-for-file "foo.pdf" :face 'bibtex-actions-icon-dim)))
          (note . (,(all-the-icons-icon-for-file "foo.txt") .
                   ,(all-the-icons-icon-for-file "foo.txt" :face 'bibtex-actions-icon-dim)))
          (link .
                (,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'all-the-icons-dpurple) .
                 ,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'bibtex-actions-icon-dim)))))
  ;; Here we define a face to dim non 'active' icons, but preserve alignment
  (defface bibtex-actions-icon-dim
    '((((background dark)) :foreground "#282c34")
      (((background light)) :foreground "#fafafa"))
    "Face for obscuring/dimming icons"
    :group 'all-the-icons-faces)

  ;; Set my own formatting for `bibtex-actions-insert-reference'
  (defun kb/bibtex-completion-insert-reference (keys)
    "Insert references for entries in KEYS without \"-\" on new line."
    (let* ((refs (--map
                  (s-word-wrap fill-column (bibtex-completion-apa-format-reference it))
                  keys)))
      (insert (s-join "\n" refs))
      ))
  (advice-add 'bibtex-completion-insert-reference :override 'kb/bibtex-completion-insert-reference)
  )

;;; Oc-bibtex-actions
;; Bibtex-actions compatible with `org-cite'
(use-package oc-bibtex-actions
  :demand t                            ; Otherwise it won't be required anywhere
  :straight nil
  :after (bibtex-actions oc)
  :custom
  ;; Use `bibtex-actions'
  (org-cite-insert-processor 'oc-bibtex-actions)
  (org-cite-follow-processor 'oc-bibtex-actions)
  )

;;; Ox-pandoc
;; Export to whatever file format pandoc can export to
(use-package ox-pandoc
  :defer 15
  :after ox
  :ensure-system-package pandoc
  )

;;; org-citations-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-citations-rcp)
