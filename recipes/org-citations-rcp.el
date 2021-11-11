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

;;; Org-cite
;; Built-in citations in org-mode
(use-package oc
  :straight nil
  :after citar
  :general
  (:keymaps 'org-mode-map
            [remap citar-insert-citation] '(org-cite-insert :which-key "Insert citation"))
  :custom
  (org-cite-global-bibliography kb/bib-files)
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
  (citar-file-open-note-function 'orb-citar-edit-note)
  )

;;; Citar
;; Alternative to `ivy-bibtex' and `helm-bibtex'
(use-package citar
  :demand t
  :general (:prefix "C-c b"
                    "b" '(citar-insert-citation :which-key "Insert citation")
                    "r" '(citar-insert-reference :which-key "Insert reference")
                    "o" '(citar-open-notes :which-key "Open note")
                    )
  :custom
  (citar-bibliography kb/bib-files)
  (citar-presets '("has:note")) ; A list of predefined searches
  (citar-templates
   '((main . "${author editor:40}   ${date year issued:4}    ${title:115}")
     (suffix . "     ${=type=:20}    ${tags keywords keywords:*}")
     (note . "#+title: Notes on ${author editor}, ${title}") ; For new notes
     ))
  :config
  ;; Configuring all-the-icons. From
  ;; https://github.com/bdarcus/citar#rich-ui
  (setq citar-symbols
        `((pdf . (,(all-the-icons-icon-for-file "foo.pdf" :face 'all-the-icons-dred) .
                  ,(all-the-icons-icon-for-file "foo.pdf" :face 'citar-icon-dim)))
          (note . (,(all-the-icons-icon-for-file "foo.txt") .
                   ,(all-the-icons-icon-for-file "foo.txt" :face 'citar-icon-dim)))
          (link .
                (,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'all-the-icons-dpurple) .
                 ,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'citar-icon-dim)))))
  ;; Here we define a face to dim non 'active' icons, but preserve alignment
  (defface citar-icon-dim
    '((((background dark)) :foreground "#282c34")
      (((background light)) :foreground "#fafafa"))
    "Face for obscuring/dimming icons"
    :group 'all-the-icons-faces)

  ;; Set my own formatting for `citar-insert-reference'
  (defun kb/bibtex-completion-insert-reference (keys)
    "Insert references for entries in KEYS without \"-\" on new line."
    (let* ((refs (--map
                  (s-word-wrap fill-column (bibtex-completion-apa-format-reference it))
                  keys)))
      (insert (s-join "\n" refs))
      ))
  (advice-add 'bibtex-completion-insert-reference :override 'kb/bibtex-completion-insert-reference)
  )

;;; Oc-citar
;; Citar compatibility with `org-cite'
(use-package citar-org
  :demand t                            ; Otherwise it won't be required anywhere
  :straight nil
  :after (citar oc)
  :custom
  ;; Use `citar'
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
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
