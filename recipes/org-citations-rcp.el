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
  :general
  (:keymaps 'org-mode-map
            [remap citar-insert-citation] '(org-cite-insert :wk "Insert citation"))
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
  )

;;; Citar
;; Alternative to `ivy-bibtex' and `helm-bibtex'
(use-package citar
  :commands (citar-insert-citation citar-insert-reference citar-open-notes kb/citar-capture)
  :general
  (kb/note-keys
    "C" '(kb/citar-capture :wk "Citar-capture")
    )
  (:keymaps 'org-mode-map
            :prefix "C-c b"
            "b" '(citar-insert-citation :wk "Insert citation")
            "r" '(citar-insert-reference :wk "Insert reference")
            "o" '(citar-open-notes :wk "Open note")
            )
  :custom
  (citar-bibliography kb/bib-files)
  (citar-templates
   '((main . "${author editor:30}   ${date year issued:4}    ${title:110}")
     (suffix . "     ${=type=:20}    ${tags keywords keywords:*}")
     (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
     (note . "#+title: Notes on ${author editor}, ${title}") ; For new notes
     ))
  (citar-notes-paths `(,kb/roam-dir))
  (citar-open-note-function 'orb-citar-edit-note) ; Open notes in `org-roam'
  (citar-at-point-function 'embark-act) ; Use `embark'
  :config
  (general-advice-add '(citar-insert-citation citar-insert-reference citar-open-notes) :before #'citar-refresh)

  ;; Configuring all-the-icons. From
  ;; https://github.com/bdarcus/citar#rich-ui
  (setq citar-symbols
        `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) .
                ,(all-the-icons-faicon "file-o" :face 'citar-icon-dim :v-adjust -0.1) )
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) .
                ,(all-the-icons-material "speaker_notes" :face 'citar-icon-dim :v-adjust -0.3))
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) .
                ,(all-the-icons-octicon "link" :face 'citar-icon-dim :v-adjust 0.01))))
  (setq citar-symbol-separator "  ")
  ;; Here we define a face to dim non 'active' icons, but preserve alignment
  (defface citar-icon-dim
    ;; Based on solaire's faces
    '((((background dark)) :foreground "#212428")
      (((background light)) :foreground "#f0f0f0"))
    "Face for obscuring/dimming icons"
    :group 'all-the-icons-faces)

  ;; Create a new node from a bibliographic source. Taken from
  ;; https://jethrokuan.github.io/org-roam-guide/
  (defun kb/citar-capture (keys-entries)
    (interactive (list (citar-select-ref :multiple nil :rebuild-cache t)))
    (citar-refresh)                     ; Make sure citar updates its cache
    (let ((title (citar--format-entry-no-widths (cdr keys-entries)
                                                "${author editor}${date urldate} :: ${title}")))
      (org-roam-capture- :templates
                         '(("r" "reference" plain "%?" :if-new
                            (file+head "${citekey}.org"
                                       ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}
#+filetags: %(kb/insert-lit-category)")
                            :immediate-finish t
                            :unnarrowed t))
                         :info (list :citekey (car keys-entries))
                         :node (org-roam-node-create :title title)
                         :props '(:finalize find-file)))))

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
