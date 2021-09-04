;;; org-roam-other-rcp.el --- Summary
;;
;;; Commentary:
;;
;; This is all the configuration for packages related to org-roam
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'custom-directories-rcp)
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Citations
;;;;; Bibtex-completion
(use-package bibtex-completion
  :demand t                             ; Other citation packages depend on this
  :custom
  (bibtex-completion-notes-path kb/roam-dir) ; Irrelevant since I use org-roam-bibtex instead
  (bibtex-completion-library-path (concat kb/roam-dir "bibliographic/bib-pdfs")) ; Where bibtex searches for pdfs
  (bibtex-completion-bibliography (concat kb/roam-dir "bibliographic/master-lib.bib"))
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

  ;; Citation format when citing using `ivy-bibtex'
  (bibtex-completion-format-citation-functions
   '((org-mode . bibtex-completion-format-citation-cite) ; I changed this line
     (latex-mode . bibtex-completion-format-citation-cite)
     (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
     (python-mode . bibtex-completion-format-citation-sphinxcontrib-bibtex)
     (rst-mode . bibtex-completion-format-citation-sphinxcontrib-bibtex)
     (default . bibtex-completion-format-citation-default)
     ))

  ;; Template for new note (but I use orb for this)
  ;; Taken from `org-roam-capture-templates'; copied from "Reference without pdf
  ;; notes". Doesn't matter though since `org-roam-bibtex' forces a template
  ;; selection
  (bibtex-completion-notes-template-multiple-files "#+filetags: %(kb/insert-lit-category)\n#+title: ${citekey} ${title}\nSource: ${author-or-editor}\nDate: %<%b %d, %Y>")

  ;; Symbols used for indicating the availability of notes and PDF files
  (bibtex-completion-pdf-symbol "🖇")
  (bibtex-completion-notes-symbol "🖋")
  )

;;;;; Bibtex-actions
;; Alternative to `ivy-bibtex' and `helm-bibtex'
(use-package bibtex-actions
  :after bibtex-completion
  :general
  (kb/leader-keys
    "fa" '(bibtex-actions-insert-citation :which-key "Insert citation")
    "fA" '(bibtex-actions-open-notes :which-key "Open note")
    )
  :custom
  ;; What the minibuffer displays
  (bibtex-actions-template '("${author editor:40}   ${date year issued:4}    ${title:115}" . "     ${=type=:20}    ${tags keywords keywords:*}"))

  ;; A list of predefined searches
  (bibtex-actions-presets '("has:note"))

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
  )

;;;;; Org-ref
;; Bibtex is a way to add bibliographic information (e.g. references/citations to
;; equations, sources, images, etc) in latex. Ivy/helm-bibtex is a way to access
;; the .bib files bibtex makes. Org-ref is a way to directly insert citations
;; and references into latex and org files
(use-package org-ref
  :after bibtex-completion
  :custom
  (org-ref-default-bibliography bibtex-completion-bibliography)
  (org-ref-bibliography-notes (concat kb/roam-dir "bibliographic/bib-notes.org")) ; Irrelevant for me - I have it here just in case
  (org-ref-pdf-directory bibtex-completion-library-path)
  (org-ref-notes-directory kb/roam-dir) ; Same directory as org-roam

  (org-ref-completion-library 'org-ref-reftex) ; Org completion
  (org-ref-note-title-format
   "* TODO %y - %t\n :PROPERTIES:\n  :CUSTOM_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n")
  (org-ref-notes-function 'orb-edit-notes)
  (org-ref-default-citation-link "autocite")
  :config
  (setq org-latex-default-packages-alist '(("AUTO" "inputenc" t
                                            ("pdflatex"))
                                           ("T1" "fontenc" t
                                            ("pdflatex"))
                                           ("" "graphicx" t)
                                           ("" "grffile" t)
                                           ("" "longtable" nil)
                                           ("" "wrapfig" nil)
                                           ("" "rotating" nil)
                                           ("normalem" "ulem" t)
                                           ("" "amsmath" t)
                                           ("" "textcomp" t)
                                           ("" "amssymb" t)
                                           ("" "capt-of" nil)
                                           ("hidelinks" "hyperref" nil)) ; Ugly boxes
        )

  ;; Files removed after `org-export' to LaTeX
  (add-to-list 'org-latex-logfiles-extensions "tex")
  (add-to-list 'org-latex-logfiles-extensions "bbl")
  ;; (add-to-list 'org-latex-logfiles-extensions "pdf")
  (add-to-list 'org-latex-logfiles-extensions "synctex.gz")
  )

;;;;; Org-roam-bibtex
;; Ivy/helm-bibtex (which integrates with bibtex-completion) integration with
;; org-roam (provides templates and modifies edit notes action)
(use-package org-roam-bibtex
  :straight (org-roam-bibtex :type git :host github :repo "org-roam/org-roam-bibtex" :branch "master")
  :requires (org-ref org-roam)
  :after (bibtex-completion org-ref)
  :ghook ('org-roam-db-autosync-mode-hook 'org-roam-bibtex-mode nil nil t)
  :custom
  (orb-preformat-keywords
   '("citekey" "title" "url" "file" "author-or-editor" "keywords"))
  )

;;;; Note-taking
;;;;; Pdf-tools
;; View pdfs and interact with them. Has many dependencies
;; https://github.com/politza/pdf-tools#compiling-on-fedora
(use-package pdf-tools
  :straight (pdf-tools :type git :host github :repo "vedang/pdf-tools") ; Repo of current maintainer
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  ;; Ensure it's installed without manually doing so
  :hook ((server-after-make-frame . pdf-tools-install)
         ('window-setup . pdf-tools-install))
  :custom
  (pdf-view-display-size 'fit-width)
  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil)
  )

;;;;; Org-noter
(use-package org-noter
  :defer 10                      ; Load so it doesn't defer to noter insert call
  :general
  (:keymaps 'org-noter-doc-mode-map
            "M-o" 'org-noter-insert-note)
  (kb/leader-keys
    "on" '(org-noter :which-key "Org-noter")
    )
  :custom
  (org-noter-notes-search-path kb/roam-dir)
  (org-noter-separate-notes-from-heading t) ; Add blank line betwwen note heading and content
  (org-noter-notes-window-location 'horizontal-split) ; Horizontal split between notes and pdf
  (org-noter-always-create-frame nil) ; Don't open frame
  (org-noter-hide-other nil) ; Show notes that aren't synced with (you're on)
  (org-noter-auto-save-last-location t) ; Go to last location
  (org-noter-kill-frame-at-session-end nil) ; Don't close frame when killing pdf buffer
  )

;;;; Org-transclusion
;; Enable transclusion of org files
(use-package org-transclusion
  :disabled t ; Issue with org-roam-node-capture
  :straight (org-transclusion :type git :host github :repo "nobiot/org-transclusion")
  :after org-roam
  :ghook ('org-mode-hook 'org-transclusion-activate)
  :general
  (kb/leader-keys
    "Tc" '(org-transclusion-mode :which-key "Toggle mode")
    "TR" '(org-transclusion-refresh :which-key "Refresh")
    "Tm" '(org-transclusion-make-from-link :which-key "Make")
    "Ta" '(org-transclusion-add :which-key "Add")
    "Tr" '(org-transclusion-remove :which-key "Remove")
    "Ts" '(org-transclusion-live-sync-start :which-key "Edit start")
    "Te" '(org-transclusion-live-sync-exit :which-key "Edit exit")
    )
  :custom
  (org-transclusion-include-first-section t)
  (org-transclusion-exclude-elements '(property-drawer keyword))
  :config
  ;; Currently need to look through Roam directory, not just agenda files
  (org-id-update-id-locations (org-roam--list-all-files))
  )

;;;; Org-roam-ui
;; Newer `org-roam-server' for org-roam V2.
(use-package org-roam-ui
  :straight (org-roam-ui :type git :host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :init
  (use-package websocket)
  (use-package simple-httpd)
  (use-package f)
  :custom
  (org-roam-ui-open-on-start nil) ; Don't open graph on startup
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-custom-theme '(list
                              (bg . "#1E2029")
                              (bg-alt . "#282a36")
                              (fg . "#f8f8f2")
                              (fg-alt . "#6272a4")
                              (red . "#ff5555")
                              (orange . "#f1fa8c")
                              (yellow ."#ffb86c")
                              (green . "#50fa7b")
                              (cyan . "#8be9fd")
                              (blue . "#ff79c6")
                              (violet . "#8be9fd")
                              (magenta . "#bd93f9"))
                            )
  )

;;; org-roam-other-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-roam-other-rcp)
