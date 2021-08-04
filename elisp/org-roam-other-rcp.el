;;; org-roam-other-rcp.el --- Summary
;;
;;; Commentary:
;;
;; This is all the configuration for packages related to org-roam
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Citations
;;;;; Bibtex-completion
(use-package bibtex-completion
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

  ;; Citation format when citing using `ivy-bibtex'
  (bibtex-completion-format-citation-functions
   '((org-mode . kb/bibtex-completion-format-citation-autocite) ; I changed this line
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
  :after (bibtex-completion selectrum embark consult ivy-bibtex consult)
  :custom
  ;; What the minibuffer displays
  (bibtex-actions-template '((t . "${author:40}   ${title:130}")))
  (bibtex-actions-template-suffix '((t . "     ${=type=:20}")))

  ;; A list of predefined searches
  (bibtex-actions-presets '("has:note"))

  (bibtex-actions-at-point-function 'embark-act)

  ;; Initial input depending on `bibtex-actions-open-*'
  (bibtex-actions-initial-inputs
   '((pdf    . "has:pdf")
     (note   . "has:note")
     (link   . "has:link")
     (source . "has:link\\|has:pdf"))
   )
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

  ;; Enhanced multiple selection experience. Replaced the built-in method
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Custom keymap for `bibtex-actions'
  (general-define-key
   :keymaps 'bibtex-actions-map
   "t" '("reference | add pdf attachment" . bibtex-actions-add-pdf-attachment)
   "a" '("reference | add pdf to library" . bibtex-actions-add-pdf-to-library)
   "b" '("reference | insert bibtex" . bibtex-actions-insert-bibtex)
   "c" '("reference | insert citation" . bibtex-actions-insert-citation)
   "k" '("reference | insert key" . bibtex-actions-insert-key)
   "r" '("reference | insert" . bibtex-actions-insert-reference) ; Changed
   "o" '("reference | open source" . bibtex-actions-open)
   "e" '("reference | open entry" . bibtex-actions-open-entry)
   "l" '("reference | open link" . bibtex-actions-open-link)
   "n" '("reference | open notes" . bibtex-actions-open-notes)
   "p" '("reference | open pdf" . bibtex-actions-open-pdf)
   "R" '("reference | refresh library" . bibtex-actions-refresh) ; Changed
   ;; Embark doesn't currently use the menu description.
   ;; https://github.com/oantolin/embark/issues/251
   "RET" '("reference | default action" . bibtex-actions-run-default-action)
   )

  (general-define-key
   :keymaps 'minibuffer-local-map
   "M-b" 'bibtex-actions-insert-preset
   )

  (kb/leader-keys
    "fa" '(bibtex-actions-insert-citation :which-key "Insert citation")
    "fA" '(bibtex-actions-open-notes :which-key "Open note")
    )
  )

;;;;; Ivy-bibtex
;; Use ivy or helm search frontend with the bibtex-completion backend
(use-package ivy-bibtex
  ;; :disabled t ; Replaced by `bibtex-actions'. Can't disable because I need the ivy faces
  :after org-roam
  :init
  (defun kb/bibtex-completion-format-citation-autocite (keys)
    "My own bibtex-completion-format. Accepts KEYS."
    (s-join ", "
            (--map (format "autocite:%s" it) keys)))
  :custom
  (ivy-bibtex-default-action 'ivy-bibtex-insert-citation) ; Edit notes on default selection

  :config
  ;; ivy-bibtex requires ivy's `ivy--regex-ignore-order` which I already
  ;; have set in ivy-re-builders-alist
  (autoload 'ivy-bibtex "ivy-bibtex" "" t)

  (ivy-set-actions ; Actions shown after M-o
   'ivy-bibtex
   '(("p" ivy-bibtex-open-any "Open PDF, URL, or DOI")
     ("e" ivy-bibtex-edit-notes "Edit notes")
     ("c" ivy-bibtex-insert-citation "Insert citation")
     ("k" ivy-bibtex-insert-key "Insert key")
     ("r" ivy-bibtex-insert-reference "Insert reference")
     ("P" ivy-bibtex-open-annotated-pdf "Open annotated PDF (if present)") ; This last function doesn't have an associated action yet (for annotated pdfs)
     ("a" bibtex-completion-add-pdf-to-library "Add pdf to library")
     ))

  (general-define-key
   "C-c b" '(ivy-bibtex :which-key "Ivy-bibtex")
   ;; "fA" '(ivy-bibtex-with-notes :which-key "Ivy-bibtex only notes")
   )
  )

;;;;; Org-ref
;; Bibtex is a way to add bibliographic information (e.g. references/citations to
;; equations, sources, images, etc) in latex. Ivy/helm-bibtex is a way to access
;; the .bib files bibtex makes. Org-ref is a way to directly insert citations
;; and references into latex and org files
(use-package org-ref
  :demand t ; Hard dependency for `org-roam-bibtex' (and maybe others)
  :after ivy
  :custom
  (org-ref-default-bibliography bibtex-completion-bibliography)
  (org-ref-bibliography-notes (concat kb/roam-dir "bibliographic/bib-notes.org")) ; Irrelevant for me - I have it here just in case
  (org-ref-pdf-directory bibtex-completion-library-path)
  (org-ref-notes-directory kb/roam-dir) ; Same directory as org-roam

  (org-ref-completion-library 'org-ref-ivy-cite) ; Use ivy
  (org-ref-note-title-format
   "* TODO %y - %t\n :PROPERTIES:\n  :CUSTOM_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n")
  (org-ref-notes-function 'orb-edit-notes)
  (org-ref-default-citation-link "autocite")

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
  :config
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
  :straight (org-roam-bibtex :type git :host github :repo "org-roam/org-roam-bibtex" :branch "origin/master")
  :after (org-roam org-ref ivy-bibtex bibtex-actions)
  :custom
  (orb-preformat-keywords
   '("citekey" "title" "url" "file" "author-or-editor" "keywords")
   )
  :config
  (org-roam-bibtex-mode)
  )

;;;; Note-taking
;;;;; Pdf-tools
;; View pdfs and interact with them. Has many dependencies
;; https://github.com/politza/pdf-tools#compiling-on-fedora
(use-package pdf-tools
  :straight (pdf-tools :type git :host github :repo "vedang/pdf-tools") ; Repo of current maintainer
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :custom
  (pdf-view-display-size 'fit-width)
  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil)
  :config

  (general-define-key ; Unbind SPC so it's prefix instead
   :keymaps 'pdf-view-mode-map
   [remap pdf-view-scroll-up-or-next-page] nil
   )
  )

;;;;; Org-noter
(use-package org-noter
  :demand t ; Demand so it doesn't defer to noter insert call
  :custom
  (org-noter-notes-search-path kb/roam-dir)
  (org-noter-separate-notes-from-heading t) ; Add blank line betwwen note heading and content
  (org-noter-notes-window-location 'horizontal-split) ; Horizontal split between notes and pdf
  (org-noter-always-create-frame nil) ; Don't open frame
  (org-noter-hide-other nil) ; Show notes that aren't synced with (you're on)
  (org-noter-auto-save-last-location t) ; Go to last location
  (org-noter-kill-frame-at-session-end nil) ; Don't close frame when killing pdf buffer
  :config
  (add-hook 'org-noter-doc-mode-hook ; Add keykinds only for org-noter pdf (doc)
            (lambda ()
              (general-define-key
               :keymaps 'local
               "M-o" 'org-noter-insert-note)
              ))

  (kb/leader-keys
    "on" '(org-noter :which-key "Org-noter")
    )
  )

;;;; Org-transclusion
;; Enable transclusion of org files
(use-package org-transclusion
  :disabled t ; Issue with org-roam-node-capture
  :after org-roam
  :straight (org-transclusion :type git :host github :repo "nobiot/org-transclusion")
  :hook (org-mode . org-transclusion-activate)
  :custom
  (org-transclusion-include-first-section t)
  (org-transclusion-exclude-elements '(property-drawer keyword))
  :config
  ;; Currently need to look through Roam directory, not just agenda files
  (org-id-update-id-locations (org-roam--list-all-files))

  ;; Make fringe in referenced node invisible
  (set-face-attribute
   'org-transclusion-source-fringe nil
   :foreground (face-background 'default)
   :background (face-background 'default))

  (kb/leader-keys
    "Tc" '(org-transclusion-mode :which-key "Toggle mode")
    "TR" '(org-transclusion-refresh :which-key "Refresh")
    "Tm" '(org-transclusion-make-from-link :which-key "Make")
    "Ta" '(org-transclusion-add :which-key "Add")
    "Tr" '(org-transclusion-remove :which-key "Remove")
    "Ts" '(org-transclusion-live-sync-start :which-key "Edit start")
    "Te" '(org-transclusion-live-sync-exit :which-key "Edit exit")
    )
  )

;;;; Org-roam-ui
(use-package org-roam-ui
  :after (websocket simple-httpd f org-roam)
  :straight (org-roam-ui :type git :host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :hook (after-init . org-roam-ui-mode)
  :custom
  (org-roam-ui-open-on-start nil) ; Don't open graph on startup
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

;;;;; Dependencies of `org-roam-ui'.
(use-package websocket)
(use-package simple-httpd)
(use-package f)

;;; org-roam-other-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-roam-other-rcp)
