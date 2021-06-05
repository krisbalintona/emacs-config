;;; org-roam-other-rcp.el --- Summary
;;
;;; Commentary:
;;
;; This is all the configuration for packages related to org-roam
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Citations
;;;;; Ivy/Helm-bibtex
;; Use ivy or helm search frontend with the bibtex-completion backend
(use-package ivy-bibtex
  :after org-roam
  :custom
  (bibtex-completion-notes-path kb/roam-dir) ; Irrelevant since I use org-roam-bibtex instead
  (bibtex-completion-library-path (concat kb/roam-dir "bibliographic/bib-pdfs")) ; Where bibtex searches for pdfs
  (bibtex-completion-bibliography (concat kb/roam-dir "bibliographic/master-lib.bib"))
  (bibtex-completion-pdf-field "file") ; Zotero stores pdfs in a field called file - this settings allows bibtex to find the pdf
  (bibtex-completion-pdf-open-function ; Use okular to open a pdf
   (lambda (fpath)
     (call-process "okular" nil 0 nil fpath)))
  (bibtex-completion-browser-function 'browse-url-default-browser) ; Use default browser to open
  (ivy-bibtex-default-action 'ivy-bibtex-edit-notes) ; Edit notes on defualt selection

  ;; Template for new note (but I use orb for this)
  (bibtex-completion-notes-template-multiple-files
   (concat
    "#+TITLE: ${title}\n"
    "#+ROAM_KEY: cite:${=key=}\n"
    "* TODO Notes\n"
    ":PROPERTIES:\n"
    ":Custom_ID: ${=key=}\n"
    ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
    ":AUTHOR: ${author-abbrev}\n"
    ":JOURNAL: ${journaltitle}\n"
    ":DATE: ${date}\n"
    ":YEAR: ${year}\n"
    ":DOI: ${doi}\n"
    ":URL: ${url}\n"
    ":END:\n\n")
   )

  ;; Symbols used for indicating the availability of notes and PDF files
  (bibtex-completion-pdf-symbol "🖇")
  (bibtex-completion-notes-symbol "🖋")
  :config
  ;; ivy-bibtex requires ivy's `ivy--regex-ignore-order` which I already
  ;; have set in ivy-re-builders-alist
  (autoload 'ivy-bibtex "ivy-bibtex" "" t)

  (ivy-set-actions ; Actions shown after M-o
   'ivy-bibtex
   '(("p" ivy-bibtex-open-any "Open PDF, URL, or DOI")
     ("e" ivy-bibtex-edit-notes "Edit notes")
     ("c" ivy-bibtex-insert-citation "Insert citation")
     ("r" ivy-bibtex-insert-reference "Insert reference")
     ("P" ivy-bibtex-open-annotated-pdf "Open annotated PDF (if present)") ; This last function doesn't have an associated action yet (for annotated pdfs)
     ("a" bibtex-completion-add-pdf-to-library "Add pdf to library")
     ))

  (kb/leader-keys
    "fa" '(ivy-bibtex :which-key "Ivy-bibtex")
    "fA" '(ivy-bibtex-with-notes :which-key "Ivy-bibtex only notes")
    )
  )

;;;;; Org-ref
;; Bibtex is a way to add bibliographic information (e.g. references/citations to
;; equations, sources, images, etc) in latex. Ivy/helm-bibtex is a way to access
;; the .bib files bibtex makes. Org-ref is a way to directly insert citations
;; and references into latex and org files
(use-package org-ref
  :after ivy
  :custom
  (org-ref-notes-directory kb/roam-dir) ; Same directory as org-roam
  (org-ref-bibliography-notes (concat kb/roam-dir "bibliographic/bib-notes.org")) ; Irrelevant for me - I have it here just in case
  (org-ref-pdf-directory (concat kb/roam-dir "bibliographic/bib-pdfs/"))
  (org-ref-default-bibliography (concat kb/roam-dir "bibliographic/master-lib.bib"))
  (org-ref-completion-library 'org-ref-ivy-cite) ; Use ivy
  (org-ref-note-title-format
   "* TODO %y - %t\n :PROPERTIES:\n  :CUSTOM_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n")
  (org-ref-notes-function 'orb-edit-notes)
  (org-ref-default-citation-link "autocite")
  )

;;;;; Org-roam-bibtex
;; Ivy/helm-bibtex (which integrates with bibtex-completion) integration with
;; org-roam (provides templates and modifies edit notes action)
(use-package org-roam-bibtex
  :straight (org-roam-bibtex :type git :host github :repo "org-roam/org-roam-bibtex" :branch "origin/org-roam-v2") ; For org-roam v2
  :after (org-roam org-ref ivy-bibtex)
  :custom
  (orb-preformat-keywords
   '(("citekey" . "=key=") "title" "url" "file" "author-or-editor" "keywords"))
  :config
  (org-roam-bibtex-mode)
  )

;;;; Note-taking
;;;;; Pdf-tools
;; View pdfs and interact with them. Has many dependencies
;; https://github.com/politza/pdf-tools#compiling-on-fedora
(use-package pdf-tools
  :straight (pdf-tools :type git :host github :repo "vedang/pdf-tools") ; Repo of current maintianer
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

;;; org-roam-other-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-roam-other-rcp)
