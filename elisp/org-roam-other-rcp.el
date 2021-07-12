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
  :init
  (defun kb/bibtex-completion-format-citation-autocite (keys)
    "My own bibtex-completion-format. Accepts KEYS."
    (s-join ", "
            (--map (format "autocite:%s" it) keys)))
  :custom
  (bibtex-completion-notes-path kb/roam-dir) ; Irrelevant since I use org-roam-bibtex instead
  (bibtex-completion-library-path (concat kb/roam-dir "bibliographic/bib-pdfs")) ; Where bibtex searches for pdfs
  (bibtex-completion-bibliography (concat kb/roam-dir "bibliographic/master-lib.bib"))
  (bibtex-completion-pdf-field "file") ; Zotero stores pdfs in a field called file - this settings allows bibtex to find the pdf
  (bibtex-completion-pdf-open-function ; Use okular to open a pdf
   (lambda (fpath)
     (call-process "okular" nil 0 nil fpath)))
  (bibtex-completion-browser-function 'browse-url-default-browser) ; Use default browser to open
  (ivy-bibtex-default-action 'ivy-bibtex-insert-citation) ; Edit notes on default selection

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
     ("k" ivy-bibtex-insert-key "Insert key")
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
  (org-ref-default-bibliography (concat kb/roam-dir "bibliographic/master-lib.bib"))
  (org-ref-bibliography-notes (concat kb/roam-dir "bibliographic/bib-notes.org")) ; Irrelevant for me - I have it here just in case
  (org-ref-pdf-directory (concat kb/roam-dir "bibliographic/bib-pdfs/"))
  (org-ref-notes-directory kb/roam-dir) ; Same directory as org-roam

  (org-ref-completion-library 'org-ref-ivy-cite) ; Use ivy
  (org-ref-note-title-format
   "* TODO %y - %t\n :PROPERTIES:\n  :CUSTOM_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n")
  (org-ref-notes-function 'orb-edit-notes)
  (org-ref-default-citation-link "autocite")
  :config  
  ;; Files removed after `org-export' to LaTeX
  (add-to-list 'org-latex-logfiles-extensions "tex")
  (add-to-list 'org-latex-logfiles-extensions "bbl")
  (add-to-list 'org-latex-logfiles-extensions "pdf")
  (add-to-list 'org-latex-logfiles-extensions "synctex.gz")
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

;;;; Additional code
;;;;; Number of backlinks in `orgroam' buffer
;; Include number of backlinks for each node in the org-roam buffer.
;; From https://gist.github.com/nobiot/852978b41b1869df3cf9180202f5bbc9
(define-minor-mode nobiot/org-roam-v2-extensions-mode
  "Toggle custom enhancements for Org-roam V2.
It does not work for V1."
  :init-value nil
  :lighter nil
  :global t
  (cond
   (nobiot/org-roam-v2-extensions-mode
    ;; Activate
    (require 'org-roam)
    (advice-add #'org-roam-node--annotation :override #'nobiot/org-roam-node--annotation))
   (t
    ;; Deactivate
    (advice-remove #'org-roam-node--annotation #'nobiot/org-roam-node--annotation))))

(defun nobiot/org-roam-node--annotation (node-title)
  "Return the annotation string for a NODE-TITLE.
This custom function enhances Org-roam's function of the same
name to include number of backlinks for the node."
  (let* ((node (get-text-property 0 'node node-title))
         (tags (org-roam-node-tags node))
         (count)
         (annotation))
    (setq count (caar (org-roam-db-query
                       [:select (funcall count source)
                                :from links
                                :where (= dest $s1)
                                :and (= type "id")]
                       (org-roam-node-id node))))
    (concat annotation
            (when tags (format " (%s)" (string-join tags ", ")))
            (when count (format " [%d]" count)))))

;;;;; Additional column in `org-roam-node-find'
;; From https://org-roam.discourse.group/t/find-node-ui-possibilities-for-v2/1422/15
(setq org-roam-node-display-template "${tags:25} ${title:*} ${file:30}")

(defun nobiot/org-roam-get-file-title (filename)
  "Return the title of the file node for FILENAME."
  (caar (org-roam-db-query
        [:select [title] :from nodes :where (and (= level 0)(= file $s1))] filename)))

(defun nobiot/org-roam-node--format-entry (node width)
  "Formats NODE for display in the results list.
WIDTH is the width of the results list.
nobit has modified one line of this function (see the source comment) to get title of the file."
  (let ((format (org-roam--process-display-format org-roam-node-display-template)))
    (s-format (car format)
              (lambda (field)
                (let* ((field (split-string field ":"))
                       (field-name (car field))
                       (field-width (cadr field))
                       (getter (intern (concat "org-roam-node-" field-name)))
                       (field-value (or (funcall getter node) "")))
                  (when (and (equal field-name "tags")
                             field-value)
                    (setq field-value (org-roam--tags-to-str field-value)))
                  (when (and (equal field-name "file")
                             field-value)
                    (setq field-value (nobiot/org-roam-get-file-title field-value))) ;; << Changed by nobiot
                  (if (not field-width)
                      field-value
                    (setq field-width (string-to-number field-width))
                    (truncate-string-to-width
                     field-value
                     (if (> field-width 0)
                         field-width
                       (- width (cdr format)))
                     0 ?\s)))))))

(advice-add #'org-roam-node--format-entry :override #'nobiot/org-roam-node--format-entry)

;;; org-roam-other-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-roam-other-rcp)
