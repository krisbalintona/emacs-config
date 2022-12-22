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
  :general (:keymaps 'org-mode-map
            [remap citar-insert-citation] '(org-cite-insert :wk "Insert citation"))
  :custom
  (org-cite-global-bibliography kb/bib-files)
  (org-cite-csl-locales-dir (file-name-concat user-emacs-directory "locales/"))
  (org-cite-csl-styles-dir (expand-file-name "~/Documents/Zotero/styles/"))
  (org-cite-export-processors
   '((md . (csl "chicago-fullnote-bibliography.csl"))   ; Footnote reliant
     (latex biblatex)                                 ; For humanities
     (odt . (csl "chicago-fullnote-bibliography.csl"))  ; Footnote reliant
     (docx . (csl "chicago-fullnote-bibliography.csl")) ; Footnote reliant
     (t . (csl "modern-language-association.csl"))      ; Fallback
     ))
  :custom-face
  ;; Have citation link faces look closer to as they were for `org-ref'
  (org-cite ((t (:foreground "DarkSeaGreen4"))))
  (org-cite-key ((t (:foreground "forest green" :slant italic)))))

;;; Org-roam-bibtex
;; Integrate citation backends with latex's citation management systems like
;; `org-cite'
(use-package org-roam-bibtex
  :after (org-roam oc)
  :ghook 'org-mode-hook ; FIXME 2021-09-14: Make so that I don't need to call in this way
  )

;;; Citar
;; Alternative to `ivy-bibtex' and `helm-bibtex'
(use-package citar
  :straight (citar :type git :host github :repo "emacs-citar/citar" :includes (citar-org))
  :commands (citar-insert-citation citar-insert-reference citar-open-notes kb/org-roam-node-from-cite)
  :general
  (kb/note-keys
    "C" '(kb/org-roam-node-from-cite :wk "Citar-capture"))
  (:keymaps 'org-mode-map
   :prefix "C-c b"
   "b" '(citar-insert-citation :wk "Insert citation")
   "r" '(citar-insert-reference :wk "Insert reference")
   "o" '(citar-open-notes :wk "Open note"))
  :custom
  (citar-bibliography kb/bib-files)
  (citar-templates
   '((main . "${author editor:30}   ${date year issued:4}    ${title:110}")
     (suffix . "     ${=type=:20}    ${tags keywords keywords:*}")
     (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
     (note . "#+title: Notes on ${author editor}, ${title}") ; For new notes
     ))
  ;; Configuring all-the-icons. From
  ;; https://github.com/bdarcus/citar#rich-ui
  (citar-symbols
   `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) .
           ,(all-the-icons-faicon "file-o" :face 'kb/citar-icon-dim :v-adjust -0.1) )
     (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) .
           ,(all-the-icons-material "speaker_notes" :face 'kb/citar-icon-dim :v-adjust -0.3))
     (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) .
           ,(all-the-icons-octicon "link" :face 'kb/citar-icon-dim :v-adjust 0.01))))
  (citar-symbol-separator "  ")

  (citar-notes-paths (list kb/notes-dir))
  (citar-open-note-function 'orb-citar-edit-note) ; Open notes in `org-roam'
  (citar-at-point-function 'embark-act)           ; Use `embark'
  :init
  ;; Here we define a face to dim non 'active' icons, but preserve alignment.
  ;; Change to your own theme's background(s)
  (defface kb/citar-icon-dim
    ;; Based on solaire's faces
    '((((background dark)) :foreground "#212428")
      (((background light)) :foreground "#f0f0f0"))
    "Face for having icons' color be identical to the theme
  background when \"not shown\".")
  :config
  ;; Create a new node from a bibliographic source. Taken from
  ;; https://jethrokuan.github.io/org-roam-guide/
  (defun kb/org-roam-node-from-cite (keys-entries)
    (interactive (list (citar-select-ref :multiple nil :rebuild-cache t)))
    (let ((title (citar--format-entry-no-widths (cdr keys-entries)
                                                "${author editor}${date urldate} :: ${title}")))
      (org-roam-capture- :templates
                         '(("r" "reference" plain
                            "%?"
                            :if-new (file+head "references/${citekey}.org"
                                               ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}
#+filetags: %(kb/insert-lit-category)\n")
                            :immediate-finish t
                            :unnarrowed t))
                         :info (list :citekey (car keys-entries))
                         :node (org-roam-node-create :title title)
                         :props '(:finalize find-file))))

  ;; Add prefix and suffix text immediately after insertion
  (defun kb/citar-org-update-pre-suffix ()
    "Change the pre/suffix text of the reference at point.

My version also adds a space in the suffix so I don't always have
to manually add one myself."
    (interactive)

    ;; Enable `typo' typographic character cycling in minibuffer. Particularly
    ;; useful in adding en- and em-dashes in citation suffixes (e.g. for page
    ;; ranges)
    (when (featurep 'typo)
      (add-hook 'minibuffer-mode-hook 'typo-mode)) ; Enable dashes

    (let* ((datum (org-element-context))
           (datum-type (org-element-type datum))
           (ref (if (eq datum-type 'citation-reference) datum
                  (error "Not on a citation reference")))
           (key (org-element-property :key ref))
           ;; TODO handle space delimiter elegantly.
           (pre (read-string "Prefix text: " (org-element-property :prefix ref)))
           (post (read-string "Suffix text: " (org-element-property :suffix ref))))

      ;; Change post to automatically have one space prior to any user-inputted
      ;; suffix
      (setq post
            (if (string= (replace-regexp-in-string "\s-*" "" post) "")
                ""     ; If there is nothing of substance (e.g. an empty string)
              (replace-regexp-in-string "^[\s-]*" " " post))) ; Only begin with one space

      (setf (buffer-substring (org-element-property :begin ref)
                              (org-element-property :end ref))
            (org-element-interpret-data
             `(citation-reference
               (:key ,key :prefix ,pre :suffix ,post)))))

    ;; Remove hook if it was added earlier
    (remove-hook 'minibuffer-mode-hook 'typo-mode))
  (advice-add 'citar-org-update-pre-suffix :override #'kb/citar-org-update-pre-suffix)

  ;; Run `citar-org-update-pre-suffix' (which is overridden by my
  ;; `kb/citar-org-update-pre-suffix') right after `org-cite-insert' to
  ;; immediately set its prefix and suffix
  (advice-add 'org-cite-insert :after #'(lambda (args)
                                           (save-excursion
                                             (left-char) ; First move point inside citation
                                             (citar-org-update-pre-suffix)))))
;;; Citar-org
;; Use `citar' with `org-cite'
(use-package citar-org
  :after oc
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))

;;; org-citations-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-citations-rcp)
