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
  :general
  (:keymaps 'org-mode-map
            [remap citar-insert-citation] '(org-cite-insert :wk "Insert citation"))
  :custom
  (org-cite-global-bibliography kb/bib-files)
  (org-cite-csl-locales-dir (expand-file-name (concat user-emacs-directory "locales/")))
  (org-cite-csl-styles-dir (expand-file-name "~/Documents/Zotero/styles/"))
  (org-cite-export-processors
   '((md csl "chicago-fullnote-bibliography.csl")   ; Footnote reliant
     (latex biblatex)                               ; For humanities
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
  :after (org-roam oc)
  :ghook 'org-mode-hook ; FIXME 2021-09-14: Make so that I don't need to call in this way
  )

;;; Citar
;; Alternative to `ivy-bibtex' and `helm-bibtex'
(use-package citar
  :straight (citar :type git :host github :repo "emacs-citar/citar" :includes citar-org)
  :commands (citar-insert-citation citar-insert-reference citar-open-notes kb/org-roam-node-from-cite)
  :general
  (kb/note-keys
    "C" '(kb/org-roam-node-from-cite :wk "Citar-capture")
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
  ;; Add prefix and suffix text immediately after insertion
  (defun kb/citar-org-update-pre-suffix ()
    "Change the pre/suffix text of the reference at point. My version
that also adds a space in the suffix so I don't always have to
manually add one myself."
    (interactive)
    (let* ((datum (org-element-context))
           (datum-type (org-element-type datum))
           (ref (if (eq datum-type 'citation-reference) datum
                  (error "Not on a citation reference")))
           (key (org-element-property :key ref))
           ;; TODO handle space delimiter elegantly.
           (pre (read-string "Prefix text: " (org-element-property :prefix ref)))
           (post (read-string "Suffix text: " (org-element-property :suffix ref))))
      (setq post
            (if (string= (replace-regexp-in-string "\s-*" "" post) "")
                ""       ; If there is nothing of substance (e.g. just a string)
              (replace-regexp-in-string "^[\s-]*" " " post) ; Only begin with one space
              ))
      (setf (buffer-substring (org-element-property :begin ref)
                              (org-element-property :end ref))
            (org-element-interpret-data
             `(citation-reference
               (:key ,key :prefix ,pre :suffix ,post))))))
  (advice-add 'citar-org-update-pre-suffix :override #'kb/citar-org-update-pre-suffix)
  (advice-add 'org-cite-insert :after #'(lambda (args)
                                          (require 'typo)
                                          (add-hook 'minibuffer-mode-hook 'typo-mode) ; Enable dashes
                                          (save-excursion ; End with point after citation
                                            (left-char)
                                            (citar-org-update-pre-suffix))
                                          (remove-hook 'minibuffer-mode-hook 'typo-mode)))
  (advice-add 'org-cite-insert :around #'(lambda (orig-fun &rest args)
                                           (let ((kb/typo-cycle-message nil)) ; Disable annoying echos when in minibuffer
                                             (apply orig-fun args))))

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
                         :props '(:finalize find-file)))))

;;; Citar-org
;; Citar compatibility with `org-cite'
(use-package citar-org
  :commands (citar-insert-citation citar-insert-reference citar-open-notes kb/org-roam-node-from-cite)
  :after (citar oc)
  :custom
  ;; Use `citar'
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  )

;;; org-citations-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-citations-rcp)
