;;; Denote
(use-package denote
  :pin gnu-elpa-devel
  :autoload (denote-directory-files krisb-denote-auto-rename-file krisb-denote-sluggify-keyword)
  :commands (denote denote-open-or-create)
  :hook ((dired-mode . denote-dired-mode)
         (denote-dired-mode . toggle-truncate-lines))
  :bind ( :map krisb-note-keymap
          ("c" . denote-create-note)
          ("f" . denote-open-or-create)
          ("i" . denote-link-or-create)
          ("e" . denote-org-extras-extract-org-subtree)
          ("k" . denote-rename-file-keywords)
          ("l" . denote-backlinks)
          ("L" . denote-find-backlink)
          ("h" . denote-org-extras-backlinks-for-heading))
  :custom
  (denote-directory krisb-notes-directory)
  (denote-prompts '(subdirectory title keywords signature template))
  (denote-file-name-slug-functions '((title . denote-sluggify-title)
                                     (signature . denote-sluggify-signature)
                                     (keyword . krisb-denote-sluggify-keyword)))
  (denote-known-keywords nil)

  (denote-rename-confirmations '(add-front-matter))
  (denote-org-front-matter "#+title: %s
#+date: %s
#+filetags: %s
#+identifier: %s
")
  (denote-templates
   '((plain . "\n")
     (source-note . "
#+begin_src org :exports none
  + Source:
#+end_src\n")
     (mla . "#+latex_class: mla
#+cite_export: biblatex mla-new
#+professor:
#+course:
#+export_file_name:

* Potential titles

* 1 Draft                                                     :export:ignore:

* Works Cited                                                 :ignore:export:

#+begin_export LaTeX
\\newpage
\\center
#+end_export

#+print_bibliography:")
     (buoy . "* Responses

* Biographical information

+ Buoy nominations :: tk
+ Instagram handle :: tk

* Potential titles

1.

* 1 Draft                                                     :ignore:export:
")))

  ;; Buffer name
  (denote-rename-buffer-format "%s %t%b")
  (denote-rename-buffer-backlinks-indicator " ⟷")

  ;; Backlinks
  ;; If we want to use a CUSTOM_ID, we should manually create it.  Otherwise,
  ;; setting this to the value `'id' will over-populate my headings with
  ;; CUSTOM_IDs whenever I call `org-capture'.
  (denote-org-store-link-to-heading 'context)
  (denote-backlinks-show-context t)
  (denote-backlinks-display-buffer-action
   '((display-buffer-reuse-window display-buffer-below-selected)
     (window-height . fit-window-to-buffer)
     (post-command-select-window . t)
     (dedicated . t)
     (window-parameters . ((mode-line-format . none)))))

  ;; Miscellaneous
  (denote-date-prompt-use-org-read-date t)
  :config
  (denote-rename-buffer-mode 1)
  (denote-menu-bar-mode 1)

  (krisb-modus-themes-setup-faces
   "denote"
   (set-face-attribute 'denote-faces-link nil :weight 'normal :foreground fg-active-argument :inherit 'unspecified)
   (set-face-attribute 'denote-faces-signature nil :weight 'bold)
   (set-face-attribute 'denote-faces-title nil :weight 'semibold :foreground cyan-cooler)
   (set-face-attribute 'denote-faces-keywords nil :foreground keyword :slant 'italic)
   (set-face-attribute 'denote-faces-date nil :foreground 'unspecified :inherit 'shadow))

  ;; Camel cased keywords
  (defun krisb-denote-sluggify-keyword (str)
    "Sluggify STR while joining separate words.
  My version camelCases keywords."
    (require 's)
    (s-lower-camel-case (denote-slug-hyphenate str)))

  ;; Add inbox to `org-refile-targets'
  (with-eval-after-load 'org-refile
    (add-to-list 'org-refile-targets
                 `(,(car (denote-directory-files "20221011T101254")) . (:maxlevel . 2))))

  ;; Zettel metadata template
  (with-eval-after-load 'org
    (add-to-list 'org-structure-template-alist
                 '("m" . "src org :exports none")))

  ;; Custom link formatting for denote org links
  (defun krisb-denote-link-ol-get-heading ()
    "Get current Org heading text.
My version uses the full outline path instead of just heading text."
    (let ((heading-text (org-get-heading :no-tags :no-todo :no-priority :no-comment))
          (outline-path (org-get-outline-path)))
      (if outline-path
          (mapconcat #'identity (append outline-path (list heading-text)) " > ")
        heading-text)))
  (advice-add 'denote-link-ol-get-heading :override #'krisb-denote-link-ol-get-heading))

;;; Denote-journal-extras
(use-package denote-journal-extras
  :ensure nil
  :custom
  (denote-journal-extras-directory
   (expand-file-name "commonplace_book/journal" denote-directory))
  (denote-journal-extras-keyword "journal")
  (denote-journal-extras-title-format 'day-date-month-year-24h))

;;; Krisb-denote-ext
(use-package krisb-denote-ext
  :ensure nil
  :after denote
  :hook (after-save . krisb-denote-ext-auto-rename-file))

;;; Denote-explore
;; Useful Denote utilities
(use-package denote-explore
  :after denote
  ;; Don't forget to install the required dependencies required for my chosen
  ;; `denote-explore-network-format'
  :ensure-system-package ((dot . graphviz)
                          (R . r))
  :custom
  (denote-explore-network-directory     ; Have to end path in slash
   (no-littering-expand-var-file-name "denote-explore/"))
  (denote-explore-network-format 'd3.js)
  (denote-explore-network-graphviz-filetype "pdf")
  (denote-explore-network-keywords-ignore '("archive")))

;;; Citar-denote
(use-package citar-denote
  :demand t
  :after citar
  :diminish
  :bind ( :map krisb-note-keymap
          ("b b" . citar-denote-link-reference)
          ("b o" . citar-denote-dwim)
          ("b c" . citar-create-note)
          ("b n" . citar-denote-open-note)
          ("b k a" . citar-denote-add-citekey)
          ("b k r" . citar-denote-remove-citekey))
  :custom
  (citar-denote-subdir "/bib/")
  (citar-denote-signature nil)
  (citar-denote-title-format nil)       ; Use citekey as title
  (citar-denote-title-format-authors 2)
  (citar-denote-title-format-andstr "and")
  (citar-denote-keyword "bib")
  (citar-denote-use-bib-keywords nil)
  (citar-denote-template 'default)
  (citar-denote-file-types
   `((org
      :reference-format "#+reference: %s\n" ; Keep single space
      :reference-regex "^#\\+reference\\s-*:")
     (markdown-yaml
      :reference-format "reference:  %s\n"
      :reference-regex "^reference\\s-*:")
     (markdown-toml
      :reference-format "reference  = %s\n"
      :reference-regex "^reference\\s-*=")
     (text
      :reference-format "reference:  %s\n"
      :reference-regex "^reference\\s-*:")))
  :config
  (citar-denote-mode 1)

  ;; Keep the reference keyword after Denote's identifier keyword
  (defun krisb-citar-denote--add-reference (citekey file-type)
    "Like `citar-denote--add-reference' but adds reference to specific line.
Namely, adds the #+reference after the #+identifier line."
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (rx bol (literal "#+identifier:")) nil t)
      (if (eq (or file-type 'org) 'org)
          (forward-line 1)
        (forward-line -2))
      (insert
       (format (citar-denote--reference-format file-type) citekey))))
  (advice-add 'citar-denote--add-reference :override #'krisb-citar-denote--add-reference))

;;; Denote-interface
(use-package denote-interface
  :vc (:url "git@github.com:krisbalintona/denote-interface.git"
            :rev :newest)
  :autoload denote-interface--signature-lessp
  :bind ( :map krisb-note-keymap
          ("m" . denote-interface-list)
          ("r" . denote-interface-set-signature-list)
          ("R" . denote-interface-set-signature-minibuffer))
  :custom
  (denote-interface-signature-column-width
   (+ 6 (cl-loop for file in (denote-directory-files)
                 maximize (length (denote-retrieve-filename-signature file)))))
  (denote-interface-title-column-width 120)
  (denote-interface-starting-filter-presets
   '("zettels/[^z-a]*" "bib/[^z-a]*"))
  (denote-interface-starting-filter "zettels/[^z-a]*")
  :init
  (with-eval-after-load 'denote
    (setopt denote-sort-signature-comparison-function #'denote-interface--signature-lessp)))

;;; Provide
(provide 'krisb-denote)
