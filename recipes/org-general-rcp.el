;;; org-general-rcp.el --- Summary
;;
;;; Commentary:
;;
;; This is all the configuration of the org-roam package
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Org-mode itself
(use-package org
  :straight (org-mode :type git
                      :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
                      :pre-build ("make" "oldorg" "EMACS=/usr/local/bin/emacs")
                      :files (:defaults "lisp")
                      :includes (ox ox-odt ox-latex org-footnote org-attach org-refile))
  :gfhook
  'prettify-symbols-mode
  'variable-pitch-mode
  'visual-line-mode
  '(lambda ()
     (eldoc-mode -1))
  '(lambda ()
     (require 'prog-mode)
     (push '("->" . ?➡) prettify-symbols-alist)
     (push '("<-" . ?⬅) prettify-symbols-alist)
     (prettify-symbols-mode))
  :general
  (:keymaps 'org-mode-map
            "M-u" 'org-up-element
            "M-d" 'org-down-element
            "M-n" 'org-forward-heading-same-level
            "M-p" 'org-backward-heading-same-level)
  (:keymaps 'org-mode-map
            :states '(normal visual motion)
            "zi" 'org-toggle-inline-images)
  (:keymaps 'org-mode-map
            :states 'insert
            "M-k" 'org-metaup
            "M-j" 'org-metadown
            "M-K" 'org-shiftmetaup
            "M-J" 'org-shiftmetadown
            "C-a" 'org-beginning-of-line)
  (kb/note-keys
    "c" '(org-capture :wk "Org-capture")
    )
  (kb/mark-keys
    "g" '(org-mark-ring-goto :wk "Goto last mark")
    "a" '(org-mark-ring-push :wk "Push to mark-ring")
    )
  :custom
  (org-directory kb/org-dir)

  ;; Aesthetics
  (org-startup-indented t) ; Start with `org-indent-mode'?
  (org-startup-folded 'nofold)
  (org-ellipsis " ")
  (org-hide-emphasis-markers t)          ; Remove org-mode markup characters
  (org-fontify-quote-and-verse-blocks t) ; Properly syntax highlight block contents
  (org-pretty-entities t)           ; Show as UTF-8 characters (useful for math)
  (org-pretty-entities-include-sub-superscripts nil) ; Show super- and subscripts?
  (org-hidden-keywords '(title)) ; hide #+TITLE:
  (org-highlight-sparse-tree-matches nil) ; Don't highlight spare tree matches

  ;; For writing
  (org-special-ctrl-a/e t) ; Make ^ and $ ignore tags and leading stars
  (org-src-tab-acts-natively t) ; Treat tabs in src blocks the same as if it
  (org-src-window-setup 'current-window) ; Open src block window on current buffer were in the language's major mode

  ;; For opening files based on extension
  (org-file-apps
   '(("\\.docx\\'" . eaf-org-open-file)
     ("\\.odt\\'" . eaf-org-open-file)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . eaf-org-open-file)
     ("\\.pdf\\'" . eaf-org-open-file)
     (directory . emacs)
     (auto-mode . emacs)
     ))

  ;; Misc
  (org-ctrl-k-protect-subtree t)
  (org-element-use-cache t)             ; Testing
  :config
  (advice-add 'org-ctrl-c-ret :after #'evil-insert-state) ; Entire insert-state after M-RET

  ;; Org link parameters (good for modifying faces)
  (make-face 'kb/org-roam-link-to-node)
  (modify-face 'kb/org-roam-link-to-node "goldenrod3" nil nil nil t nil nil nil)
  (org-link-set-parameters "id" :follow 'org-id-open :face 'kb/org-roam-link-to-node)
  :config
  ;; Use EAF to open PDFs
  (defun eaf-org-open-file (file &optional link)
    "A wrapper function on `eaf-open'. Open in another window and
move to that window."
    (when (< (length (window-list)) 2)
      (split-window-right))
    (other-window 1)
    (eaf-open file)))

;;; Related to org-export
;;;; Org-export
(use-package ox
  :custom
  (org-export-with-sub-superscripts nil)
  ;; Async export
  (org-export-in-background nil)          ; Have it be default?
  (org-export-async-debug t)
  (org-export-async-init-file (locate-library "quickstart")))

;;;; Ox-odt
(use-package ox-odt
  :custom
  (org-odt-preferred-output-format "docx") ; Convert to docx at the end of conversion
  )

;;;; Ox-latex
(use-package ox-latex
  :custom
  (org-latex-compiler "xelatex")
  ;; (org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
  ;; Templates
  (org-latex-title-command "\\maketitle")
  (org-latex-toc-command
   "\\renewcommand{\\contentsname}{
  \\begin{center}
    Table of Contents
  \\end{center}
}
\\tableofcontents
\\newpage")
  (org-export-with-toc nil)
  (org-latex-default-packages-alist
   '(("AUTO" "inputenc" t
      ("pdflatex"))
     ("T1" "fontenc" t
      ("pdflatex"))
     ("" "graphicx" t)
     ("" "longtable" nil)
     ("" "wrapfig" nil)
     ("" "rotating" nil)
     ("normalem" "ulem" t)
     ("" "amsmath" t)
     ("" "amssymb" t)
     ("" "capt-of" nil)
     ("" "hyperref" nil)
     ("" "lipsum" nil)                  ; Sample text
     ))
  (org-latex-hyperref-template
   "\\hypersetup{
  pdfauthor={%a},
  pdftitle={%t},
  pdfkeywords={%k},
  pdfsubject={%d},
  pdfcreator={%c},
  pdflang={%L}}\n")
  :config
  (add-to-list 'org-latex-classes '("mla"
                                    "% * Preamble
\\documentclass[12pt,letterpaper]{article}

% * Default packages?
[NO-DEFAULT-PACKAGES]

% ** MLA package
\\usepackage{ifpdf}
\\usepackage{/home/krisbalintona/Documents/org-database/latex/packages/mla}"
                                    ("\\section*{%s}" . "\\section*{%s}")
                                    ("\\subsection*{%s}" . "\\subsection*{%s}")
                                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                                    ))
  (add-to-list 'org-latex-classes '("review"
                                    "% * Preamble
\\documentclass[a4paper,landscape]{article}

% * Default packages?
[NO-DEFAULT-PACKAGES]

% ** Review package
\\usepackage{ifpdf}
\\usepackage{/home/krisbalintona/Documents/org-database/latex/packages/review}"
                                    ("\\section{%s}" . "\\section*{%s}")
                                    ("\\subsection{%s}" . "\\subsection*{%s}")
                                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                                    ))

  ;; Support more file keywords for MLA papers
  (org-export-define-backend 'latex     ; Recognize the professor and course keywords
    '((bold . org-latex-bold)
      (center-block . org-latex-center-block)
      (clock . org-latex-clock)
      (code . org-latex-code)
      (drawer . org-latex-drawer)
      (dynamic-block . org-latex-dynamic-block)
      (entity . org-latex-entity)
      (example-block . org-latex-example-block)
      (export-block . org-latex-export-block)
      (export-snippet . org-latex-export-snippet)
      (fixed-width . org-latex-fixed-width)
      (footnote-definition . org-latex-footnote-definition)
      (footnote-reference . org-latex-footnote-reference)
      (headline . org-latex-headline)
      (horizontal-rule . org-latex-horizontal-rule)
      (inline-src-block . org-latex-inline-src-block)
      (inlinetask . org-latex-inlinetask)
      (italic . org-latex-italic)
      (item . org-latex-item)
      (keyword . org-latex-keyword)
      (latex-environment . org-latex-latex-environment)
      (latex-fragment . org-latex-latex-fragment)
      (line-break . org-latex-line-break)
      (link . org-latex-link)
      (node-property . org-latex-node-property)
      (paragraph . org-latex-paragraph)
      (plain-list . org-latex-plain-list)
      (plain-text . org-latex-plain-text)
      (planning . org-latex-planning)
      (property-drawer . org-latex-property-drawer)
      (quote-block . org-latex-quote-block)
      (radio-target . org-latex-radio-target)
      (section . org-latex-section)
      (special-block . org-latex-special-block)
      (src-block . org-latex-src-block)
      (statistics-cookie . org-latex-statistics-cookie)
      (strike-through . org-latex-strike-through)
      (subscript . org-latex-subscript)
      (superscript . org-latex-superscript)
      (table . org-latex-table)
      (table-cell . org-latex-table-cell)
      (table-row . org-latex-table-row)
      (target . org-latex-target)
      (template . org-latex-template)
      (timestamp . org-latex-timestamp)
      (underline . org-latex-underline)
      (verbatim . org-latex-verbatim)
      (verse-block . org-latex-verse-block)
      ;; Pseudo objects and elements.
      (latex-math-block . org-latex-math-block)
      (latex-matrices . org-latex-matrices))
    :menu-entry
    '(?l "Export to LaTeX"
         ((?L "As LaTeX buffer" org-latex-export-as-latex)
          (?l "As LaTeX file" org-latex-export-to-latex)
          (?p "As PDF file" org-latex-export-to-pdf)
          (?o "As PDF file and open"
              (lambda (a s v b)
                (if a (org-latex-export-to-pdf t s v b)
                  (org-open-file (org-latex-export-to-pdf nil s v b)))))))
    :filters-alist '((:filter-options . org-latex-math-block-options-filter)
                     (:filter-paragraph . org-latex-clean-invalid-line-breaks)
                     (:filter-parse-tree org-latex-math-block-tree-filter
                                         org-latex-matrices-tree-filter
                                         org-latex-image-link-filter)
                     (:filter-verse-block . org-latex-clean-invalid-line-breaks))
    :options-alist
    '((:latex-class "LATEX_CLASS" nil org-latex-default-class t)
      (:latex-class-options "LATEX_CLASS_OPTIONS" nil nil t)
      (:latex-header "LATEX_HEADER" nil nil newline)
      (:latex-header-extra "LATEX_HEADER_EXTRA" nil nil newline)
      (:description "DESCRIPTION" nil nil parse)
      (:keywords "KEYWORDS" nil nil parse)
      (:subtitle "SUBTITLE" nil nil parse)
      ;; MLA file keywords
      (:professor "PROFESSOR" nil nil parse)
      (:course "COURSE" nil nil parse)
      ;; Other variables.
      (:latex-active-timestamp-format nil nil org-latex-active-timestamp-format)
      (:latex-caption-above nil nil org-latex-caption-above)
      (:latex-classes nil nil org-latex-classes)
      (:latex-default-figure-position nil nil org-latex-default-figure-position)
      (:latex-default-table-environment nil nil org-latex-default-table-environment)
      (:latex-default-quote-environment nil nil org-latex-default-quote-environment)
      (:latex-default-table-mode nil nil org-latex-default-table-mode)
      (:latex-diary-timestamp-format nil nil org-latex-diary-timestamp-format)
      (:latex-engraved-options nil nil org-latex-engraved-options)
      (:latex-engraved-preamble nil nil org-latex-engraved-preamble)
      (:latex-engraved-theme "LATEX_ENGRAVED_THEME" nil org-latex-engraved-theme)
      (:latex-footnote-defined-format nil nil org-latex-footnote-defined-format)
      (:latex-footnote-separator nil nil org-latex-footnote-separator)
      (:latex-format-drawer-function nil nil org-latex-format-drawer-function)
      (:latex-format-headline-function nil nil org-latex-format-headline-function)
      (:latex-format-inlinetask-function nil nil org-latex-format-inlinetask-function)
      (:latex-hyperref-template nil nil org-latex-hyperref-template t)
      (:latex-image-default-scale nil nil org-latex-image-default-scale)
      (:latex-image-default-height nil nil org-latex-image-default-height)
      (:latex-image-default-option nil nil org-latex-image-default-option)
      (:latex-image-default-width nil nil org-latex-image-default-width)
      (:latex-images-centered nil nil org-latex-images-centered)
      (:latex-inactive-timestamp-format nil nil org-latex-inactive-timestamp-format)
      (:latex-inline-image-rules nil nil org-latex-inline-image-rules)
      (:latex-link-with-unknown-path-format nil nil org-latex-link-with-unknown-path-format)
      (:latex-src-block-backend nil nil org-latex-src-block-backend)
      (:latex-listings-langs nil nil org-latex-listings-langs)
      (:latex-listings-options nil nil org-latex-listings-options)
      (:latex-minted-langs nil nil org-latex-minted-langs)
      (:latex-minted-options nil nil org-latex-minted-options)
      (:latex-prefer-user-labels nil nil org-latex-prefer-user-labels)
      (:latex-subtitle-format nil nil org-latex-subtitle-format)
      (:latex-subtitle-separate nil nil org-latex-subtitle-separate)
      (:latex-table-scientific-notation nil nil org-latex-table-scientific-notation)
      (:latex-tables-booktabs nil nil org-latex-tables-booktabs)
      (:latex-tables-centered nil nil org-latex-tables-centered)
      (:latex-text-markup-alist nil nil org-latex-text-markup-alist)
      (:latex-title-command nil nil org-latex-title-command)
      (:latex-toc-command nil nil org-latex-toc-command)
      (:latex-compiler "LATEX_COMPILER" nil org-latex-compiler)
      ;; Redefine regular options.
      (:date "DATE" nil "\\today" parse)))
  (defun kb/org-latex-template (contents info) ; Parse the professor and course keywords
    "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
    (let ((title (org-export-data (plist-get info :title) info))
          (spec (org-latex--format-spec info)))
      (concat
       ;; Time-stamp.
       (and (plist-get info :time-stamp-file)
            (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
       ;; LaTeX compiler.
       (org-latex--insert-compiler info)
       ;; Document class and packages.
       (org-latex-make-preamble info)
       ;; Possibly limit depth for headline numbering.
       (let ((sec-num (plist-get info :section-numbers)))
         (when (integerp sec-num)
           (format "\\setcounter{secnumdepth}{%d}\n" sec-num)))
       ;; Author.
       (let ((author (and (plist-get info :with-author)
                          (let ((auth (plist-get info :author)))
                            (and auth (org-export-data auth info)))))
             (email (and (plist-get info :with-email)
                         (org-export-data (plist-get info :email) info))))
         (cond ((and author email (not (string= "" email)))
                (format "\\author{%s\\thanks{%s}}\n" author email))
               ((or author email) (format "\\author{%s}\n" (or author email)))))
       ;; Date.
       (let ((date (and (plist-get info :with-date) (org-export-get-date info))))
         (format "\\date{%s}\n" (org-export-data date info)))
       ;; Title and subtitle.
       (let* ((subtitle (plist-get info :subtitle))
              (formatted-subtitle
               (when subtitle
                 (format (plist-get info :latex-subtitle-format)
                         (org-export-data subtitle info))))
              (separate (plist-get info :latex-subtitle-separate)))
         (concat
          (format "\\title{%s%s}\n" title
                  (if separate "" (or formatted-subtitle "")))
          (when (and separate subtitle)
            (concat formatted-subtitle "\n"))))

       ;; Professor
       (let ((last-name (org-export-data (plist-get info :professor) info)))
         (format "\\newcommand{\\professor}{%s}\n"
                 (if (string= last-name "")
                     "PROFESSOR")
                 (concat "Professor" last-name)))

       ;; Course
       (let ((course (org-export-data (plist-get info :course) info)))
         (format "\\newcommand{\\course}{%s}\n"
                 (if (string= course "")
                     "COURSE"
                   course)))

       ;; Hyperref options.
       (let ((template (plist-get info :latex-hyperref-template)))
         (and (stringp template)
              (format-spec template spec)))
       ;; engrave-faces-latex preamble
       (when (and (eq org-latex-src-block-backend 'engraved)
                  (org-element-map (plist-get info :parse-tree)
                      '(src-block inline-src-block) #'identity
                      info t))
         (org-latex-generate-engraved-preamble info t))
       ;; Document start.
       "\\begin{document}\n\n"
       ;; Title command.
       (let* ((title-command (plist-get info :latex-title-command))
              (command (and (stringp title-command)
                            (format-spec title-command spec))))
         (org-element-normalize-string
          (cond ((not (plist-get info :with-title)) nil)
                ((string= "" title) nil)
                ((not (stringp command)) nil)
                ((string-match "\\(?:[^%]\\|^\\)%s" command)
                 (format command title))
                (t command))))
       ;; Table of contents.
       (let ((depth (plist-get info :with-toc)))
         (when depth
           (concat (when (integerp depth)
                     (format "\\setcounter{tocdepth}{%d}\n" depth))
                   (plist-get info :latex-toc-command))))
       ;; Document's body.
       contents
       ;; Creator.
       (and (plist-get info :with-creator)
            (concat (plist-get info :creator) "\n"))
       ;; Document end.
       "\\end{document}")))
  (advice-add 'org-latex-template :override #'kb/org-latex-template))

;;;; Org-contrib
(use-package org-contrib
  :after org
  :init
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)) ; The ignore tag will export contents but ignore heading
  )

;;;; Ox-pandoc
;; Export to whatever file format pandoc can export to
(use-package ox-pandoc
  :commands org-export-dispatch
  :ensure-system-package pandoc
  )

;;; Org-footnote
(use-package org-footnote
  :after org
  :general (:keymaps 'org-mode-map
                     "C-x f" '(org-footnote-new :wk "New footnote"))
  :custom
  (org-footnote-section nil)            ; Don't create footnote headline
  (org-footnote-auto-adjust t)          ; Automatically renumber
  (org-footnote-define-inline t) ; Write footnote content where you declare rather in a particular section (i.e. `org-footnote-section')?
  (org-footnote-fill-after-inline-note-extraction t) ; Not sure what this does
  )

;;; Org-attach
(use-package org-attach
  :custom
  (org-attach-preferred-new-method 'id) ; Necessary to add the ATTACH tag
  (org-attach-auto-tag "ATTACH")       ; See `org-roam-db-node-include-function'
  (org-attach-id-dir "attachments/")
  (org-attach-dir-relative t)        ; Use relative file paths?
  (org-attach-method 'cp)            ; Attach copies of files
  (org-attach-archive-delete 'query) ; If subtree is deleted or archived, ask user
  ;; Use timestamps as UUIDs and in attachment directory hierarchy
  (org-id-method 'ts)
  (org-attach-id-to-path-function-list
   '(org-attach-id-ts-folder-format
     org-attach-id-uuid-folder-format))
  )

;;; Org-refile
(use-package org-refile
  :after org-roam
  :general (kb/note-keys
             "r" '(org-refile :wk "Org-refile"))
  :custom
  (org-refile-targets
   `((kb/find-blog-files-org . (:maxlevel . 4))
     (org-agenda-files . (:regexp . "tnaoirnta")) ; This random string will remove all headlines
     ("/home/krisbalintona/Documents/org-database/roam/inbox.org" . (:level . 0)) ; Inbox file
     (nil . (:level . 1))
     ))
  (org-refile-use-cache nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  :init
  (defun kb/find-blog-files-org ()
    "Return a list of org files which are within the blog directory of org-roam."
    (org-roam--list-files (concat kb/roam-dir "blog")))
  :config
  ;; Workaround for orderless issue with `org-refile'. See
  ;; https://github.com/minad/vertico#org-refile
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (when (featurep 'vertico)
    (advice-add #'org-olpath-completing-read :around
                (lambda (&rest args)
                  (minibuffer-with-setup-hook
                      (lambda () (setq-local completion-styles '(basic)))
                    (apply args)))
                ))
  )

;;; Org-visibility
;; Persist org headline folded/unfolded states
(use-package org-visibility
  :disabled t                           ; Still buggy
  :ghook 'org-mode-hook
  :custom
  (org-visibility-state-file (no-littering-expand-var-file-name "org/.org-visibility"))
  (org-visibility-include-paths nil)
  (org-visibility-include-regexps '("\\.org\\'")) ; Persist all org files regardless of location
  (org-visibility-exclude-paths nil)
  (org-visibility-maximum-tracked-files 500)
  (org-visibility-maximum-tracked-days 60)
  (org-visibility-display-messages nil)) ; Annoying echo area updates

;;; Aesthetics
;;;; Org-superstar
;; Descendant of (and thus superior to) org-bullets
(use-package org-superstar  ;; Improved version of org-bullets
  :ghook 'org-mode-hook
  :custom
  (inhibit-compacting-font-caches t) ; Stop slowdown

  (org-cycle-level-faces nil)
  (org-n-level-faces 5)

  (org-superstar-leading-bullet ?\s)    ; Render leading stars as spaces!
  (org-superstar-leading-fallback ?\s)  ; Hide away leading stars on terminal.
  (org-indent-mode-turns-on-hiding-stars nil) ; Nil according to readme
  (org-hide-leading-stars nil)                ; Nil according to readme
  (org-superstar-remove-leading-stars nil)    ; Keep indentation from `org-indent'

  (org-superstar-cycle-headline-bullets nil) ; Don't repeat bullets in hierarchy
  (org-superstar-todo-bullet-alist
   '(("TODAY" . 9744)
     ("PROG" . 9744)
     ("NEXT" . 9744)
     ("TODO" . 9744)
     ("DONE" . 9745)
     ("CANCELLED" . 9745)
     ("[ ]"  . 9744)
     ("[X]"  . 9745)))
  (org-superstar-special-todo-items t)  ; Cool todo headlines?
  (org-superstar-headline-bullets-list '("⚝" "●" "⊙" "○"))

  (org-superstar-prettify-item-bullets t)
  (org-superstar-first-inlinetask-bullet ?▶)
  (org-superstar-item-bullet-alist      ; Plain lists
   '((?+ . ?➣)
     (?- . ?»)
     (?* . ?￮)
     ))
  )

;;;; Org-bars
(use-package org-bars
  :after org
  :commands org-bars-mode
  :straight (org-bars :type git :host github :repo "tonyaldon/org-bars")
  :custom
  (org-bars-with-dynamic-stars-p nil)   ; Custom headline stars?
  :preface            ; Should be in preface or else the package won't be loaded
  (add-hook 'org-mode-hook #'(lambda ()
                               (setq-local line-spacing 0) ; For smooth lines
                               (org-bars-mode))
            100)      ; Doesn't work if it is added to the beginning of the hook
  )

;;;; Visual-fill-column
;; Soft wrap lines at fill-column
(use-package visual-fill-column
  :straight (visual-fill-column :type git :host github :repo "joostkremers/visual-fill-column")
  :ghook 'org-mode-hook 'mu4e-view-mode-hook
  :custom
  (visual-fill-column-width 120)
  (visual-fill-column-center-text t)
  :custom
  (split-window-preferred-function 'visual-fill-column-split-window-sensibly) ; Be able to vertically split windows that have wide margins
  )

;;;; Org-fancy-priorities
;; Icons for org priorities
(use-package org-fancy-priorities
  :ghook 'org-mode-hook
  :custom
  (org-fancy-priorities-list '((?A . "💀")
                               (?B . "🔥")
                               (?C . "🌟")
                               (?D . "🏃")
                               (?E . "👍")
                               (?F . "🧋")))
  )
;;; Custom functions
;;;; Better RET
;; Alter RET to behave more usefully (like in Doom)
(require 'keybinds-evil-rcp)

;; Requisite helper functions
(defun +org/table-previous-row ()
  "Go to the previous row (same column) in the current table. Before doing so,
re-align the table if necessary. (Necessary because org-mode has a
`org-table-next-row', but not `org-table-previous-row')"
  (interactive)
  (org-table-maybe-eval-formula)
  (org-table-maybe-recalculate-line)
  (if (and org-table-automatic-realign
           org-table-may-need-update)
      (org-table-align))
  (let ((col (org-table-current-column)))
    (beginning-of-line 0)
    (when (or (not (org-at-table-p)) (org-at-table-hline-p))
      (beginning-of-line))
    (org-table-goto-column col)
    (skip-chars-backward "^|\n\r")
    (when (org-looking-at-p " ")
      (forward-char))))
(defun +org-get-todo-keywords-for (&optional keyword)
  "Returns the list of todo keywords that KEYWORD belongs to."
  (when keyword
    (cl-loop for (type . keyword-spec)
             in (cl-remove-if-not #'listp org-todo-keywords)
             for keywords =
             (mapcar (lambda (x) (if (string-match "^\\([^(]+\\)(" x)
                                     (match-string 1 x)
                                   x))
                     keyword-spec)
             if (eq type 'sequence)
             if (member keyword keywords)
             return keywords)))
(defun +org--insert-item (direction)
  (let ((context (org-element-lineage
                  (org-element-context)
                  '(table table-row headline inlinetask item plain-list)
                  t)))
    (pcase (org-element-type context)
      ;; Add a new list item (carrying over checkboxes if necessary)
      ((or `item `plain-list)
       ;; Position determines where org-insert-todo-heading and org-insert-item
       ;; insert the new list item.
       (if (eq direction 'above)
           (org-beginning-of-item)
         (org-end-of-item)
         (backward-char))
       (org-insert-item (org-element-property :checkbox context))
       ;; Handle edge case where current item is empty and bottom of list is
       ;; flush against a new heading.
       (when (and (eq direction 'below)
                  (eq (org-element-property :contents-begin context)
                      (org-element-property :contents-end context)))
         (org-end-of-item)
         (org-end-of-line)))

      ;; Add a new table row
      ((or `table `table-row)
       (pcase direction
         ('below (save-excursion (org-table-insert-row t))
                 (org-table-next-row))
         ('above (save-excursion (org-shiftmetadown))
                 (+org/table-previous-row))))

      ;; Otherwise, add a new heading, carrying over any todo state, if
      ;; necessary.
      (_
       (let ((level (or (org-current-level) 1)))
         ;; I intentionally avoid `org-insert-heading' and the like because they
         ;; impose unpredictable whitespace rules depending on the cursor
         ;; position. It's simpler to express this command's responsibility at a
         ;; lower level than work around all the quirks in org's API.
         (pcase direction
           (`below
            (let (org-insert-heading-respect-content)
              (goto-char (line-end-position))
              (org-end-of-subtree)
              (insert "\n" (make-string level ?*) " ")))
           (`above
            (org-back-to-heading)
            (insert (make-string level ?*) " ")
            (save-excursion (insert "\n"))))
         (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                     (todo-type    (org-element-property :todo-type context)))
           (org-todo
            (cond ((eq todo-type 'done)
                   ;; Doesn't make sense to create more "DONE" headings
                   (car (+org-get-todo-keywords-for todo-keyword)))
                  (todo-keyword)
                  ('todo)))))))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p)))
      (evil-insert 1))))
(defun +org--toggle-inline-images-in-subtree (&optional beg end refresh)
  "Refresh inline image previews in the current heading/tree."
  (let ((beg (or beg
                 (if (org-before-first-heading-p)
                     (line-beginning-position)
                   (save-excursion (org-back-to-heading) (point)))))
        (end (or end
                 (if (org-before-first-heading-p)
                     (line-end-position)
                   (save-excursion (org-end-of-subtree) (point)))))
        (overlays (cl-remove-if-not (lambda (ov) (overlay-get ov 'org-image-overlay))
                                    (ignore-errors (overlays-in beg end)))))
    (dolist (ov overlays nil)
      (delete-overlay ov)
      (setq org-inline-image-overlays (delete ov org-inline-image-overlays)))
    (when (or refresh (not overlays))
      (org-display-inline-images t t beg end)
      t)))

;; Actual function
(defun +org/dwim-at-point (&optional arg)
  "Do-what-I-mean at point.

    If on a:
    - checkbox list item or todo heading: toggle it.
    - clock: update its time.
    - headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
      subtree; update statistics cookies/checkboxes and ToCs.
    - footnote reference: jump to the footnote's definition
    - footnote definition: jump to the first reference of this footnote
    - table-row or a TBLFM: recalculate the table's formulas
    - table-cell: clear it and go into insert mode. If this is a formula cell,
      recaluclate it instead.
    - babel-call: execute the source block
    - statistics-cookie: update it.
    - latex fragment: toggle it.
    - link: follow it
    - otherwise, refresh all inline images in current tree."
  (interactive "P")
  (let* ((context (org-element-context))
         (type (org-element-type context)))
    ;; skip over unimportant contexts
    (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
      (setq context (org-element-property :parent context)
            type (org-element-type context)))
    (pcase type
      (`headline
       (cond ((memq (bound-and-true-p org-goto-map)
                    (current-active-maps))
              (org-goto-ret))
             ((and (fboundp 'toc-org-insert-toc)
                   (member "TOC" (org-get-tags)))
              (toc-org-insert-toc)
              (message "Updating table of contents"))
             ((string= "ARCHIVE" (car-safe (org-get-tags)))
              (org-force-cycle-archived))
             ((or (org-element-property :todo-type context)
                  (org-element-property :scheduled context))
              (org-todo
               (if (eq (org-element-property :todo-type context) 'done)
                   (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                       'todo)
                 'done))))
       ;; Update any metadata or inline previews in this subtree
       (org-update-checkbox-count)
       (org-update-parent-todo-statistics)
       (when (and (fboundp 'toc-org-insert-toc)
                  (member "TOC" (org-get-tags)))
         (toc-org-insert-toc)
         (message "Updating table of contents"))
       (let* ((beg (if (org-before-first-heading-p)
                       (line-beginning-position)
                     (save-excursion (org-back-to-heading) (point))))
              (end (if (org-before-first-heading-p)
                       (line-end-position)
                     (save-excursion (org-end-of-subtree) (point))))
              (overlays (ignore-errors (overlays-in beg end)))
              (latex-overlays
               (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                           overlays))
              (image-overlays
               (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                           overlays)))
         (+org--toggle-inline-images-in-subtree beg end)
         (if (or image-overlays latex-overlays)
             (org-clear-latex-preview beg end)
           (org--latex-preview-region beg end))))

      (`clock (org-clock-update-time-maybe))

      (`footnote-reference
       (org-footnote-goto-definition (org-element-property :label context)))

      (`footnote-definition
       (org-footnote-goto-previous-reference (org-element-property :label context)))

      ((or `planning `timestamp)
       (org-follow-timestamp-link))

      ((or `table `table-row)
       (if (org-at-TBLFM-p)
           (org-table-calc-current-TBLFM)
         (ignore-errors
           (save-excursion
             (goto-char (org-element-property :contents-begin context))
             (org-call-with-arg 'org-table-recalculate (or arg t))))))

      (`table-cell
       (org-table-blank-field)
       (org-table-recalculate arg)
       (when (and (string-empty-p (string-trim (org-table-get-field)))
                  (bound-and-true-p evil-local-mode))
         (evil-change-state 'insert)))

      (`babel-call
       (org-babel-lob-execute-maybe))

      (`statistics-cookie
       (save-excursion (org-update-statistics-cookies arg)))

      ((or `src-block `inline-src-block)
       (org-babel-execute-src-block arg))

      ((or `latex-fragment `latex-environment)
       (org-latex-preview arg))

      (`link
       (let* ((lineage (org-element-lineage context '(link) t))
              (path (org-element-property :path lineage)))
         (if (or (equal (org-element-property :type lineage) "img")
                 (and path (image-type-from-file-name path)))
             (+org--toggle-inline-images-in-subtree
              (org-element-property :begin lineage)
              (org-element-property :end lineage))
           (org-open-at-point arg))))

      ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
       (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
         (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

      (_
       (if (or (org-in-regexp org-ts-regexp-both nil t)
               (org-in-regexp org-tsr-regexp-both nil  t)
               (org-in-regexp org-link-any-re nil t))
           (call-interactively #'org-open-at-point)
         (+org--toggle-inline-images-in-subtree
          (org-element-property :begin context)
          (org-element-property :end context)))))))

(general-define-key
 :keymaps 'org-mode-map
 :states '(normal visual motion)
 "RET" '(+org/dwim-at-point :wk "RET-DWIM at point")
 )

;;;; Better C-RET
;; Alter the functionality of C-RET to be more useful based on context
(require 'keybinds-evil-rcp)
(defun +org--insert-item (direction)
  (let ((context (org-element-lineage
                  (org-element-context)
                  '(table table-row headline inlinetask item plain-list)
                  t)))
    (pcase (org-element-type context)
      ;; Add a new list item (carrying over checkboxes if necessary)
      ((or `item `plain-list)
       ;; Position determines where org-insert-todo-heading and org-insert-item
       ;; insert the new list item.
       (if (eq direction 'above)
           (org-beginning-of-item)
         (org-end-of-item)
         (backward-char))
       (org-insert-item (org-element-property :checkbox context))
       ;; Handle edge case where current item is empty and bottom of list is
       ;; flush against a new heading.
       (when (and (eq direction 'below)
                  (eq (org-element-property :contents-begin context)
                      (org-element-property :contents-end context)))
         (org-end-of-item)
         (org-end-of-line)))

      ;; Add a new table row
      ((or `table `table-row)
       (pcase direction
         ('below (save-excursion (org-table-insert-row t))
                 (org-table-next-row))
         ('above (save-excursion (org-shiftmetadown))
                 (+org/table-previous-row))))

      ;; Otherwise, add a new heading, carrying over any todo state, if
      ;; necessary.
      (_
       (let ((level (or (org-current-level) 1)))
         ;; I intentionally avoid `org-insert-heading' and the like because they
         ;; impose unpredictable whitespace rules depending on the cursor
         ;; position. It's simpler to express this command's responsibility at a
         ;; lower level than work around all the quirks in org's API.
         (pcase direction
           (`below
            (let (org-insert-heading-respect-content)
              (goto-char (line-end-position))
              (org-end-of-subtree)
              (insert "\n" (make-string level ?*) " ")))
           (`above
            (org-back-to-heading)
            (insert (make-string level ?*) " ")
            (save-excursion (insert "\n"))))
         (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                     (todo-type    (org-element-property :todo-type context)))
           (org-todo
            (cond ((eq todo-type 'done)
                   ;; Doesn't make sense to create more "DONE" headings
                   (car (+org-get-todo-keywords-for todo-keyword)))
                  (todo-keyword)
                  ('todo)))))))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p)))
      (evil-insert 1))))


(defun +org/insert-item-below (count)
  "Inserts a new heading, table cell or item below the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'below)))

(general-define-key
 :keymaps 'org-mode-map
 :states '(normal visual motion)
 "C-<return>" '+org/insert-item-below
 )

;;; Ancillary functionality
;;;; Org-cliplink
;; Paste https links with automatic descriptions
(use-package org-cliplink
  :general
  (kb/yank-kill-keys
    "b" '(org-cliplink :wk "Paste https"))
  )

;;;; Org-download
;; Insert images and screenshots into select modes
(use-package org-download
  :hook (org-mode . org-download-enable)
  :general (kb/yank-kill-keys
             "i" '(org-download-clipboard :wk "Paste image from clipboard"))
  :custom
  (org-download-method 'attach)
  (org-download-screenshot-method "scrot -s %s") ; Use scrot
  (org-download-link-format "[[download:%s]]\n")
  (org-download-annotate-function (lambda (_link) ""))
  (org-download-image-dir org-attach-id-dir)
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y-%m-%d_%H-%M-%S_") ; Default
  )

;;;; Typo-mode
;; Typography stuff for quotations, hyphens, back-ticks, etc.
(use-package typo
  :ghook 'org-mode-hook
  :config
  (defvar kb/typo-cycle-message t
    "Whether beginning a `typo' cycle echos to the minibuffer.")
  (defun kb/typo-insert-cycle (cycle)
    "Insert the strings in CYCLE"
    (let ((i 0)
          (repeat-key last-input-event)
          repeat-key-str)
      (insert (nth i cycle))
      (setq repeat-key-str (format-kbd-macro (vector repeat-key) nil))
      (while repeat-key
        (if kb/typo-cycle-message       ; Wrapped in if statement
            (message "(Inserted %s; type %s for other options)"
                     (typo-char-name (nth i cycle))
                     repeat-key-str))
        (if (equal repeat-key (read-event))
            (progn
              (clear-this-command-keys t)
              (delete-char (- (length (nth i cycle))))
              (setq i (% (+ i 1)
                         (length cycle)))
              (insert (nth i cycle))
              (setq last-input-event nil))
          (setq repeat-key nil)))
      (when last-input-event
        (clear-this-command-keys t)
        (setq unread-command-events (list last-input-event)))))
  (advice-add 'typo-insert-cycle :override #'kb/typo-insert-cycle)

  ;; My own cycles
  (define-typo-cycle typo-cycle-right-single-quotation-mark
    "Cycle through the typewriter apostrophe and the right quotation mark.

If used with a numeric prefix argument N, N typewriter apostrophes
will be inserted."
    ("'" "’"))                          ; Swapped these two
  )

;;; org-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-general-rcp)
