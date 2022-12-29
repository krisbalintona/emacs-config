;;; org-export-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Everything related to exporting in org.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Org-export
(use-package ox
  :custom
  (org-export-with-sub-superscripts nil)
  (org-export-with-section-numbers nil)
  (org-time-stamp-formats '("%Y-%m-%d %a" . "%Y-%m-%d %a %H:%M"))
  (org-display-custom-times t)
  (org-time-stamp-custom-formats '("%b %d, %Y" . "%a %b %d, %Y %H:%M"))
  (org-image-actual-width 700)          ; Image widths on export 
  
  ;; Async export
  (org-export-in-background nil)          ; Have it be default?
  (org-export-async-debug t)
  (org-export-async-init-file (locate-library "quickstart"))
  :config
  ;; Taken from
  ;; https://endlessparentheses.com/better-time-stamps-in-org-export.html
  (defun kb/org-export-filter-timestamp-reformat (timestamp backend info)
    "Remove <> or [] around time-stamps."
    (cond
     ((org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string "[<>]\\|[][]" "" timestamp))
     ((org-export-derived-backend-p backend 'html)
      (replace-regexp-in-string "&[lg]t;\\|[][]" "" timestamp))))
  (add-to-list 'org-export-filter-timestamp-functions #'kb/org-export-filter-timestamp-reformat))

;;; Ox-odt
(use-package ox-odt
  :custom
  (org-odt-preferred-output-format "docx") ; Convert to docx at the end of conversion
  )

;;; Ox-latex
;;;; Itself
(use-package ox-latex
  :custom
  (org-latex-compiler "lualatex")
  ;; Latexmk makes everything simple
  (org-latex-pdf-process
   (list "latexmk -shell-escape  -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"))
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
  (setf (alist-get "mla" org-latex-classes nil nil #'string=)
        `(,(concat "% * Preamble
\\documentclass[12pt,letterpaper]{article}

% * Default packages?
[NO-DEFAULT-PACKAGES]

% ** MLA package
\\usepackage{" (directory-file-name org-directory) "/latex/packages/mla}\n")
          ("\\section{%s}" . "\\section*{%s}")
          ("\\subsection{%s}" . "\\subsection*{%s}")
          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
          ("\\paragraph{%s}" . "\\paragraph*{%s}")
          ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
          )
        (alist-get "cms" org-latex-classes nil nil #'string=)
        `(,(concat "% * Preamble
\\documentclass[12pt,letterpaper]{article}

% * Default packages?
[NO-DEFAULT-PACKAGES]

% ** CMS package
\\usepackage{" (directory-file-name org-directory) "/latex/packages/chicago-manual-style}\n")
          ("\\section{%s}" . "\\section*{%s}")
          ("\\subsection{%s}" . "\\subsection*{%s}")
          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
          ("\\paragraph{%s}" . "\\paragraph*{%s}")
          ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
          )
        (alist-get "review" org-latex-classes nil nil #'string=)
        `(,(concat "% * Preamble
\\documentclass[a4paper,landscape]{article}

% * Default packages?
[NO-DEFAULT-PACKAGES]

% ** Review package
\\usepackage{" org-directory "latex/packages/review}\n")
          ("\\section{%s}" . "\\section*{%s}")
          ("\\subsection{%s}" . "\\subsection*{%s}")
          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
          ("\\paragraph{%s}" . "\\paragraph*{%s}")
          ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
          ))
  )

;;;; Custom org-export latex backend
;; Support more file keywords; used for my papers
(with-eval-after-load 'ox-latex
  (defun kb/org-latex-template (contents info)
    "This is my custom definition of `org-latex-template' which (i)
processes the professor and course file properties, (ii) formats
the date according to the style, and (iii) allows the date to be
chosen."
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

       ;; Customized date
       (let* ((fmt (or org-export-date-timestamp-format
                       (pcase (plist-get info :latex-class)
                         ("mla" "%e %B, %Y")
                         ("cms" "%B %e, %Y")
                         (t "%B %e, %Y"))))
              (date (and (plist-get info :with-date) (org-export-get-date info fmt))))
         (format "\\date{%s}\n" (or date "\\today")))

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

       ;; Process the professor and course keywords
       (let* ((professor-value (org-export-data (plist-get info :professor) info))
              ;; Current removes all whitespace and "Professor" substrings
              (last-name (replace-regexp-in-string (rx (or whitespace "Professor")) "" professor-value)))
         (format "\\newcommand{\\professor}{%s}\n"
                 (if (string= last-name "")
                     "PROFESSOR"        ; When empty or nil
                   (concat "Professor " last-name))))
       (let ((course (org-export-data (plist-get info :course) info)))
         (format "\\newcommand{\\course}{%s}\n"
                 (if (string= course "")
                     "COURSE"           ; When empty or nil
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

  ;; Recognize the professor and course keywords
  (org-export-define-backend 'latex
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
      ;; Use template which processes professor and course keywords
      (template . kb/org-latex-template)
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
      (:latex-compiler "LATEX_COMPILER" nil org-latex-compiler))))

;;;; Custom links
(with-eval-after-load 'ol
;;;;; Colored text
  (defun kb/org-latex-color-export (link description format)
    "TODO"
    (let ((desc (or description link)))
      (cond
       ((eq format 'latex) (format "\\textcolor{%s}{%s}" link desc))
       (t desc))
      ))
  (org-link-set-parameters "color"
                           :face #'(lambda (path) `(:foreground ,path))
                           :export #'kb/org-latex-color-export)

;;;;; [ end ]
  )

;;; Org-contrib
(use-package org-contrib
  :after org
  :init
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)) ; The ignore tag will export contents but ignore heading
  )

;;; Ox-pandoc
;; Export to whatever file format pandoc can export to
(use-package ox-pandoc
  :commands org-export-dispatch
  :ensure-system-package pandoc
  )

;;; org-export-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-export-rcp)
