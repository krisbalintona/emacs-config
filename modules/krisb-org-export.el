;; -*- lexical-binding: t; -*-

;;; Ox (org-export)
(use-package ox
  :ensure nil
  :custom
  (org-export-coding-system 'utf-8)
  (org-export-with-tags t)
  (org-export-with-smart-quotes t)
  (org-export-with-sub-superscripts '{}) ; Requires brackets to recognize superscripts and subscripts
  (org-export-with-section-numbers nil)
  (org-time-stamp-formats               ; Format of time stamps in the file
   '("%Y-%m-%d %a" . "%Y-%m-%d %a %H:%M"))
  (org-display-custom-times t)          ; Export with custom time stamps?
  (org-time-stamp-custom-formats        ; Format of exported time stamps
   '("%a, %b %-d" . "%a, %b %-d (%-H:%M%p)"))

  (org-image-actual-width 700)          ; Image widths on export

  ;; Asynchronous
  (org-export-in-background nil)        ; Default?
  (org-export-async-debug t)
  (org-export-async-init-file (locate-library "quickstart")) ; TODO 2024-10-19: Make a "quickstart" init.el
  :config
  ;; Taken from
  ;; https://endlessparentheses.com/better-time-stamps-in-org-export.html
  (defun krisb-org-export-filter-timestamp-reformat (timestamp backend info)
    "Remove <> or [] surrounding time-stamps when exporting HTML and LaTeX."
    (cond
     ((org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string "[<>]\\|[][]" "" timestamp))
     ((org-export-derived-backend-p backend 'html)
      (replace-regexp-in-string "&[lg]t;\\|[][]" "" timestamp))))
  (add-to-list 'org-export-filter-timestamp-functions #'krisb-org-export-filter-timestamp-reformat))

;;; Ox-odt
(use-package ox-odt
  :ensure nil
  ;; For AUR:
  ;; :ensure-system-package (soffice . libreoffice-still)
  :custom
  (org-odt-preferred-output-format "docx")) ; Convert to .docx at the end of conversion

;;; Ox-html
(use-package ox-html
  :ensure nil
  :custom
  ;; Resolve HTML exports not using the name of the target of ID links, using
  ;; internal ID values instead.  As per
  ;; https://github.com/meedstrom/org-node?tab=readme-ov-file#appendix-iii-random-tips
  (org-html-prefer-user-labels t))

;;; Ox-latex
(use-package ox-latex
  :ensure nil
  :after org
  :custom
  ;; A more QoL compiler that is generally slower at compiling.
  (org-latex-compiler "lualatex")
  ;; Most comprehensive exporting method of code blocks.  This requires the
  ;; Emacs package engrave-faces (available from GNU ELPA), and the LaTeX
  ;; package fvextra be installed.  For more information on configuring the
  ;; output format, read the option's docstring.
  (org-latex-src-block-backend 'engraved)
  ;; Templates commands
  (org-latex-title-command
   "\\maketitle")
  (org-latex-toc-command
   "\\renewcommand{\\contentsname}{
  \\begin{center}
    Table of Contents
  \\end{center}
}
\\tableofcontents
\\newpage\n")
  (org-export-with-toc nil)             ; I generally don't want a ToC exported
  (org-latex-packages-alist
   '(("" "lipsum" nil)))                ; Sample text
  (org-latex-hyperref-template
   "\\hypersetup{
pdfauthor={%a},
pdftitle={%t},
pdfkeywords={%k},
pdfsubject={%d},
pdfcreator={%c},
pdflang={%L}
colorlinks={true},
hidelinks={true}}\n")                   ; Hide hyperlinks
  :config
;;;; Bespoke `org-latex-classes'
  ;; For papers using the Modern Language Association (MLA) citation style
  (add-to-list 'org-latex-classes
               `("mla" ,(format "%% * Preamble
\\documentclass[12pt,letterpaper]{article}

%% * Org-export macro blocks
[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]

%% * MLA package
\\usepackage{%s/latex/packages/mla}"
                                (directory-file-name krisb-org-directory))
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; For papers using the Chicago Manual of Style (CMS) citation style
  (add-to-list 'org-latex-classes
               `("cms" ,(format "%% * Preamble
\\documentclass[12pt,letterpaper]{article}

%% * Org-export macro blocks
[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]

%% * My CMS package
\\usepackage{%s/latex/packages/chicago-manual-style}"
                                (directory-file-name krisb-org-directory))
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; For my bespoke "review" sheets
  (add-to-list 'org-latex-classes
               `("review" ,(format "%% * Preamble
\\documentclass[a4paper,landscape]{article}

%% * Org-export macro blocks
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]

%% ** Review package
\\usepackage{%s/latex/packages/review}"
                                   (directory-file-name krisb-org-directory))
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;;;;; Processing org-latex export of custom keywords
  ;; Process bespoke keywords for my academic papers in org-export.  Ordinarily,
  ;; only keywords recognized by the org-export backend are processed.
  ;; Consequently, we cannot use `org-export-filter-keyword-functions'.
  ;; Instead, we use `org-export-before-processing-hook' to transform the
  ;; desired keywords into another form that is recognized and exported
  ;; appropriately by the export backend
  ;;
  ;; See (info "(org) Advanced Export Configuration") for more information on
  ;; the order of steps in the export process.
  (defun krisb-org-latex-before-processing---paper-keywords (backend)
    "Transform custom keywords related to academic papers for LaTeX export.
This function transforms the \"professor\" and \"course\" keywords.
They are replaced with a \"#+LATEX_HEADER\" keyword-value that exports
into the form my bespoke .sty files expect.

BACKEND is the org-export backend, as a symbol."
    (when (org-export-derived-backend-p backend 'latex)
      (save-excursion
        (while (re-search-forward org-keyword-regexp nil t)
          (let ((key (match-string 1))
                (val (match-string 2))
                command arg)
            (pcase (upcase key)
              ("PROFESSOR"
               (setq command "professor"
                     arg (concat "Professor " (if (string-empty-p val)
                                                  ;; When keyword value is
                                                  ;; empty, use "PROFESSOR" as a
                                                  ;; placeholder
                                                  "PROFESSOR"
                                                val))))
              ("COURSE"
               (setq command "course"
                     arg (if (string-empty-p val)
                             ;; When keyword value is empty, use "COURSE" as a
                             ;; placeholder
                             "COURSE"
                           val))))
            ;; Execute the replacement
            (when (and command arg)
              (replace-match (format "#+LATEX_HEADER: \\newcommand{\\%s}{%s}" command arg) nil t)))))))

  (add-to-list 'org-export-before-processing-hook #'krisb-org-latex-before-processing---paper-keywords))

;;; Provide
(provide 'krisb-org-export)
