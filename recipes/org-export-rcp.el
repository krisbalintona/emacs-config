;;; org-export-rcp.el --- Org-export config          -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Config relaed specifically to `org-export'.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Ox (org-export)
(use-package ox
  :ensure nil
  ;; Call after `org' since some of the options below are from `org', not
  ;; `org-export', so they will be overwritten if this use-package loads before
  ;; `org' does
  :after org
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

;;;; Custom processing of #+INCLUDE keyword
;; Use denote links or denote:DENOTEID as the file path for #+INCLUDE keywords
(with-eval-after-load 'ox
  (require 'denote)
  (defun kb/org-export-parse-include-value (value &optional dir)
    "Extract the various parameters from #+include: VALUE.

More specifically, this extracts the following parameters to a
plist: :file, :coding-system, :location, :only-contents, :lines,
:env, :minlevel, :args, and :block.

The :file parameter is expanded relative to DIR.

The :file, :block, and :args parameters are extracted
positionally, while the remaining parameters are extracted as
plist-style keywords.

Any remaining unmatched content is passed through
`org-babel-parse-header-arguments' (without evaluation) and
provided as the :unmatched parameter.

This version that overrides the original takes into account denote IDs.
See the HACK comment below."
    (let* (location
           (coding-system
            (and (string-match ":coding +\\(\\S-+\\)>" value)
                 (prog1 (intern (match-string 1 value))
                   (setq value (replace-match "" nil nil value)))))
           (file
            (and (string-match "^\\(\".+?\"\\|\\S-+\\)\\(?:\\s-+\\|$\\)" value)
                 (let ((matched (match-string 1 value)) stripped)
                   (setq value (replace-match "" nil nil value))
                   (when (string-match "\\(::\\(.*?\\)\\)\"?\\'"
                                       matched)
                     (setq location (match-string 2 matched))
                     (setq matched
                           (replace-match "" nil nil matched 1)))
                   ;; HACK 2024-02-25: Added the following sexp. Checks if link
                   ;; has a denote ID, and if so, changes the matched (file) to
                   ;; the file corresponding to that ID. Allows me to use denote
                   ;; links as the included file
                   (when (string-match denote-id-regexp matched)
                     (setq matched
                           (denote-get-path-by-id (match-string 0 matched))))
                   (setq stripped (org-strip-quotes matched))
                   (if (org-url-p stripped)
                       stripped
                     (expand-file-name stripped dir)))))
           (only-contents
            (and (string-match ":only-contents *\\([^: \r\t\n]\\S-*\\)?"
                               value)
                 (prog1 (org-not-nil (match-string 1 value))
                   (setq value (replace-match "" nil nil value)))))
           (lines
            (and (string-match
                  ":lines +\"\\([0-9]*-[0-9]*\\)\""
                  value)
                 (prog1 (match-string 1 value)
                   (setq value (replace-match "" nil nil value)))))
           (env (cond
                 ((string-match "\\<example\\>" value) 'literal)
                 ((string-match "\\<export\\(?: +\\(.*\\)\\)?" value)
                  'literal)
                 ((string-match "\\<src\\(?: +\\(.*\\)\\)?" value)
                  'literal)))
           ;; Minimal level of included file defaults to the
           ;; child level of the current headline, if any, or
           ;; one.  It only applies is the file is meant to be
           ;; included as an Org one.
           (minlevel
            (and (not env)
                 (if (string-match ":minlevel +\\([0-9]+\\)" value)
                     (prog1 (string-to-number (match-string 1 value))
                       (setq value (replace-match "" nil nil value)))
                   (get-text-property (point)
                                      :org-include-induced-level))))
           (args (and (eq env 'literal)
                      (prog1 (match-string 1 value)
                        (when (match-string 1 value)
                          (setq value (replace-match "" nil nil value 1))))))
           (block (and (or (string-match "\"\\(\\S-+\\)\"" value)
                           (string-match "\\<\\(\\S-+\\)\\>" value))
                       (or (= (match-beginning 0) 0)
                           (not (= ?: (aref value (1- (match-beginning 0))))))
                       (prog1 (match-string 1 value)
                         (setq value (replace-match "" nil nil value))))))
      (list :file file
            :coding-system coding-system
            :location location
            :only-contents only-contents
            :lines lines
            :env env
            :minlevel minlevel
            :args args
            :block block
            :unmatched (org-babel-parse-header-arguments value t))))
  (advice-add 'org-export-parse-include-value :override #'kb/org-export-parse-include-value))

;;;; Ox-odt
(use-package ox-odt
  :ensure nil
  :custom
  (org-odt-preferred-output-format "docx")) ; Convert to docx at the end of conversion

;;;; Ox-latex
;;;;; Itself
(use-package ox-latex
  :ensure nil
  :after org
  :custom
  (org-latex-compiler "lualatex")
  ;; Beautiful source block exports to latex. Read help buffer for much more
  ;; info on how this is done, requirements, and options
  (org-latex-src-block-backend 'engraved)
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
\\newpage
")
  (org-export-with-toc nil)
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
  (add-to-list 'org-latex-classes
               `("mla" ,(concat "% * Preamble
\\documentclass[12pt,letterpaper]{article}

% * Default packages?
[NO-DEFAULT-PACKAGES]

% ** MLA package
\\usepackage{" (directory-file-name org-directory) "/latex/packages/mla}\n\\usepackage[style=mla-new]{biblatex}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               `("cms" ,(concat "% * Preamble
\\documentclass[12pt,letterpaper]{article}

% * Default packages?
[NO-DEFAULT-PACKAGES]

% ** CMS package
\\usepackage{" (directory-file-name org-directory) "/latex/packages/chicago-manual-style}\n\\usepackage[notes,short,backend=biber]{biblatex}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               `("review" ,(concat "% * Preamble
\\documentclass[a4paper,landscape]{article}

% * Default packages?
[NO-DEFAULT-PACKAGES]

% ** Review package
\\usepackage{" org-directory "latex/packages/review}\n")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;;;;; Custom org-export latex backend
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
                         (_ "%B %e, %Y"))))
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
              (last-name (string-trim professor-value)))
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

  (defun org-latex-paper-export-as-latex
      (&optional async subtreep visible-only body-only ext-plist)
    "Export current buffer as a LaTeX buffer.

Uses my 'latex-paper' backend. See the original
`org-latex-export-as-latex' for more details."
    (interactive)
    (org-export-to-buffer 'latex-paper "*Org LATEX Export*"
      async subtreep visible-only body-only ext-plist (lambda () (LaTeX-mode))))

  (defun org-latex-paper-export-to-latex
      (&optional async subtreep visible-only body-only ext-plist)
    "Export current buffer to a LaTeX file.

Uses my 'latex-paper' backend. See the original
`org-latex-export-to-latex' for more details."
    (interactive)
    (let ((outfile (org-export-output-file-name ".tex" subtreep)))
      (org-export-to-file 'latex-paper outfile
        async subtreep visible-only body-only ext-plist)))

  (defun org-latex-paper-export-to-pdf
      (&optional async subtreep visible-only body-only ext-plist)
    "Export current buffer to LaTeX then process through to PDF.

Uses my 'latex-paper' backend. See the original
`org-latex-export-to-pdf' for more details."
    (interactive)
    (let ((outfile (org-export-output-file-name ".tex" subtreep)))
      (org-export-to-file 'latex-paper outfile
        async subtreep visible-only body-only ext-plist
        #'org-latex-compile)))

  ;; Recognize the 'professor' and 'course' keywords
  (org-export-define-derived-backend 'latex-paper 'latex
    :translate-alist '((template . kb/org-latex-template))
    :menu-entry
    '(?j "Export to LaTeX (Paper)"
         ((?L "As LaTeX buffer" org-latex-paper-export-as-latex)
          (?l "As LaTeX file" org-latex-paper-export-to-latex)
          (?p "As PDF file" org-latex-paper-export-to-pdf)
          (?o "As PDF file and open"
              (lambda (a s v b)
                (if a (org-latex-paper-export-to-pdf t s v b)
                  (org-open-file (org-latex-paper-export-to-pdf nil s v b)))))))
    :options-alist
    '((:professor "PROFESSOR" nil nil parse)
      (:course "COURSE" nil nil parse))))

;;;;; Custom links
(with-eval-after-load 'ol
;;;;;; Colored text
  (defun kb/org-link-color-export (link description format)
    (let ((desc (or description link)))
      (cond
       ((eq format 'latex) (format "\\textcolor{%s}{%s}" link desc))
       (t desc))))

  (org-link-set-parameters "color"
                           :face (lambda (path) `(:foreground ,path))
                           :export #'kb/org-link-color-export
                           :complete (lambda (&optional _)
                                       (concat "color:"
                                               (completing-read "Choose color: " (list-colors-duplicates (defined-colors))))))

;;;;;; [ end ]
  )

;;;; Org-contrib
(use-package org-contrib
  :after org
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))) ; The ignore tag will export contents but ignore heading

;;;; Ox-pandoc
;; Export to whatever file format pandoc can export to
(use-package ox-pandoc
  :after org
  :defer 20
  :ensure-system-package pandoc)

;;;; Ox-clip
(use-package ox-clip
  :ensure-system-package ((xclip)
                          (wl-copy . wl-clipboard))
  :bind
  ( :map kb/yank-keys
    ("x" . ox-clip-formatted-copy))
  :config
  (setq ox-clip-linux-cmd
        (if (string-equal (getenv "XDG_SESSION_TYPE") "wayland")
            "wl-copy --type text/html < $%f"
          "xclip -verbose -i \"%f\" -t text/html -selection clipboard")))

(provide 'org-export-rcp)
;;; org-export-rcp.el ends here
