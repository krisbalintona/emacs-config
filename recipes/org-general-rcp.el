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
  :mode (("\\.docx\\'" . doc-view-mode)
         ("\\.odt\\'" . doc-view-mode))
  :gfhook
  'prettify-symbols-mode
  'variable-pitch-mode
  'visual-line-mode
  :general
  (:keymaps 'org-mode-map
            "M-u" 'org-up-element
            "M-d" 'org-down-element
            "M-n" 'org-forward-heading-same-level
            "M-p" 'org-backward-heading-same-level
            )
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
  (kb/leader-keys
    "nn" '(org-capture :which-key "Org-capture")
    "mg" '(org-mark-ring-goto :which-key "Goto last mark")
    "ma" '(org-mark-ring-push :which-key "Push to mark-ring")
    )
  :custom
  ;; Aesthetics
  (org-startup-indented t) ; Start with `org-indent-mode'?
  (org-startup-folded 'nofold)
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
   '(("\\.docx\\'" . "soffice -writer %s")
     ("\\.odt\\'" . "soffice -writer %s")
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . default)
     (directory . emacs)
     (auto-mode . emacs)
     ))
  :config
  (advice-add 'org-ctrl-c-ret :after #'evil-insert-state) ; Entire insert-state after M-RET
  )

;;; Related to org-export
;;;; Ox-odt
(use-package ox-odf
  :after (ox org)
  :straight nil
  :custom
  (org-odt-preferred-output-format "docx") ; Automatically create a `.docx' version
  )

;;;; Ox-latex
(use-package ox-latex
  :after (ox org)
  :straight nil
  :custom
  ;; Templates
  (org-latex-title-command
   "\\begin{flushleft}
%a\\\\
%c\\\\
%k\\\\
\\daymonthyeardate\\\\
\\end{flushleft}

\\begin{center}
%t
\\end{center}")
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
     ("hidelinks" "hyperref" nil)
     ))

  ;; Processes for org-to-latex conversion
  ;; (org-latex-pdf-process
  ;;  '("%latex -interaction nonstopmode -output-directory %o %f"
  ;;    "biber --output-directory %o $(basename %f .tex)"
  ;;    "%latex -interaction nonstopmode -output-directory %o %f"
  ;;    "%latex -interaction nonstopmode -output-directory %o %f")
  ;;  )
  (org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")) ; From https://github.com/jkitchin/org-ref
  (org-latex-with-hyperref nil) ; Don't use the hyperref LaTeX package when exporting from org-mode

  :config
  (add-to-list 'org-latex-classes
               '("mla"
                 "% * Preamble
\\documentclass[12pt]{article}

% ** Fancy quotes
\\usepackage[american]{babel}
\\usepackage[style=american, debug=true, strict=true, threshold=4]{csquotes}
\\SetCiteCommand{\\autocite} % Tell csquotes to use biblatex's \autocite for citations

% ** Font
% Times New Roman
\\usepackage{newtxtext}
\\usepackage{newtxmath} % For math symbols
% For heading sizes
\\usepackage{sectsty}
\\sectionfont{\\fontsize{12}{15}\\selectfont}

% ** Margins
\\usepackage[letterpaper]{geometry}
\\geometry{top=1.0in, bottom=1.0in, left=1.0in, right=1.0in}

% ** Line and after-sentence spacing
\\usepackage{setspace}
\\setstretch{2}%
\\frenchspacing% Single spaces after sentences

% ** MLA date format
\\usepackage{datetime2}
\\makeatletter
\\newcommand{\\daymonthyeardate}{ % MLA formatted date
\\@dtm@day\\ \\DTMenglishmonthname{\\@dtm@month}, \\@dtm@year}%
\\makeatother

% ** Fancy page header
\\usepackage{fancyhdr}
\\pagestyle{fancy} % Have header and footer separated into left, right, and center sections
\\pagenumbering{arabic}
\\lhead{}
\\chead{}
\\rhead{Balintona \\thepage}
\\lfoot{}
\\cfoot{}
\\rfoot{}
% To make sure we actually have header 0.5in away from top edge
\\renewcommand{\\headrulewidth}{0pt}
\\renewcommand{\\footrulewidth}{0pt}
% 12pt is one-sixth of an inch. Subtract this from 0.5in to get headsep value
\\setlength\\headsep{0.333in}

% ** Paragraph indentation
\\setlength{\\parindent}{0.5in}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                 ))
  )

;;;; Org-contrib
(use-package org-contrib
  :demand t
  :after org
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)) ; The ignore tag will export contents but ignore heading
  )

;;; Org-footnote
(use-package org-footnote
  :straight nil
  :after org
  :custom
  (org-footnote-section nil) ; Don't create footnote headline
  (org-footnote-auto-adjust t) ; Automatically renumber
  )

;;; Org-attach
(use-package org-attach
  :straight nil
  :custom
  (org-attach-id-dir "attachments/")
  (org-attach-dir-relative t)        ; Use relative file paths
  (org-attach-method 'cp)            ; Attach copies of files
  (org-attach-archive-delete 'query) ; If subtree is deleted or archived, ask user
  )

;;; Org-refile
(use-package org-refile
  :after org-roam
  :straight nil
  :general (kb/leader-keys
             "nr" '(org-refile :which-key "Org-refile"))
  :custom
  (org-refile-targets
   `((kb/find-blog-files-org :maxlevel . 1)
     ;; `((kb/find-blog-files-org :tag . "blog")
     (org-agenda-files :regexp . "tnaoirnta") ; This random string will remove all headlines
     ("/home/krisbalintona/Documents/org-database/roam/inbox.org" :level . 0) ; Inbox file
     ))
  (org-refile-use-outline-path 'file)
  (org-refile-use-cache nil)
  ;; (org-refile-history t) ; FIXME 2021-10-09: For some reason makes `org-refile' not work
  (org-refile-allow-creating-parent-nodes 'confirm)
  :config
  (defun kb/find-blog-files-org ()
    "Return a list of files which are within the blog directory of org-roam."
    (directory-files-recursively (concat org-roam-directory "blog") "")
    )
  )

;;; Aesthetics
;;;; Org-superstar
;; Descendant of (and thus superior to) org-bullets
(use-package org-superstar  ;; Improved version of org-bullets
  :ghook 'org-mode-hook
  :custom
  (org-cycle-level-faces nil)
  (org-n-level-faces 5)

  (org-superstar-leading-bullet ?\s) ;; Render leading stars as spaces!
  (org-superstar-leading-fallback ?\s) ; Hide away leading stars on terminal.
  (org-indent-mode-turns-on-hiding-stars nil)
  (org-hide-leading-stars t) ;; t means no character is there at all
  (org-superstar-remove-leading-stars t)

  (org-superstar-cycle-headline-bullets nil) ; Don't repeat bullets in hierarchy
  (org-superstar-todo-bullet-alist
   '(("TODO" . 9744)
     ("[ ]"  . 9744)
     ("DONE" . 9745)
     ("[X]"  . 9745)))
  (org-superstar-headline-bullets-list
   '("⚝" "●" "◉" "○"))
  (org-superstar-first-inlinetask-bullet ?▶)
  ;; org-superstar-item-bullet-alist ; For plainlist bullets

  (inhibit-compacting-font-caches t) ; Stop slowdown
  )

;;;; Org-bars
(use-package org-bars
  :after org
  :straight (org-bars :type git :host github :repo "tonyaldon/org-bars")
  :config
  (add-hook 'org-mode-hook 'org-bars-mode 100) ; Doesn't work if it is added to the beginning of the hook
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
 "RET" '(+org/dwim-at-point :which-key "RET-DWIM at point")
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
  (kb/leader-keys
    "ib" '(org-cliplink :which-key "Paste https"))
  )

;;;; Org-download
;; Insert images and screenshots into select modes
(use-package org-download
  :gfhook ('org-mode-hook 'org-download-enable)
  :general
  (kb/leader-keys
    "ii" '(org-download-clipboard :which-key "Paste image from clipboard")
    )
  :custom
  (org-download-method 'attach)
  (org-download-screenshot-method "scrot -s %s") ; Use scrot
  (org-download-link-format "[[download:%s]]\n")
  (org-download-annotate-function (lambda (_link) ""))
  :config
  (setq-default org-download-image-dir (concat org-directory "resources/")
                org-download-heading-lvl nil
                org-download-timestamp "%Y-%m-%d_%H-%M-%S_") ; Default
  )

;;;; Typo-mode
;; Typography stuff for quotations, hyphens, back-ticks, etc.
(use-package typo
  :ghook 'org-mode-hook
  )

;;; org-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-general-rcp)
