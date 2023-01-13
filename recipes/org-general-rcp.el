;;; org-general-rcp.el --- Summary
;;
;;; Commentary:
;;
;; All my general org configurations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Org
;;;; Itself
(use-package org
  :straight (org :type git
                 :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
                 :local-repo "org"
                 :depth full
                 :pre-build (straight-recipes-org-elpa--build)
                 :build (:not autoloads)
                 :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))
                 :includes (org-num org-indent ox ox-odt ox-latex org-footnote org-attach org-refile oc))
  :gfhook
  'variable-pitch-mode
  'visual-line-mode
  'visual-fill-column-mode
  '(lambda ()
      (eldoc-mode -1))
  :general
  (:keymaps 'org-mode-map
   "H-s" 'org-store-link
   "C-M-<up>" 'org-up-element)
  (kb/note-keys
    "c" '(org-capture :wk "Org-capture"))
  :custom
  (org-directory kb/org-dir)
  (org-special-ctrl-a/e t)
  (org-src-window-setup 'current-window) ; Open src block window on current buffer were in the language's major mode

  (org-startup-folded 'nofold)
  (org-ellipsis " ")
  (org-hide-emphasis-markers t)     ; Remove org-mode markup characters
  (org-pretty-entities t)           ; Show as UTF-8 characters (useful for math)
  (org-pretty-entities-include-sub-superscripts nil) ; Show super- and subscripts?
  (org-hidden-keywords nil)
  (org-ctrl-k-protect-subtree 'error)

  (org-return-follows-link t)
  (org-insert-heading-respect-content nil) ; Let M-RET make heading in place

  (org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . default)
     ("\\.docx\\'" . system)
     ("\\.odt\\'" . system)))

  (org-fold-catch-invisible-edits 'show)
  (org-edit-timestamp-down-means-later t)

  ;; Org-babel
  (org-confirm-babel-evaluate nil)      ; Can be dangerous! Observe for now
  :config
  (when (bound-and-true-p evil-local-mode)
    (advice-add 'org-ctrl-c-ret :after #'evil-insert-state))) ; Entire insert-state after M-RET

;;;; Org-num
(use-package org-num
  :straight nil
  :diminish
  :general (kb/toggle-keys
             :keymaps 'org-mode-map
             "n" 'org-num-mode))

;;;; Org-indent
(use-package org-indent
  :straight nil
  :diminish
  :custom
  (org-startup-indented t))

;;;; Org-footnote
(use-package org-footnote
  :custom
  (org-footnote-section nil)            ; Don't create footnote headline
  (org-footnote-auto-adjust t)          ; Automatically renumber
  (org-footnote-define-inline t)) ; Write footnote content where you declare rather in a particular section (i.e. `org-footnote-section')?

;;;; Org-attach
(use-package org-attach
  :custom
  (org-attach-preferred-new-method 'id) ; Necessary to add the ATTACH tag
  (org-attach-auto-tag "ATTACH")       ; See `org-roam-db-node-include-function'
  (org-attach-dir-relative nil)        ; Use relative file paths?
  (org-attach-id-dir (expand-file-name "resources" org-directory))
  (org-attach-method 'cp)            ; Attach copies of files
  (org-attach-archive-delete 'query) ; If subtree is deleted or archived, ask user
  ;; Use timestamps as UUIDs and in attachment directory hierarchy
  (org-id-method 'ts)
  (org-attach-id-to-path-function-list
   '(org-attach-id-ts-folder-format
     org-attach-id-uuid-folder-format)))

;;;; Org-refile
(use-package org-refile
  :custom
  (org-refile-targets
   `((org-agenda-files . (:tag . "type"))
     (org-agenda-files . (:tag . "project"))
     (kb/find-blog-files-org . (:level . 0))
     (kb/find-blog-files-org . (:tag . "project"))
     ;; OPTIMIZE 2023-01-08: Right now I manually add a refile target entry for
     ;; each of my working todos. Is there a way to avoid this?
     (kb/find-blog-files-org . (:todo . "ACTIVE"))
     (kb/find-blog-files-org . (:todo . "TODO"))
     (kb/find-blog-files-org . (:todo . "MAYBE"))
     (nil . (:level . 3))))
  (org-refile-use-cache nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
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
                                                 (apply args))))))

;;;; Org-faces
(use-package org-faces
  :straight nil
  :custom
  (org-fontify-whole-block-delimiter-line t)
  (org-fontify-quote-and-verse-blocks t)
  )

;;;; Org-src
(use-package org-src
  :straight nil
  :custom
  (org-src-fontify-natively t)
  (org-src-block-faces nil)
  )

;;;; Org-visibility
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
  :gfhook 'kb/org-superstar-auto-lightweight-mode
  :custom
  (inhibit-compacting-font-caches t) ; Stop slowdown

  ;; Headlines
  (org-superstar-leading-bullet ?\s)          ; Render leading stars as spaces!
  (org-indent-mode-turns-on-hiding-stars nil) ; `nil' if I use `org-indent'
  (org-hide-leading-stars nil)          ; Must be `nil' according to readme
  (org-superstar-remove-leading-stars t) ; non-`nil' results in more consistent headline indentation
  (org-superstar-headline-bullets-list '("⚝" "●" "⊙" "○"))
  (org-n-level-faces 5)
  (org-cycle-level-faces nil)
  (org-superstar-cycle-headline-bullets nil) ; Don't repeat bullets in hierarchy

  ;; Todos
  (org-superstar-special-todo-items t)
  ;; Update when I change `org-todo-keywords'
  (org-superstar-todo-bullet-alist
   '(("PROG" . 9744)
     ("ACTIVE" . 9744)
     ("TODO" . 9744)
     ("WAITING" . 9745)
     ("MAYBE" . 9745)
     ("DONE" . 9745)
     ("CANCELLED" . 9745)
     ("[ ]"  . 9744)
     ("[X]"  . 9745)))

  ;; Plain lists
  (org-superstar-prettify-item-bullets t)
  (org-superstar-first-inlinetask-bullet ?▶)
  (org-superstar-item-bullet-alist
   '((?+ . "◦")                         ; List taken from `org-modern'
     (?- . "–")
     (?* . "•")))
  :init
  ;; See https://github.com/emacsmirror/org-superstar#fast-plain-list-items
  (defun kb/org-superstar-auto-lightweight-mode ()
    "Start Org Superstar differently depending on the number of lists items."
    (let ((list-items
           (count-matches "^[ \t]*?\\([+-]\\|[ \t]\\*\\)"
                          (point-min) (point-max))))
      (unless (< list-items 100)
        (org-superstar-toggle-lightweight-lists)))))

;;;; Org-bars
(use-package org-bars
  :disabled                    ; Not much value, and sometimes even distracting
  :straight (org-bars :type git :host github :repo "tonyaldon/org-bars")
  :ghook 'org-mode-hook
  :init
  ;; Set these in init for some reason
  (setq org-bars-with-dynamic-stars-p nil ; Custom headline stars?
        org-bars-org-indent-mode t
        org-bars-extra-pixels-height 6 ; Use when headline font is larger than 1.0
        org-bars-color-options
        '(:desaturate-level-faces 30
          :darken-level-faces 15)))

;;;; Visual-fill-column
;; Soft wrap lines at fill-column
(use-package visual-fill-column
  :ghook 'emacs-news-mode-hook
  :custom
  (visual-fill-column-width 120)
  (visual-fill-column-center-text t)
  (split-window-preferred-function 'visual-fill-column-split-window-sensibly)) ; Be able to vertically split windows that have wide margins

;;;; Org-appear
;; Show hidden characters (e.g. emphasis markers, link brackets) when point is
;; over enclosed content
(use-package org-appear
  :ghook 'org-mode-hook
  :custom
  (org-appear-delay 0.0)
  (org-appear-trigger 'always)
  (org-appear-autoemphasis t)
  (org-appear-autolinks nil)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-inside-latex t))

;;;; Org-modern
(use-package org-modern
  :disable
  :custom
  (org-modern-label-border 1)
  (org-modern-timestamp t)
  (org-modern-keyword nil)
  (org-modern-table t)
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 0)
  (org-modern-list ; I swap the defaults for + and *
   '((?+ . "•")
     (?- . "–")
     (?* . "◦")))
  :init
  (global-org-modern-mode))

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
;;;; Org-web-tools
;; Paste https links with automatic descriptions
(use-package org-web-tools
  :general (kb/yank-kill-keys
             :keymaps 'org-mode-map
             "b" '(org-web-tools-insert-link-for-url :wk "Paste https")))

;;;; Org-download
;; Insert images and screenshots into select modes
(use-package org-download
  :hook (org-mode . org-download-enable)
  :general (kb/yank-kill-keys
             :keymaps 'org-mode-map
             "i" '(org-download-clipboard :wk "Paste image from clipboard"))
  :custom
  (org-download-method 'attach)
  (org-download-screenshot-method "scrot -s %s") ; Use scrot
  (org-download-image-dir org-attach-id-dir)
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y-%m-%d_%H-%M-%S_")) ; Default

;;;; Typo-mode
;; Typography stuff for quotations, hyphens, back-ticks, etc.
(use-package typo
  :disabled          ; Opt for `electric-quote-mode' and `prettify-symbols-mode'
  :ghook 'org-mode-hook
  :config
  (defun kb/typo-insert-cycle (cycle)
    "Insert the strings in CYCLE"
    (let ((i 0)
          (repeat-key last-input-event)
          repeat-key-str)
      (insert (nth i cycle))
      (setq repeat-key-str (format-kbd-macro (vector repeat-key) nil))
      (while repeat-key

        ;; Don't show the echo messages about cycling when typing in the
        ;; minibuffer
        (unless (minibufferp (current-buffer))
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

If used with a numeric prefix argument N, N typewriter
apostrophes will be inserted."
                     ("'" "’")))        ; Swapped these two

;;; org-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-general-rcp)
