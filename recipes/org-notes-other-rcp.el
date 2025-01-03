;;; org-notes-other-rcp.el --- Other note-taking things  -*- lexical-binding: t; -*-

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

;; Packages indirectly related to my note-taking workflow.

;;; Code:
(require 'custom-directories-rcp)
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Generalized

;;;;; Org-remark
;;;;;; Itself
(use-package org-remark
  :diminish (org-remark-mode org-remark-global-tracking-mode)
  :hook (on-first-file . org-remark-global-tracking-mode)
  :bind
  ( :map org-remark-mode-map
    ("C-c r r" . (lambda () (interactive) (org-remark-highlights-load)))
    ("C-c r l" . org-remark-mark-line)
    ("C-c r d" . org-remark-delete)
    ("C-c r c" . org-remark-change)
    ("C-c r t" . org-remark-toggle)
    ("C-c r o" . org-remark-open)
    ("C-c r v" . org-remark-view)
    ("C-c r n" . org-remark-next)
    ("C-c r p" . org-remark-prev)
    :repeat-map kb/org-remark-mode-repeat-map
    ("d" . org-remark-delete)
    ("c" . org-remark-change)
    ("t" . org-remark-toggle)
    ("o" . org-remark-open)
    ("v" . org-remark-view)
    ("n" . org-remark-next)
    ("p" . org-remark-prev))
  :custom
  (org-remark-source-file-name 'abbreviate-file-name)
  (org-remark-notes-file-name
   (no-littering-expand-var-file-name "org-remark/marginalia.org"))
  (org-remark-notes-display-buffer-action `((display-buffer-in-side-window)
                                            (side . right)
                                            (slot . 1)))
  (org-remark-create-default-pen-set nil) ; Make my own pens
  (org-remark-notes-auto-delete nil)
  :config
  (with-eval-after-load 'eww
    (org-remark-eww-mode 1))
  (with-eval-after-load 'nov
    (org-remark-nov-mode 1))
  (with-eval-after-load 'info
    (org-remark-info-mode 1))

  (with-eval-after-load 'all-the-icons
    (setopt org-remark-icon-notes (all-the-icons-material "details")
            org-remark-icon-position-adjusted (all-the-icons-material "error")
            org-remark-line-icon (all-the-icons-faicon "sticky-note"))))

;;;;;; Custom `org-remark' pens
;; Sets up `org-remark' faces according to the following schema:
;; - Resonant (red)
;; - Thesis (yellow)
;; - Detail (blue)
;; - Outline (green)
;; - External (purple/magenta)
(with-eval-after-load 'org-remark
  (defface kb/org-remark-resonant-face nil
    "Face for resonant annotations.")
  (defface kb/org-remark-resonant-minor-face nil
    "Face for less resonant (underlined) annotations.")

  (defface kb/org-remark-thesis-face nil
    "Face for thesis annotations.")
  (defface kb/org-remark-thesis-minor-face nil
    "Face for less thesis (underlined) annotations.")

  (defface kb/org-remark-detail-face nil
    "Face for detail annotations.")
  (defface kb/org-remark-detail-minor-face nil
    "Face for less detail (underlined) annotations.")

  (defface kb/org-remark-outline-face nil
    "Face for outline annotations.")
  (defface kb/org-remark-outline-minor-face nil
    "Face for less outline (underlined) annotations.")

  (defface kb/org-remark-external-face nil
    "Face for \"external\" annotations.")
  (defface kb/org-remark-external-minor-face nil
    "Face for less \"external\" (underlined) annotations.")

  (defun kb/org-remark-setup-pen-colors ()
    "Set up pen colors."
    (modus-themes-with-colors
      (set-face-attribute 'kb/org-remark-resonant-face nil
                          :background bg-red-intense)
      (set-face-attribute 'kb/org-remark-resonant-minor-face nil
                          :underline `(:color ,bg-red-intense :style wave))

      (set-face-attribute 'kb/org-remark-thesis-face nil
                          :background bg-yellow-subtle)
      (set-face-attribute 'kb/org-remark-thesis-minor-face nil
                          :underline `(:color ,bg-yellow-subtle :style wave))

      (set-face-attribute 'kb/org-remark-detail-face nil
                          :background bg-blue-subtle)
      (set-face-attribute 'kb/org-remark-detail-minor-face nil
                          :underline `(:color ,bg-blue-subtle :style wave))

      (set-face-attribute 'kb/org-remark-outline-face nil
                          :background bg-green-subtle)
      (set-face-attribute 'kb/org-remark-outline-minor-face nil
                          :underline `(:color ,bg-green-subtle :style wave))

      (set-face-attribute 'kb/org-remark-external-face nil
                          :background bg-magenta-intense)
      (set-face-attribute 'kb/org-remark-external-minor-face nil
                          :underline `(:color ,bg-magenta-intense :style wave))))
  (kb/org-remark-setup-pen-colors)
  (add-hook 'kb/themes-hook #'kb/org-remark-setup-pen-colors)

  (org-remark-create "resonant"
                     'kb/org-remark-resonant-face
                     `(CATEGORY "resonant" help-echo "Annotation that resonates with me."))
  (org-remark-create "resonant-underline"
                     'kb/org-remark-resonant-minor-face
                     `(CATEGORY "resonant" help-echo "Annotation that resonates with me but I don't want to be as noticeable."))

  (org-remark-create "thesis"
                     'kb/org-remark-thesis-face
                     `(CATEGORY "thesis" help-echo "Annotation that denotes something relevant to a thesis."))
  (org-remark-create "thesis-underline"
                     'kb/org-remark-thesis-minor-face
                     `(CATEGORY "thesis" help-echo "Annotation that denotes something relevant to a thesis but I don't want to be as noticeable."))

  (org-remark-create "detail"
                     'kb/org-remark-detail-face
                     `(CATEGORY "detail" help-echo "Annotation that denotes a notable detail."))
  (org-remark-create "detail-underline"
                     'kb/org-remark-detail-minor-face
                     `(CATEGORY "detail" help-echo "Annotation that denotes a notable detail but I don't want to be as noticeable."))

  (org-remark-create "outline"
                     'kb/org-remark-outline-face
                     `(CATEGORY "outline" help-echo "Annotation that foreshadows structure or main idea(s)."))
  (org-remark-create "outline-underline"
                     'kb/org-remark-outline-minor-face
                     `(CATEGORY "outline" help-echo "Annotation that foreshadows structure or main idea(s) but I don't want to be as noticeable."))

  (org-remark-create "external"
                     'kb/org-remark-external-face
                     `(CATEGORY "external" help-echo "Annotation that resonates with me but is external to the text."))
  (org-remark-create "external-underline"
                     'kb/org-remark-external-minor-face
                     `(CATEGORY "external" help-echo "Annotation that resonates with me but is external to the text but I don't want to be as noticeable."))

  (require 'transient)
  (transient-define-prefix kb/org-remark-mark-transient ()
    "Transient menu for my pre-defined `org-remark' pens."
    [["Resonant"
      ("r" "Highlight" org-remark-mark-resonant)
      ("R" "Underline" org-remark-mark-resonant-underline)]
     ["Thesis"
      ("t" "Highlight" org-remark-mark-thesis)
      ("T" "Underline" org-remark-mark-thesis-underline)]]
    [["Detail"
      ("d" "Highlight" org-remark-mark-detail)
      ("D" "Underline" org-remark-mark-detail-underline)]
     ["Outline"
      ("o" "Highlight" org-remark-mark-outline)
      ("O" "Underline" org-remark-mark-outline-underline)]
     ["External"
      ("e" "Highlight" org-remark-mark-external)
      ("E" "Underline" org-remark-mark-external-underline)]])
  (bind-key "C-c r m" #'kb/org-remark-mark-transient 'org-remark-mode-map))

;;;;; Org-transclusion
;; Enable transclusion of org files
(use-package org-transclusion
  :hook (org-mode . org-transclusion-mode)
  :custom
  (org-transclusion-include-first-section t)
  (org-transclusion-exclude-elements '(property-drawer)))

;;;;; Paw
(use-package paw
  :disabled    ; NOTE 2024-09-23: Such messy code and idiosyncratic practices...
  :vc (:url "https://github.com/chenyanming/paw.git"
            :rev :newest)
  :hook
  (wallabag-entry-mode . paw-annotation-mode)
  :custom
  ;; TODO 2024-09-23: The parent directory of the following three paths must be
  ;; set, or paw errors... Make issues about this.
  (paw-cache-dir (no-littering-expand-var-file-name "paw/cache"))
  (paw-tts-cache-dir (no-littering-expand-var-file-name "paw/edge-tts"))
  (paw-note-dir (no-littering-expand-var-file-name "paw/notes"))
  (paw-annotation-read-only-enable t)
  (paw-view-note-after-editting-note nil)
  (paw-svg-enable t))

;;;; PDFs
;;;;; DocView
(use-package doc-view
  :custom
  (doc-view-resolution 192))

;;;;; Pdf-tools
;;;;;; Itself
;; View pdfs and interact with them. Has many dependencies
;; https://github.com/politza/pdf-tools#compiling-on-fedora
(use-package pdf-tools
  ;; You have to call `pdf-tools-install' to have PDF files use pdf-view-mode
  ;; and have everything required loaded
  :hook (on-first-file . pdf-tools-install)
  :bind ( :map pdf-view-mode-map
          ("C-c C-r a" . pdf-view-auto-slice-minor-mode)
          ;; Also lets you scroll via `scroll-other-window'
          ([remap scroll-up-command] . pdf-view-scroll-up-or-next-page)
          ([remap scroll-down-command] . pdf-view-scroll-down-or-previous-page))
  ;; :demand ; FIXME 2024-10-03: I use this because I don't know why the package isn't loading what's needed when I need it
  ;; FIXME 2024-01-13: There is an issue between `org-noter-insert-precise-note'
  ;; and this fork. I've even tried merging this fork to upstream/master to no
  ;; avail. I like continuous scrolling so I'll return to this at a later date.
  ;; FIXME 2024-01-13: This is a pull request fork that implements continuous
  ;; scrolling (`pdf-view-roll-minor-mode'). Revert to upstream once the pull
  ;; request is merged. See https://github.com/vedang/pdf-tools/pull/224
  ;; :ensure (:type git
  ;;          :host github
  ;;          :repo "aikrahguzar/pdf-tools"
  ;;          :branch "upstream-pdf-roll"
  ;;          :remotes ("upstream" :repo "vedang/pdf-tools"))
  :config
  (add-to-list 'desktop-locals-to-save 'pdf-view-register-alist))

;;;;;; Pdf-view
(use-package pdf-view
  :ensure nil
  :autoload kb/pdf-cleanup-windows-h
  :hook
  ;; FIXME 2024-01-13: Uncomment this once the above issues between the official
  ;; release and pending pull request are resolved.
  ;; (pdf-view-mode . pdf-view-roll-minor-mode)
  (pdf-view-mode . (lambda () (add-hook 'kill-buffer-hook #'kb/pdf-cleanup-windows-h nil t)))
  :custom
  (pdf-view-resize-factor 1.1)
  (pdf-view-display-size 'fit-page)
  (pdf-view-continuous nil) ; REVIEW 2024-01-16: Change this when I get to use image-roll?
  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick t)
  :config
  ;; Taken from Doom
  (defun kb/pdf-cleanup-windows-h ()
    "Kill left-over annotation buffers when the document is killed."
    ;; We add a guard here because sometimes things go wrong and this function
    ;; is called before `pdf-annot' is loaded, causing an error
    (when (featurep 'pdf-annot)
      (when (buffer-live-p pdf-annot-list-document-buffer)
        (pdf-info-close pdf-annot-list-document-buffer))
      (when (buffer-live-p pdf-annot-list-buffer)
        (kill-buffer pdf-annot-list-buffer))
      (let ((contents-buffer (get-buffer "*Contents*")))
        (when (and contents-buffer (buffer-live-p contents-buffer))
          (kill-buffer contents-buffer))))))

;;;;;; Pdf-outline
(use-package pdf-outline
  :ensure nil
  :custom
  (pdf-outline-enable-imenu t)
  (pdf-outline-display-labels t)
  (pdf-outline-imenu-use-flat-menus nil))

;;;;;; Pdf-annot
(use-package pdf-annot
  :ensure nil
  :hook (pdf-annot-list-mode . (lambda () (hl-line-mode -1)))
  :custom
  (pdf-annot-color-history              ; "Default" colors
   '("yellow" "SteelBlue1" "SeaGreen3" "LightSalmon1" "MediumPurple1"))
  (pdf-annot-list-format '((page . 3)
                           (color . 8)
                           (text . 68)
                           (type . 10)))
  (pdf-annot-list-highlight-type nil)
  :config
  ;; Fit the "contents" window to buffer height
  (defun kb/pdf-annot-list-context-function (id buffer)
    "Show the contents of an Annotation.

For an annotation identified by ID, belonging to PDF in BUFFER,
get the contents and display them on demand."
    (with-current-buffer (get-buffer-create "*Contents*")
      (set-window-buffer nil (current-buffer))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when id
          (save-excursion
            (insert
             (pdf-annot-print-annotation
              (pdf-annot-getannot id buffer)))))
        (read-only-mode 1))
      (fit-window-to-buffer)
      (visual-line-mode)))
  (advice-add 'pdf-annot-list-context-function :override #'kb/pdf-annot-list-context-function)

  ;; Set the display action (e.g. window parameters) for the "context buffer"
  ;; (the buffer that shows annotation contents in `pdf-annot-mode')
  (setq tablist-context-window-display-action
        '((display-buffer-reuse-window tablist-display-buffer-split-below-and-attach)
          (window-height . 0.25)
          (inhibit-same-window . t)
          (window-parameters (no-other-window . t)
                             (mode-line-format . none)))))

;;;;;; Custom entry formatter
(with-eval-after-load 'pdf-tools
  (defun kb/pdf-annot--make-entry-formatter (a)
    "Return a formatter function for annotation A.

A formatter function takes a format cons-cell and returns
pretty-printed output."
    (lambda (fmt)
      (let ((entry-type (car fmt))
            (entry-width (cdr fmt))
            ;; Taken from css-mode.el
            (contrasty-color
             (lambda (name)
               (if (> (color-distance name "black") 292485)
                   "black" "white")))
            (prune-newlines
             (lambda (str)
               (replace-regexp-in-string "\n" " " str t t))))
        (cl-ecase entry-type
          (date (propertize (pdf-annot-print-property a 'modified)
                            'date
                            (pdf-annot-get a 'modified)))
          (page (pdf-annot-print-property a 'page))
          (label (funcall prune-newlines
                          (pdf-annot-print-property a 'label)))
          (contents
           (truncate-string-to-width
            (funcall prune-newlines
                     (pdf-annot-print-property a 'contents))
            entry-width))
          (type
           (let ((color (pdf-annot-get a 'color))
                 (type (pdf-annot-print-property a 'type)))
             (if (and pdf-annot-list-highlight-type color)
                 (propertize
                  type 'face
                  `(:background ,color
                                :foreground ,(funcall contrasty-color color)))
               type)))
          (color
           (let* ((color (pdf-annot-get a 'color)))
             (propertize
              color 'face
              `(:background ,color
                            :foreground ,(funcall contrasty-color color)))))
          (text
           (let* ((page (pdf-annot-get a 'page))
                  (edges (or (when (featurep 'org-noter)
                               (org-noter-pdf--edges-to-region (alist-get 'markup-edges a)))
                             (pdf-annot-get a 'edges)))
                  (raw-text
                   (pdf-info-gettext page
                                     edges
                                     pdf-view-selection-style
                                     pdf-annot-list-document-buffer))
                  (processed-text
                   (replace-regexp-in-string "\n" " "
                                             (replace-regexp-in-string "-\n" "" raw-text)))
                  (text-length (length processed-text)))
             (when (< entry-width text-length)
               (add-text-properties entry-width text-length '(display "…") processed-text))
             processed-text))))))
  (advice-add 'pdf-annot--make-entry-formatter :override 'kb/pdf-annot--make-entry-formatter))

;;;;;; Avy keys to highlight region in PDF
;; Use an avy-like interface to highlight region in pdf-view-mode. Heavily based
;; off of
;; https://github.com/dalanicolai/dala-emacs-lisp/blob/master/pdf-avy-highlight.el
;; with modifications
(with-eval-after-load 'pdf-view
  (require 'avy)

  (defcustom kb/avy-pdf-links-convert-pointsize-scale 0.02
    "The scale factor for the -pointsize convert command.

This determines the relative size of the font, when interactively
reading links."
    :group 'pdf-links
    :type '(restricted-sexp :match-alternatives
                            ((lambda (x) (and (numberp x)
                                         (<= x 1)
                                         (>= x 0))))))

  (defun kb/avy-pdf-links-read-char-action (query prompt)
    "Using PROMPT, interactively read a link-action.
BORROWED FROM `pdf-links-read-link-action'.
See `pdf-links-action-perform' for the interface."
    (pdf-util-assert-pdf-window)
    (let* ((links (pdf-info-search-string
                   query
                   (pdf-view-current-page)
                   (current-buffer)))
           (keys (pdf-links-read-link-action--create-keys
                  (length links)))
           (key-strings (mapcar (apply-partially 'apply 'string)
                                keys))
           (alist (cl-mapcar 'cons keys links))
           (size (pdf-view-image-size))
           (colors (pdf-util-face-colors 'pdf-links-read-link pdf-view-dark-minor-mode))
           (args (list
                  :foreground (car colors)
                  :background (cdr colors)
                  :formats
                  `((?c . ,(lambda (_edges) (pop key-strings)))
                    (?P . ,(number-to-string
                            (max 1 (* (cdr size)
                                      kb/avy-pdf-links-convert-pointsize-scale)))))
                  :commands pdf-links-read-link-convert-commands
                  :apply (pdf-util-scale-relative-to-pixel
                          (mapcar (lambda (l) (car (cdr (assq 'edges l))))
                                  links)))))
      (unless links
        (error "No links on this page"))
      (unwind-protect
          (let ((image-data nil))
            (unless image-data
              (setq image-data (apply 'pdf-util-convert-page args ))
              (pdf-cache-put-image
               (pdf-view-current-page)
               (car size) image-data 'pdf-links-read-link-action))
            (pdf-view-display-image
             (create-image image-data (pdf-view-image-type) t))
            (pdf-links-read-link-action--read-chars prompt alist))
        (pdf-view-redisplay))))

  (defun kb/avy-pdf-timed-input ()
    "BORROWED FORM `avy--read-candidates'"
    (let ((str "")
          char break)
      (while (and (not break)
                  (setq char
                        (read-char (format "char%s: "
                                           (if (string= str "")
                                               str
                                             (format " (%s)" str)))
                                   t
                                   (and (not (string= str ""))
                                        avy-timeout-seconds))))
        ;; Unhighlight
        (cond
         ;; Handle RET
         ((= char 13)
          (if avy-enter-times-out
              (setq break t)
            (setq str (concat str (list ?\n)))))
         ;; Handle C-h, DEL
         ((memq char avy-del-last-char-by)
          (let ((l (length str)))
            (when (>= l 1)
              (setq str (substring str 0 (1- l))))))
         ;; Handle ESC
         ((= char 27)
          (keyboard-quit))
         (t
          (setq str (concat str (list char))))))
      str))

  (defun kb/avy-pdf-get-coordinates (end)
    "Prompt for PDF coordinates using avy-like interface."
    (let* ((query (kb/avy-pdf-timed-input))
           (coords
            (list (or (kb/avy-pdf-links-read-char-action query
                                                         (format "Please specify %s (SPC scrolls): "
                                                                 (if end "region end" "region beginning")))
                      (error "No char selected")))))
      (car (alist-get 'edges (car coords)))))

  (defun kb/avy-pdf-highlight (&optional activate)
    "Use an avy-like interface to highlight region in PDF.

If called with ACTIVATE, then also activate the created
annotation immediately after creation."
    (interactive "P")
    (let* ((start (kb/avy-pdf-get-coordinates nil))
           (end (kb/avy-pdf-get-coordinates :end))
           (edges (append (cl-subseq start 0 2) (cl-subseq end 2 4)))
           (pdf-annot-activate-created-annotations activate))
      (pdf-annot-add-markup-annotation edges
                                       (let ((type (completing-read "Markup type (default highlight): "
                                                                    '("squiggly" "highlight" "underline" "strike-out")
                                                                    nil t)))
                                         (if (equal type "") 'highlight (intern type)))
                                       (pdf-annot-read-color "Annotation color: "))))

  (bind-key [remap avy-goto-char-timer] #'kb/avy-pdf-highlight pdf-view-mode-map))

;;;;;; Pdf-annot-list custom (tablist) color filter
(with-eval-after-load 'pdf-annot
  (defun kb/pdf-annot-list-filter-color-regexp ()
    "Get a prompt to filter for the color column's colors.
The offered colors are those already present in the document's
highlights."
    (interactive)
    (unless (derived-mode-p 'tabulated-list-mode)
      (error "Buffer is not in Tabulated List Mode"))
    (let ((unique-colors
           ;; Get unique colors directly from PDF
           (-non-nil
            (cl-remove-duplicates
             (mapcar
              (lambda (a) (pdf-annot-get a 'color))
              (pdf-annot-getannots nil nil pdf-annot-list-document-buffer))
             :test #'string=)))
          (nearby-color)
          (color-alist))
      ;; Scrape unique colors and closest neighboring defined color name
      (let ((lowest-dist most-positive-fixnum)
            (dist))
        (dolist (uc unique-colors)
          (dolist (c (defined-colors))
            (setq dist (color-distance c uc))
            (when (< dist lowest-dist)
              (setq nearby-color c
                    lowest-dist dist)))
          (push (list (propertize (format "%s (%s)" nearby-color uc)
                                  ;; Taken from pdf-annot.el
                                  'face `(:foreground ,(if (> (color-distance uc "black")
                                                              292485)
                                                           "black" "white")
                                                      :background ,uc))
                      uc)
                color-alist)
          (setq lowest-dist most-positive-fixnum)))

      (let* ((selections (completing-read-multiple "Select color: "
                                                   (mapcar 'car color-alist)
                                                   nil t))
             (color-filter (when selections
                             (concat "("
                                     (string-join
                                      (cl-loop for s in selections
                                               collect (concat "Color =~ " (cadr (assoc-string s color-alist))))
                                      " || ")
                                     ")"))))
        (tablist-push-filter (tablist-filter-parse color-filter)
                             (called-interactively-p 'any)))))

  (defun kb/pdf-annot-list-filter-regexp ()
    "Drop-in replacement for `tablist-push-regexp-filter'.
Calls `kb/pdf-annot-list-filter-color-regexp' when point is in
the color column."
    (interactive)
    (if (string= "Color" (tablist-read-column-name nil))
        (kb/pdf-annot-list-filter-color-regexp)
      (call-interactively 'tablist-push-regexp-filter)))

  (bind-key [remap tablist-push-regexp-filter] #'kb/pdf-annot-list-filter-regexp 'pdf-annot-list-mode-map))

;;;;;; Custom org-link type for PDF annotations
;; NOTE 2024-02-10: Code copied from the code shared on Thu, 08 Feb 2024
;; 22:13:50 +0000 by Juan Manuel Macías <maciaschain@posteo.net> in the
;; Emacs-devel mailing list. The original uses the modification date, whereas
;; this verion uses the annotation ID, which should be unique even upon
;; deletions and additions of annotations. This is because the annotation IDs
;; are robust (don't change upon modifying content of annotation). Additionally,
;; the original version relies on an in-buffer `re-search-forward', meaning it
;; only worked if you had the current modified date as a column in
;; `pdf-annot-list-format'; this is avoided if we use ID's like
;; `pdf-annot-list-display-annotation-from-id' likes.
(with-eval-after-load 'org
  (defun kb/org-pdf-annot-store-link ()
    "Stores link to annotation via its annotate.
Uses the current annotation at point's ID."
    (when (eq major-mode 'pdf-annot-list-mode)
      (let* ((annot-buf pdf-annot-list-document-buffer)
             (pdf-file (buffer-file-name annot-buf))
             (annot (pdf-annot-getannot (tabulated-list-get-id) annot-buf))
             (id (pdf-annot-print-property annot 'id))
             (page (pdf-annot-print-property annot 'page))
             (link (concat "pdf-annot:" pdf-file "::" id))
             (desc (format "%s (annot. on p. %s)" (file-name-nondirectory pdf-file) page)))
        (org-link-store-props
         :type "pdf-annot"
         :link link
         :description desc))))

  (defun kb/org-pdf-annot-follow-link (path)
    "Open pdf-tools link."
    (let ((id (if (string-match "::\\(.+\\)" path)
                  (match-string 1 path)
                (error "[kb/org-pdf-annot-follow-link] Not a valid id!")))
          (file-path (replace-regexp-in-string "::.+" "" path)))
      (find-file file-path)
      (pdf-annot-list-annotations)
      (with-current-buffer (format "*%s's annots*" (file-name-sans-extension (buffer-name)))
        (goto-line (save-excursion
                     (goto-char (point-min))
                     ;; Find line whose tabulated-list-id corresponds to
                     ;; annotation ID
                     (let ((row-id (get-text-property (point) 'tabulated-list-id)))
                       (while (not (or (eq row-id (intern id))
                                       (= (line-number-at-pos (point)) (line-number-at-pos (point-max)))))
                         (forward-line)
                         (setq row-id (get-text-property (point) 'tabulated-list-id))))
                     (line-number-at-pos)))
        (pdf-annot-list-display-annotation-from-id id))))

  (org-link-set-parameters
   "pdf-annot"
   :follow #'kb/org-pdf-annot-follow-link
   :store #'kb/org-pdf-annot-store-link))

;;;;;; Modify PDF metadata
;; Emacs wrapper and convenience functions for changing package metadata using
;; `pdftk'. See https://unix.stackexchange.com/a/72457 for more information on
;; the CLI commands involved.
(with-eval-after-load 'pdf-tools
  (system-packages-ensure "pdftk")

  (defun kb/pdf-tools--metadata-bookmark-section ()
    "Insert bookmark metadata section."
    (interactive)
    (save-excursion
      (insert "BookmarkBegin\nBookmarkTitle: \nBookmarkLevel: 1\nBookmarkPageNumber: tk\n"))
    (move-end-of-line 2))

  (defvar kb/pdf-tools-metadata-mode-map
    (let ((km (make-sparse-keymap)))
      (define-key km (kbd "C-c C-b") #'kb/pdf-tools--metadata-bookmark-section)
      km)
    "Mode map for `kb/pdf-tools-metadata-mode'.")

  (define-derived-mode kb/pdf-tools-metadata-mode fundamental-mode "Metadata"
    "Major mode for altering and viewing PDF metadata."
    :interactive t
    (use-local-map kb/pdf-tools-metadata-mode-map))

  (defun kb/pdf-tools--metadata-modify (pdf-file)
    "Modify PDF-FILE metadata."
    (interactive (list (buffer-file-name)))
    (unless (string= "pdf" (file-name-extension pdf-file))
      (error "File is not a PDF!"))
    (unless (executable-find "pdftk")
      (error "System executable `pdftk' not found. Please install executable on filesystem to proceed."))
    (let* ((pdf-name (file-name-sans-extension (file-name-nondirectory pdf-file)))
           (buf-name (concat "*pdf-tools metadata: " pdf-name))
           (metadata-file (concat "/tmp/pdf-tools-metadata--" pdf-name))
           (temp-pdf (make-temp-file "/tmp/pdf-tools-metadata--temp-pdf"))
           (metadata-dump-command (concat "pdftk \"" pdf-file "\" dump_data"))
           (metadata-update-command
            (concat "pdftk \"" pdf-file "\" update_info \"" metadata-file "\" output \"" temp-pdf "\""))
           (commit-func (lambda ()
                          "Commit the changes to PDF metadata."
                          (interactive)
                          (with-current-buffer buf-name
                            (widen)
                            (write-region (point-min) (point-max) metadata-file))
                          (shell-command metadata-update-command "*pdf-tools metadata: CLI output")
                          (kill-buffer buf-name)
                          ;; Have to do it this way since `pdftk' does not allow
                          ;; having the output file be the input file
                          (rename-file temp-pdf pdf-file t)
                          (message "Updated metadata!"))))
      (save-buffer)
      (with-current-buffer (get-buffer-create buf-name)
        (insert (shell-command-to-string metadata-dump-command))
        (goto-char (point-min))
        (kb/pdf-tools-metadata-mode))
      (pop-to-buffer buf-name)
      (define-key kb/pdf-tools-metadata-mode-map (kbd "C-c C-c") commit-func)
      (set-buffer-modified-p nil)
      (message "Press `C-c C-c' when finished editing PDF metadata")))
  (define-key pdf-view-mode-map (kbd "C-c m") #'kb/pdf-tools--metadata-modify))

;;;;; Saveplace-pdf-view
;; Save place in pdf-view buffers
(use-package saveplace-pdf-view
  :after saveplace
  :demand)

;;;;; Org-noter
(use-package org-noter
  ;; :ensure (:protocol ssh
  ;;                    :fetcher github
  ;;                    :repo "org-noter/org-noter"
  ;;                    :files ("*.el" "modules" (:exclude "*-test-utils.el" "*-devel.el"))
  ;;                    :remotes ("remote" :repo "krisbalintona/org-noter"))
  :vc (:url "https://github.com/krisbalintona/org-noter.git"
            :rev :newest)
  :bind
  ( :map pdf-misc-minor-mode-map
    (("I" . nil)
     ("M" . 'pdf-misc-display-metadata))
    :map org-noter-doc-mode-map
    (("i" . org-noter-insert-precise-note)
     ("I" . org-noter-insert-precise-note-toggle-no-questions)
     ("C-i" . org-noter-insert-note)
     ("C-S-i" . org-noter-insert-note-toggle-no-questions)
     ("C-M-i" . nil)
     ("M-i" . nil)
     ;; FIXME 2024-01-13: Choose better keybind
     ("C-M-s-\"" . org-noter-pdf--create-missing-annotation)))
  :custom
  (org-noter-notes-search-path `(,kb/notes-dir))
  ;; FIXME 2024-01-12: I am not currently using org-noter, but when I do, I can
  ;; create a notes file and set it here like thus
  ;; (org-noter-default-notes-file-names
  ;;  (list (file-relative-name (car (directory-files-recursively
  ;;                                  kb/notes-dir "20240111T235139"))
  ;;                            kb/notes-dir)))
  (org-noter-always-create-frame nil)
  (org-noter-kill-frame-at-session-end nil) ; Don't close frame when killing pdf buffer
  (org-noter-use-indirect-buffer t)
  (org-noter-disable-narrowing nil)
  (org-noter-hide-other t)
  (org-noter-auto-save-last-location nil)
  (org-noter-separate-notes-from-heading t)
  (org-noter-highlight-selected-text t) ; Always leave highlights from annotations
  (org-noter-arrow-foreground-color "red")
  (org-noter-arrow-background-color "black")
  (org-noter-doc-property-in-notes nil)
  (org-noter-insert-note-no-questions nil) ; Activate this setting if I rarely type my own titles
  (org-noter-max-short-selected-text-length 0) ; Always enclose in quote block
  :config
  (org-noter-enable-update-renames)

  (defun kb/org-noter-pdf--get-selected-text (mode)
    (when (and (eq mode 'pdf-view-mode)
               (pdf-view-active-region-p))
      (let* ((raw-text (mapconcat 'identity (pdf-view-active-region-text) ? ))
             (process-text-1 (replace-regexp-in-string "-\n" "" raw-text))
             (process-text-2 (replace-regexp-in-string "\n" " " process-text-1)))
        process-text-2)))
  (advice-add 'org-noter-pdf--get-selected-text :override #'kb/org-noter-pdf--get-selected-text))

;;;; Videos
;;;;; MPV
;; Dependency for packages that interact with mpv
(use-package mpv
  ;; NOTE 2024-03-31: See
  ;; https://github.com/kljohann/mpv.el/issues/31#issuecomment-1856491594 for
  ;; why I use the latest GitHub version
  :vc (:rev :newest)
  :custom
  (mpv-default-options (list "--save-position-on-quit")))

;;;;; Custom MPV notes
(with-eval-after-load 'org
  (require 'mpv)

  (defun kb/mpv-play ()
    "Call `mpv-start'.
Prompts for file in `org-attach-directory' if existent. Otherwise,
prompts for file in `default-directory'.

Behaves specially in dired buffers. In those cases case, marked files
will be played as a playlist as chronologically displayed in the dired
buffer. If no files are marked, just the file at point is played. (It is
useful to `dired-sort-toggle-or-edit' to control the ordering of files.
To reverse the order, pass the \"-r\" flag to the listing switches, done
by calling `dired-sort-toggle-or-edit' with `C-u'.)"
    (interactive)
    (let* ((dir (if (org-attach-dir)
                    (file-name-as-directory (org-attach-dir))
                  default-directory))
           (files
            (or (cl-remove-if
                 (lambda (f)
                   "Ensure F is not a directory and is a video file."
                   (not (and (not (directory-name-p f))
                             (member (file-name-extension f)
                                     ;; OPTIMIZE 2024-03-31: I hard-code this,
                                     ;; but I don't know if there's a better way
                                     ;; to recognize video extensions
                                     (list "mp4" "avi" "mkv" "mov" "wmv" "flv" "webm" "mpeg" "m4v" "3gp")))))
                 ;; NOTE 2024-03-31: If no files are marked, the file at point
                 ;; is treated as marked
                 (dired-get-marked-files))
                (expand-file-name (read-file-name "Select video: " dir)))))
      (apply #'mpv-start files)))

  (defun kb/mpv-jump-to-playback-position (time)
    "Prompt user for a time to jump to.
Takes HH:MM:SS time format. Uses `org-timer-hms-to-secs' to parse user
input."
    (interactive "MJump to time (HH:MM:SS format): ")
    (let ((secs (org-timer-hms-to-secs time)))
      (mpv-seek secs)))

  ;; Keymap
  (defvar-keymap kb/mpv-map
    :doc "Keymap for my mpv.el commands for use in `org-mode'.
Commands that control MPV playback mimic MPV keybinds."
    :repeat (mpv-pause mpv-seek-backward mpv-seek-forward)
    "o" #'kb/mpv-play
    "O" #'mpv-play-url
    "k" #'mpv-kill
    "p" #'mpv-pause
    "b" #'mpv-seek-backward
    "f" #'mpv-seek-forward
    "g" #'kb/mpv-jump-to-playback-position
    "9" #'mpv-volume-decrease
    "0" #'mpv-volume-increase
    "[" #'mpv-speed-decrease
    "]" #'mpv-speed-increase
    "P" #'mpv-jump-to-playlist-entry
    "i" #'mpv-insert-playback-position)
  (define-key global-map (kbd "C-M-s-m") kb/mpv-map)

  ;; Taken from https://github.com/kljohann/mpv.el/wiki
  (defun kb/mpv-org-metareturn-insert-playback-position ()
    "When on an `org-timer' formatted list, insert playback position."
    (when-let ((item-beg (org-in-item-p)))
      (when (and (not (bound-and-true-p org-timer-start-time))
                 (mpv-live-p)
                 (save-excursion
                   (goto-char item-beg)
                   (and (not (org-invisible-p)) (org-at-item-timer-p))))
        (mpv-insert-playback-position t))))
  (add-hook 'org-metareturn-hook #'kb/mpv-org-metareturn-insert-playback-position)

  ;; Go to timestamps with `org-open-at-point'
  (add-hook 'org-open-at-point-functions 'mpv-seek-to-position-at-point))

;;;;; Ytdl
(use-package ytdl
  :ensure-system-package (yt-dlp)
  :custom
  (ytdl-command "yt-dlp")
  (ytdl-always-query-default-filename 'yes-confirm)
  (ytdl-music-folder (expand-file-name "~/Music/"))
  (ytdl-video-folder (expand-file-name "~/Videos/"))
  (ytdl-download-types
   `(("Downloads" "d" ytdl-download-folder ytdl-download-extra-args)
     ("Music"  "m" ytdl-music-folder ytdl-music-extra-args)
     ("Videos" "v"  ytdl-video-folder ytdl-video-extra-args)
     ("Temp" "t" ,(expand-file-name "/tmp/") ("-S" "res:720,fps"))))
  :config
  (with-eval-after-load 'org
    ;; Custom `org-attach' integration
    (defun kb/ytdl-org-attach (url)
      "Download and video from URL and attach it to `org-attach-dir'.
A modified version of `ytdl-download'."
      (interactive "MProvide URL: ")
      (when (ytdl--youtube-dl-missing-p)
        (error "youtube-dl is not installed."))
      (let* ((dir (or (org-attach-dir) (org-attach-dir-get-create)))
             (destination (expand-file-name (ytdl--get-filename dir url) dir))
             (extra-ytdl-args '("--write-auto-sub" "--write-sub" "--sub-lang" "en" "--convert-subs" "srt" ; Create .srt file
                                ;; Set maximum resolution and file type
                                "-S" "res:720,fps,ext:mp4:m4a"
                                "--recode" "mp4"))
             (dl-type-name "Org-attach"))
        (ytdl--download-async url
                              destination
                              extra-ytdl-args
                              nil
                              dl-type-name)))

    (add-to-list 'org-attach-commands
                 '((?Y ?\C-Y) kb/ytdl-org-attach
                   "Provide a URL and have \"ytdl\" download the corresponding video and attach that file.")
                 t)))

;;;; Org-roam-ui
;; Newer `org-roam-server' for org-roam V2.
(use-package org-roam-ui
  :disabled
  :ensure (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :custom
  (org-roam-ui-browser-function 'browse-url-default-browser) ; Open in my actual browser, to avoid opening in EAF
  (org-roam-ui-open-on-start nil) ; Don't open graph on startup
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-custom-theme '(list
                              (bg . "#1E2029")
                              (bg-alt . "#282a36")
                              (fg . "#f8f8f2")
                              (fg-alt . "#6272a4")
                              (red . "#ff5555")
                              (orange . "#f1fa8c")
                              (yellow ."#ffb86c")
                              (green . "#50fa7b")
                              (cyan . "#8be9fd")
                              (blue . "#ff79c6")
                              (violet . "#8be9fd")
                              (magenta . "#bd93f9"))
                            )
  :preface
  (use-package websocket :demand t)
  (use-package simple-httpd :demand t)
  (use-package f :demand t)
  )

;;;; Delve
(use-package delve
  :disabled t                           ; Don't use
  :ensure (delve :type git :host github :repo "publicimageltd/delve")
  :hook
  (delve-mode . delve-compact-view-mode)
  ;; FIXME 2022-05-27: `delve--zettel-preview' seems necessary to prevent cmacro
  ;; compiler error for `kb/delve--key--toggle-preview'.
  :commands delve delve--zettel-preview
  :general
  ("<f6>" 'delve)
  (:keymaps 'delve-mode-map
            "<backtab>" 'kb/delve--key-backtab
            )
  (:keymaps 'delve-mode-map
            :states 'visual
            "d" 'delve--key--multi-delete
            "DEL" 'delve--key--multi-delete
            "y" 'kb/delve-visual-copy-nodes
            )
  (:keymaps 'delve-mode-map
            :states 'normal
            "DEL" 'delve--key--multi-delete
            "q" 'bury-buffer
            "t" 'delve--key--insert-tagged
            "T" 'delve--key--insert-node-by-tags
            "dd" '(lambda ()
                    (interactive)
                    (kill-line)
                    (forward-line -1))
            "P" 'delve--key--yank       ; Paste above
            "p" '(lambda ()                  ; Paste below
                   (interactive)
                   (save-excursion
                     (next-line)
                     (delve--key--yank))
                   (next-line))
            "r" 'delve--key--sync
            "gr" 'delve--key--sync
            "h" 'delve--key--insert-heading
            "n" 'delve--node-transient-key
            "s" 'delve--key--sort
            "C-p" 'delve--key--collect-into-pile
            "I" 'delve--key--insert-query-or-pile
            "v" 'delve-compact-view-mode
            "f" 'delve--key--fromlinks
            "b" 'delve--key--backlinks
            "RET" 'kb/delve--key--toggle-preview
            "C-o" 'delve--key--open-zettel
            "o" 'delve--key--open-zettel
            "go" 'delve--key--open-zettel
            "+" 'delve--key--add-tags
            "-" 'delve--key--remove-tags
            "M-d" 'kb/delve-mark-duplicates
            "Y" 'kb/delve-copy-zettel-title
            "yy" 'evil-yank-line
            )
  :custom
  (delve-storage-paths (concat kb/notes-dir "delve-storage/"))
  (delve-dashboard-tags '("working"))
  :init
  ;; Must be loaded before delve
  (setq delve-minor-mode-prefix-key (kbd "M-n"))
  :config
  (delve-global-minor-mode)

  ;; My own functions below
  (lister-defkey kb/delve--key--toggle-preview (ewoc pos prefix node)
                 "Toggle the display of the preview of ZETTEL."
                 (let ((zettel (delve--current-item-or-error 'delve--zettel ewoc pos)))
                   ;; Unhide the sublist first
                   (lister-with-sublist-below ewoc pos beg end
                                              (lister--outline-hide-show ewoc beg end nil))
                   ;; Then show preview
                   (let ((preview (and (not (delve--zettel-preview zettel))
                                       (or (delve--get-preview-contents zettel)
                                           "No preview available"))))
                     (setf (delve--zettel-preview zettel) preview)
                     (lister-refresh-at lister-local-ewoc :point))
                   ))

  (defvar kb/delve-cycle-global-contents t
    "Are all the contents of the given delve buffer (EWOC)
        shown or hidden? nil if hidden, t if shown.")
  (defun kb/delve--key-backtab (ewoc &optional prefix)
    "In EWOC, toggle hiding or showing all sublists.

When called with PREFIX, hide all previews."
    (interactive (list lister-local-ewoc current-prefix-arg))
    (lister-walk-nodes ewoc
                       #'(lambda (ewoc new-node)
                           (let* ((current-item (delve--current-item nil ewoc new-node))
                                  (is-zettel (cl-typep current-item 'delve--zettel))
                                  (has-preview (when is-zettel (delve--zettel-preview current-item)))
                                  )
                             ;; Hide only when...
                             (when (and (not (lister-sublist-below-p ewoc new-node)) ; Non-parents
                                        (not has-preview) ; No preview
                                        (< 0 (lister-node-get-level new-node)) ; Not top-level
                                        )
                               (lister--outline-hide-show ewoc new-node new-node kb/delve-cycle-global-contents))
                             ;; Then hide if with prefix, hide all previews
                             (when (and prefix has-preview)
                               (setf (delve--zettel-preview current-item) nil)
                               (lister-refresh-at ewoc new-node)) ; Refresh to update visually
                             ))
                       :first :last)
    (if (lister--outline-invisible-p ewoc :point) ; End a non-invisible node
        (lister-goto ewoc (lister-parent-node ewoc :point)))
    (setq-local kb/delve-cycle-global-contents (not kb/delve-cycle-global-contents)))

  (lister-defkey kb/delve-mark-duplicates (ewoc pos prefix node)
                 "Mark duplicate org-roam nodes in the current delve buffer."
                 (let ((id-tracker (list)))
                   (lister-save-current-node ewoc
                                             (lister-walk-nodes ewoc
                                                                #'(lambda (ewoc new-node)
                                                                    (let ((id (delve--zettel-id (delve--current-item nil ewoc new-node))))
                                                                      (if (member id id-tracker)
                                                                          ;; (lister-delete-at ewoc new-node)
                                                                          (lister-mode-mark ewoc new-node)
                                                                        (setq id-tracker (append id-tracker (list id))))))
                                                                :first :last
                                                                #'(lambda (new-node) (cl-typep (delve--current-item nil ewoc new-node) 'delve--zettel))))))
  (lister-defkey kb/delve-visual-copy-nodes (ewoc pos prefix node)
                 "Copy current node(s) when region is active."
                 (when (region-active-p)
                   (copy-region-as-kill (mark) (point) 'region)))
  (lister-defkey kb/delve-copy-zettel-title (ewoc pos prefix node)
                 "Copy current org-roam node's (delve--zettel) title."
                 (let* ((item (delve--current-item-or-error 'delve--zettel ewoc node))
                        (title (delve--zettel-title item))
                        (file (delve--zettel-file item)))
                   (kill-new title)
                   (message (format "The node title \"%s\" from %s has been copied" title file)))))

;;;; Lister
;; Interactive list library for `delve'
(use-package lister
  :disabled
  :general
  (:keymaps 'lister-mode-map
            "M-k" 'kb/lister-mode-up
            "M-j" 'kb/lister-mode-down
            "M-h" 'lister-mode-left
            "M-l" 'kb/lister-mode-right
            ;; Use the initial versions of the functions for these
            "M-K" '(lambda ()
                     (interactive) ; Ignore constraint of same indentation level
                     (funcall-interactively 'lister-mode-up lister-local-ewoc :point '(4)))
            "M-J" '(lambda ()
                     (interactive) ; Ignore constraint of same indentation level
                     (funcall-interactively 'lister-mode-down lister-local-ewoc :point '(4)))
            "M-H" 'kb/lister-mode-left-sublist
            "M-L" 'kb/lister-mode-right-sublist
            "gk" 'lister-mode-forward-same-level
            "gj" 'lister-mode-backward-same-level
            "zu" 'lister-mode-up-parent
            "gh" 'lister-mode-up-parent
            )
  (:keymaps 'lister-mode-map
            :states '(normal visual)
            "m" 'lister-mode-mark
            "u" 'lister-mode-unmark
            "U" 'lister-mode-unmark-all
            )
  :config
  (require 'lister-mode) ; Require since this proves the "core" definitions for the functions below

  ;; Helpers: movement vertically
  (defun kb/lister-move-item-up (ewoc pos)
    "Move item and its sublist one up, preserving `org-mode'-like indentation."
    (let* ((move-fn 'lister--prev-visible-node)
           (from-node (lister--parse-position ewoc pos)) ; Current node
           ;; Prefer `lister-sublist-below-p' over `lister-sublist-at-p' because
           ;; the latter, for some reason, does not consider the first and last
           ;; nodes to be a part of sublists...
           (to-node (if (lister-sublist-below-p ewoc from-node)
                        (cadr (lister--locate-sublist ewoc (ewoc-next ewoc from-node)))
                      from-node))
           (move-to-node (lister--next-node-same-level ewoc from-node move-fn)))
      (unless move-to-node
        (error "No movement possible"))
      ;; Move the range of nodes starting from the current node (from-node) and
      ;; ending with to-node to move-to-node's position. If from-node does not
      ;; have a sublist, then to-node will be from-node. If it does, then
      ;; to-node will be the last node in the sublist.
      (lister--move-list ewoc from-node to-node move-to-node nil)
      ))
  (defun kb/lister-move-item-down (ewoc pos &optional ignore-level)
    "Move item and its sublist one down, preserving `org-mode'-like indentation."
    (let* ((move-fn 'lister--next-visible-node)
           (current-node (lister--parse-position ewoc pos))
           (target-node (lister--next-node-same-level ewoc current-node move-fn)))
      (unless target-node
        (error "No movement possible"))
      ;; Move the next valid node (target-node), which takes its sublist if it
      ;; exists (see `kb/lister-move-item-up'), above the current-node.
      (kb/lister-move-item-up ewoc target-node)
      ))

  ;; Helpers: movement horizontally
  (defun kb/lister-move-item-right (ewoc pos node)
    "In EWOC, increase indentation level of the item at POS.

But don't indent if indenting breaks the structure of the tree."
    (let ((indentation-current (lister-get-level-at ewoc pos))
          (first-node (lister--parse-position ewoc :first)))
      ;; Don't indent if it's the first node
      (unless (equal node first-node)
        (lister-set-level-at ewoc pos (1+ indentation-current)))
      ))

  ;; New keybinds
  (lister-defkey kb/lister-mode-up (ewoc pos prefix node)
                 "Move the item at point one up, preserving `org-mode'-like tree
structure."
                 (kb/lister-move-item-up ewoc pos))
  (lister-defkey kb/lister-mode-down (ewoc pos prefix node)
                 "Move the item at point one down, preserving `org-mode'-like tree
structure."
                 (kb/lister-move-item-down ewoc pos prefix))
  (lister-defkey kb/lister-mode-right (ewoc pos prefix node)
                 "Move the item at point to the right, preserving `org-mode'-like
tree structure."
                 (kb/lister-move-item-right ewoc pos node))
  (lister-defkey kb/lister-mode-left-sublist (ewoc pos prefix node)
                 "Move the node at point and its sublist, if any, to the left."
                 (let ((indentation-current (lister-node-get-level node)))
                   (if (and (< 0 indentation-current) (lister-sublist-below-p ewoc node))
                       (progn
                         (lister-set-node-level ewoc node (1- indentation-current))
                         (lister-move-sublist-left ewoc (ewoc-next ewoc node)))
                     (lister-move-item-left ewoc pos))))
  (lister-defkey kb/lister-mode-right-sublist (ewoc pos prefix node)
                 "Move the node at point and its sublist, if any, to the right."
                 (if (lister-sublist-below-p ewoc node)
                     (progn
                       (lister-move-sublist-right ewoc (ewoc-next ewoc node))
                       ;; Move sublist before current node because the current node becomes
                       ;; part of the sublist if indented first
                       (lister-set-node-level ewoc node (1+ (lister-node-get-level node))))
                   (kb/lister-move-item-right ewoc pos node))))

(provide 'org-notes-other-rcp)
;;; org-notes-other-rcp.el ends here
