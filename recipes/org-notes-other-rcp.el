;;; org-notes-other-rcp.el --- Summary  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Packages indirectly related to my note-taking workflow.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'custom-directories-rcp)
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; PDFs
;;;; Pdf-tools
;; View pdfs and interact with them. Has many dependencies
;; https://github.com/politza/pdf-tools#compiling-on-fedora
(use-package pdf-tools
  ;; FIXME 2024-01-13: There is an issue between `org-noter-insert-precise-note'
  ;; and this fork. I've even tried merging this fork to upstream/master to no
  ;; avail. I like continuous scrolling so I'll return to this at a later date.
  ;; FIXME 2024-01-13: This is a pull request fork that implements continuous
  ;; scrolling (`pdf-view-roll-minor-mode'). Revert to upstream once the pull
  ;; request is merged. See https://github.com/vedang/pdf-tools/pull/224
  ;; :elpaca (:type git
  ;;          :host github
  ;;          :repo "aikrahguzar/pdf-tools"
  ;;          :branch "upstream-pdf-roll"
  ;;          :remotes ("upstream" :repo "vedang/pdf-tools"))
  :hook ((elpaca-after-init . pdf-tools-install)
         ;; FIXME 2024-01-13: Uncomment this once the above issues between the
         ;; official release and pending pull request are resolved.
         ;; (pdf-view-mode . pdf-view-roll-minor-mode)
         (pdf-view-mode . (lambda ()
                            (add-hook 'kill-buffer-hook #'kb/pdf-cleanup-windows-h nil t)))
         (pdf-annot-list-mode . (lambda ()
                                  (hl-line-mode -1))))
  :custom
  (pdf-view-resize-factor 1.1)
  (pdf-view-display-size 'fit-page)
  (pdf-view-continuous nil) ; REVIEW 2024-01-16: Change this when I get to use image-roll?
  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick t)
  (pdf-annot-color-history              ; "Default" colors
   '("yellow" "SteelBlue1" "SeaGreen3" "LightSalmon1" "MediumPurple1"))
  (pdf-annot-list-format '((page . 3)
                           (color . 8)
                           (text . 68)
                           (type . 10)))
  (pdf-annot-list-highlight-type nil)
  :init
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
      (fit-window-to-buffer)))
  (advice-add 'pdf-annot-list-context-function
              :override 'kb/pdf-annot-list-context-function)

  ;; Taken from Doom
  (defun kb/pdf-cleanup-windows-h ()
    "Kill left-over annotation buffers when the document is killed."
    (when (buffer-live-p pdf-annot-list-document-buffer)
      (pdf-info-close pdf-annot-list-document-buffer))
    (when (buffer-live-p pdf-annot-list-buffer)
      (kill-buffer pdf-annot-list-buffer))
    (let ((contents-buffer (get-buffer "*Contents*")))
      (when (and contents-buffer (buffer-live-p contents-buffer))
        (kill-buffer contents-buffer))))

  (defun kb/org-noter-pdf--get-selected-text (mode)
    (when (and (eq mode 'pdf-view-mode)
               (pdf-view-active-region-p))
      (let* ((raw-text (mapconcat 'identity (pdf-view-active-region-text) ? ))
             (process-text-1 (replace-regexp-in-string "-\n" "" raw-text))
             (process-text-2 (replace-regexp-in-string "\n" " " process-text-1)))
        process-text-2)))
  (advice-add 'org-noter-pdf--get-selected-text
              :override #'kb/org-noter-pdf--get-selected-text)
  :config
  (setq tablist-context-window-display-action ; For context buffer
        '((display-buffer-reuse-window tablist-display-buffer-split-below-and-attach)
          (window-height . 0.25)
          (inhibit-same-window . t)
          (window-parameters (no-other-window . t)
                             (mode-line-format . none)))))

;;;; Custom entry formatter
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

;;;; Avy keys to highlight region in PDF
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
                                       'highlight
                                       (pdf-annot-read-color "Annotation color: "))))

  (general-define-key :keymaps 'pdf-view-mode-map
                      [remap avy-goto-char-timer] #'kb/avy-pdf-highlight))

;;;; Pdf-annot-list custom (tablist) color filter
(with-eval-after-load 'pdf-tools
  (defun kb/pdf-annot-list-filter-color-regexp ()
    "Regexp search the text after selecting a color.
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
                                     ")")))
             (regexp-string (read-string "Regexp to search: "))
             (regexp-filter (unless (string-empty-p regexp-string)
                              (format "Text =~ \"%s\"" regexp-string))))
        (tablist-push-filter
         (tablist-filter-parse (concat color-filter
                                       (when (and color-filter regexp-filter) " && ")
                                       regexp-filter))
         (called-interactively-p 'any)))))

  (general-define-key :keymaps 'pdf-annot-list-mode-map
                      "/ c" 'kb/pdf-annot-list-filter-color-regexp))

;;;; Saveplace-pdf-view
;; Save place in pdf-view buffers
(use-package saveplace-pdf-view
  :demand)

;;;; Org-noter
(use-package org-noter
  :elpaca (:protocol ssh
                     :fetcher github
                     :repo "org-noter/org-noter"
                     :files ("*.el" "modules" (:exclude "*-test-utils.el" "*-devel.el"))
                     :remotes ("remote" :repo "krisbalintona/org-noter"))
  :general
  (:keymaps 'pdf-misc-minor-mode-map
            "I" nil
            "M" 'pdf-misc-display-metadata)
  (:keymaps 'org-noter-doc-mode-map
            "i" 'org-noter-insert-precise-note
            "I" 'org-noter-insert-precise-note-toggle-no-questions
            "C-i" 'org-noter-insert-note
            "C-S-i" 'org-noter-insert-note-toggle-no-questions
            "C-M-i" nil
            "M-i" nil
            ;; FIXME 2024-01-13: Choose better keybind
            "H-\"" 'org-noter-pdf--create-missing-annotation)
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
  (org-noter-enable-update-renames))

;;; Zotxt
;; Integration between Emacs and Zotero
(use-package zotxt
  :custom
  (zotxt-default-bibliography-style "modern-language-association")
  (org-zotxt-link-description-style :citekey)
  :init
  (require 'org-zotxt-noter)
  (org-zotxt-mode)

  ;; Allow for file-level org-noter sessions
  (defun kb/org-zotxt-noter (arg)
    "Like `org-noter', but use Zotero.

If no document path property is found, will prompt for a Zotero
search to choose an attachment to annotate, then calls `org-noter'.

If a document path property is found, simply call `org-noter'.

See `org-noter' for details and ARG usage."
    (interactive "P")
    (require 'org-noter nil t)
    (unless (eq major-mode 'org-mode)
      (error "Org mode not running"))
    (unless (fboundp 'org-noter)
      (error "`org-noter' not installed"))
    ;; (if (org-before-first-heading-p)
    ;;     (error "`org-zotxt-noter' must be issued inside a heading"))
    (let* ((document-property (org-entry-get nil org-noter-property-doc-file (not (equal arg '(4)))))
           (document-path (when (stringp document-property) (expand-file-name document-property))))
      (if (and document-path (not (file-directory-p document-path)) (file-readable-p document-path))
          (call-interactively #'org-noter)
        (let ((arg arg))
          (deferred:$
           (zotxt-choose-deferred)
           (deferred:nextc it
                           (lambda (item-ids)
                             (zotxt-get-item-deferred (car item-ids) :paths)))
           (deferred:nextc it
                           (lambda (item)
                             (org-zotxt-get-item-link-text-deferred item)))
           (deferred:nextc it
                           (lambda (resp)
                             (let ((path (org-zotxt-choose-path (cdr (assq 'paths (plist-get resp :paths))))))
                               (org-entry-put nil org-zotxt-noter-zotero-link (org-zotxt-make-item-link resp))
                               (org-entry-put nil org-noter-property-doc-file path))
                             (call-interactively #'org-noter)))
           (deferred:error it #'zotxt--deferred-handle-error))))))
  (advice-add 'org-zotxt-noter :override #'kb/org-zotxt-noter))

;;; Org-remark
(use-package org-remark
  :after org
  :hook ((Info-mode eww-mode) . org-remark-mode)
  :general (:keymaps 'org-remark-mode-map
                     :prefix "C-c r"
                     "r" '(lambda () (interactive) (org-remark-highlights-load))
                     "m" 'org-remark-mark
                     "d" 'org-remark-delete
                     "c" 'org-remark-change
                     "t" 'org-remark-toggle
                     "o" 'org-remark-open
                     "v" 'org-remark-view
                     "n" 'org-remark-view-next
                     "p" 'org-remark-view-prev)
  :custom
  (org-remark-notes-auto-delete :auto-delete)
  (org-remark-source-file-name 'abbreviate-file-name)
  (org-remark-notes-file-name
   (no-littering-expand-var-file-name "org-remark/marginalia.org"))
  :config
  (diminish 'org-remark-mode)
  (org-remark-global-tracking-mode)
  (diminish 'org-remark-global-tracking-mode)

  (with-eval-after-load 'eww
    (org-remark-eww-mode 1))
  (with-eval-after-load 'nov
    (org-remark-nov-mode 1))
  (with-eval-after-load 'info
    (org-remark-info-mode 1)))

;;; Org-transclusion
;; Enable transclusion of org files
(use-package org-transclusion
  :after org-roam
  :hook (org-mode . org-transclusion-activate)
  :general
  (kb/toggle-keys
    "c" '(org-transclusion-mode :wk "Toggle mode")
    "R" '(org-transclusion-refresh :wk "Refresh")
    "m" '(org-transclusion-make-from-link :wk "Make")
    "a" '(org-transclusion-add :wk "Add")
    "r" '(org-transclusion-remove :wk "Remove")
    "s" '(org-transclusion-live-sync-start :wk "Edit start")
    "e" '(org-transclusion-live-sync-exit :wk "Edit exit")
    )
  :custom
  (org-transclusion-include-first-section t)
  (org-transclusion-exclude-elements '(property-drawer keyword)))

;;; Org-roam-ui
;; Newer `org-roam-server' for org-roam V2.
(use-package org-roam-ui
  :elpaca (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
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

;;; Delve
(use-package delve
  :disabled t                           ; Don't use
  :elpaca (delve :type git :host github :repo "publicimageltd/delve")
  :gfhook #'delve-compact-view-mode
  ;; FIXME 2022-05-27: `delve--zettel-preview' seems necessary to prevent cmacro
  ;; compiler error for `kb/delve--key--toggle-preview'.
  :commands delve delve--zettel-preview
  :general
  ("<f2>" 'delve)
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

;;; Lister
;; Interactive list library for `delve'
(use-package lister
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
  :init
  (require 'lister-mode) ; Require since this proves the "core" definitions for the functions below
  :config
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

;;; org-notes-other-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-notes-other-rcp)
