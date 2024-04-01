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

;;;; PDFs
;;;;; Pdf-tools
;; View pdfs and interact with them. Has many dependencies
;; https://github.com/politza/pdf-tools#compiling-on-fedora
(use-package pdf-tools
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
  :hook ((after-init . pdf-tools-install)
         ;; FIXME 2024-01-13: Uncomment this once the above issues between the
         ;; official release and pending pull request are resolved.
         ;; (pdf-view-mode . pdf-view-roll-minor-mode)
         (pdf-view-mode . (lambda ()
                            (add-hook 'kill-buffer-hook #'kb/pdf-cleanup-windows-h nil t)))
         (pdf-view-mode . (lambda ()         ; Invert mouse scrolling
                            (if (boundp 'mwheel-scroll-up-function)
                                (setq-local mwheel-scroll-up-function
                                            #'pdf-view-scroll-down-or-previous-page))
                            (if (boundp 'mwheel-scroll-down-function)
                                (setq-local mwheel-scroll-down-function
                                            #'pdf-view-scroll-up-or-next-page))))
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
      (fit-window-to-buffer)
      (visual-line-mode)))
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

  (general-define-key :keymaps 'pdf-view-mode-map
                      [remap avy-goto-char-timer] #'kb/avy-pdf-highlight))

;;;;;; Pdf-annot-list custom (tablist) color filter
(with-eval-after-load 'pdf-tools
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

  (general-define-key :keymaps 'pdf-annot-list-mode-map
                      [remap tablist-push-regexp-filter] 'kb/pdf-annot-list-filter-regexp))

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
    (when (equal (format "%s" major-mode) "pdf-annot-list-mode")
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

  (defun kb/pdf-tools--metadata-modify (pdf-file)
    "Modify PDF-FILE metadata."
    (interactive (list (buffer-file-name)))
    (unless (string= "pdf" (file-name-extension pdf-file))
      (error "File is not a PDF!"))
    (let* ((pdf-name (file-name-sans-extension (file-name-nondirectory pdf-file)))
           (buf-name (concat "*pdf-tools metadata: " pdf-name))
           (metadata-file (concat "/tmp/pdf-tools-metadata--" pdf-name))
           (temp-pdf (make-temp-file "/tmp/pdf-tools-metadata--"))
           (metadata-dump-command (concat "pdftk '" pdf-file "' dump_data"))
           (metadata-update-command
            (concat "pdftk '" pdf-file "' update_info '" metadata-file "' output '" temp-pdf "'"))
           (keymap (make-sparse-keymap))
           (commit-func (lambda ()
                          "Commit the changes to PDF metadata."
                          (interactive)
                          (with-current-buffer buf-name
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
        (goto-char (point-min)))
      (pop-to-buffer buf-name)
      (define-key keymap (kbd "C-c C-c") commit-func)
      (use-local-map keymap)
      (set-buffer-modified-p nil)
      (message "Press `C-c C-c' when finished editing package metadata")))
  (define-key pdf-view-mode-map (kbd "C-c m") #'kb/pdf-tools--metadata-modify))

;;;;; Saveplace-pdf-view
;; Save place in pdf-view buffers
(use-package saveplace-pdf-view
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
    "Call `mpv-start' from the org-attach directory."
    (interactive)
    (let* ((dir (if (org-attach-dir)
                    (file-name-as-directory (org-attach-dir))
                  default-directory))
           (file-name (expand-file-name (read-file-name "Select video: " dir))))
      (mpv-start file-name)))

  (defun kb/mpv-jump-to-playback-position (time)
    "Prompt user for a time to jump to.
Takes HH:MM:SS time format. Uses `org-timer-hms-to-secs' to parse user
input."
    (interactive "MJump to time (HH:MM:SS format): ")
    (org-timer-hms-to-secs time))

  ;; Keymap
  (defvar-keymap kb/mpv-org-map
    :doc "Keymap for my mpv.el commands for use in `org-mode'.
Commands that control MPV playback mimic MPV keybinds."
    :repeat (mpv-pause mpv-seek-backward)
    "o" #'kb/mpv-play
    "O" #'mpv-play-url
    "i" #'mpv-insert-playback-position
    "p" #'mpv-pause
    "b" #'mpv-seek-backward
    "f" #'mpv-seek-forward
    "g" #'kb/mpv-jump-to-playback-position
    "9" #'mpv-volume-decrease
    "0" #'mpv-volume-increase
    "[" #'mpv-speed-decrease
    "]" #'mpv-speed-increase)
  (define-key global-map (kbd "H-m") kb/mpv-org-map)

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
  :init
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

;;;; Zotxt
;; Integration between Emacs and Zotero
(use-package zotxt
  :demand
  :custom
  (zotxt-default-bibliography-style "modern-language-association")
  (org-zotxt-link-description-style :citekey)
  :config
  ;; FIXME 2024-03-05: Don't know how to deal with this using package.el
  ;; (require 'org-zotxt-noter)
  (org-zotxt-mode 1)

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

;;;; Org-remark
(use-package org-remark
  :demand
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
                     "n" 'org-remark-next
                     "p" 'org-remark-prev)
  :custom
  (org-remark-notes-auto-delete :auto-delete)
  (org-remark-source-file-name 'abbreviate-file-name)
  (org-remark-notes-file-name
   (no-littering-expand-var-file-name "org-remark/marginalia.org"))
  (org-remark-create-default-pen-set nil) ; Make my own pens
  :config
  (diminish 'org-remark-mode)
  ;; (org-remark-global-tracking-mode 1)
  (diminish 'org-remark-global-tracking-mode)

  (with-eval-after-load 'eww
    (org-remark-eww-mode 1))
  (with-eval-after-load 'nov
    (org-remark-nov-mode 1))
  (with-eval-after-load 'info
    (org-remark-info-mode 1))

  ;; My pens
  (org-remark-create "red"
                     `(:background "PaleVioletRed4")
                     `(CATEGORY "amazing" help-echo "Wow!"))
  (org-remark-create "red-line"
                     `(:underline '(:color "PaleVioletRed4" :style wave))
                     `(CATEGORY "surprise" help-echo "Wow!"))
  (org-remark-create "yellow"
                     `(:background "yellow3")
                     `(CATEGORY "important"))
  (org-remark-create "green"
                     `(:underline (:color "lawn green" :style wave))
                     `(CATEGORY "outline")))

;;;; Org-transclusion
;; Enable transclusion of org files
(use-package org-transclusion
  :hook (org-mode . org-transclusion-mode)
  :custom
  (org-transclusion-include-first-section t)
  (org-transclusion-exclude-elements '(property-drawer)))

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
  :gfhook #'delve-compact-view-mode
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

(provide 'org-notes-other-rcp)
;;; org-notes-other-rcp.el ends here
