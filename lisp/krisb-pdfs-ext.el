;;; krisb-pdfs-ext.el --- pdf-view-mode extensions   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords: multimedia, lisp

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

;; Extensions for pdf-view-mode and other PDF-related workflows.

;;; Code:
(require 'cl-macs)
(require 'pdf-annot)
(require 'pdf-links)
(require 'avy)

;;; Custom entry formatter
(declare-function org-noter-pdf--edges-to-region "org-noter")
(defun krisb-pdf-annot--make-entry-formatter (a)
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
               (propertize type 'face
                           `(:background ,color
                                         :foreground ,(funcall contrasty-color color)))
             type)))
        (color
         (let* ((color (pdf-annot-get a 'color)))
           (propertize color 'face
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
(advice-add 'pdf-annot--make-entry-formatter :override 'krisb-pdf-annot--make-entry-formatter)

;;; Avy keys to highlight region in PDF
;; Use an avy-like interface to highlight region in pdf-view-mode. Heavily based
;; off of
;; https://github.com/dalanicolai/dala-emacs-lisp/blob/master/pdf-avy-highlight.el
;; with modifications
(defcustom krisb-avy-pdf-links-convert-pointsize-scale 0.02
  "The scale factor for the -pointsize convert command.

This determines the relative size of the font, when interactively
reading links."
  :group 'pdf-links
  :type '(restricted-sexp :match-alternatives
                          ((lambda (x) (and (numberp x)
                                            (<= x 1)
                                            (>= x 0))))))

(defun krisb-avy-pdf-links-read-char-action (query prompt)
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
                                    krisb-avy-pdf-links-convert-pointsize-scale)))))
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

(defun krisb-avy-pdf-timed-input ()
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

(defun krisb-avy-pdf-get-coordinates (end)
  "Prompt for PDF coordinates using avy-like interface."
  (let* ((query (krisb-avy-pdf-timed-input))
         (coords
          (list (or (krisb-avy-pdf-links-read-char-action query
                                                          (format "Please specify %s (SPC scrolls): "
                                                                  (if end "region end" "region beginning")))
                    (error "No char selected")))))
    (car (alist-get 'edges (car coords)))))

;;;###autoload
(defun krisb-avy-pdf-highlight (&optional activate)
  "Use an avy-like interface to highlight region in PDF.

If called with ACTIVATE, then also activate the created
annotation immediately after creation."
  (interactive "P")
  (let* ((start (krisb-avy-pdf-get-coordinates nil))
         (end (krisb-avy-pdf-get-coordinates :end))
         (edges (append (cl-subseq start 0 2) (cl-subseq end 2 4)))
         (pdf-annot-activate-created-annotations activate))
    (pdf-annot-add-markup-annotation edges
                                     (let ((type (completing-read "Markup type (default highlight): "
                                                                  '("squiggly" "highlight" "underline" "strike-out")
                                                                  nil t)))
                                       (if (equal type "") 'highlight (intern type)))
                                     (pdf-annot-read-color "Annotation color: "))))

;;; Pdf-annot-list custom (tablist) color filter
;;;###autoload
(defun krisb-pdf-annot-list-filter-color-regexp ()
  "Get a prompt to filter for the color column's colors.
The offered colors are those already present in the document's
highlights."
  (interactive)
  (unless (derived-mode-p 'tabulated-list-mode)
    (error "Buffer is not in Tabulated List Mode"))
  (let ((unique-colors
         ;; Get unique colors directly from PDF
         (seq-filter
          #'identity
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

;;;###autoload
(defun krisb-pdf-annot-list-filter-regexp ()
  "Drop-in replacement for `tablist-push-regexp-filter'.
Calls `krisb-pdf-annot-list-filter-color-regexp' when point is in
the color column."
  (interactive)
  (if (string= "Color" (tablist-read-column-name nil))
      (krisb-pdf-annot-list-filter-color-regexp)
    (call-interactively 'tablist-push-regexp-filter)))

;;; Custom org-link type for PDF annotations
;; Code copied from the code shared on Thu, 08 Feb 2024 22:13:50 +0000 by Juan
;; Manuel Macías <maciaschain@posteo.net> in the Emacs-devel mailing list. The
;; original uses the modification date, whereas this verion uses the annotation
;; ID, which should be unique even upon deletions and additions of annotations.
;; This is because the annotation IDs are robust (don't change upon modifying
;; content of annotation). Additionally, the original version relies on an
;; in-buffer `re-search-forward', meaning it only worked if you had the current
;; modified date as a column in `pdf-annot-list-format'; this is avoided if we
;; use ID's like `pdf-annot-list-display-annotation-from-id' likes.
(defun krisb-pdf-annot-org-store-link ()
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

(defun krisb-pdf-annot-org-follow-link (path)
  "Open pdf-tools link."
  (let ((id (if (string-match "::\\(.+\\)" path)
                (match-string 1 path)
              (error "[krisb-pdf-annot-org-follow-link] Not a valid id!")))
        (file-path (replace-regexp-in-string "::.+" "" path)))
    (find-file file-path)
    (pdf-annot-list-annotations)
    (with-current-buffer (format "*%s's annots*" (file-name-sans-extension (buffer-name)))
      (goto-char (point-min))
      (forward-line (save-excursion
                      (goto-char (point-min))
                      ;; Find line whose tabulated-list-id corresponds to
                      ;; annotation ID
                      (let ((row-id (get-text-property (point) 'tabulated-list-id)))
                        (while (not (or (eq row-id (intern id))
                                        (= (line-number-at-pos (point)) (line-number-at-pos (point-max)))))
                          (forward-line)
                          (setq row-id (get-text-property (point) 'tabulated-list-id))))
                      (1- (line-number-at-pos))))
      (pdf-annot-list-display-annotation-from-id id))))

(with-eval-after-load 'ol
  (org-link-set-parameters
   "pdf-annot"
   :follow #'krisb-pdf-annot-org-follow-link
   :store #'krisb-pdf-annot-org-store-link))

;;; Modify PDF metadata
;; Emacs wrapper and convenience functions for changing package metadata using
;; `pdftk'.  See https://unix.stackexchange.com/a/72457 for more information on
;; the CLI commands involved.
;;;###autoload
(defun krisb-pdf-tools-metadata-bookmark-section ()
  "Insert bookmark metadata section."
  (interactive)
  (save-excursion
    (insert "\nBookmarkBegin\nBookmarkTitle: \nBookmarkLevel: 1\nBookmarkPageNumber: "))
  (move-end-of-line 2))

(defun krisb-pdf-tools-metadata-label-section ()
  "Insert bookmark metadata section."
  (interactive)
  (let* ((possible-styles
          '("DecimalArabicNumerals"
            "LowercaseRomanNumerals"
            "UppercaseRomanNumerals"
            "UppercaseLetters"
            "LowercaseLetters"
            "NoNumber"))
         (style
          (completing-read "Label style: " possible-styles nil t)))
    (save-excursion
      (insert "\n"
              "PageLabelBegin\n"
              "PageLabelNewIndex: 1\n"
              "PageLabelStart: 1\n"
              "PageLabelNumStyle: " style))))

(defvar-keymap krisb-pdf-tools-metadata-mode-map
  :doc "Mode map for `krisb-pdf-tools-metadata-mode'."
  "C-c C-b" #'krisb-pdf-tools-metadata-bookmark-section
  "C-c C-l" #'krisb-pdf-tools-metadata-label-section)

(define-derived-mode krisb-pdf-tools-metadata-mode fundamental-mode "Metadata"
  "Major mode for altering and viewing PDF metadata."
  :interactive t
  (use-local-map krisb-pdf-tools-metadata-mode-map))

;;;###autoload
(defun krisb-pdf-tools-metadata-modify (pdf-file)
  "Modify PDF-FILE metadata."
  (interactive (list (buffer-file-name)))
  (unless (string= "pdf" (file-name-extension pdf-file))
    (user-error "File is not a PDF!"))
  (unless (executable-find "pdftk")
    (error "System executable `pdftk' not found. Please install executable on filesystem to proceed"))
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
      (krisb-pdf-tools-metadata-mode))
    (pop-to-buffer buf-name)
    (define-key krisb-pdf-tools-metadata-mode-map (kbd "C-c C-c") commit-func)
    (set-buffer-modified-p nil)
    (message (substitute-command-keys "Press `C-c C-c' when finished editing PDF metadata. To see keybinds, press \\[describe-mode]"))))

;;; Provide
(provide 'krisb-pdfs-ext)
;;; krisb-pdfs-ext.el ends here
