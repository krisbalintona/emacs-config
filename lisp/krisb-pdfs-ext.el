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

;;; Save and restore PDF registers via bookmark handler
;; Drop-in extensions for `pdf-view-bookmark-make-record' and
;; `pdf-view-bookmark-jump-handler'.  Override those functions with the two
;; below.

;; NOTE 2024-10-31: When using desktop.el, do not add `pdf-view-register-alist'
;; to `desktop-locals-to-save': that causes an infinite loop.  Either choose
;; this solution (modifying the bookmark handler) or that one.  I opt for this
;; one since other packages utilize bookmarks (e.g. activities.el).

;;;###autoload
(defun krisb-pdf-view-bookmark-make-record  (&optional no-page no-slice no-size no-origin)
  "Create a bookmark PDF record.
The optional, boolean args exclude certain attributes.

My version of this function also saves the value of the
`pdf-view-register-alist' buffer local variable."
  (let ((displayed-p (eq (current-buffer)
                         (window-buffer))))
    (cons (buffer-name)
          (append (bookmark-make-record-default nil t 1)
                  `(,(cons 'registers (buffer-local-value 'pdf-view-register-alist (current-buffer)))
                    ,(cons 'midnight-p (buffer-local-value 'pdf-view-midnight-minor-mode (current-buffer)))
                    ,(unless no-page
                       (cons 'page (pdf-view-current-page)))
                    ,(unless no-slice
                       (cons 'slice (and displayed-p
                                         (pdf-view-current-slice))))
                    ,(unless no-size
                       (cons 'size pdf-view-display-size))
                    ,(unless no-origin
                       (cons 'origin
                             (and displayed-p
                                  (let ((edges (pdf-util-image-displayed-edges nil t)))
                                    (pdf-util-scale-pixel-to-relative
                                     (cons (car edges) (cadr edges)) nil t)))))
                    (handler . pdf-view-bookmark-jump-handler))))))

;;;###autoload
(defun krisb-pdf-view-bookmark-jump-handler (bmk)
  "The bookmark handler-function interface for bookmark BMK.
See also `pdf-view-bookmark-make-record'.

My version of this function also restores the value of the
`pdf-view-register-alist' buffer local variable."
  (let ((registers (bookmark-prop-get bmk 'registers))
        (midnight-p (bookmark-prop-get bmk 'midnight-p))
        (page (bookmark-prop-get bmk 'page))
        (slice (bookmark-prop-get bmk 'slice))
        (size (bookmark-prop-get bmk 'size))
        (origin (bookmark-prop-get bmk 'origin))
        (file (bookmark-prop-get bmk 'filename))
        (show-fn-sym (make-symbol "pdf-view-bookmark-after-jump-hook")))
    (fset show-fn-sym
          (lambda ()
            (remove-hook 'bookmark-after-jump-hook show-fn-sym)
            (unless (derived-mode-p 'pdf-view-mode)
              (pdf-view-mode))
            (with-selected-window
                (or (get-buffer-window (current-buffer) 0)
                    (selected-window))
              (if midnight-p
                  (pdf-view-midnight-minor-mode 1)
                (pdf-view-midnight-minor-mode -1))
              (when registers
                (setq-local pdf-view-register-alist registers))
              (when size
                (setq-local pdf-view-display-size size))
              (when slice
                (apply 'pdf-view-set-slice slice))
              (when (numberp page)
                (pdf-view-goto-page page))
              (when origin
                (let ((size (pdf-view-image-size t)))
                  (image-set-window-hscroll
                   (round (/ (* (car origin) (car size))
                             (frame-char-width))))
                  (image-set-window-vscroll
                   (round (/ (* (cdr origin) (cdr size))
                             (if pdf-view-have-image-mode-pixel-vscroll
                                 1
                               (frame-char-height))))))))))
    (add-hook 'bookmark-after-jump-hook show-fn-sym)
    (set-buffer (or (find-buffer-visiting file)
                    (find-file-noselect file)))))

;;; Provide
(provide 'krisb-pdfs-ext)
;;; krisb-pdfs-ext.el ends here
