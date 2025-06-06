;; -*- lexical-binding: t; -*-

;;; Doc-view
(use-package doc-view
  :custom
  (doc-view-resolution 192))

;;; Pdf-tools
;;;; Itself
;; View pdfs and interact with them. Has many dependencies
;; https://github.com/politza/pdf-tools#compiling-on-fedora
(use-package pdf-tools
  ;; 2025-04-03: We manually add an auto-mode-alist entry to lazy-load this
  ;; package.  I want to avoid calling `pdf-loader-install' or
  ;; `pdf-tools-install' immediately at startup because I've had startup
  ;; complications when the pdf-tools install is malformed.  So I lazy load this
  ;; package and only call those functions after this package loads (i.e., in
  ;; the :config block).
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :bind ( :map pdf-view-mode-map
          ("C-c C-r a" . pdf-view-auto-slice-minor-mode)
          ;; Additionally useful since it lets you scroll via
          ;; `scroll-other-window'
          ([remap scroll-up-command] . pdf-view-scroll-up-or-next-page)
          ([remap scroll-down-command] . pdf-view-scroll-down-or-previous-page))
  :config
  ;; Must call `pdf-tools-install' or `pdf-loader-install' to have PDF files use
  ;; pdf-view-mode and have everything required loaded.  The latter defers
  ;; loading; see its docstring and
  ;; https://github.com/vedang/pdf-tools?tab=readme-ov-file#installing-pdf-tools-elisp-code
  (pdf-loader-install))

;;;; Pdf-view
(use-package pdf-view
  :ensure nil
  :autoload krisb-pdf-view-cleanup-windows-h
  :hook (pdf-view-mode . (lambda () (add-hook 'kill-buffer-hook #'krisb-pdf-view-cleanup-windows-h nil t)))
  :custom
  (pdf-view-resize-factor 1.1)
  (pdf-view-display-size 'fit-page)
  (pdf-view-continuous nil) ; REVIEW 2024-01-16: Change this when I get to use image-roll?
  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick t)
  :config
  ;; Taken from Doom
  (defun krisb-pdf-view-cleanup-windows-h ()
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

;;;; Krisb-pdfs-ext
;; Emacs wrapper and convenience functions for changing package metadata using
;; `pdftk'. See https://unix.stackexchange.com/a/72457 for more information on
;; the CLI commands involved.
(use-package krisb-pdfs-ext
  :ensure nil
  :after pdf-view
  :demand t
  :bind ( :map pdf-view-mode-map
          ([remap avy-goto-char-timer] . krisb-avy-pdf-highlight-region-by-char)
          :map pdf-annot-list-mode-map
          ([remap tablist-push-regexp-filter] . krisb-pdf-annot-list-filter-regexp)))

;;;; Pdf-outline
(use-package pdf-outline
  :ensure nil
  :after pdf-tools
  :custom
  (pdf-outline-enable-imenu t)
  (pdf-outline-display-labels t)
  (pdf-outline-imenu-use-flat-menus nil))

;;;; Pdf-annot
(use-package pdf-annot
  :ensure nil
  :after pdf-view
  :demand t
  :hook ((pdf-annot-list-mode . (lambda () (hl-line-mode -1)))
         (pdf-annot-list-mode . krisb-pdf-annot--setup-context-window-display-action))
  :custom
  (pdf-annot-color-history ; "Default" color list. Appears at the top of annotation color change commands
   '("yellow" "SteelBlue1" "SeaGreen3" "LightSalmon1" "MediumPurple1"))
  (pdf-annot-list-format '((page . 3)
                           (color . 8)
                           (text . 68)
                           (type . 10)))
  (pdf-annot-list-highlight-type nil)
  :config
  ;; Fit the "contents" window to buffer height
  (defun krisb-pdf-annot-list-context-function (id buffer)
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
  (advice-add 'pdf-annot-list-context-function :override #'krisb-pdf-annot-list-context-function)

  (defun krisb-pdf-annot--setup-context-window-display-action ()
    "Set the display action for the \"context buffer\".
The context buffer is the buffer that shows annotation contents in
`pdf-annot-mode'"
    (setq-local tablist-context-window-display-action
                '((display-buffer-reuse-window tablist-display-buffer-split-below-and-attach)
                  (window-height . 0.25)
                  (inhibit-same-window . t)
                  (window-parameters (no-other-window . t)
                                     (mode-line-format . none))))))

;;; Saveplace-pdf-view
;; Save place in pdf-view buffers
(use-package saveplace-pdf-view
  :after saveplace)

;;; Pdf-meta-edit
(use-package pdf-meta-edit
  :load-path "/home/krisbalintona/emacs-repos/packages/pdf-meta-edit/"
  :bind ( :map pdf-view-mode-map
          ("C-c M" . pdf-meta-edit-modify)))

;;; Provide
(provide 'krisb-pdfs)
;;; krisb-pdfs.el ends here
