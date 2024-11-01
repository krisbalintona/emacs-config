;;; Doc-view
(use-package doc-view
  :custom
  (doc-view-resolution 192))

;;; Pdf-tools
;;;; Itself
;; View pdfs and interact with them. Has many dependencies
;; https://github.com/politza/pdf-tools#compiling-on-fedora
(use-package pdf-tools
  ;; FIXME 2024-01-13: This is a pull request fork that implements continuous
  ;; scrolling (`pdf-view-roll-minor-mode'). See
  ;; https://github.com/vedang/pdf-tools/pull/224
  ;; Must call `pdf-tools-install' or `pdf-loader-install' (which defers
  ;; loading; see its docstring and
  ;; https://github.com/vedang/pdf-tools?tab=readme-ov-file#installing-pdf-tools-elisp-code)
  ;; to have PDF files use pdf-view-mode and have everything required loaded
  :hook (on-first-buffer . pdf-loader-install)
  :bind ( :map pdf-view-mode-map
          ("C-c C-r a" . pdf-view-auto-slice-minor-mode)
          ;; Additionally useful since it lets you scroll via
          ;; `scroll-other-window'
          ([remap scroll-up-command] . pdf-view-scroll-up-or-next-page)
          ([remap scroll-down-command] . pdf-view-scroll-down-or-previous-page)))

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
  :init
  (advice-add 'pdf-view-bookmark-make-record :override #'krisb-pdf-view-bookmark-make-record)
  (advice-add 'pdf-view-bookmark-jump-handler :override #'krisb-pdf-view-bookmark-jump-handler)
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
  :ensure-system-package pdftk
  :after pdf-view
  :demand t
  :bind ( :map pdf-view-mode-map
          ("C-c m" . krisb-pdf-tools-metadata-modify)
          ("C-;" . krisb-avy-pdf-highlight)
          :map pdf-annot-list-mode-map
          ([remap tablist-push-regexp-filter] . krisb-pdf-annot-list-filter-regexp)))

;;;; Pdf-outline
(use-package pdf-outline
  :ensure nil
  :custom
  (pdf-outline-enable-imenu t)
  (pdf-outline-display-labels t)
  (pdf-outline-imenu-use-flat-menus nil))

;;;; Pdf-annot
(use-package pdf-annot
  :ensure nil
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

;;; Provide
(provide 'krisb-pdfs)
;;; krisb-pdfs.el ends here
