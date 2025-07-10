;; -*- lexical-binding: t; -*-

;;; Doc-view
(use-package doc-view
  :custom
  (doc-view-resolution 192))

;;; Pdf-tools

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

;;; Provide
(provide 'krisb-pdfs)
;;; krisb-pdfs.el ends here
