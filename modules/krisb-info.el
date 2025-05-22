;; -*- lexical-binding: t; -*-

;;;; Info-colors
;; Fontify useful parts of info buffers
(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node))

;;; Provide
(provide 'krisb-info)
