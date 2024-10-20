;;;; Info
(use-package info
  :hook (Info-selection . mixed-pitch)
  :custom
  (Info-isearch-search nil))            ; Restore default isearch behavior

;;;; Info-colors
;; Fontify useful parts of info buffers
(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node))

;;; Provide
(provide 'krisb-info)
