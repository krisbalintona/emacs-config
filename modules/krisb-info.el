;;;; Info
(use-package info
  :hook ((Info-selection . mixed-pitch-mode)
         (Info-selection . krisb-info-font-resize))
  :custom
  (Info-isearch-search nil)             ; Restore default isearch behavior
  :config
  (defun krisb-info-font-resize ()
    "Increase the font size of text in Info buffers."
    (face-remap-set-base 'default `(:height 1.2))))

;;;; Info-colors
;; Fontify useful parts of info buffers
(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node))

;;; Provide
(provide 'krisb-info)
