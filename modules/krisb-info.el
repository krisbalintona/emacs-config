;;;; Info
(use-package info
  :custom
  (Info-isearch-search nil))            ; Restore default isearch behavior

;;;; Info-variable-pitch
;; Mixed pitch in Info pages
(use-package info-variable-pitch
  ;; :ensure (info-variable-pitch :type git :host github :repo "kisaragi-hiu/info-variable-pitch")
  :vc (:url "https://github.com/kisaragi-hiu/info-variable-pitch.git")
  :hook (Info-selection . info-variable-pitch-mode))

;;;; Info-colors
;; Fontify useful parts of info buffers
(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node))


;;; Provide
(provide 'krisb-info)
