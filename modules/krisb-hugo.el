;;; Ox-hugo
;; Using the Hugo static cite generator as an option for exporting files
(use-package ox-hugo
  :after org
  :ensure-system-package (hugo go)
  :custom
  (org-hugo-base-dir (expand-file-name "hugo/" org-directory))
  (org-hugo-section "posts")

  (org-hugo-use-code-for-kbd t)
  (org-hugo-link-desc-insert-type nil) ; TODO 2024-11-01: Check if I want to change this value
  (org-hugo-export-with-toc nil)    ; Default to nil and set on a per-post basis

  (org-hugo-auto-set-lastmod t)
  (org-hugo-suppress-lastmod-period 604800)) ; Only use lastmod if modified at least a week later

;;; Krisb-ox-hugo-ext
(use-package krisb-ox-hugo-ext
  :ensure nil
  :after ox-hugo
  :config
  ;; `nil' if you don't want to export to the static directory.  This is
  ;; desirable if, for instance, you leverage page bundles for each post,
  ;; causing resources to be contained within each bundle.
  (when krisb-org-hugo-bundle-workflow
    (setopt org-hugo-default-static-subdirectory-for-externals nil)))

;;; Provide
(provide 'krisb-hugo)
