;;; org-blogging-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Everything necessary for creating static websites using org-mode.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Ox-hugo
;; Using the Hugo static cite generator as an option for exporting files
(use-package ox-hugo
  :defer 7
  :commands kb/org-hugo-org-roam-sync-all
  :ensure-system-package hugo
  :custom
  (org-hugo-base-dir (concat org-directory "hugo/"))
  (org-hugo-section "posts")
  (org-hugo-auto-set-lastmod nil)       ; Use lastmod?
  (org-hugo-suppress-lastmod-period 604800) ; Only use lastmod if modified at least a week later
  :init
  (defvar kb/org-hugo-exclude-tags '("blog" "project" "ATTACH")
    "Tags to exclude. Look at `kb/org-hugo--tag-processing-fn-ignore-tags-maybe'.")
  :config
  ;; Org-export all files in an org-roam subdirectory. Modified from
  ;; https://sidhartharya.me/exporting-org-roam-notes-to-hugo/
  (defun kb/org-hugo-org-roam-sync-all ()
    "Export all org-roam files to Hugo in my blogging directory."
    (interactive)
    (require 'org-roam)
    (org-roam-update-org-id-locations) ; Necessary for id's to be recognized for exports
    (dolist (fil (org-roam--list-files (expand-file-name (concat kb/roam-dir "blog/"))))
      (with-current-buffer (find-file-noselect fil)
        (org-hugo-export-wim-to-md)
        (unless (member (get-buffer (buffer-name)) (buffer-list)) ; Kill buffer unless it already exists
          (kill-buffer))
        )))

  (defun kb/org-hugo--tag-processing-fn-ignore-tags-maybe (tag-list info)
    "Ignore tags which match a string found in `kb/org-hugo-exclude-tags'."
    (cl-set-difference tag-list kb/org-hugo-exclude-tags :test #'equal))
  (add-to-list 'org-hugo-tag-processing-functions #'kb/org-hugo--tag-processing-fn-ignore-tags-maybe)
  )

;;; org-blogging-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-blogging-rcp)
