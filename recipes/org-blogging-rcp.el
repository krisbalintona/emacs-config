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
  :after (ox org-roam)
  :ensure-system-package hugo
  :custom
  (org-hugo-base-dir (concat org-directory "hugo/"))
  (org-hugo-section "posts")
  (org-hugo-auto-set-lastmod nil)       ; Use lastmod?
  (org-hugo-suppress-lastmod-period 604800) ; Only use lasfmod if modified at least a week later
  :config
  ;; NOTE 2021-09-12: To ensure that anchor links directly to the headline are
  ;; functional, we need to patch `ox-html` to respect the ID property as the
  ;; anchor tag. From
  ;; https://github.com/org-roam/org-roam/wiki/Hitchhiker's-Rough-Guide-to-Org-roam-V2#export
  (defun kb/org-html--reference (datum info &optional named-only)
    "Return an appropriate reference for DATUM.

DATUM is an element or a `target' type object.  INFO is the
current export state, as a plist.

When NAMED-ONLY is non-nil and DATUM has no NAME keyword, return
nil.  This doesn't apply to headlines, inline tasks, radio
targets and targets."
    (let* ((type (org-element-type datum))
           (user-label
            (org-element-property
             (pcase type
               ((or `headline `inlinetask) :CUSTOM_ID)
               ((or `radio-target `target) :value)
               (_ :name))
             datum))
           (user-label (or user-label
                           (when-let ((path (org-element-property :ID datum)))
                             (concat "ID-" path)))))
      (cond
       ((and user-label
             (or (plist-get info :html-prefer-user-labels)
                 ;; Used CUSTOM_ID property unconditionally.
                 (memq type '(headline inlinetask))))
        user-label)
       ((and named-only
             (not (memq type '(headline inlinetask radio-target target)))
             (not user-label))
        nil)
       (t
        (org-export-get-reference datum info)))))
  (advice-add 'org-html--reference :override #'kb/org-html--reference)

  ;; Org-export all files in an org-roam subdirectory. Modified from
  ;; https://sidhartharya.me/exporting-org-roam-notes-to-hugo/
  (defun kb/org-hugo-org-roam-sync-all()
    "Export all org-roam files to Hugo in my blogging directory."
    (interactive)
    (org-roam-update-org-id-locations) ; Necessary for id's to be recognized for exports
    (let* ((buffers (buffer-list)))
      (dolist (fil (org-roam--list-files (expand-file-name (concat org-roam-directory "blog"))))
        (with-current-buffer (find-file-noselect fil)
          (org-hugo-export-wim-to-md)
          (unless (member (get-buffer (buffer-name)) buffers) ; Kill buffer unless it already exists
            (kill-buffer)
            )
          ))
      ))
  )

;;; org-blogging-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-blogging-rcp)
