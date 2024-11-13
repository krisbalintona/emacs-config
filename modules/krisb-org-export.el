;;; Ox (org-export)
(use-package ox
  :ensure nil
  :custom
  (org-export-coding-system 'utf-8)
  (org-export-with-tags t)
  (org-export-with-smart-quotes t)
  (org-export-with-sub-superscripts '{}) ; Requires brackets to recognize superscripts and subscripts
  (org-export-with-section-numbers nil)
  (org-time-stamp-formats               ; Format of time stamps in the file
   '("%Y-%m-%d %a" . "%Y-%m-%d %a %H:%M"))
  (org-display-custom-times t)          ; Export with custom time stamps?
  (org-time-stamp-custom-formats        ; Format of exported time stamps
   '("%a, %b %-d" . "%a, %b %-d (%-H:%M%p)"))

  (org-image-actual-width 700)          ; Image widths on export

  ;; Asynchronous
  (org-export-in-background nil)        ; Default?
  (org-export-async-debug t)
  (org-export-async-init-file (locate-library "quickstart")) ; TODO 2024-10-19: Make a "quickstart" init.el
  :config
  ;; Taken from
  ;; https://endlessparentheses.com/better-time-stamps-in-org-export.html
  (defun krisb-org-export-filter-timestamp-reformat (timestamp backend info)
    "Remove <> or [] surrounding time-stamps when exporting HTML and LaTeX."
    (cond
     ((org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string "[<>]\\|[][]" "" timestamp))
     ((org-export-derived-backend-p backend 'html)
      (replace-regexp-in-string "&[lg]t;\\|[][]" "" timestamp))))
  (add-to-list 'org-export-filter-timestamp-functions #'krisb-org-export-filter-timestamp-reformat))


;;; Ox-odt
(use-package ox-odt
  :ensure nil
  :custom
  (org-odt-preferred-output-format "docx")) ; Convert to .docx at the end of conversion

;;; Provide
(provide 'krisb-org-export)
