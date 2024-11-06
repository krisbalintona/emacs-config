;;; Org-roam
(use-package org-roam
  :custom
  (org-roam-directory (expand-file-name "org-roam" org-directory))
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n g" . org-roam-graph))
  :custom
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%dT%H%M%S>.org"
                         "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-node-display-template (concat (propertize "(${directories})  " 'face 'shadow)
                                          "${hierarchy:120} "
                                          (propertize "${tags:*}" 'face 'org-tag)))
  (org-roam-db-node-include-function
   (lambda () (not (member "ATTACH" (org-get-tags)))))
  :config
  (org-roam-db-autosync-mode 1)

  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (format "%s" (car (split-string dirs "/")))
      ""))

  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    (let ((level (org-roam-node-level node)))
      (concat
       (when (> level 0) (concat (org-roam-node-file-title node) " > "))
       (when (> level 1) (concat (string-join (org-roam-node-olp node) " > ") " > "))
       (org-roam-node-title node)))))

;;; Org-roam-ui
(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-open-on-start nil)
  :config
  (org-roam-ui-mode 1))

;;; Provide
(provide 'krisb-org-roam)
