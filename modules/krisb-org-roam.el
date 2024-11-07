;;; Org-roam
(use-package org-roam
  :autoload org-roam-node-from-id
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n g" . org-roam-graph))
  :custom
  (org-roam-directory (expand-file-name "org-roam" org-directory))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%dT%H%M%S>.org"
                         "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-node-display-template
   (concat (propertize "/${directories:12} " 'face 'shadow)
           "${hierarchy:120} "
           (propertize "${tags:*}" 'face 'org-tag)))
  (org-roam-db-node-include-function
   (lambda () (not (member "ATTACH" (org-get-tags)))))
  :config
  (org-roam-db-autosync-mode 1)

  ;; TODO 2024-11-06: Can I change these method names to a krisb-* namespace?
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

;;; Citar-org-roam
(use-package citar-org-roam
  :after (citar org-roam)
  :bind ( :map krisb-note-keymap
          ("b r" . citar-org-roam-ref-add))
  :custom
  (citar-org-roam-subdir "references/")
  (citar-org-roam-capture-template-key "d")
  (citar-org-roam-template-fields
   '((:citar-title . ("title"))
     (:citar-author . ("author" "editor"))
     (:citar-date . ("date" "year" "issued"))
     (:citar-pages . ("pages"))
     (:citar-type . ("=type="))
     ;; Allow citar to pass the "key" field, which we can use in
     ;; `citar-org-roam-note-title-template' as "=key=", like ${=key=}
     (:citar-key . ("=key="))))
  (citar-org-roam-note-title-template "${=key=} ${citar-key}")
  :config
  (citar-org-roam-mode 1))

;;; Provide
(provide 'krisb-org-roam)
