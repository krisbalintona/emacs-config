;;; Org-roam
(use-package org-roam
  :autoload org-roam-node-from-id
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n ta" . org-roam-tag-add)
         ("C-c n tr" . org-roam-tag-remove)
         ("C-c n g" . org-roam-graph))
  :custom
  (org-roam-directory krisb-notes-directory)
  (org-roam-capture-templates
   '(("p" "plain" plain "%?"
      :target (file+head "%(let* ((subdirs
        (mapcar (lambda (dir) (file-relative-name dir org-roam-directory))
                (seq-filter #'file-directory-p
                            (directory-files org-roam-directory t \"^[^.]\" t))))
       (subdir (expand-file-name
                (completing-read \"Subdirectory: \" subdirs)
                org-roam-directory)))
  (expand-file-name (concat (format-time-string \"%Y%m%dT%H%M%S\") \".org\")
                    subdir))"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("r" "reference" plain "%?"
      :target (file+head
               "references/%<%Y%m%dT%H%M%S>.org"
               "#+title: ${title}\n")
      :unnarrowed t)))
  ;; NOTE 2024-11-07: The full content of each template element is present
  ;; (i.e. searchable) even if visually absent/truncated
  (org-roam-node-display-template
   (concat (propertize "${directories:12} " 'face 'shadow)
           "${hierarchy:120} "
           (propertize "${tags:60}" 'face 'org-tag)))
  (org-roam-node-annotation-function
   (lambda (node)
     "Show modification time annotation.
Taken from
https://github.com/org-roam/org-roam/wiki/User-contributed-Tricks#modification-time-annotation-in-org-roam-node-find-minad."
     (marginalia--time (org-roam-node-file-mtime node))))
  (org-roam-db-node-include-function
   (lambda () (not (member "ATTACH" (org-get-tags)))))
  :config
  (org-roam-db-autosync-mode 1)

  ;; TODO 2024-11-06: Can I change these method names to a krisb-* namespace?
  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (format "/%s" (car (split-string dirs "/")))
      ""))

  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    (let ((level (org-roam-node-level node)))
      (concat
       (when (> level 0)
         (concat (org-roam-node-file-title node) " > "))
       (when (> level 1)
         (concat (string-join (org-roam-node-olp node) " > ")
                 " > "))
       (org-roam-node-title node)))))

;;; Org-roam-ui
(use-package org-roam-ui
  :after org-roam
  :diminish (org-roam-ui-mode org-roam-ui-follow-mode)
  :custom
  (org-roam-ui-open-on-start nil)
  :config
  (defun krisb-org-roam-ui-update-theme (&optional _)
    "Update org-roam-ui theme if `org-roam-ui-sync-theme' is non-nil.
This function is added to `enable-theme-functions' and can also be
called outright."
    (when (and org-roam-ui-sync-theme org-roam-ui-mode)
      (call-interactively 'org-roam-ui-sync-theme)))
  (krisb-org-roam-ui-update-theme)

  ;; Update graph theme on theme change
  (add-hook 'enable-theme-functions #'krisb-org-roam-ui-update-theme))

;;; Citar-org-roam
(use-package citar-org-roam
  :after (citar org-roam)
  :diminish citar-org-roam-mode
  :bind ( :map krisb-note-keymap
          ("b r" . citar-org-roam-ref-add)
          ("b o" . citar-org-roam-open-current-refs))
  :custom
  (citar-org-roam-subdir "references/")
  (citar-org-roam-capture-template-key "r")
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

;;; Org-roam-folgezettel
(use-package org-roam-folgezettel
  :vc ( :url "git@github.com:krisbalintona/org-roam-folgezettel.git"
        :rev :newest)
  :bind ( :map krisb-note-keymap
          ("m" . org-roam-folgezettel-list)))

;;; Provide
(provide 'krisb-org-roam)
