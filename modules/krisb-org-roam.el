;;; Org-roam
(use-package org-roam
  :autoload (org-roam-node-from-id org-roam-node-file)
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
  (org-roam-db-gc-threshold most-positive-fixnum)
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
       (if (> level 1)
           (propertize (org-roam-node-title node) 'face 'org-roam-title)
         (org-roam-node-title node)))))

  ;; Add ROAM_* properties to properties completing-read interface completions
  (dolist (prop '("ROAM_EXCLUDE"
                  "ROAM_PLACE"
                  "ROAM_PERSON"
                  "ROAM_SOURCE"
                  "ROAM_CONTEXT"
                  "ROAM_REFS"
                  "ROAM_TYPE"))
    (add-to-list 'org-default-properties prop))

  ;; Advise for archiving org-roam nodes
  (defun krisb-org-archive--compute-location-org-roam-format-string (orig-fun &rest args)
    "Take LOCATION in `org-archive--compute-location' and expand %R.
%R is expanded to the identifier for the org-roam node (at point) the archive
command is invoked in.

If there is no node at point, then expand to the file path instead."
    ;; Modify LOCATION before normal operations
    (cl-letf (((car args)
               (if (fboundp 'org-roam-node-at-point)
                   (replace-regexp-in-string "%D"
                                             (or (org-roam-node-id (org-roam-node-at-point 'assert))
                                                 (buffer-file-name (buffer-base-buffer)))
                                             (car args))
                 (car args))))
      (apply orig-fun args))))

;;; Org-roam-ui
(use-package org-roam-ui
  :after org-roam
  :diminish (org-roam-ui-mode org-roam-ui-follow-mode)
  :hook (org-roam-ui-mode . krisb-org-roam-ui-update-theme)
  :custom
  (org-roam-ui-open-on-start nil)
  :config
  ;; Resolve conflict with desktop.el behavior.  See
  ;; https://github.com/org-roam/org-roam-ui/issues/202#issuecomment-1014711909
  (add-to-list 'desktop-minor-mode-table
               '(org-roam-ui-mode nil))
  (add-to-list 'desktop-minor-mode-table
               '(org-roam-ui-follow-mode nil))

  (defun krisb-org-roam-ui-update-theme (&optional _)
    "Update org-roam-ui theme if `org-roam-ui-sync-theme' is non-nil.
This function is added to `enable-theme-functions' and can also be
called outright."
    (when (and org-roam-ui-sync-theme org-roam-ui-mode)
      (call-interactively 'org-roam-ui-sync-theme)))

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
  :load-path "/home/krisbalintona/emacs-repos/packages/org-roam-folgezettel/"
  :bind ( :map krisb-note-keymap
          ("m" . org-roam-folgezettel-list)))

;;; Provide
(provide 'krisb-org-roam)
