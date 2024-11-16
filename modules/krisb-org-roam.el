;; -*- lexical-binding: t; -*-

;;; Org-roam
(use-package org-roam
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
     ("s" "source" plain "%?"
      :target (file+head "thoughts/%<%Y%m%dT%H%M%S>.org"
                         ":PROPERTIES:
:ROAM_TYPE: source
:ROAM_%^{Context or source?|SOURCE|CONTEXT}: %(org-roam-node-insert)
:END:
#+title: ${title}\n")
      :unnarrowed t)
     ("c" "collection" plain "%?"
      :target (file+head "thoughts/%<%Y%m%dT%H%M%S>.org"
                         ":PROPERTIES:
:ROAM_TYPE: collection
:END:

#+title: ${title}\n")
      :unnarrowed t)
     ("r" "reference" plain "%?"
      :target (file+head "references/%<%Y%m%dT%H%M%S>.org"
                         "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-db-node-include-function
   (lambda () (not (member "ATTACH" (org-get-tags)))))
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-mode-sections
   '(org-roam-backlinks-section
     org-roam-reflinks-section
     org-roam-unlinked-references-section))
  ;; The full content of each template element is present (i.e. searchable) even
  ;; if visually absent/truncated
  (org-roam-node-display-template
   (concat "${directories-display-template:8} "
           ;; FIXME 2024-11-16: For some reason using :* to automatically set
           ;; length produces too-wide a column
           (concat "${address-display-template:"
                   (number-to-string
                    (1+ (cl-loop for node in (org-roam-node-list)
                                 maximize (length (org-roam-node-address node)))))
                   "}")
           "${type-display-template}"
           "${person-display-template}"
           "${hierarchy}"
           (propertize " ${tags:60}" 'face 'org-tag)))
  :config
  (org-roam-db-autosync-mode 1)

  ;; See (info "(org-roam) org-roam-export")
  (with-eval-after-load 'ox-html
    (require 'org-roam-export))

  ;; Add ROAM_* properties to properties completing-read interface completions
  (dolist (prop '("ROAM_EXCLUDE"
                  "ROAM_PLACE"
                  "ROAM_PERSON"
                  "ROAM_SOURCE"
                  "ROAM_CONTEXT"
                  "ROAM_REFS"
                  "ROAM_TYPE"))
    (add-to-list 'org-default-properties prop))

  ;; Set inherited default values for some ROAM_* properties
  (add-to-list 'org-global-properties '("ROAM_TYPE" . "source collection pointer"))

  ;; Advise for archiving org-roam nodes
  (defun krisb-org-archive--compute-location-org-roam-format-string (orig-fun &rest args)
    "Take LOCATION in `org-archive--compute-location' and expand %R.
%R is expanded to the identifier for the org-roam node (at point) the archive
command is invoked in.

If there is no node at point, then expand to the file path instead."
    ;; Modify LOCATION before normal operations
    (cl-letf (((car args)
               (if (fboundp 'org-roam-node-at-point)
                   (replace-regexp-in-string "%R"
                                             (or (org-roam-node-id (org-roam-node-at-point 'assert))
                                                 (buffer-file-name (buffer-base-buffer)))
                                             (car args))
                 (car args))))
      (apply orig-fun args)))

  ;; Custom face for ID links to org-roam-nodes
  (krisb-modus-themes-setup-faces
   "org-roam-link"
   (org-link-set-parameters
    "id"
    :face (lambda (id)
            (if (org-roam-node-from-id id)
                `(:foreground ,keyword)
              'org-link))))

  ;; Custom stored description
  (org-link-set-parameters
   "id"
   :store (lambda (&optional interactive?)
            (if (and (equal major-mode 'org-mode)
                     ;; We want to check more than if there is a node at point;
                     ;; we want to make sure ID corresponds to an existing node
                     (org-roam-node-from-id (org-id-get)))
                (org-link-store-props :type "id"
                                      :link (concat "id:" (org-id-get))
                                      :description (org-roam-node-formatted (org-roam-node-at-point)))
              (funcall 'org-id-store-link-maybe interactive?)))))

;;; Org-roam-node
(use-package org-roam-node
  :ensure nil
  ;; I add this as a separate use-package in order to autoload these functions.
  ;; Autoloading them in org-roam's use-package creates a dependency loop
  :autoload (org-roam-node-from-id org-roam-node-file org-roam-node-list))

;;; Krisb-org-roam-ext
(use-package krisb-org-roam-ext
  :ensure nil
  :requires org-roam
  :custom
  ;; Customize how nodes are inserted via `org-roam-node-insert'
  (org-roam-node-formatter 'krisb-org-roam-node-formatter))

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
  :hook (org-roam-folgezettel-mode . hl-line-mode)
  :bind ( :map krisb-note-keymap
          ("m" . org-roam-folgezettel-list)))

;;; Provide
(provide 'krisb-org-roam)
