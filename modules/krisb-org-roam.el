;; -*- lexical-binding: t; -*-

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
  ;; NOTE 2024-11-07: The full content of each template element is present
  ;; (i.e. searchable) even if visually absent/truncated
  (org-roam-node-display-template
   (concat (propertize "${directories:8} " 'face 'shadow)
           "${index-numbering:"
           (number-to-string
            (1+ (cl-loop for node in (org-roam-node-list)
                         maximize (length (cdr (assoc "ROAM_PLACE" (org-roam-node-properties node) #'string-equal))))))
           "}"
           "${type}"
           "${person}"
           "${hierarchy}"
           (propertize " ${tags:60}" 'face 'org-tag)))
  (org-roam-db-node-include-function
   (lambda () (not (member "ATTACH" (org-get-tags)))))
  (org-roam-db-gc-threshold most-positive-fixnum)
  :config
  (org-roam-db-autosync-mode 1)

  ;; See (info "(org-roam) org-roam-export")
  (with-eval-after-load 'ox-html
    (require 'org-roam-export))

  ;; TODO 2024-11-06: Can I change these method names to a krisb-* namespace?
  ;; Bespoke org-roam-node accessors
  (cl-defmethod org-roam-node-index-numbering ((node org-roam-node))
    (let ((index-number (cdr (assoc "ROAM_PLACE" (org-roam-node-properties node) #'string-equal))))
      (when (and index-number (not (string-empty-p index-number)))
        (propertize (string-trim (format "%s" index-number)) 'face 'shadow))))

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

  (cl-defmethod org-roam-node-type ((node org-roam-node))
    (let ((index-number (cdr (assoc "ROAM_TYPE" (org-roam-node-properties node) #'string-equal))))
      (when (and index-number (not (string-empty-p index-number)))
        (propertize (format "&%s " index-number) 'face 'font-lock-doc-face))))

  (cl-defmethod org-roam-node-person ((node org-roam-node))
    (let ((person (cdr (assoc "ROAM_PERSON" (org-roam-node-properties node) #'string-equal))))
      (when (and person (not (string-empty-p person)))
        (propertize (format "@%s " person) 'face 'font-lock-keyword-face))))

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
                   (replace-regexp-in-string "%D"
                                             (or (org-roam-node-id (org-roam-node-at-point 'assert))
                                                 (buffer-file-name (buffer-base-buffer)))
                                             (car args))
                 (car args))))
      (apply orig-fun args)))

  ;; Customize ID type org links
  (krisb-modus-themes-setup-faces
   "org-roam-link"
   (org-link-set-parameters
    "id"
    ;; Custom face for ID links to org-roam-nodes
    :face (lambda (id)
            (if (org-roam-node-from-id id)
                `(:foreground ,keyword)
              'org-link))))
  (org-link-set-parameters
   "id"
   ;; Custom stored description
   :store (lambda (&optional interactive?)
            (if (org-roam-node-at-point)
                (let* ((node (org-roam-node-at-point))
                       (address (org-roam-node-index-numbering node))
                       (hierarchy (org-roam-node-hierarchy node))
                       (description (concat (when address (format "(%s) " address)) hierarchy)))
                  (org-link-store-props :type "id"
                                        :link (concat "id:" (org-id-get-create))
                                        :description description))
              (apply 'org-id-store-link-maybe interactive?)))))

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
