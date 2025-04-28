;; -*- lexical-binding: t; -*-

;; -*- lexical-binding: t; -*-

;;; Org-roam
(use-package org-roam
  :bind ( :map krisb-note-keymap
          ("f" . org-roam-node-find)
          ("i" . org-roam-node-insert)
          ("c" . org-roam-capture)
          ("l" . org-roam-buffer-toggle)
          ("ta" . org-roam-tag-add)
          ("tr" . org-roam-tag-remove)
          ("g" . org-roam-graph))
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
      :unnarrowed t
      :immediate-finish t)
     ("s" "source" plain "%?"
      :target (file+head "main/%<%Y%m%dT%H%M%S>.org"
                         ":PROPERTIES:
:ROAM_TYPE: source
:ROAM_%^{Context or source?|SOURCE|CONTEXT}: %(org-roam-node-insert)
:END:\n#+title: ${title}\n")
      :unnarrowed t
      :immediate-finish t)
     ("c" "collection" plain "%?"
      :target (file+head "main/%<%Y%m%dT%H%M%S>.org"
                         ":PROPERTIES:
:ROAM_BOX: main
:ROAM_TYPE: collection
:END:\n#+title: ${title}\n")
      :unnarrowed t
      :immediate-finish t)
     ("r" "reference" plain "%?"
      :target (file+head "references/%<%Y%m%dT%H%M%S>.org"
                         "#+title: ${title}\n")
      :unnarrowed t
      :immediate-finish t)
     ("b" "blog" plain "%?"
      :target (file+head "main/%<%Y%m%dT%H%M%S>.org"
                         "#+title: ${title}
#+filetags:
#+hugo_bundle:
#+export_file_name: index
#+hugo_tags:
#+hugo_categories:
#+hugo_publishdate:
#+hugo_lastmod:
#+hugo_custom_front_matter:
#+hugo_custom_front_matter: :TableOfContents false
#+hugo_draft: true
#+hugo_paired_shortcodes:\n")
      :unnarrowed t
      :immediate-finish t)))
  (org-roam-db-node-include-function
   (lambda () (not (member "ATTACH" (org-get-tags)))))
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-mode-sections
   '((org-roam-backlinks-section :unique t)
     org-roam-reflinks-section))
  :config
  ;; 2025-03-24: Using org-node/indexed.el to replace this.  I dont enable it at
  ;; all because enabling causes a `org-roam-db-sync' on startup since it
  ;; detects that indexed's db isn't its own...
  ;; (org-roam-db-autosync-mode 1)

  ;; See (info "(org-roam) org-roam-export")
  (with-eval-after-load 'ox-html
    (require 'org-roam-export))

  ;; Advise for archiving org-roam nodes
  (defun krisb-org-archive--compute-location-org-roam-format-string (orig-fun &rest args)
    "Take LOCATION in `org-archive--compute-location' and expand %R.
Meant to be used as around advice for `org-archive--compute-location'.

%R is expanded to the identifier for the org-roam node (at point) the
archive command is invoked in. If there is no node at point, then expand
to the file path instead."
    ;; Modify LOCATION before normal operations
    (cl-letf (((car args)
               (if (fboundp 'org-roam-node-at-point)
                   (replace-regexp-in-string "%R"
                                             (if (org-roam-db-node-p)
                                                 (org-roam-node-id (org-roam-node-at-point 'assert))
                                               (buffer-file-name (buffer-base-buffer)))
                                             (car args))
                 (car args))))
      (apply orig-fun args)))
  (advice-add 'org-archive--compute-location :around #'krisb-org-archive--compute-location-org-roam-format-string)

  ;; Custom face for ID links to org-roam-nodes.  I prefer to change their
  ;; foreground color to differentiate them from other types of links as well as
  ;; to use a lighter face because a buffer packed-full of org links can become
  ;; visually distracting and cluttered otherwise.
  (org-link-set-parameters
   "id"
   :face (lambda (id)
           (if (org-roam-node-from-id id)
               '(:weight light :inherit font-lock-keyword-face)
             'org-link)))

  ;; Custom stored description
  (org-link-set-parameters
   "id"
   :store (lambda (&optional interactive?)
            (let* ((id (org-id-get))
                   (node (org-roam-node-from-id id)))
              (if (and (equal major-mode 'org-mode)
                       ;; We want to check more than if there is a node at
                       ;; point; we want to make sure ID corresponds to an
                       ;; existing node
                       node)
                  (org-link-store-props :type "id"
                                        :link (concat "id:" id)
                                        :description (org-roam-node-formatted node))
                (funcall 'org-id-store-link-maybe interactive?)))))

  ;; Add files with node(s) tagged with "__orgAgenda" but not with "archive" to
  ;; `org-agenda-files' list
  (with-eval-after-load 'org-agenda
    (dolist (file (mapcar #'org-roam-node-file
                          (cl-remove-if-not
                           (lambda (node)
                             (let ((tags (org-roam-node-tags node)))
                               (and (member "__orgAgenda" tags)
                                    (not (member "archive" tags)))))
                           (org-roam-node-list))))
      (add-to-list 'org-agenda-files file))
    (setopt org-agenda-hide-tags-regexp "^__"))

  ;; Fold headings by default
  (add-to-list 'org-roam-buffer-postrender-functions #'magit-section-show-level-2))

;;; Krisb-org-roam-ext
(use-package krisb-org-roam-ext
  :ensure nil
  :bind ( :map krisb-note-keymap
          ("." . krisb-org-roam-ext-properties-transient))
  :custom
  ;; Customize how nodes are inserted via `org-roam-node-insert'
  (org-roam-node-formatter 'krisb-org-roam-node-formatter)
  :config
  ;; The full content of each template element is present (and therefore
  ;; searchable) even if visually absent/truncated
  (setopt org-roam-node-display-template
          (concat "${directories-display-template:8} "
                  ;; FIXME 2024-11-16: For some reason using :* to automatically set
                  ;; length produces too-wide a column
                  (concat "${address-display-template:"
                          (number-to-string
                           (1+ (or (cl-loop for node in (org-roam-node-list)
                                            maximize (length (org-roam-node-address node)))
                                   2))) ; Have default: prevents startup error when (org-roam-node-list) is nil
                          "}")
                  "${type-display-template}"
                  "${person-display-template}"
                  "${hierarchy}"
                  (propertize " ${tags:60}" 'face 'org-tag)))

  ;; Define bespoke transient menu for node properties
  (transient-define-prefix krisb-org-roam-ext-properties-transient ()
    "Transient menu for setting org-roam properties."
    ["Properties"
     ["Generic"
      (org-id-get-create
       :key "a"
       :transient t
       :description ,(krisb-org-roam-ext-transient--dyn-roam-property-description "Add ID" "ID" "Modify ID"))
      (org-expiry-insert-created
       :key "C"
       :transient t
       :description ,(krisb-org-roam-ext-transient--dyn-roam-property-description "Add CREATED" "CREATED"))]
     ["All nodes"
      (krisb-org-roam-ext-set-roam-box
       :key "b"
       :transient t
       :description ,(krisb-org-roam-ext-transient--dyn-roam-property-description "Set ROAM_BOX" "ROAM_BOX"))
      (krisb-org-roam-ext-toggle-roam-exclude
       :key "e"
       :transient t
       :description ,(krisb-org-roam-ext-transient--dyn-roam-property-description "Toggle ROAM_EXCLUDE" "ROAM_EXCLUDE"))]
     ["Idea-specific"
      (krisb-org-roam-ext-set-roam-type
       :key "t"
       :transient t
       :description ,(krisb-org-roam-ext-transient--dyn-roam-property-description "Set ROAM_TYPE" "ROAM_TYPE"))
      (krisb-org-roam-ext-set-roam-source
       :key "s"
       :transient t
       :description ,(krisb-org-roam-ext-transient--dyn-roam-property-description "Set ROAM_SOURCE" "ROAM_SOURCE"))
      (krisb-org-roam-ext-set-roam-context
       :key "c"
       :transient t
       :description ,(krisb-org-roam-ext-transient--dyn-roam-property-description "Set ROAM_CONTEXT" "ROAM_CONTEXT"))
      (krisb-org-roam-ext-set-roam-person
       :key "r"
       :transient t
       :description ,(krisb-org-roam-ext-transient--dyn-roam-property-description "Set ROAM_PERSON" "ROAM_PERSON"))
      (krisb-org-roam-ext-set-roam-place
       :key "p"
       :transient t
       :description ,(krisb-org-roam-ext-transient--dyn-roam-property-description "Set ROAM_PLACE" "ROAM_PLACE"))]]
    [["Navigation"
      ("C-u" "Up heading" org-up-heading :transient t)
      ("C-p" "Next heading" org-previous-visible-heading :transient t)
      ("C-n" "Next heading" org-next-visible-heading :transient t)
      ("C-f" "Forward heading same level" org-forward-heading-same-level :transient t)
      ("C-b" "Backward heading same level" org-backward-heading-same-level :transient t)]
     ["Visibility"
      ("M-t" "Toggle visibility of heading contents" krisb-org-roam-ext-toggle-heading-content-visibility :transient t)
      ("M-T" "Toggle visibility of properties drawer" krisb-org-roam-ext-toggle-properties-visibility :transient t)]]))

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
  ;; 2025-03-25: Use `citar-org-node' now.  We forcefully disable this to
  ;; prevent this package from internally calling an `org-roam-db-sync' with t,
  ;; which, with `indexed-roam-mode' and `indexed-roam-overwrite' to non-nil,
  ;; will cause a full, long database sync on startup.
  :disabled t
  :after (:any citar org-roam)
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
  :hook ((org-roam-folgezettel-mode . hl-line-mode)
         (org-roam-folgezettel-mode . (lambda () (setq-local line-spacing 0.2))))
  :bind ( :map krisb-note-keymap
          ("m" . org-roam-folgezettel-list)
          ("s" . org-roam-folgezettel-show-node-in-list))
  :custom
  (org-roam-folgezettel-default-filter-query '(box "main"))
  :init
  (with-eval-after-load 'vtable
    (el-patch-defun vtable-goto-object (object)
      "Go to OBJECT in the current table.
Return the position of the object if found, and nil if not."
      (let ((start (point)))
        (vtable-beginning-of-table)
        (save-restriction
          (narrow-to-region (point) (save-excursion (vtable-end-of-table)))
          (if (text-property-search-forward 'vtable-object object (el-patch-swap #'eq #'equal))
              (progn
                (forward-line -1)
                (point))
            (goto-char start)
            nil)))))
  :config
  (with-eval-after-load 'embark
    (require 'org-roam-folgezettel-embark))

  ;; We must add these after their default values are set by org
  (with-eval-after-load 'org
    ;; Add ROAM_* properties to properties completing-read interface completions
    (dolist (prop '("ROAM_EXCLUDE"
                    "ROAM_PLACE"
                    "ROAM_PERSON"
                    "ROAM_SOURCE"
                    "ROAM_CONTEXT"
                    "ROAM_REFS"
                    "ROAM_TYPE"
                    "ROAM_BOX"))
      (add-to-list 'org-default-properties prop))

    ;; Set inherited default values for some ROAM_* properties
    (add-to-list 'org-global-properties '("ROAM_TYPE" . "source collection pointer"))
    (add-to-list 'org-use-property-inheritance "ROAM_BOX")))

;;; Provide
(provide 'krisb-org-roam)
