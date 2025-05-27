;; -*- lexical-binding: t; -*-

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
  ;; which, with `org-mem-roam-mode' and `org-mem-roam-overwrite' to non-nil,
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

;;; Provide
(provide 'krisb-org-roam)
