;;; Org-node
(use-package org-node
  :bind ( :map krisb-note-keymap
          ("l" . org-node-context-toggle))
  :custom
  (org-node-ask-directory t)
  (org-node-datestamp-format "%Y%m%dT%H%M%S--")
  (org-node-context-persist-on-disk t)
  (org-node-affixation-fn 'krisb-org-node-affixation-fn)
  (org-node-alter-candidates t)
  (org-node-filter-fn
   (lambda (node)
     "Predicate for whether to include NODE.
If non-nil, include.  If nil, exclude.  This predicate excludes these
nodes:
- With non-nil ROAM_EXCLUDE property value.
- Node in org-agenda file."
     (not (or (assoc "ROAM_EXCLUDE" (org-node-get-properties node))
              (org-agenda-file-p (org-node-get-file node))))))
  :config
  (org-node-cache-mode 1)
  (org-node-context-follow-mode 1)

  ;; Bespoke `org-node-find'
  (cl-defmethod krisb-org-node-box-or-dir ((node org-node))
    "Return a fontified value of the ROAM_BOX property of NODE.
If the ROAM_BOX property of NODE is nil, returns the directory name
containing NODE instead."
    (let ((box (cdr (assoc "ROAM_BOX" (org-node-get-properties node) #'string-equal)))
          (dir (file-name-nondirectory
                (directory-file-name
                 (file-name-directory (org-node-get-file node))))))
      (propertize (concat "/" (or box dir)) 'face 'shadow)))

  (cl-defmethod krisb-org-node-place ((node org-node))
    "Return a fontified value of the ROAM_PLACE property of NODE."
    (when-let ((place (cdr (assoc "ROAM_PLACE" (org-node-get-properties node)))))
      (propertize place 'face 'shadow)))

  (cl-defmethod krisb-org-node-type ((node org-node))
    "Return a fontified value of the ROAM_TYPE property of NODE."
    (when-let ((type (cdr (assoc "ROAM_TYPE" (org-node-get-properties node) #'string-equal))))
      (propertize (concat "&" type) 'face 'font-lock-doc-face)))

  (cl-defmethod krisb-org-node-person ((node org-node))
    "Return a fontified value of the ROAM_PERSON property of NODE."
    (let ((person (cdr (assoc "ROAM_PERSON" (org-node-get-properties node) #'string-equal))))
      (when person
        (propertize (concat "@" person) 'face 'font-lock-keyword-face))))

  (cl-defmethod krisb-org-node-olp-full ((node org-node))
    "Return the full outline path of NODE fontified.
The full outline path of NODE (given by `org-node-get-olp-full')
surrounded by parentheses and whose parts are separated by \" > \".
Additionally, the entire string is fontified to the shadow face."
    (let ((olp
           (propertize (string-join (org-node-get-olp-full node) " > ") 'face 'shadow)))
      (unless (string-empty-p olp)
        (concat
         (propertize "(" 'face 'shadow)
         olp
         (propertize ")" 'face 'shadow)))))

  (cl-defmethod krisb-org-node-tags ((node org-node))
    "Return the full outline path of NODE fontified."
    (when-let ((tags (org-node-get-tags node)))
      (propertize (concat "#" (string-join tags "# ")) 'face 'org-tag)))

  (defun krisb-org-node-affixation-fn (node title)
    "Given NODE and TITLE, add a bespoke prefix and suffix.
For use as `org-node-affixation-fn'."
    (let ((box-or-dir (krisb-org-node-box-or-dir node))
          (place (krisb-org-node-place node))
          (type (krisb-org-node-type node))
          (person (krisb-org-node-person node))
          (olp-full (krisb-org-node-olp-full node))
          (tags (krisb-org-node-tags node)))
      (list title
            ;; Prefix
            (concat (when box-or-dir (concat box-or-dir " "))
                    (when place (concat place " "))
                    (when type (concat place " "))
                    (when person (concat person " ")))
            ;; Suffix
            (concat " "
                    (when olp-full (concat olp-full " "))
                    tags)))))

;;; Org-node-fakeroam
(use-package org-node-fakeroam
  :after org-roam
  :bind ( :map krisb-note-keymap
          ([remap org-roam-buffer-toggle] . org-node-context-toggle))
  :custom
  (org-roam-db-update-on-save nil)      ; Don't update DB on save, not needed
  (org-roam-link-auto-replace nil)      ; Don't look for "roam:" links on save
  :config
  (org-roam-db-autosync-mode -1)
  (org-node-fakeroam-db-feed-mode 1))   ; Keep Roam DB up to date

;;; Provide
(provide 'krisb-org-node)
