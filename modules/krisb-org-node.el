;; -*- lexical-binding: t; -*-

;;; Org-node
(use-package org-node
  :vc ( :url "https://github.com/meedstrom/org-node.git"
        :branch "dev"
        :rev :newest)
  :hook (find-file . krisb-org-node-rename-buffer-name-to-title)
  :bind ( :map krisb-note-keymap
          ("l" . org-node-context-toggle)
          ([remap org-roam-buffer-toggle] . org-node-context-toggle)
          ("f" . org-node-find)
          ("i" . org-node-insert-link)
          ("t a" . org-node-add-tags-here))
  :custom
  (org-node-ask-directory t)
  (org-node-datestamp-format "%Y%m%dT%H%M%S--")
  (org-node-context-persist-on-disk t)
  (org-node-cache-everything t) ; For programming purposes; populates `org-node--file<>lnum.node.'
  (org-node-affixation-fn #'krisb-org-node-affixation-fn)
  (org-node-alter-candidates t)
  (org-node-custom-link-format-fn #'krisb-org-node-custom-link-format-fn)
  (org-node-filter-fn #'krisb-org-node-filter-fn)
  (org-node-warn-title-collisions nil)
  (org-node-renames-allowed-dirs (list krisb-notes-directory))
  :config
  (org-node-cache-mode 1)
  (org-node-context-follow-mode 1)

  ;; Bespoke filtering (exclusion) function.
  (defun krisb-org-node-filter-fn (node)
    "Predicate for whether to include NODE.
If non-nil, include.  If nil, exclude.  This predicate excludes these
nodes:
- With non-nil ROAM_EXCLUDE property value."
    (not (or
          (string= "t" (string-trim (cdr (assoc "ROAM_EXCLUDE" (org-node-get-properties node)))))
          ;; More conditions here
          )))

  ;; Rename buffer to the file's title if the file is an org-node.
  ;; NOTE 2025-04-23: We add this to `find-file-hook' rather than
  ;; `org-mode-hook' since successive calls to
  ;; `krisb-org-node-rename-buffer-name-to-title' always change the buffer name because
  ;; of `generate-new-buffer-name' (which must be used to avoid naming
  ;; conflicts).  Not sure if this is avoidable.  But this suffices for now.
  (defun krisb-org-node-rename-buffer-name-to-title ()
    "Rename buffer to its #+TITLE property.
This only occurs when the file is an org-node node."
    (when (derived-mode-p 'org-mode)
      (when-let ((id (save-excursion
                       (save-restriction
                         (widen)
                         (org-entry-get (point-min) "ID"))))
                 (node (gethash id org-nodes))
                 (title (org-node-get-title node)))
        (rename-buffer (generate-new-buffer-name title (buffer-name))))))

  ;; Bespoke `org-node-find'
  (cl-defmethod krisb-org-node-get-box ((node org-node))
    "Return the value of the ROAM_BOX property of NODE."
    (cdr (assoc "ROAM_BOX" (org-node-get-properties node) #'string-equal)))

  (cl-defmethod krisb-org-node-box-or-dir ((node org-node))
    "Return a fontified value of the ROAM_BOX property of NODE.
If the ROAM_BOX property of NODE is nil, returns the directory name
containing NODE instead."
    (let ((box (krisb-org-node-get-box node))
          (dir (file-name-nondirectory
                (directory-file-name
                 (file-name-directory (org-node-get-file node))))))
      (propertize (or box (concat "/" dir)) 'face 'shadow)))

  (cl-defmethod krisb-org-node-get-place ((node org-node))
    "Return the value of the ROAM_PLACE property of NODE."
    (cdr (assoc "ROAM_PLACE" (org-node-get-properties node))))

  (cl-defmethod krisb-org-node-get-type ((node org-node))
    "Return the value of the ROAM_TYPE property of NODE."
    (cdr (assoc "ROAM_TYPE" (org-node-get-properties node) #'string-equal)))

  (cl-defmethod krisb-org-node-get-person ((node org-node))
    "Return the value of the ROAM_PERSON property of NODE."
    (cdr (assoc "ROAM_PERSON" (org-node-get-properties node) #'string-equal)))

  (cl-defmethod krisb-org-node-olp-full-propertized ((node org-node))
    "Return the full outline path of NODE fontified.
The full outline path of NODE (given by `org-node-get-olp-full')
surrounded by parentheses and whose parts are separated by \" > \".
Additionally, the entire string is fontified to the shadow face."
    (let ((olp (propertize (string-join (org-node-get-olp-full node) " > ") 'face 'shadow)))
      (unless (string-empty-p olp)
        (concat
         (propertize "(" 'face 'shadow)
         olp
         (propertize ")" 'face 'shadow)))))

  (cl-defmethod krisb-org-node-tags-propertized ((node org-node))
    "Return the full outline path of NODE fontified."
    (when-let ((tags (org-node-get-tags node)))
      (propertize (concat "#" (string-join tags " #")) 'face 'org-tag)))

  (defun krisb-org-node-affixation-fn (node title)
    "Given NODE and TITLE, add a bespoke prefix and suffix.
For use as `org-node-affixation-fn'."
    (let ((box-or-dir (krisb-org-node-box-or-dir node))
          (place (krisb-org-node-get-place node))
          (type (krisb-org-node-get-type node))
          (person (krisb-org-node-get-person node))
          (olp-full (krisb-org-node-olp-full-propertized node))
          (tags (krisb-org-node-tags-propertized node)))
      (list title
            ;; Prefix
            (concat (when box-or-dir (concat box-or-dir " "))
                    (when place (propertize (concat place " ") 'face 'shadow))
                    (when type (propertize (concat "&" type " ") 'face 'font-lock-doc-face))
                    (when person (propertize (concat "@" person " ") 'face 'font-lock-keyword-face)))
            ;; Suffix
            (concat " "
                    (when olp-full (concat olp-full " "))
                    tags))))

  ;; Bespoke `org-node-custom-link-format-fn' function
  (cl-defmethod krisb-org-node-custom-link-format-fn ((node org-node))
    "Bespoke function for `org-node-custom-link-format-fn'."
    (if (or (file-in-directory-p (org-node-get-file node) krisb-org-agenda-directory)
            (file-in-directory-p (org-node-get-file node) krisb-org-archive-directory))
        (org-node-get-title node)
      (let* ((place (krisb-org-node-get-place node))
             (type (krisb-org-node-get-type node))
             (title (org-node-get-title node))
             (file-title (org-node-get-file-title node)))
        (concat (when place (format "(%s) " place))
                (when type (format "{%s} " type))
                title
                (when (or (not (string= title file-title))
                          (not file-title))
                  (propertize (concat " (" file-title ")") 'face 'shadow)))))))

;;; Org-node-fakeroam
(use-package org-node-fakeroam
  :disabled t                           ; 2025-03-20: We have org-mem.el now
  :after org-roam
  :custom
  (org-roam-db-update-on-save nil)      ; Don't update DB on save, not needed
  (org-roam-link-auto-replace nil)      ; Don't look for "roam:" links on save
  :config
  (org-roam-db-autosync-mode -1)
  (org-node-fakeroam-db-feed-mode 1))   ; Keep Roam DB up to date

;;; Citar-org-node
(use-package citar-org-node
  :ensure nil
  :load-path "/home/krisbalintona/emacs-repos/packages/citar-org-node/"
  :after (:any citar org-node)
  :demand t
  :diminish
  :bind ( :map krisb-note-keymap
          ("b a" . citar-org-node-add-refs)
          ("b o" . citar-org-node-open-resource))
  :config
  (citar-org-node-mode 1))

;;; Org-Mem
(use-package org-mem
  :vc ( :url "https://github.com/meedstrom/org-mem.git"
        :rev :newest)
  :custom
  (org-mem-do-sync-with-org-id t)
  (org-mem-watch-dirs (list krisb-org-directory))
  (org-mem-do-warn-title-collisions nil)
  :config
  (org-mem-updater-mode 1)
  (org-mem-roamy-db-mode 1) ; 2025-04-02: This is required for collecting ROAM_REFS information

  ;; NOTE 2025-03-23: Not enabled for now because I do not use it and it is in
  ;; flux, so I may enable in the future when it is more stable and finalized.
  ;; (org-mem-orgdb-mode 1)
  ;; End dependence on `org-roam-db-sync'
  (with-eval-after-load 'org-roam
    (setopt org-roam-db-update-on-save nil
            org-mem-roam-overwrite t)  ; Write to on-disk db, not a diskless one
    (org-roam-db-autosync-mode -1)))

;;; Provide
(provide 'krisb-org-node)
