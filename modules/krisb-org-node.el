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
  (org-node-affixation-fn 'krisb-org-node-affixation-fn)
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
    (not (or (assoc "ROAM_EXCLUDE" (org-node-get-properties node)))))

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
        (rename-buffer (generate-new-buffer-name title)))))

  ;; Bespoke `org-node-find'
  (cl-defmethod krisb-org-node-get-box ((node indexed-org-entry))
    "Return the value of the ROAM_BOX property of NODE."
    (cdr (assoc "ROAM_BOX" (org-node-get-properties node) #'string-equal)))

  (cl-defmethod krisb-org-node-box-or-dir ((node indexed-org-entry))
    "Return a fontified value of the ROAM_BOX property of NODE.
If the ROAM_BOX property of NODE is nil, returns the directory name
containing NODE instead."
    (let ((box (krisb-org-node-get-box node))
          (dir (file-name-nondirectory
                (directory-file-name
                 (file-name-directory (org-node-get-file node))))))
      (propertize (or box (concat "/" dir)) 'face 'shadow)))

  (cl-defmethod krisb-org-node-get-place ((node indexed-org-entry))
    "Return the value of the ROAM_PLACE property of NODE."
    (cdr (assoc "ROAM_PLACE" (org-node-get-properties node))))

  (cl-defmethod krisb-org-node-get-type ((node indexed-org-entry))
    "Return the value of the ROAM_TYPE property of NODE."
    (cdr (assoc "ROAM_TYPE" (org-node-get-properties node) #'string-equal)))

  (cl-defmethod krisb-org-node-get-person ((node indexed-org-entry))
    "Return the value of the ROAM_PERSON property of NODE."
    (cdr (assoc "ROAM_PERSON" (org-node-get-properties node) #'string-equal)))

  (cl-defmethod krisb-org-node-olp-full-propertized ((node indexed-org-entry))
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

  (cl-defmethod krisb-org-node-tags-propertized ((node indexed-org-entry))
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
  (cl-defmethod krisb-org-node-custom-link-format-fn ((node indexed-org-entry))
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
  :disabled t                           ; 2025-03-20: We have indexed.el now
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

;;; Indexed
(use-package indexed
  :vc ( :url "https://github.com/meedstrom/indexed.git"
        :rev :newest)
  :custom
  (indexed-sync-with-org-id t)
  (indexed-org-dirs (list krisb-org-directory))
  (indexed-warn-title-collisions nil)
  :config
  (indexed-updater-mode 1)
  (indexed-roam-mode 1) ; 2025-04-02: This is required for collecting ROAM_REFS information

  ;; NOTE 2025-03-23: Not enabled for now because I do not use it and it is in
  ;; flux, so I may enable in the future when it is more stable and finalized.
  ;; (indexed-orgdb-mode 1)
  ;; End dependence on `org-roam-db-sync'
  (with-eval-after-load 'org-roam
    (setopt org-roam-db-update-on-save nil
            indexed-roam-overwrite t)  ; Write to on-disk db, not a diskless one
    (org-roam-db-autosync-mode -1))

  ;; 2025-04-22: Patch `indexed-org-parser--parse-file' for remove "problems"
  ;; identified by indexed.el.  Not yet upstreamed, so I manually do it.  For an
  ;; explanation of this solution and a description of the bug, see
  ;; https://github.com/meedstrom/indexed/issues/6#issuecomment-2764573168.
  (el-patch-defun indexed-org-parser--parse-file (FILE)
    "Gather entries, links and other data in FILE."
    (unless (eq indexed-org-parser--buf (current-buffer))
      (indexed-org-parser--init-buf-and-switch))
    (setq indexed-org-parser--found-links nil)
    ;; TODO: Upcase all or downcase all the let-bindings, no longer semantic
    (let ((file-todo-option-re
           (rx bol (* space) (or "#+todo: " "#+seq_todo: " "#+typ_todo: ")))
          missing-file
          found-entries
          file-data
          problem
          USE-TAG-INHERITANCE
          HEADING-POS HERE FAR END ID-HERE ID FILE-ID CRUMBS
          DRAWER-BEG DRAWER-END
          TITLE FILE-TITLE LNUM
          TODO-STATE TODO-RE FILE-TODO-SETTINGS
          TAGS FILE-TAGS HERITABLE-TAGS
          SCHED DEADLINE CLOSED PRIORITY LEVEL PROPS)
      (condition-case err
          (catch 'file-done
            (when (not (file-readable-p FILE))
              ;; FILE does not exist, user probably deleted or renamed a file.
              (setq missing-file FILE)
              (throw 'file-done t))
            ;; Skip symlinks, they cause duplicates if the true file is also in
            ;; the file list.  Note that symlinks should not be treated how we
            ;; treat missing files.
            (when (file-symlink-p FILE)
              (throw 'file-done t))
            ;; NOTE: Don't use `insert-file-contents-literally'!  It sets
            ;; `coding-system-for-read' to `no-conversion', which results in
            ;; wrong values for HEADING-POS when the file contains any Unicode.
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert-file-contents FILE))

            (el-patch-swap
              (let* ((DIR (file-name-directory FILE))
                     (DIR-LOCALS
                      (cdr (or (assoc DIR indexed-org-parser--all-dir-locals)
                               ;; HACK: Oh boy.
                               (let* ((default-directory DIR)
                                      (buffer-file-name FILE)
                                      (major-mode 'org-mode)
                                      (enable-local-variables :safe)
                                      (new (hack-dir-local--get-variables nil)))
                                 (if new
                                     (push new indexed-org-parser--all-dir-locals)
                                   (push (cons DIR nil) indexed-org-parser--all-dir-locals))
                                 new))))
                     (FILE-LOCALS (append (hack-local-variables--find-variables)
                                          (hack-local-variables-prop-line)))
                     (LOCAL-USE-TAG-INHERITANCE (or (assq 'org-use-tag-inheritance FILE-LOCALS)
                                                    (assq 'org-use-tag-inheritance DIR-LOCALS))))
                (setq USE-TAG-INHERITANCE (if LOCAL-USE-TAG-INHERITANCE
                                              (cdr LOCAL-USE-TAG-INHERITANCE)
                                            $use-tag-inheritance)))
              (let* ((DIR (file-name-directory FILE))
                     (default-directory DIR)
                     (buffer-file-name FILE)
                     (major-mode 'org-mode)
                     (enable-local-variables :safe)
                     (FILE-LOCALS (append (hack-local-variables--find-variables)
                                          (hack-local-variables-prop-line)))
                     (enable-dir-variables t)
                     (dir-or-cache (dir-locals-find-file default-directory))
                     (class (cond
                             ((stringp dir-or-cache)
                              (dir-locals-read-from-dir dir-or-cache))
                             ((consp dir-or-cache)
                              (nth 1 dir-or-cache))))
                     (DIR-LOCALS (cdr (assq major-mode (dir-locals-get-class-variables class)))))
                (setq USE-TAG-INHERITANCE (cond
                                           ((assq 'org-use-tag-inheritance FILE-LOCALS)
                                            (cdr (assq 'org-use-tag-inheritance FILE-LOCALS)))
                                           ((assq 'org-use-tag-inheritance DIR-LOCALS)
                                            (cdr (assq 'org-use-tag-inheritance DIR-LOCALS)))
                                           (t $use-tag-inheritance)))))

            (goto-char 1)
            ;; If the very first line of file is a heading, don't try to scan any
            ;; file-level front matter.  Our usage of
            ;; `indexed-org-parser--next-heading' cannot handle that edge-case.
            (if (looking-at-p "\\*")
                (progn
                  (setq FILE-ID nil)
                  (setq FILE-TITLE nil)
                  (setq TODO-RE $default-todo-re))
              ;; Narrow until first heading
              (when (indexed-org-parser--next-heading)
                (narrow-to-region 1 (point))
                (goto-char 1))
              ;; Rough equivalent of `org-end-of-meta-data' for the file
              ;; level front matter, can jump somewhat too far but that's ok
              (setq FAR (if (re-search-forward "^ *?[^#:\n]" nil t)
                            (1- (point))
                          ;; There's no content other than front matter
                          (point-max)))
              (goto-char 1)
              (setq PROPS
                    (if (re-search-forward "^[\t\s]*:properties:" FAR t)
                        (progn
                          (forward-line 1)
                          (indexed-org-parser--collect-properties
                           (point)
                           (if (re-search-forward "^[\t\s]*:end:" FAR t)
                               (pos-bol)
                             (error "Couldn't find :END: of drawer"))))
                      nil))
              (setq HERE (point))
              (setq FILE-TAGS
                    (if (re-search-forward "^#\\+filetags: " FAR t)
                        (split-string
                         (buffer-substring (point) (pos-eol))
                         ":" t)
                      nil))
              (goto-char HERE)
              (setq TODO-RE
                    (if (re-search-forward file-todo-option-re FAR t)
                        (progn
                          (setq FILE-TODO-SETTINGS nil)
                          ;; Because you can have multiple #+todo: lines...
                          (while (progn
                                   (push (buffer-substring (point) (pos-eol))
                                         FILE-TODO-SETTINGS)
                                   (re-search-forward
                                    file-todo-option-re FAR t)))
                          (indexed-org-parser--make-todo-regexp
                           (string-join FILE-TODO-SETTINGS " ")))
                      $default-todo-re))
              (goto-char HERE)
              (setq FILE-TITLE (when (re-search-forward "^#\\+title: +" FAR t)
                                 (string-trim-right
                                  (indexed-org-parser--org-link-display-format
                                   (buffer-substring (point) (pos-eol))))))
              (setq FILE-ID (cdr (assoc "ID" PROPS)))
              (goto-char HERE)
              ;; Don't count org-super-links backlinks as forward links
              ;; TODO: Rewrite more readably
              (if (re-search-forward "^[   ]*:BACKLINKS:" nil t)
                  (progn
                    (setq END (point))
                    (unless (search-forward ":end:" nil t)
                      (error "Couldn't find :END: of drawer"))
                    ;; Collect from end of backlinks drawer to first heading
                    (indexed-org-parser--collect-links-until nil FILE-ID FILE))
                (setq END (point-max)))
              (goto-char HERE)
              (indexed-org-parser--collect-links-until END FILE-ID FILE)
              (push (record 'indexed-org-entry
                            nil
                            nil
                            FILE
                            0
                            FILE-ID
                            1
                            nil
                            1
                            nil
                            PROPS
                            nil
                            nil
                            FILE-TAGS
                            (or FILE-TITLE (file-name-nondirectory FILE))
                            nil)
                    found-entries)
              (goto-char (point-max))
              ;; We should now be at the first heading
              (widen))

            (setq file-data
                  (record 'indexed-file-data
                          FILE
                          FILE-TITLE
                          -1 ;; to amend at the end
                          (ceiling (float-time
                                    (file-attribute-modification-time
                                     (file-attributes FILE))))
                          -1 ;; to amend at the end
                          FILE-ID))

            ;; Prep
            (setq LNUM (line-number-at-pos))
            (setq CRUMBS nil)
            (setq FILE-TAGS (and USE-TAG-INHERITANCE
                                 (cl-loop for tag in FILE-TAGS
                                          unless (member tag $nonheritable-tags)
                                          collect tag)))

            ;; Loop over the file's headings
            (while (not (eobp))
              (catch 'entry-done
                ;; Narrow til next heading
                (narrow-to-region (point)
                                  (save-excursion
                                    (or (indexed-org-parser--next-heading)
                                        (point-max))))
                (setq HEADING-POS (point))
                (setq LEVEL (skip-chars-forward "*"))
                (skip-chars-forward " ")
                (let ((case-fold-search nil))
                  (setq TODO-STATE
                        (if (looking-at TODO-RE)
                            (prog1 (buffer-substring (point) (match-end 0))
                              (goto-char (match-end 0))
                              (skip-chars-forward " "))
                          nil))
                  ;; [#A] [#B] [#C]
                  (setq PRIORITY
                        (if (looking-at "\\[#[A-Z0-9]+\\]")
                            (prog1 (match-string 0)
                              (goto-char (match-end 0))
                              (skip-chars-forward " "))
                          nil)))
                ;; Skip statistics-cookie such as "[2/10]"
                (when (looking-at "\\[[0-9]*/[0-9]*\\]")
                  (goto-char (match-end 0))
                  (skip-chars-forward " "))
                (setq HERE (point))
                ;; Any tags in heading?
                (if (re-search-forward " +:.+: *$" (pos-eol) t)
                    (progn
                      (goto-char (match-beginning 0))
                      (setq TAGS (split-string (match-string 0) ":" t " *"))
                      (setq TITLE (string-trim-right
                                   (indexed-org-parser--org-link-display-format
                                    (buffer-substring HERE (point))))))
                  (setq TAGS nil)
                  (setq TITLE (string-trim-right
                               (indexed-org-parser--org-link-display-format
                                (buffer-substring HERE (pos-eol))))))
                ;; REVIEW: This is possibly overkill, and could be
                ;;         written in a way easier to follow.
                ;; Gotta go forward 1 line, see if it is a planning-line, and
                ;; if it is, then go forward 1 more line, and if that is a
                ;; :PROPERTIES: line, then we're safe to collect properties
                (forward-line 1)
                (setq HERE (point))
                (setq FAR (pos-eol))
                (setq SCHED
                      (if (re-search-forward "[\t\s]*SCHEDULED: +" FAR t)
                          (prog1
                              (indexed-org-parser--stamp-to-iso8601
                               (buffer-substring
                                (point)
                                (+ (point) (skip-chars-forward "^]>\n"))))
                            (goto-char HERE))
                        nil))
                (setq DEADLINE
                      (if (re-search-forward "[\t\s]*DEADLINE: +" FAR t)
                          (prog1
                              (indexed-org-parser--stamp-to-iso8601
                               (buffer-substring
                                (point)
                                (+ (point) (skip-chars-forward "^]>\n"))))
                            (goto-char HERE))
                        nil))
                (setq CLOSED
                      (if (re-search-forward "[\t\s]*CLOSED: +" FAR t)
                          (prog1
                              (indexed-org-parser--stamp-to-iso8601
                               (buffer-substring
                                (point)
                                (+ (point) (skip-chars-forward "^]>\n"))))
                            (goto-char HERE))
                        nil))
                (when (or SCHED DEADLINE CLOSED)
                  ;; Alright, so there was a planning-line, meaning any
                  ;; :PROPERTIES: are not on this line but the next.
                  (forward-line 1)
                  (setq FAR (pos-eol)))
                (skip-chars-forward "\t\s")
                (setq PROPS
                      (if (looking-at-p ":properties:")
                          (progn
                            (forward-line 1)
                            (indexed-org-parser--collect-properties
                             (point)
                             (if (re-search-forward "^[\t\s]*:end:" nil t)
                                 (pos-bol)
                               (error "Couldn't find :END: of drawer"))))
                        nil))
                (setq ID (cdr (assoc "ID" PROPS)))
                (setq HERITABLE-TAGS
                      (and USE-TAG-INHERITANCE
                           (cl-loop for tag in TAGS
                                    unless (member tag $nonheritable-tags)
                                    collect tag)))
                ;; CRUMBS is a list that can look like
                ;;    ((3 "Heading" "id1234" ("noexport" "work" "urgent"))
                ;;     (2 "Another heading" "id6532" ("work"))
                ;;     (... ... ... ...))
                ;; if the previous heading looked like
                ;;    *** Heading  :noexport:work:urgent:
                ;;       :PROPERTIES:
                ;;       :ID: id1234
                ;;       :END:
                ;; It lets us track context so we know the outline path to the
                ;; current entry and what tags it should be able to inherit.
                ;; Update the list.
                (cl-loop until (> LEVEL (or (caar CRUMBS) 0))
                         do (pop CRUMBS)
                         finally do
                         (push (list LEVEL TITLE ID HERITABLE-TAGS)
                               CRUMBS))
                (push (record 'indexed-org-entry
                              CLOSED
                              DEADLINE
                              FILE
                              LEVEL
                              ID
                              LNUM
                              (nreverse (mapcar #'cadr (cdr CRUMBS)))
                              HEADING-POS
                              PRIORITY
                              PROPS
                              SCHED
                              (delete-dups
                               (apply #'append
                                      FILE-TAGS
                                      (mapcar #'cadddr (cdr CRUMBS))))
                              TAGS
                              TITLE
                              TODO-STATE)
                      found-entries)

                ;; Heading analyzed, now collect links in entry body!

                (setq ID-HERE
                      (or ID
                          (cl-loop for crumb in CRUMBS thereis (caddr crumb))
                          FILE-ID
                          (throw 'entry-done t)))
                (setq HERE (point))
                ;; Don't count org-super-links backlinks.
                ;; TODO: Generalize this mechanism, use configurable lists
                ;; `$structures-to-ignore' and `$drawers-to-ignore'
                (setq DRAWER-BEG (re-search-forward "^[    ]*:BACKLINKS:" nil t))
                (setq DRAWER-END
                      (and DRAWER-BEG
                           (or (search-forward ":end:" nil t)
                               (error "Couldn't find :END: of drawer"))))

                ;; Collect links inside the heading
                (goto-char HEADING-POS)
                (indexed-org-parser--collect-links-until (pos-eol) ID-HERE FILE)
                ;; Collect links between property drawer and backlinks drawer
                (goto-char HERE)
                (when DRAWER-BEG
                  (indexed-org-parser--collect-links-until DRAWER-BEG ID-HERE FILE))
                ;; Collect links until next heading
                (goto-char (or DRAWER-END HERE))
                (indexed-org-parser--collect-links-until (point-max) ID-HERE FILE))
              (goto-char (point-max))
              (setq LNUM (+ (- LNUM 1) (line-number-at-pos)))
              (widen))

            (aset file-data 3 LNUM)
            (aset file-data 5 (point)))

        ;; Don't crash when there is an error signal, just report it.
        ;; Could allow for plural problems here, but one per file is plenty
        (( t error )
         (setq problem (list (format-time-string "%H:%M") FILE (point) err))))

      (list (if missing-file (list missing-file))
            (if file-data (list file-data))
            found-entries
            indexed-org-parser--found-links
            (if problem (list problem))))))

;;; Provide
(provide 'krisb-org-node)
