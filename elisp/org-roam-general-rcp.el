;;; org-roam-general-rcp.el --- Summary
;;
;;; Commentary:
;;
;; This is all the configuration of the org-roam package
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Org-roam
(use-package org-roam
  :straight (org-roam :type git :host github :repo "org-roam/org-roam" :branch "origin/master")
  :after company ; Necessary for some reason
  :custom
  (org-roam-directory kb/roam-dir)
  (org-roam-dailies-directory (concat kb/roam-dir "journals/"))
  (org-roam-verbose nil) ; Don't echo messages that aren't errors
  (org-roam-completion-everywhere t) ; Org-roam completion everywhere
  (org-roam-link-auto-replace t) ; Replace roam link type with file link type when possible
  ;; (org-roam-db-gc-threshold most-positive-fixnum) ; Temporarily increase GC threshold during intensive org-roam operations
  (org-roam-db-gc-threshold (* 3 838861))

  (org-use-tag-inheritance nil) ; For the way I use lit notes not to transfer source type to evergreen note status

  ;; How it appears in the minibuffer
  ;; (org-roam-node-display-template (concat " ${title:130}" (propertize " ⸽ ${file:50} ⸽ ${tags:20}" 'face 'org-roam-dim)))
  (org-roam-node-display-template (concat "${backlinkscount:16} " "${functiontag:16} " "${othertags:13} " "${hierarchy:183}"))
  :config
  (setq org-roam-v2-ack t) ; Remove startup message which warns that this is v2
  (org-roam-setup) ; Replacement for org-roam-mode

  (add-hook 'org-roam-mode-hook #'hide-mode-line-mode) ; Hide modeline in org-roam buffer
  (add-hook 'org-mode-hook (lambda ();; Faces
                             (set-face-attribute 'org-link nil :foreground "goldenrod3" :bold nil :italic t :font kb/variable-pitch-font :height 145 :underline nil)
                             (set-face-attribute 'bookmark-face nil :foreground nil :background nil)) ; This is the fact used for captures. Its background is ugly
            )

  ;; To add back mouse click to visit the node in the backlink buffer
  (general-define-key
   :keymaps 'org-roam-mode-map
   [mouse-1] #'org-roam-visit-thing
   )

  (kb/leader-keys
    ;; "nf" '((lambda ()
    ;;          (interactive)
    ;;          (org-roam-node-find nil ""
    ;;                              (org-roam-node-read nil (lambda (node)
    ;;                                                        (not (string-equal
    ;;                                                              (concat (expand-file-name kb/roam-dir) "journals/" (format-time-string "%Y" (current-time)) ".org")
    ;;                                                              (org-roam-node-file node))))
    ;;                                                  'org-roam-node-sort-by-file-mtime))
    ;;          )
    ;;        :which-key "Find file")
    ;; "nF" '((lambda ()
    ;;          (interactive)
    ;;          (org-roam-node-find t ""
    ;;                              (org-roam-node-read nil nil
    ;;                                                  'org-roam-node-sort-by-file-mtime))
    ;;          )
    ;;        :which-key "Find file other window")
    "ni" '(org-roam-node-insert :which-key "Insert note")
    "nt" '(org-roam-tag-add :which-key "Add tag")
    "nT" '(org-roam-tag-remove :which-key "Remove tag")
    "nN" '(org-id-get-create :which-key "Add ID")
    "nI" '(org-roam-jump-to-index :which-key "Go to index")
    "nl" '(org-roam-buffer-toggle :which-key "Toggle Roam buffer")
    "nL" '(org-roam-db-sync :which-key "Build cache")
    "nc" '(org-roam-capture :which-key "Roam capture")

    "nd" '(:ignore t :which-key "Roam dailies")
    ;; "ndd" '(org-roam-dailies-find-date :which-key "Find date")
    "ndd" '((lambda()
              (interactive)
              (org-roam-node-find nil "vo"
                                  (org-roam-node-read nil (lambda (node)
                                                            (string-equal
                                                             (concat (expand-file-name kb/roam-dir) "journals/" (format-time-string "%Y" (current-time)) ".org")
                                                             (org-roam-node-file node)))
                                                      'org-roam-node-sort-by-file-mtime))
              )
            :which-key "Find date")
    "ndt" '(org-roam-dailies-find-today :which-key "Today")
    "ndm" '(org-roam-dailies-find-tomorrow :which-key "Tomorrow")
    "ndy" '(org-roam-dailies-find-yesterday :which-key "Yesterday")

    "nf" '(org-roam-node-find :which-key "Temp")
    )
  )

;;;; Org-roam-capture-templates
(setq kb/lit-categories
      '("video" "book" "podcast" "article" "website" "journal" "quote" "structure")
      )

(defun kb/insert-lit-category ()
  "Insert type of literature note sources."
  (completing-read "Category: " kb/lit-categories)
  )

(setq org-roam-capture-templates
      '(("d" "Default" plain
         ""
         :if-new (file+head "${slug}-%<%b%d%Y-%H%M%S>.org"
                            "#+title: ${title}\n")
         :immediate-finish t)
        ("e" "Evergreen" plain
         ""
         :if-new (file+head "${slug}-%<%b%d%Y-%H%M%S>.org"
                            "#+filetags: :new:\n#+title: ${title}\nReference: \n\n\n")
         :jump-to-captured t)
        ("Q" "Quote" entry
         "* ${title} :quote:new:
:PROPERTIES:
:DATE: %(format-time-string \"%D\" (current-time) nil)
:TIME: %(format-time-string \"%H:%M:%S\" (current-time) nil)
:REFERENCE:
:ID: %(org-id-new)
:END:"
         :if-new (file+head "quotes-Jun062021-185530.org"
                            "#+title: Quotes\n\n\n")
         )
        ("l" "Lit Note" plain
         ""
         :if-new (file+head "${slug}-%<%b%d%Y-%H%M%S>.org"
                            "#+filetags: %(kb/insert-lit-category)\n#+title: ${title}\nSource: \nDate: %<%b %d, %Y>")
         :immediate-finish t
         :jump-to-captured t)
        ("r" "Reference without pdf notes" plain
         ""
         :if-new (file+head "${citekey}-${slug}-%<%b%d%Y-%H%M%S>.org"
                            "#+filetags: %(kb/insert-lit-category)\n#+title: ${citekey} ${title}\nSource: ${author-or-editor}\nDate: %<%b %d, %Y>")
         :immediate-finish t)
        ("R" "Reference with pdf notes" plain
         ""
         :if-new (file+head "${citekey}-${title}-%(format-time-string \"%b%d%Y-%H%M%S\" (current-time) nil).org"
                            "#+filetags: %(kb/insert-lit-category)\n#+title: ${citekey} ${title}\nSource: ${author-or-editor}\nDate: %<%b %d, %Y>\n\n* Notes\n:PROPERTIES:\n:Custom_ID: ${citekey}\n:URL: ${url}\n:AUTHOR: ${author-or-editor}\n:NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")\n:NOTER_PAGE:\n:END:\n\n"))
        )
      )

;;;; Org-roam-dailies-capture-templates
(setq org-roam-dailies-capture-templates
      '(("d" "Default" plain
         "* %?
         :PROPERTIES:
         :TIME: %(format-time-string \"%H:%M:%S\" (current-time) nil)
         :END:"
         :if-new
         (file+datetree "journals/%<%Y>.org" week))
        ("w" "Writing" plain
         "* %? :c_writing:
:PROPERTIES:
:TIME: %(format-time-string \"%H:%M:%S\" (current-time) nil)
:END:"
         :if-new
         (file+datetree "journals/%<%Y>.org" week))
        ))

;;;; Custom updating descriptions
;; Credit to @nobiot for helping me
(defun kb/org-roam-update-link-desc--action (buffer)
  "Updates the link descriptions for all org-roam insertions in a given buffer.
Currently limited to only fix links whose UUID was automatically generated by
Org."
  ;; Get all ids in buffer
  (with-current-buffer buffer
    ;; (print buffer) ; Uncomment for bugfixing. Check *Messages* buffer
    (let* (links)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (symbol-value 'org-link-bracket-re) nil t)
          (if (equal (buffer-substring-no-properties ; Get only links with ids (not https, etc)
                      (+ (match-beginning 0) 2)
                      (+ (match-beginning 0) 4))
                     "id")
              (push (buffer-substring-no-properties ; Get only the id
                     (+ (match-beginning 0) 5)
                     (+ (match-beginning 0) 41))
                    links)
            (push "NOT ID" links)))
        (setq links (nreverse links))
        ;; (print links) ; Uncomment for bugfixing. Check *Messages* buffer
        )
      ;; Update all org-roam insertions in buffer
      (save-excursion
        (goto-char (point-min))
        (dolist (link links)
          (let* ((id link)
                 (node (org-roam-populate (org-roam-node-create :id id))))
            (re-search-forward (symbol-value 'org-link-bracket-re) nil t)
            (if (equal (buffer-substring-no-properties ; Limit to only links with ids
                        (+ (match-beginning 0) 2)
                        (+ (match-beginning 0) 4))
                       "id")
                (replace-match (org-link-make-string
                                (concat "id:" (org-roam-node-id node)) (org-roam-node-title node)
                                ))
              (print "Skipped because not an ID!") ; Uncomment for bugfixing. Check *Messages* buffer
              )))
        ))))

(defun kb/org-roam-update-link-desc ()
  "Run kb/org-roam-update-link-desc--action on current buffer or all org-roam
files if called with universal argument."
  (interactive)
  (let* ((checkall (equal current-prefix-arg '(4))) ; Universal-argument check
         (files (if checkall ; Taken from `org-roam-doctor'
                    (org-roam--list-all-files)
                  (unless (org-roam-file-p)
                    (user-error "Not in an org-roam file"))
                  `(,(buffer-file-name)))
                ))
    (save-window-excursion ; Taken from `org-roam-doctor-start'
      (let ((existing-buffers (org-roam-buffer-list)))
        (org-id-update-id-locations)
        (dolist (file files) ; Save all opened files and kill if not opened already
          (let ((buffer (find-file-noselect file)))

            ;; Where I insert my custom function instead
            (kb/org-roam-update-link-desc--action buffer) 

            (unless (memq buffer existing-buffers)
              (with-current-buffer buffer
                (save-buffer))
              (kill-buffer buffer))))
        ))
    (message "Done!")
    ))

;;;; Hide property drawers
;; From https://github.com/org-roam/org-roam/wiki/Hitchhiker%27s-Rough-Guide-to-Org-roam-V2#hiding-properties
(defun kb/org-hide-properties ()
  "Hide all org-mode headline property drawers in buffer. Could be slow if buffer has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t)))))

(defun kb/org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t))

(defun kb/org-toggle-properties ()
  "Toggle visibility of property drawers."
  (interactive)
  (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
      (progn
        (kb/org-show-properties)
        (put 'org-toggle-properties-hide-state 'state 'shown))
    (progn
      (kb/org-hide-properties)
      (put 'org-toggle-properties-hide-state 'state 'hidden))))

(general-define-key
 :keymaps 'org-mode-map
 "C-c p t" 'kb/org-toggle-properties)

;;;; Additional code
;;;;; Number of backlinks in `orgroam' buffer
;; Include number of backlinks for each node in the org-roam buffer.
;; From https://gist.github.com/nobiot/852978b41b1869df3cf9180202f5bbc9
(define-minor-mode nobiot/org-roam-v2-extensions-mode
  "Toggle custom enhancements for Org-roam V2.
It does not work for V1."
  :init-value nil
  :lighter nil
  :global t
  (cond
   (nobiot/org-roam-v2-extensions-mode
    ;; Activate
    (require 'org-roam)
    (advice-add #'org-roam-node--annotation :override #'nobiot/org-roam-node--annotation))
   (t
    ;; Deactivate
    (advice-remove #'org-roam-node--annotation #'nobiot/org-roam-node--annotation))))

(defun nobiot/org-roam-node--annotation (node-title)
  "Return the annotation string for a NODE-TITLE.
This custom function enhances Org-roam's function of the same
name to include number of backlinks for the node."
  (let* ((node (get-text-property 0 'node node-title))
         (tags (org-roam-node-tags node))
         (count)
         (annotation))
    (setq count (caar (org-roam-db-query
                       [:select (funcall count source)
                                :from links
                                :where (= dest $s1)
                                :and (= type "id")]
                       (org-roam-node-id node))))
    (concat annotation
            (when tags (format " (%s)" (string-join tags ", ")))
            (when count (format " [%d]" count)))))

;;;;; Customizing `org-roam-node-find'
;;;;;; Nobiot
;; From https://org-roam.discourse.group/t/find-node-ui-possibilities-for-v2/1422/15
(defun kb/org-roam--tags-to-str (tags)
  "My custom version to convert list of TAGS into a string."
  (if (> (length tags) 0)
      ;; (propertize (concat "(" (mapconcat #'identity tags ", ") ")")
      ;;             'face 'org-roam-dim)
      (propertize (mapconcat (lambda (s) (concat "@" s)) tags ", ") 'face 'org-roam-dim)
    ""
    )
  )
(advice-add #'org-roam--tags-to-str :override #'kb/org-roam--tags-to-str)

(defun nobiot/org-roam-get-file-title (filename)
  "Return the title of the file node for FILENAME."
  (caar (org-roam-db-query
         [:select [title] :from nodes :where (and (= level 0)(= file $s1))] filename)))

(defun nobiot/org-roam-node--format-entry (node width)
  "Formats NODE for display in the results list.
WIDTH is the width of the results list.
nobit has modified one line of this function (see the source comment) to get title of the file."
  (let ((format (org-roam--process-display-format org-roam-node-display-template)))
    (s-format (car format)
              (lambda (field)
                (let* ((field (split-string field ":"))
                       (field-name (car field))
                       (field-width (cadr field))
                       (getter (intern (concat "org-roam-node-" field-name)))
                       (field-value (or (funcall getter node) "")))
                  (when (and (equal field-name "tags")
                             field-value)
                    (setq field-value (org-roam--tags-to-str field-value)))

                  ;; (when (and (equal field-name "file") ; By nobiot
                  ;;            field-value)
                  ;;   (setq field-value (nobiot/org-roam-get-file-title field-value))
                  ;;   )

                  (when (and (equal field-name "file") ; My custom file
                             field-value)
                    (if (string= (nobiot/org-roam-get-file-title field-value) (funcall 'org-roam-node-title node))
                        (setq field-value "")
                      (setq field-value (propertize (concat "#" (nobiot/org-roam-get-file-title field-value)) 'face 'org-roam-dim))
                      )
                    )

                  (when (and (equal field-name "olp")
                             field-value)
                    (setq field-value (string-join field-value " > ")))
                  (if (not field-width)
                      field-value
                    (setq field-width (string-to-number field-width))
                    (truncate-string-to-width
                     field-value
                     (if (> field-width 0)
                         field-width
                       (- width (cdr format)))
                     0 ?\s)))))))
(advice-add #'org-roam-node--format-entry :override #'nobiot/org-roam-node--format-entry)

;;;;;; Slack (current) From
;; From https://github.com/hieutkt/.doom.d/blob/master/config.el#L690-L745 or
;; https://orgroam.slack.com/archives/CV20S23C0/p1626662183035800
(require 'all-the-icons)

(cl-defmethod org-roam-node-filetitle ((node org-roam-node))
  "Return the file TITLE for the node."
  (org-roam-get-keyword "TITLE" (org-roam-node-file node))
  )

(cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
  (let* ((count (caar (org-roam-db-query
                       [:select (funcall count source)
                                :from links
                                :where (= dest $s1)
                                :and (= type "id")]
                       (org-roam-node-id node))))
         )
    (if (> count 0)
        (concat (propertize "=has:backlinks=" 'display (all-the-icons-material "link" :face 'all-the-icons-dblue :height 0.9)) (format "%d" count))
      (concat (propertize "=not-backlinks=" 'display (all-the-icons-material "link" :face 'org-roam-dim :height 0.9))  " ")
      )
    ))

(cl-defmethod org-roam-node-functiontag ((node org-roam-node))
  "The first tag of notes are used to denote note type"
  (let* ((specialtags kb/lit-categories)
         (tags (seq-filter (lambda (tag) (not (string= tag "ATTACH"))) (org-roam-node-tags node)))
         (functiontag (seq-intersection specialtags tags 'string=))
         )
    (concat
     ;; (if functiontag
     ;;     (propertize "=has:functions=" 'display (all-the-icons-octicon "gear" :face 'all-the-icons-silver :v-adjust 0.02 :height 0.8))
     ;;   (propertize "=not-functions=" 'display (all-the-icons-octicon "gear" :face 'org-roam-dim :v-adjust 0.02 :height 0.8))
     ;;   )
     (if functiontag
         (propertize "=@=" 'display (all-the-icons-faicon "tags" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.7))
       (propertize "= =" 'display (all-the-icons-faicon "tags" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.7))
       )
     " "
     (string-join functiontag ", "))
    ))

(cl-defmethod org-roam-node-othertags ((node org-roam-node))
  "Return the file TITLE for the node."
  (let* ((tags (seq-filter (lambda (tag) (not (string= tag "ATTACH"))) (org-roam-node-tags node)))
         (specialtags kb/lit-categories)
         (othertags (seq-difference tags specialtags 'string=))
         )
    (concat
     ;; " "
     ;; (if othertags
     ;;     (propertize "=has:tags=" 'display (all-the-icons-faicon "tags" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.8))
     ;;   (propertize "=not-tags=" 'display (all-the-icons-faicon "tags" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.8))
     ;;   )
     ;; " "
     (if othertags
         (propertize "=@=" 'display "")
       (propertize "= =" 'display "")
       )
     (propertize (string-join othertags ", ") 'face 'all-the-icons-dgreen))
    ))

(cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
  "Return the hierarchy for the node."
  (let* ((title (org-roam-node-title node))
         (olp (mapcar (lambda (s) (if (> (length s) 10) (concat (substring s 0 10)  "...") s)) (org-roam-node-olp node)))
         (level (org-roam-node-level node))
         (filetitle (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
         (shortentitle (if (> (length filetitle) 20) (concat (substring filetitle 0 20)  "...") filetitle))
         (separator (concat " " (all-the-icons-material "chevron_right") " "))
         )
    (cond
     ((>= level 1) (concat (propertize (format "=level:%d=" level) 'display (all-the-icons-material "list" :face 'all-the-icons-blue))
                           " "
                           (propertize shortentitle 'face 'org-roam-dim)
                           (propertize separator 'face 'org-roam-dim)
                           title))
     (t (concat (propertize (format "=level:%d=" level) 'display (all-the-icons-material "insert_drive_file" :face 'all-the-icons-yellow))
                " "
                title))
     )
    ))

;;; org-roam-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-roam-general-rcp)
