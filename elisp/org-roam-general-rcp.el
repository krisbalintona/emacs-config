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

  (org-footnote-section nil) ; Don't put footnotes in headline

  ;; How it appears in the minibuffer
  ;; (org-roam-node-display-template (concat " ${title:130}" (propertize " ⸽ ${file:50} ⸽ ${tags:20}" 'face 'org-roam-dim)))
  (org-roam-node-display-template (concat "${backlinkscount:16} " "${functiontag:16} " "${othertags:13} " "${hierarchy:183}"))

  ;; Roam buffer format
  (org-roam-mode-section-functions
   '(org-roam-backlinks-section
     org-roam-reflinks-section
     org-roam-unlinked-references-section
     )
   )
  :init
  (setq org-roam-v2-ack t) ; Remove startup message which warns that this is v2

  (defun kb/roam-filter-journals (node binary)
    "Takes NODE. If BINARY is `t', then return all nodes that aren't in the
journals directory."
    (if binary
        (not (string-equal
              (concat
               (expand-file-name kb/roam-dir)
               "journals/"
               (format-time-string "%Y" (current-time))
               ".org")
              (org-roam-node-file node))
             )
      (string-equal
       (concat
        (expand-file-name kb/roam-dir)
        "journals/"
        (format-time-string "%Y" (current-time))
        ".org")
       (org-roam-node-file node))
      )
    )
  :config
  (org-roam-setup)

  ;; Org roam buffer
  (add-to-list 'magit-section-initial-visibility-alist '(org-roam-node-section . hide))
  (add-to-list 'magit-section-initial-visibility-alist '(org-roam-backlinks . hide))
  (add-hook 'org-roam-mode-hook (lambda ()
                                  (hide-mode-line-mode) ; Hide modeline in org-roam buffer
                                  (visual-line-mode)
                                  )
            )
  (with-eval-after-load 'atom-one-dark-theme
    (custom-theme-set-faces
     `atom-one-dark
     `(org-link ((t (:foreground "goldenrod3" :bold nil :italic t :font ,kb/variable-pitch-font :height 145 :underline nil))))
     `(bookmark-face ((t (:foreground nil :background nil))))
     )
    )

  ;; Annoying. Closes frame when I want to add a footnote
 (general-define-key "C-x C-c" 'nil)

  ;; To add back mouse click to visit the node in the backlink buffer
  (general-define-key
   :keymaps 'org-roam-mode-map
   [mouse-1] #'org-roam-visit-thing
   )

  (kb/leader-keys
    "nf" '((lambda ()
             (interactive)
             (org-roam-node-visit (org-roam-node-read nil
                                                      (lambda (node) (kb/roam-filter-journals node t))
                                                      )
                                  nil)
             )
           :which-key "Find file")
    "nF" '((lambda ()
             (interactive)
             (org-roam-node-visit (org-roam-node-read nil
                                                      (lambda (node) (kb/roam-filter-journals node t))
                                                      )
                                  t)
             )
           :which-key "Find file other window")

    "ni" '(org-roam-node-insert :which-key "Insert note")

    "nc" '(org-roam-capture :which-key "Roam capture")

    "nN" '(org-id-get-create :which-key "Add ID")
    "nt" '(org-roam-tag-add :which-key "Add tag")
    "nT" '(org-roam-tag-remove :which-key "Remove tag")

    "nh" '((lambda ()
             (interactive)
             (find-file "~/Documents/org-database/roam/index-Jun042021-183426.org")
             )
           :which-key "Go to index")

    "nl" '(org-roam-buffer-toggle :which-key "Toggle Roam buffer")
    "nL" '(org-roam-buffer :which-key "New Roam buffer")

    "nb" '(org-roam-db-sync :which-key "Build cache")

    "nd" '(:ignore t :which-key "Roam dailies")
    "ndd" '((lambda ()
              (interactive)
              (org-roam-node-visit (org-roam-node-read nil
                                                       (lambda (node) (kb/roam-filter-journals node nil))
                                                       ))
              )
            :which-key "Find date")
    "ndt" '(org-roam-dailies-find-today :which-key "Today")
    "ndm" '(org-roam-dailies-find-tomorrow :which-key "Tomorrow")
    "ndy" '(org-roam-dailies-find-yesterday :which-key "Yesterday")
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
         (file+datetree "%<%Y>.org" week))
        ("w" "Writing" plain
         "* %? :c_writing:
:PROPERTIES:
:TIME: %(format-time-string \"%H:%M:%S\" (current-time) nil)
:END:"
         :if-new
         (file+datetree "%<%Y>.org" week))
        )
      )

;;;; Customizing `org-roam-node-find'
;; From https://github.com/hieutkt/.doom.d/blob/master/config.el#L690-L745 or
;; https://orgroam.slack.com/archives/CV20S23C0/p1626662183035800
(with-eval-after-load 'org-roam
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
  )

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
 "C-c p t" 'kb/org-toggle-properties
 )

;;;; Additional code
;;;;; Only update database while idle
;; From
;; https://orgmode-exocortex.com/2021/07/22/configure-org-roam-v2-to-update-database-only-when-idle/
;; (with-eval-after-load 'org-roam
;;   ;; queue for files that will be updated in org-roam-db when emacs is idle
;;   (setq org-roam-db-update-queue (list))
;;   ;; save the original update function;
;;   (setq orig-update-file (symbol-function 'org-roam-db-update-file))
;;   ;; then redefine the db update function to add the filename to a queue
;;   (defun org-roam-db-update-file (&optional file-path)
;;     ;; do same logic as original to determine current file-path if not passed as arg
;;     (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
;;     (message "org-roam: scheduling update of %s" file-path)
;;     (if (not (memq file-path org-roam-db-update-queue))
;;         (push file-path org-roam-db-update-queue)))

;;   ;; this function will be called when emacs is idle for a few seconds
;;   (defun org-roam-db-idle-update-files ()
;;     ;; go through queued filenames one-by-one and update db
;;     ;; if we're not idle anymore, stop. will get rest of queue next idle.
;;     (while (and org-roam-db-update-queue (current-idle-time))
;;       ;; apply takes function var and list
;;       (apply orig-update-file (list (pop org-roam-db-update-queue)))))

;;   ;; we'll only start updating db if we've been idle for this many seconds
;;   (run-with-idle-timer 5 t #'org-roam-db-idle-update-files)
;;   )

;;;;; Find a node which links to any other given node
;; From
;; https://ag91.github.io/blog/2021/03/12/find-org-roam-notes-via-their-relations/
(defun kb/find-node-backlink (arg &optional node choices)
  "Navigate notes by link. With universal ARG try to use only to navigate the tags of the current note. Optionally takes a selected NODE and filepaths CHOICES."
  (interactive "P")
  (let* ((depth (if (numberp arg) arg 1))
         (choices
          (or choices
              (when arg
                (-map #'org-roam-backlink-target-node (org-roam-backlinks-get (org-roam-node-from-id (or (ignore-errors (org-roam-node-id node))
                                                                                                         (org-id-get-create))))))))
         (all-notes (org-roam-node--completions))
         (completions
          (or (--filter (-contains-p choices (cdr it)) all-notes) all-notes))
         (next-node
          ;; taken from org-roam-node-read
          (let* ((nodes completions)
                 (node (completing-read
                        "Node: "
                        (lambda (string pred action)
                          (if (eq action 'metadata)
                              '(metadata
                                (annotation-function . (lambda (title)
                                                         (funcall org-roam-node-annotation-function
                                                                  (get-text-property 0 'node title))))
                                (category . org-roam-node))
                            (complete-with-action action nodes string pred))))))
            (or (cdr (assoc node nodes))
                (org-roam-node-create :title node)))
          )
         )
    (if (equal node next-node)
        (org-roam-node-visit node)
      (kb/find-node-backlink nil next-node (cons next-node (-map #'org-roam-backlink-source-node (org-roam-backlinks-get next-node))))
      )))

;;;;; Number of backlinks in `org-roam' minibuffer
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

;;; org-roam-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-roam-general-rcp)
