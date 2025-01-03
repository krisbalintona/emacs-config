;;; org-roam-general-rcp.el --- Summary
;;
;;; Commentary:
;;
;; This is all the configuration of the org-roam package
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'custom-directories-rcp)
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Org-roam
(use-package org-roam
  :disabled                             ; In favor of denote
  :commands kb/find-blog-files-org
  :hook
  ((org-roam-mode . hide-mode-line-mode)
   (org-roam-mdoe . visual-line-mode))
  :general
  (:keymaps 'org-roam-mode-map ; To add back mouse click to visit the node in the backlink buffer
            "<tab>" 'magit-section-toggle
            [mouse-1] #'org-roam-buffer-visit-thing)
  (kb/note-keys
    "f" '(org-roam-node-find :wk "Find file")
    "F" '((lambda ()
            (interactive)
            (org-roam-node-find t))
          :wk "Find file other window")

    "i" '(org-roam-node-insert :wk "Insert note")

    "l" '(org-roam-buffer-toggle :wk "Toggle Roam buffer")
    "L" '(org-roam-buffer-display-dedicated :wk "New Roam buffer")

    "b" '(org-roam-db-sync :wk "Build cache"))
  :custom
  (org-roam-directory kb/notes-dir)
  (org-roam-dailies-directory (concat kb/notes-dir "journals/"))
  (org-roam-file-exclude-regexp (rx (one-or-more ".draft")))
  (org-roam-db-node-include-function
   #'(lambda ()
       "Exclude nodes with the ATTACH tag"
       (not (member "ATTACH" (org-get-tags)))))

  (org-roam-verbose nil)                ; Don't echo messages that aren't errors
  (org-use-tag-inheritance nil) ; For the way I use lit notes not to transfer source type to evergreen note status
  (org-roam-db-gc-threshold most-positive-fixnum) ; Minimize GC pauses while updating the database

  (org-roam-node-default-sort 'file-atime)
  (org-roam-node-display-template ; Taken from https://jethrokuan.github.io/org-roam-guide/
   (concat " ${type:13}" "${comma-tags:35}" "${hierarchy:*}"))

  (org-roam-completion-everywhere t)    ; Org-roam completion everywhere
  (org-roam-link-auto-replace t) ; Replace roam link type with file link type when possible
  (org-roam-completion-functions
   `(org-roam-complete-link-at-point
     org-roam-complete-everywhere
     ,(cape-company-to-capf #'company-yasnippet) ; Requires `cape'
     ))
  (org-roam-mode-sections
   '(org-roam-backlinks-section
     org-roam-reflinks-section
     org-roam-unlinked-references-section))
  :init
  ;; Rename files to match their node titles
  (defun kb/org-roam-rename-to-new-title--single ()
    "Rename an org-roam file to its file-level org-roam node title.
Taken from
https://org-roam.discourse.group/t/does-renaming-title-no-longer-renames-the-filename/2018/6"
    (interactive)
    (org-with-wide-buffer ; Widen first in order to properly work in existing buffers which are narrowed
     (when-let*
         ((old-file (buffer-file-name))
          (is-roam-file (org-roam-file-p old-file))
          (file-node (save-excursion
                       (goto-char 1)
                       (org-roam-node-at-point)))
          (slug (org-roam-node-slug file-node))
          (new-file (expand-file-name (concat slug ".org")))
          (different-name? (not (string-equal old-file new-file))))
       (rename-file old-file new-file)
       (rename-buffer new-file)
       (set-visited-file-name new-file)
       (set-buffer-modified-p nil))))
  (defun kb/org-roam-rename-to-new-title-all ()
    "Export all org-roam files to Hugo in my blogging directory."
    ;; FIXME 2022-02-19: Leaves the opened buffers alive
    (interactive)
    (dolist (fil (org-roam--list-files kb/notes-dir))
      (with-current-buffer (find-file-noselect fil)
        (kb/org-roam-rename-to-new-title--single))))
  :config
  (org-roam-db-autosync-mode)

  ;; Org roam buffer section visibility
  (add-to-list 'magit-section-initial-visibility-alist '(org-roam-backlinks . show))
  (add-to-list 'magit-section-initial-visibility-alist '(org-roam-node-section . hide))

  ;; Org ID link face
  (defface kb/org-roam-link
    '((default :foreground "goldenrod3" :slant italic))
    "My face for org ID links.")
  (org-link-set-parameters "id" :face 'kb/org-roam-link))


;;; Customized `org-roam-node-find' functions
;; From https://github.com/hieutkt/.doom.d/blob/master/config.el#L690-L745 or
;; https://orgroam.slack.com/archives/CV20S23C0/p1626662183035800
(with-eval-after-load 'org-roam
  (cl-defmethod org-roam-node-filetitle ((node org-roam-node))
    "Return the file TITLE for the node."
    (org-roam-get-keyword "TITLE" (org-roam-node-file node)))

  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE.

Mostly taken from https://jethrokuan.github.io/org-roam-guide/."
    (condition-case nil
        (propertize
         (file-name-nondirectory
          (directory-file-name
           (file-name-directory
            (file-relative-name (org-roam-node-file node) org-roam-directory))))
         'face 'all-the-icons-orange)
      (error "")))

  (cl-defmethod org-roam-node-comma-tags ((node org-roam-node))
    "Return the TAGS of the node."
    (let* ((tags (seq-filter (lambda (tag) (not (string= tag "ATTACH"))) (org-roam-node-tags node))))
      (propertize (string-join tags ", ") 'face 'all-the-icons-dgreen)
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
       ((>= level 1)
        (concat (propertize shortentitle 'face 'org-roam-dim)
                (propertize separator 'face 'org-roam-dim)
                title))
       (t title)))))

;;; Capture templates
;;;; Org-roam-capture-templates
(defvar kb/org-roam-node-types
  '("working" "video" "book" "podcast" "article" "website" "journal" "quote" "structure" "musing")
  "The main categories of inputs I process.")

(defun kb/insert-lit-category ()
  "Insert type of literature note sources."
  (completing-read "Category: " kb/org-roam-node-types)
  )

(setq org-roam-capture-templates
      '(("d" "Default" plain
         ""
         :if-new (file+head "${slug}.org"
                            "#+title:${title}\n")
         :unnarrowed t
         :immediate-finish t)
        ("p" "Paper" plain
         ""
         :if-new (file+head "papers/${slug}.org"
                            "#+title:${title}
#+filetags: %^G
#+latex_class: mla
#+professor: %^{Prof}
#+course: %^{Course (paper header)}\n
#+date:
* Works Cited :ignore:\n
\\newpage
\\center\n
#+print_bibliography:")
         :unnarrowed t
         :immediate-finish t)
        ("b" "Blog" plain
         ""
         :if-new (file+head "blog/${slug}.org"
                            "#+title:${title}
#+filetags:
#+hugo_bundle:
#+export_file_name: index
#+hugo_tags:
#+hugo_categories:
#+hugo_publishdate:
#+hugo_lastmod:
#+hugo_custom_front_matter:
#+hugo_draft: true\n")
         :unnarrowed t
         :immediate-finish t)
        ))

;;;; Org-roam-dailies-capture-templates
(with-eval-after-load 'org-roam
  (require 'org-roam-dailies)
  (setq org-roam-dailies-capture-templates
        '(("d" "Default" plain
           "* %?
         :PROPERTIES:
         :TIME: %(format-time-string \"%H:%M:%S\" (current-time) nil)
         :END:"
           :if-new
           (file+datetree "%<%Y>.org" week)
           :unnarrowed nil)
          ("m" "Musing" plain
           "* %? :musing:
:PROPERTIES:
:TIME: %(format-time-string \"%H:%M:%S\" (current-time) nil)
:END:"
           :if-new
           (file+datetree "%<%Y>.org" week)
           :unnarrowed nil
           :jump-to-captured t
           :immediate-finish t)
          )
        )
  )

;;; Additional code
;;;; Hide property drawers
;; From https://github.com/org-roam/org-roam/wiki/Hitchhiker%27s-Rough-Guide-to-Org-roam-V2#hiding-properties
(defun kb/org-hide-properties ()
  "Hide all `org-mode' headline property drawers in buffer. Could be slow if buffer has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t)))))

(defun kb/org-show-properties ()
  "Show all `org-mode' property drawers hidden by org-hide-properties."
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

(bind-key "C-c p t" #'kb/org-toggle-properties 'org-mode-map)

;;;; Find a node which links to any other given node
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

;;;; Number of backlinks in `org-roam' minibuffer
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

;;;; Custom updating descriptions
;; Credit to @nobiot for helping me
(defun kb/org-roam-update-link-desc--action (buffer)
  "Update the link descriptions for all org-roam insertions in a given BUFFER.
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
                    (org-roam-list-files)
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

;;;; Copy node's file
(with-eval-after-load 'org-roam
  (defun kb/org-roam-copy-node-file ()
    (interactive)
    (let* ((node (org-roam-node-read))
           (file-path (org-roam-node-file node))
           (file-directory (file-name-directory file-path))
           (file-name-base (file-name-base (file-name-nondirectory file-path)))
           (new-file-name (concat file-name-base "-copy.org"))
           (new-file-path (file-name-concat file-directory new-file-name)))
      (copy-file file-path new-file-path)
      (find-file new-file-path))))

;;; org-roam-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-roam-general-rcp)
