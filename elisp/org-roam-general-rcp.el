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
    ;; :quelpa (org-roam :fetcher git :url "https://github.com/org-roam/org-roam" :branch "master") ; Incompatible with straight.el
    :straight (org-roam :type git :host github :repo "org-roam/org-roam" :branch "origin/v2") ; Org-roam v2
    :after company ; Necessary for some reason
    ;; :hook (window-setup-hook . 
    :custom
    (org-roam-directory kb/roam-dir)
    (org-roam-dailies-directory (concat kb/roam-dir "journals/"))
    (org-roam-verbose nil) ; Don't echo messages that aren't errors
    (org-roam-completion-everywhere t) ; Org-roam completion everywhere
    (org-roam-link-auto-replace t) ; Replace roam link type with file link type when possible
    ;; (org-roam-db-gc-threshold most-positive-fixnum) ; Temporarily increase GC threshold during intensive org-roam operations
    (org-roam-db-gc-threshold (* 3 838861))

    (org-use-tag-inheritance nil) ; For the way I use lit notes not to transfer source type to evergreen note status

    (org-roam-node-display-template "${title:80} ${tags:*}") ; Format of items in `org-roam-node-find'
    :config
    (org-roam-setup) ; Replacement for org-roam-mode
    ;; (add-to-list 'org-open-at-point-functions 'org-roam-open-id-at-point)

    (add-hook 'org-roam-buffer-mode-hook #'hide-mode-line-mode) ; Hide modeline in org-roam buffer; Doesn't work b/c no hook anymore
    (set-face-attribute 'org-link nil :foreground "goldenrod3" :bold nil :italic t :font kb/variable-pitch-font :height 145 :underline nil)
    (set-face-attribute 'bookmark-face nil :foreground nil :background nil) ; This is the fact used for captures. Its background is ugly

    ;; To add back mouse click to visit the node in the backlink buffer
    (define-key org-roam-mode-map [mouse-1] #'org-roam-visit-thing)

    (kb/leader-keys
      "nf" '(org-roam-node-find :which-key "Find file")
      "ni" '(org-roam-node-insert :which-key "Insert note")
      "nt" '(org-roam-tag-add :which-key "Add tag")
      "nT" '(org-roam-tag-remove :which-key "Remove tag")
      "nN" '(org-id-get-create :which-key "Add ID")
      "nI" '(org-roam-jump-to-index :which-key "Go to index")
      "nl" '(org-roam-buffer-toggle :which-key "Toggle Roam buffer")
      "nL" '(org-roam-db-sync :which-key "Build cache")
      "nc" '(org-roam-capture :which-key "Roam capture")
      )
    )

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

;;;; Org-roam-capture-templates
(defun kb/insert-lit-category ()
  "Common types of literature note sources."
  (completing-read "Category: "
                   '(":video:" ":book:" ":podcast:" ":article:" ":website:" ":journal_article:"))
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
        ))

;;;; Org-roam-dailies-capture-templates
(setq org-roam-dailies-capture-templates
      '(("d" "Default" entry
         "* %?
:PROPERTIES:
:TIME: %(format-time-string \"%H:%M:%S\" (current-time) nil)
:END:"
         :if-new (file+head "journals/%<%d-%m-%Y>.org"
                            "#+title: %<%b %d, %Y>\n\n"))
        ("w" "Writing" entry
         "* %? :c_writing:
:PROPERTIES:
:TIME: %(format-time-string \"%H:%M:%S\" (current-time) nil)
:END:"
         :if-new (file+head "journals/%<%d-%m-%Y>.org"
                            "#+title: %<%b %d, %Y>\n\n"))
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
                  (unless (org-roam--org-roam-file-p)
                    (user-error "Not in an org-roam file"))
                  `(,(buffer-file-name)))
                ))
    (save-window-excursion ; Taken from `org-roam-doctor-start'
      (let ((existing-buffers (org-roam--get-roam-buffers)))
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

;;; org-roam-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-roam-general-rcp)
