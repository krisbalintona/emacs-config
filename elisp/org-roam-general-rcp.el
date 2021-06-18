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
(setq org-roam-capture-templates
      '(("d" "Default" plain
         ""
         :if-new (file+head "${slug}-%<%b%d%Y-%H%M%S>.org"
                            "#+title: ${title}\n")
         :immediate-finish t)
        ("e" "Evergreen" plain
         ""
         :if-new (file+head "${slug}-%<%b%d%Y-%H%M%S>.org"
                            "#+filetags: :refine:\n#+title: ${title}\nReference: \n\n\n"))
        ("Q" "Quote" entry
         "* ${title} :quote:
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
                            "#+filetags: %^G\n#+title: ${title}\nSource: \nDate: %<%b %d, %Y>")
         :immediate-finish t)
        ("r" "Reference without pdf notes" plain
         ""
         :if-new (file+head "${citekey}-${slug}-%<%b%d%Y-%H%M%S>.org"
                            "#+filetags: %^G\n#+title: ${citekey} ${title}\nSource: ${author-or-editor}\nDate: %<%b %d, %Y>")
         :immediate-finish t)
        ("R" "Reference with pdf notes" plain
         ""
         :if-new (file+head "${citekey}-${title}-%(format-time-string \"%b%d%Y-%H%M%S\" (current-time) nil).org"
                            "#+filetags: %^G\n#+title: ${citekey} ${title}\nSource: ${author-or-editor}\nDate: %<%b %d, %Y>\n\n* Notes\n:PROPERTIES:\n:Custom_ID: ${citekey}\n:URL: ${url}\n:AUTHOR: ${author-or-editor}\n:NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")\n:NOTER_PAGE:\n:END:\n\n"))
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

;;; org-roam-general-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-roam-general-rcp)
