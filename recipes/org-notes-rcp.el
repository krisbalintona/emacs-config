;;; org-notes-rcp.el --- Summary
;;
;;; Commentary:
;;
;; This is everything directly used for my note-taking needs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'custom-directories-rcp)
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Denote
(use-package denote
  :straight (denote :type git :host github :repo "emacs-straight/denote" :files ("*" (:exclude ".git")))
  :hook (dired-mode . denote-dired-mode)
  :general
  (kb/note-keys "i" '(denote-link-insert-link :wk "Insert note"))
  :custom
  (denote-directory kb/notes-dir)
  (denote-known-keywords '("project"))
  (denote-prompts '(title keywords)))

;;; Consult-notes
(use-package consult-notes
  :straight (consult-notes :type git :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; In case using `org-roam'
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :hook (before-save . kb/denote-insert-identifier)
  :general
  (kb/note-keys "f" '(consult-notes :wk "Consult-notes"))
  :custom
  (consult-notes-file-dir-sources nil)
  ;; Denote
  (consult-notes-denote-display-id nil)
  (consult-notes-denote-dir t)
  :custom-face
  (denote-faces-link ((t (:foreground "goldenrod3" :slant italic))))
  :init
  (defun kb/denote-insert-identifier ()
    (require 'kb-vulpea)
    (when (and (denote-file-is-note-p (buffer-file-name))
               (not (vulpea-buffer-prop-get "identifier")))
      ;; Move cursor until after the first of following
      ;; properties exists: filetags, date, or title
      (while (and (not (eobp))
                  (cond
                   ((vulpea-buffer-prop-get "filetags")
                    (re-search-forward (rx bol "#+"
                                           (or "F" "f")
                                           (or "I" "i")
                                           (or "L" "l")
                                           (or "E" "e")
                                           (or "T" "t")
                                           (or "A" "a")
                                           (or "G" "g")
                                           (or "S" "s"))
                                       nil t))
                   ((vulpea-buffer-prop-get "date")
                    (re-search-forward (rx bol "#+"
                                           (or "D" "d")
                                           (or "A" "a")
                                           (or "T" "t")
                                           (or "E" "e"))
                                       nil t))
                   ((vulpea-buffer-prop-get "title")
                    (re-search-forward (rx bol "#+"
                                           (or "T" "t")
                                           (or "I" "i")
                                           (or "T" "t")
                                           (or "L" "l")
                                           (or "E" "e"))
                                       nil t))))
        (if (save-excursion (end-of-line) (eobp))
            (progn
              (end-of-line)
              (insert "\n"))
          (forward-line)
          (beginning-of-line)))
      (insert "#+identifier: " (denote-retrieve-filename-identifier (buffer-file-name)) "\n")))
  :config
  (when (locate-library "org-roam")
    (consult-notes-denote-mode))

  ;; Customize `consult-notes' interface for `denote'. Made GitHub issue here:
  ;; https://github.com/mclear-tools/consult-notes/issues/27
  (defconst consult-notes-denote--source
    (list :name     (propertize "Notes" 'face 'consult-notes-sep)
          :narrow   ?n
          :category 'consult-notes-category
          :annotate #'consult-notes-denote--annotate
          :items    (lambda ()
                      (let* ((max-width 0)
                             (cands (mapcar (lambda (f)
                                              (let* ((id (denote-retrieve-filename-identifier f))
                                                     (title-value (denote-retrieve-title-value f (denote-filetype-heuristics f)))
                                                     (title (if consult-notes-denote-display-id
                                                                (concat id " " title-value)
                                                              title-value))
                                                     (dir (file-relative-name (file-name-directory f) denote-directory))
                                                     (keywords (denote-extract-keywords-from-path f)))
                                                (let ((current-width (string-width title)))
                                                  (when (> current-width max-width)
                                                    (setq max-width (+ 24 current-width))))
                                                (propertize title 'denote-path f 'denote-keywords keywords)))
                                            (denote-directory-files))))
                        (mapcar (lambda (c)
                                  (let* ((keywords (get-text-property 0 'denote-keywords c))
                                         (path (get-text-property 0 'denote-path c))
                                         (dirs (directory-file-name (file-relative-name (file-name-directory path) denote-directory))))
                                    (concat c
                                            ;; align keywords
                                            (propertize " " 'display `(space :align-to (+ left ,(+ 2 max-width))))
                                            (format "%18s"
                                                    (if keywords
                                                        (concat (propertize "#" 'face 'consult-notes-name)
                                                                (propertize (mapconcat 'identity keywords " ") 'face 'consult-notes-name))
                                                      ""))
                                            (when consult-notes-denote-dir (format "%18s" (propertize dirs 'face 'consult-notes-name))))))
                                cands)))
          ;; Custom preview
          :state  #'consult-notes-denote--state
          ;; Create new note on match fail
          :new     #'consult-notes-denote--new-note))

  (consult-customize
   consult-notes
   :prompt "Go to..."
   :preview-key (kbd "C-M-;")))

;;; org-notes-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-notes-rcp)
