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
  :functions kb/denote-search-from-id
  :straight (denote :type git :host github :repo "emacs-straight/denote" :files ("*" (:exclude ".git")))
  :hook ((dired-mode . denote-dired-mode)
         (before-save . kb/denote-insert-identifier-maybe))
  :general (kb/note-keys
             "i" 'denote-link-insert-link
             "ta" 'denote-keywords-add
             "tr" 'denote-keywords-remove)
  :custom
  (denote-directory kb/notes-dir)
  (denote-known-keywords '("project"))
  (denote-prompts '(title keywords))
  :init
  ;; Standardizing note front-matter
  (defun kb/denote-insert-identifier-maybe ()
    (require 'org-agenda-general-rcp)
    (when (and (denote-file-is-note-p (buffer-file-name))
               (not (kb/note-buffer-prop-get "identifier")))
      (save-excursion
        (beginning-of-buffer)
        ;; Move cursor until after the first of following
        ;; properties exists: filetags, date, or title
        (while (and (not (eobp))
                    (cond
                     ((kb/note-buffer-prop-get "filetags")
                      (re-search-forward (rx bol "#+"
                                             (or "F" "f")
                                             (or "I" "i")
                                             (or "L" "l")
                                             (or "E" "e")
                                             (or "T" "t")
                                             (or "A" "a")
                                             (or "G" "g")
                                             (or "S" "s")
                                             ":")
                                         (point-max) t))
                     ((kb/note-buffer-prop-get "date")
                      (re-search-forward (rx bol "#+"
                                             (or "D" "d")
                                             (or "A" "a")
                                             (or "T" "t")
                                             (or "E" "e")
                                             ":")
                                         (point-max) t))
                     ((kb/note-buffer-prop-get "title")
                      (re-search-forward (rx bol "#+"
                                             (or "T" "t")
                                             (or "I" "i")
                                             (or "T" "t")
                                             (or "L" "l")
                                             (or "E" "e")
                                             ":")
                                         (point-max) t))))
          (cond
           ((save-excursion (end-of-line) (eobp))
            (end-of-line)
            (insert "\n"))
           (t
            (forward-line)
            (beginning-of-line))))
        (insert "#+identifier: " (denote-retrieve-filename-identifier (buffer-file-name)) "\n"))))
  (defun kb/denote-rearrange-keywords-maybe ()
    (let* ((f (buffer-file-name))
           (file-type (denote-filetype-heuristics f))
           (cur-keywords (seq-uniq (denote-retrieve-keywords-value f file-type)))
           (sorted-keywords (denote-keywords-sort (copy-list cur-keywords))))
      (denote--rewrite-keywords f sorted-keywords file-type)
      ;; Add empty filetags property if one isn't already present
      (unless (kb/note-buffer-prop-get "filetags")
        (beginning-of-buffer)
        (while (and (not (eobp))
                    (cond
                     ((kb/note-buffer-prop-get "date")
                      (re-search-forward (rx bol "#+"
                                             (or "D" "d")
                                             (or "A" "a")
                                             (or "T" "t")
                                             (or "E" "e")
                                             ":")
                                         (point-max) t))
                     ((kb/note-buffer-prop-get "title")
                      (re-search-forward (rx bol "#+"
                                             (or "T" "t")
                                             (or "I" "i")
                                             (or "T" "t")
                                             (or "L" "l")
                                             (or "E" "e")
                                             ":")
                                         (point-max) t))))
          (cond
           ((save-excursion (end-of-line) (eobp))
            (end-of-line)
            (insert "\n"))
           (t
            (forward-line)
            (beginning-of-line))))
        (insert "#+filetags:\n"))))
  (defun kb/denote-ensure-title-space ()
    (save-excursion
      (beginning-of-buffer)
      (let ((end-of-title-keyword
             (re-search-forward (rx bol "#+"
                                    (or "T" "t")
                                    (or "I" "i")
                                    (or "T" "t")
                                    (or "L" "l")
                                    (or "E" "e")
                                    ":")
                                (point-max) t)))
        (goto-char end-of-title-keyword)
        (just-one-space))))
  (defun kb/denote-standardize-front-matter ()
    (interactive)
    (require 'denote)
    (dolist (file (denote-directory-files))
      ;; Export all the files
      (with-current-buffer (find-file-noselect file)
        (read-only-mode -1)
        (kb/denote-insert-identifier-maybe)
        (kb/denote-rearrange-keywords-maybe)
        (kb/denote-ensure-title-space)
        (kb/format-buffer-indentation)
        (denote-rename-file-using-front-matter file t)

        (unless (member (get-buffer (buffer-name)) (buffer-list)) ; Kill buffer unless it already exists
          (kill-buffer)))))

  ;; Return denote file path based on ID
  (defun kb/denote-search-from-id (id)
    (let* ((full-path (car (cl-remove-if-not
                            (lambda (f) (string-match-p (rx (literal id)) f))
                            (denote-directory-files))))
           (title (denote-retrieve-title-value full-path 'org)))
      title)))

;;; Consult-notes
(use-package consult-notes
  :straight (consult-notes :type git :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; In case using `org-roam'
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :general
  (kb/note-keys "f" '(consult-notes :wk "Consult-notes"))
  :custom
  ;; File paths must have ending slashing. See
  ;; https://github.com/mclear-tools/consult-notes/issues/26#issuecomment-1356038580
  (consult-notes-file-dir-sources
   `(("Agenda" ?a ,(concat kb/agenda-dir "/"))
     ("Papers" ?p ,(expand-file-name "papers/" org-directory))
     ))
  ;; Denote
  (consult-notes-denote-display-id nil)
  (consult-notes-denote-dir t)
  :custom-face
  (denote-faces-link ((t (:foreground "goldenrod3" :slant italic))))
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
                                                                (propertize (mapconcat 'identity keywords " #") 'face 'consult-notes-name))
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
   :preview-key (kbd "C-M-;"))

  (advice-add 'denote-rename-file-using-front-matter :around
              #'(lambda (orig-fun &rest args)
                  (let ((save-silently t))
                    (apply orig-fun args)))))

;;; org-notes-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-notes-rcp)
