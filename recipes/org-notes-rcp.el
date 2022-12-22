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
  :after consult
  :straight (consult-notes :type git :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; In case using `org-roam'
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :general
  (kb/note-keys "f" '(consult-notes :wk "Consult-notes"))
  :custom
  (consult-notes-file-dir-sources nil)
  ;; Denote
  (consult-notes-denote-display-id nil)
  (consult-notes-denote-dir t)
  :config
  (when (locate-library "org-roam")
    (consult-notes-denote-mode))

  (consult-customize
   consult-notes
   :prompt "Go to..."
   :preview-key (kbd "C-M-;")))

;;; org-notes-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-notes-rcp)
