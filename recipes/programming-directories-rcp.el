;;; programming-directories-rcp.el --- Traversing and seeing directories  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Everything to do with navigating and managing directories.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Dired
;;;;; This
;; Emacs' file manager
(use-package dired
  :ensure nil
  :gfhook 'dired-hide-details-mode
  :general
  (:keymaps 'dired-mode-map
            :states 'normal
            "h" 'dired-up-directory
            "l" 'dired-find-file)
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)                 ; Guess default target directory?
  (dired-hide-details-hide-symlink-targets nil) ; Don't hide symlink targets
  (dired-kill-when-opening-new-dired-buffer t)  ; Basically `dired-single'
  (dired-listing-switches "-alhgv") ; Flags `dired' passes to `ls'
  (dired-omit-verbose nil)
  ;; Always copy/delete recursively?
  (dired-recursive-copies  'always)
  (dired-recursive-deletes 'top)
  ;; Ask whether destination dirs should get created when copying/removing files.
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t)
  :config
  (general-unbind '(normal visual motion) dired-mode-map "SPC") ; Unbind SPC so leader key is available

  ;; Mark files and do a sexp in their buffers. Based off
  ;; https://superuser.com/a/176629
  (defun kb/dired-do-sexp (sexp &optional prefix)
    "Run SEXP in marked dired files. If called with
PREFIX (`universal-argument' if interactively), run a particular
command."
    (interactive (list (if current-prefix-arg
                           (read-extended-command) ; Command
                         (read--expression "Run expression on marked files: ")) ; Sexp
                       current-prefix-arg))
    (save-window-excursion
      (mapc #'(lambda (filename)
                (with-current-buffer (find-file-noselect filename)
                  (if prefix
                      (call-interactively (intern sexp))             ; Command
                    (funcall-interactively 'eval-expression sexp)))) ; Sexp
            (dired-get-marked-files)))))

;;;;; Dired-x
(use-package dired-x
  :disabled
  :ensure nil
  :hook (dired-mode . dired-omit-mode))

;;;;; All-the-icons-dired
;; Add icons which represent file types
(use-package all-the-icons-dired
  :ghook 'dired-mode-hook
  :gfhook '(lambda () (setq-local all-the-icons-scale-factor 0.95))
  :custom
  (all-the-icons-dired-monochrome nil) ; Icon the same color as the text on the line?
  (dired-kill-when-opening-new-dired-buffer t) ; Kill dired buffer when opening new directory
  )

;;;;; Image-dired
(use-package image-dired
  :ensure nil
  :hook (dired-mode . image-dired-minor-mode)
  :custom
  (image-dired-thumb-size 150)          ; Slightly larger thumbnails
  (image-dired-external-viewer "xdg-open")
  (image-dired-thumb-relief 0)
  (dired-mouse-drag-files t))

;;;;; Dired-git
;; Show git information in dired
(use-package dired-git
  :disabled
  :ghook 'dired-mode-hook
  :custom
  (dired-git-disable-dirs '("~/"))
  (dired-git-parallel 7))               ; Number of parallel processes

;;;;; Dired-open
;; Override how dired opens files with certain extensions
(use-package dired-open
  :after dired
  :custom
  (dired-open-extensions '(("odt" . "soffice -writer")
                           ("docx" . "soffice -writer")
                           ("mp4" . "vlc")
                           ("mp3" . "vlc")
                           ("mkv" . "vlc")
                           ))
  :config
  ;; Try to use `xdg-open' before anything else
  (add-to-list 'dired-open-functions #'dired-open-xdg t)) ; Doesn't work as expected!

;;;;; Dired-hide-dotfiles
;; Hide dotfiles
(use-package dired-hide-dotfiles
  :general (:keymaps 'dired-mode-map
                     :states 'normal
                     "H" 'dired-hide-dotfiles-mode)
  :custom
  (dired-hide-dotfiles-verbose nil)) ; No announcements about hiding in echo area

;;;;; Dired-hist
;; History for dired buffers
(use-package dired-hist
  :demand
  ;; :ensure (:host github :repo "karthink/dired-hist")
  :vc (:url "https://github.com/karthink/dired-hist.git")
  :after dired
  :general
  (:keymaps 'dired-mode-map
            "l" 'dired-hist-go-back
            "r" 'dired-hist-go-forward)
  :config
  (dired-hist-mode 1))

;;;;; Consult-dir
(use-package consult-dir
  :general ([remap dired] 'consult-dir))

;;;;; Consult-buffer integration
(with-eval-after-load 'consult
  (defvar kb/consult-buffer--dired-source
    (list :name     "Dired Buffers"
          :category 'buffer
          :narrow   ?d
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    'consult--buffer-state
          :action   'consult--buffer-action
          :items (lambda ()
                   (mapcar #'buffer-name
                           (seq-filter
                            (lambda (x)
                              (eq (buffer-local-value 'major-mode x) 'dired-mode))
                            (buffer-list))))))
  (add-to-list 'consult-buffer-sources #'kb/consult-buffer--dired-source 'append))

;;;; Misc
;;;;; Affe
;; Blazing fast fuzzy finder
(use-package affe
  :disabled            ; NOTE 2024-03-17: Trying to defer to `project-find-file'
  :after orderless
  :general (:keymaps 'project-prefix-map
                     [remap project-find-file] 'affe-find)
  :custom
  ;; Found in readme: https://github.com/minad/affe
  (affe-regexp-compiler
   (lambda (input _type _ignorecase)      ; Use orderless instead of consult to regexp
     (setq input (orderless-pattern-compiler input))
     (cons input (apply-partially #'orderless--highlight input t))))
  (affe-find-command "rg --hidden --color=never --files") ; Include hidden files
  (affe-grep-command "rg --hidden --null --color=never --max-columns=1000 --no-heading --line-number -v ^$ .")) ; Include hidden files

;;;;; Dwim-shell-command
;; Many convenient wrappers involving shell commands in and out of `dired' (with
;; the ability to easily create my own)
(use-package dwim-shell-command
  :general
  ([remap shell-command] 'dwim-shell-command)
  (:keymaps 'dired-mode-map
            [remap dired-do-async-shell-command] 'dwim-shell-command
            [remap dired-do-shell-command] 'dwim-shell-command
            [remap dired-smart-shell-command] 'dwim-shell-command)
  :init
  (require 'dwim-shell-commands))       ; Set of command line utilities

(provide 'programming-directories-rcp)
;;; programming-directories-rcp.el ends here
