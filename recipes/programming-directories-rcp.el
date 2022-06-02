;;; programming-directories-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Everything to do with navigating and managing directories.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Dired
;;;; This
;; Emacs' file manager
(use-package dired
  :straight nil
  :general
  (:keymaps 'dired-mode-map
            :states 'normal
            "h" 'dired-up-directory
            "l" 'dired-find-file)
  :custom
  (dired-auto-revert-buffer t)          ; Automatically revert buffer
  (dired-dwim-target t)                 ; Guess default target directory?
  (dired-hide-details-hide-symlink-targets nil) ; Don't hide symlink targets
  (dired-listing-switches "-agho --group-directories-first") ; Flags `dired' passes to `ls'
  (image-dired-thumb-size 150)                               ; Slightly larger thumbnails
  ;; Always copy/delete recursively?
  (dired-recursive-copies  'always)
  (dired-recursive-deletes 'top)
  ;; Ask whether destination dirs should get created when copying/removing files.
  (dired-create-destination-dirs 'ask)
  :config
  (general-unbind '(normal visual motion) dired-mode-map "SPC") ; Unbind SPC so leader key is avaiable

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

;;;; All-the-icons-dired
;; Add icons which represent file types
(use-package all-the-icons-dired
  :ghook 'dired-mode-hook
  :gfhook '(lambda () (setq-local all-the-icons-scale-factor 0.95))
  :custom
  (all-the-icons-dired-monochrome nil) ; Icon the same color as the text on the line?
  (dired-kill-when-opening-new-dired-buffer t) ; Kill dired buffer when opening new directory
  )

;;;; Dired-git
;; Show git information in dired
(use-package dired-git
  :ghook 'dired-mode-hook
  :custom
  (dired-git-disable-dirs '("~/"))
  (dired-git-parallel 7)                ; Number of parallel processes
  )

;;;; Dired-open
;; Override how dired opens files with certain extensions
(use-package dired-open
  :demand t
  :after dired
  :custom
  (dired-open-extensions '(("odt" . "soffice -writer")
                           ("docx" . "soffice -writer")
                           ("mp4" . "vlc")
                           ("mp3" . "vlc")
                           ("mkv" . "vlc")
                           ))
  ;; ;; Try to use `xdg-open' before anything else
  ;; (add-to-list 'dired-open-functions #'dired-open-xdg t) ; Doesn't work as expected!
  )

;;;; Dired-rsync
;; This package adds a single command `dired-rsync' which allows the user to
;; copy marked files in a dired buffer via rsync. This is useful, especially for
;; large files, because the copy happens in the background and doesn’t lock up
;; Emacs. It is also more efficient than using tramps own encoding methods for
;; moving data between systems.
(use-package dired-rsync
  :after dired
  :general
  (:keymaps 'dired-mode-map
            "C-c C-r" '(dired-rsync :wk "Copy marked files with dired-rsync"))
  :custom
  (dired-rsync-unmark-on-completion t)
  )

;;;; Fd-dired
;; Show `find' results in a Dired buffer. Replaces the default `find-dired'
;; command.
(use-package fd-dired
  :ensure-system-package (find)
  :after dired
  :general ([remap find-dired] #'(fd-dired :wk "Fd-dired"))
  )

;;;; Dired-single
;; Use the same dired buffer for every directory you open using `dired'.
(use-package dired-single
  :general (:keymaps 'dired-mode-map
                     ;; [remap dired-find-file] 'dired-single-buffer
                     [remap dired-up-directory] 'dired-single-up-directory))

;;;; Dired-hide-dotfiles
;; Hide dotfiles
(use-package dired-hide-dotfiles
  :general (:keymaps 'dired-mode-map
                     :states 'normal
                     "H" 'dired-hide-dotfiles-mode)
  :custom
  (dired-hide-dotfiles-verbose nil)) ; No announcements about hiding in echo area

;;; Misc
;;;; Consult-dir
;; Convenient directory selection. Good synergy with dired and embark.
(use-package consult-dir
  :general
  ("C-x C-d" '(consult-dir :wk "Consult dir"))
  (:keymaps 'vertico-map
            "C-x C-d" 'consult-dir           ; Fancy directory selection
            "C-x C-j" 'consult-dir-jump-file ; Regexp for file in current directory
            )
  :custom
  (consult-dir-default-command 'find-file) ; What do I do once I choose a directory from `consult-dir'?
  )

;;;; Affe
;; Blazing fast fuzzy finder
(use-package affe
  :after orderless
  :general ("M-s M-f" #'affe-find
            "M-s M-g" #'affe-grep)
  :custom
  (affe-regexp-compiler
   #'(lambda (input type ignore-case)                ; Use orderless instead of consult to regexp
       (let ((case-fold-search ignore-case))
         (setq text (orderless-pattern-compiler input))
         (cons text (lambda (str) (orderless--highlight text str)))
         )))
  (affe-find-command "rg --hidden --color=never --files") ; Include hidden files
  (affe-grep-command "rg --hidden --null --color=never --max-columns=1000 --no-heading --line-number -v ^$ .") ; Include hidden files
  )

;;; programming-directories-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-directories-rcp)
