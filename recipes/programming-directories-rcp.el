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
;; Emacs' file manager
(use-package dired
  :straight nil
  :general
  (:keymaps 'dired-mode-map
            :states 'normal
            "h" 'dired-up-directory
            "l" 'dired-find-file)
  (kb/leader-keys
    "od" '(dired :which-key "Dired"))
  :custom
  (dired-auto-revert-buffer t)          ; Automatically revert buffer
  (dired-dwim-target nil)               ; Guess default target directory?
  (dired-hide-details-hide-symlink-targets nil) ; Don't hide symlink targets
  (dired-recursive-copies 'always)              ; Copy directories recursively?
  (dired-listing-switches "-agho --group-directories-first") ; Flags `dired' passes to `ls'
  )

;;; Dired-git
;; Show git information in dired
(use-package dired-git
  :ghook 'dired-mode-hook
  :custom
  (dired-git-disable-dirs
   '())
  (dired-git-parallel 7)                ; Number of parallel processes
  )

;;; Dired-single
;; Use the same dired buffer for every directory you open using `dired'.
(use-package dired-single
  :general (:keymaps 'dired-mode-map
                     [remap dired-up-directory] 'dired-single-up-directory
                     [remap dired-find-file] 'dired-single-buffer)
  )

;;; All-the-icons-dired
;; Add icons which represent file types
(use-package all-the-icons-dired
  :ghook 'dired-mode-hook
  :gfhook '(lambda () (setq-local all-the-icons-scale-factor 0.95))
  :custom
  (all-the-icons-dired-monochrome nil) ; Icon the same color as the text on the line?
  )

;;; Dired-open
;; Override how dired opens files with certain extensions
(use-package dired-open
  :defer 15
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

;;; Dired-hide-dotfiles
;; Hide dotfiles
(use-package dired-hide-dotfiles
  :ghook 'dired-mode-hook
  :general (:keymaps 'dired-mode-map
                     :states 'normal
                     "H" 'dired-hide-dotfiles-mode)
  :custom
  (dired-hide-dotfiles-verbose nil) ; No annoying announcements in echo area anymore
  )

;;; programming-directories-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'programming-directories-rcp)
