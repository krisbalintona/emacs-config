;; -*- lexical-binding: t; -*-

;;; Dired
;; Emacs' file manager
(use-package dired
  :ensure nil
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . turn-on-gnus-dired-mode)) ; Email attachment integration with dired
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)                 ; Guess default target directory?
  (dired-hide-details-hide-symlink-targets nil) ; Don't hide symlink targets
  (dired-kill-when-opening-new-dired-buffer t)  ; Basically `dired-single'
  (dired-listing-switches "--group-directories-first --time-style=long-iso -alhgv") ; Flags `dired' passes to `ls'
  (dired-movement-style 'bounded)
  ;; Always copy/delete recursively?
  (dired-recursive-copies  'always)
  (dired-recursive-deletes 'top)
  ;; Ask whether destination dirs should get created when copying/removing files.
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t)
  :config
  ;; Mark files and do a sexp in their buffers. Based off
  ;; https://superuser.com/a/176629
  (defun krisb-dired-eval-form (sexp &optional prefix)
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

;;; Dired-x
(use-package dired-x
  :ensure nil
  :custom
  (dired-omit-verbose nil))

;;; Nerd-icons-dired
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;;; Dired-hist
;; History for dired buffers
(use-package dired-hist
  :hook (dired-mode . dired-hist-mode)
  :bind ( :map dired-mode-map
          ("l" . dired-hist-go-back)
          ("r" . dired-hist-go-forward)))

;;; Cascading-dir-locals
;; "Provides a global minor mode that changes how Emacs handles the lookup of
;; applicable dir-locals files (".dir-locals.el"): instead of starting at the
;; directory of the visited file and moving up the directory tree only until a
;; first dir-locals file is found, collect and apply all (!) dir-locals files
;; found from the current directory up to the root one."
(use-package cascading-dir-locals
  :config
  (cascading-dir-locals-mode 1))

;;; Provide
(provide 'krisb-directories)
