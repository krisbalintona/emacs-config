;; -*- lexical-binding: t; -*-

;;; Add modules and personal lisp files to `load-path'
(dolist (path (list (expand-file-name "modules" user-emacs-directory)
                    (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path path))

;;; Set `custom-file' but do not load it
;; Write `custom-file' in a temporary directory but don't load it.
;; This is just to avoid writing Custom options to the
;; `user-init-file'.  See
;; https://protesilaos.com/emacs/dotemacs#h:f2ffe0e9-a58d-4bba-9831-cc35940ea83f
;; for a more detailed explanation.
(setopt custom-file (make-temp-file "emacs-custom-file-" nil ".el"))

;;; package.el
(setopt package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                           ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/"))
        package-archive-priorities '(("gnu-elpa" . 4)
                                     ("nongnu" . 3)
                                     ("gnu-elpa-devel" . 2)
                                     ("melpa" . 1))
        package-install-upgrade-built-in t

        load-prefer-newer t)

;;; Setup.el
(unless (package-installed-p 'setup)
  (package-install 'setup))

;;; No-littering
;; Have packages write their files in locations adhering to a
;; convention.
(setup no-littering
  (:package no-littering)
  
  ;; Set the package options according to the instructions found in
  ;; the package commentary.
  (eval-and-compile       ; Ensure values don't differ at compile time
    (setq no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory) ; Config files
          no-littering-var-directory (expand-file-name "var/" user-emacs-directory))) ; Persistent files
  ;; Ensure the directories exist
  (mkdir no-littering-etc-directory t)
  (mkdir no-littering-var-directory t)
  
  (:require no-littering)

  ;; Sets more secure values for `auto-save-file-name-transforms',
  ;; `backup-directory-alist', and
  ;; `undo-tree-history-directory-alist'.  Read docstring for a more
  ;; detailed explanation.
  (no-littering-theme-backups))
