;; -*- lexical-binding: t; -*-

;;; Add modules and lisp to load path
(dolist (path (list (expand-file-name "modules" user-emacs-directory)
                    (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path path))


;;; Package.el
;; Initialize package resources
(setopt package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                           ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/"))
        package-archive-priorities '(("gnu-elpa" . 4)
                                     ("nongnu" . 3)
                                     ("gnu-elpa-devel" . 2)
				     ("melpa" . 1))
        package-install-upgrade-built-in t

        load-prefer-newer t) ; Do not load outdated byte code files

;; As recommended in https://elpa.gnu.org/
(unless (fboundp 'package-activate-all) (package-initialize))

;;; Use-package
;; Although `use-package' is built-in starting Emacs 29.1, I should make sure
;; it's installed just in case I test/use an earlier Emacs version
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; 2025-04-10: I think I must require use-package for subsequent NAblocks on a
;; fresh install to be respected and those packages installed?
(use-package use-package
  :custom
  (setopt use-package-always-ensure t
          use-package-expand-minimally t  ; Verbosity of use-package macro
          ;; 2025-04-08: I do not set `use-package-always-defer' to non-nil
          ;; because I have had bad experiences trying to troubleshoot packages
          ;; because of the many potential deferral conditions.  Package deferrals
          ;; overlap and become interwoven.  It becomes harder when I don't notice
          ;; things are broken because a package's features were lazily loaded.
          ;;
          ;; Instead, I choose to opt-in to deferring packages (e.g. through
          ;; :defer, :bind, :after, etc.)
          use-package-always-defer nil)
  
  ;; Only be verbose when interpreted, otherwise errors are caught at compile time
  (setopt use-package-verbose (not (bound-and-true-p byte-compile-current-file)))

  
  ;; Compile statistics to be shown in `use-package-report'
  (setopt use-package-compute-statistics t)

  )

;;; No-littering
;; Set better default package paths
(use-package no-littering
  :init
  ;; According to the package instructions, these variables must be set prior to
  ;; loading the feature
  (eval-and-compile                 ; Ensure values don't differ at compile time
    (setq no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory) ; Config files
          no-littering-var-directory (expand-file-name "var/" user-emacs-directory))) ; Persistent files
  :config
  ;; Ensure the directories exist
  (mkdir no-littering-etc-directory t)
  (mkdir no-littering-var-directory t)

  ;; Read docstring.  Sets more secure values for
  ;; `auto-save-file-name-transforms', `backup-directory-alist', and
  ;; `undo-tree-history-directory-alist'.
  (no-littering-theme-backups))

;;; El-patch
(use-package el-patch)
