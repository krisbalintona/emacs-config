(dolist (path (list (expand-file-name "modules" user-emacs-directory)
                    (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path path))

;; Shave seconds off startup time by starting the scratch buffer in
;; `fundamental-mode'
(setopt initial-major-mode 'fundamental-mode
        initial-scratch-message "Hello 👋")

(setopt use-package-always-ensure t
        use-package-expand-minimally t  ; Verbosity of use-package macro
        use-package-always-defer nil)

;; Only be verbose when interpreted, otherwise errors are caught at compile time
(setopt use-package-verbose (not (bound-and-true-p byte-compile-current-file)))

;; Compile statistics to be shown in `use-package-report'
(setopt use-package-compute-statistics t)

;; Set better default package paths
(use-package no-littering
  :init
  ;; Set these variables prior to loading the feature
  (setq no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory) ; Config files
        no-littering-var-directory (expand-file-name "var/" user-emacs-directory)) ; Persistent files
  :config
  (no-littering-theme-backups)) ; Sets various built-in variables

;;; On
;; Package exposes a number of utility hooks and functions ported from Doom
;; Emacs. The hooks make it easier to speed up Emacs startup by providing
;; finer-grained control of the timing at which packages are loaded. Provides
;; the following hooks:
;; - on-first-input-hook
;; - on-init-ui-hook
;; - on-first-file-hook
;; - on-switch-frame-hook
;; - on-first-buffer-hook
;; - on-switch-buffer-hook
;; - on-switch-window-hook
(use-package on)

;;; Libraries
(require 'krisb-common)

;;; Modules
(require 'krisb-package-management)
(require 'krisb-use-package)
(require 'krisb-garbage-collection)
(require 'krisb-fonts)
