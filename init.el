(dolist (path (list (expand-file-name "modules" user-emacs-directory)
                    (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path path))

;; Shave seconds off startup time by starting the scratch buffer in
;; `fundamental-mode'
(setopt initial-major-mode 'fundamental-mode
        initial-scratch-message "Hello 👋")

;;; Libraries
(require 'krisb-common)

;;; Modules
(require 'krisb-package-management)
(require 'krisb-garbage-collection)
