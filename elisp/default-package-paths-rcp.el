;;; default-package-paths-rcp.el
;;
;;; Code:

;;; NoLittering
(use-package no-littering
  :custom
  (no-littering-etc-directory (expand-file-name "data/" user-emacs-directory)) ; Config files
  (no-littering-var-directory (expand-file-name "var/" user-emacs-directory)) ; Persistent files

  (custom-file (no-littering-expand-etc-file-name "custom.el")) ; Set custom.el path
  (auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))) ; Store auto-saved files here
  :config
  ;; Exlude these files from recent files list
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  )
;;; NoLittering

;;; LoadCustomFile
(when (file-exists-p custom-file)
  (load custom-file))
;;; LoadCustomFile


(provide 'default-package-paths-rcp)
;;; Commentary:
;; Set more sane defaults for Emacs as well as other QoL modes. These settings are
;; package-agnostic
;;
;;; default-package-paths-rcp.el ends here
