;;; esh-help-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "esh-help" "esh-help.el" (0 0 0 0))
;;; Generated autoloads from esh-help.el

(autoload 'setup-esh-help-eldoc "esh-help" "\
Setup eldoc function for Eshell." t nil)

(autoload 'esh-help-run-help "esh-help" "\
Show help for the pointed command or functions CMD.
It comes from Zsh.

\(fn CMD)" t nil)

(register-definition-prefixes "esh-help" '("esh-help-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; esh-help-autoloads.el ends here
