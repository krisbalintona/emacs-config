;;; tablist-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tablist" "tablist.el" (0 0 0 0))
;;; Generated autoloads from tablist.el

(autoload 'tablist-minor-mode "tablist" "\
Toggle Tablist minor mode on or off.

If called interactively, enable Tablist minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{tablist-minor-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'tablist-mode "tablist" "\


\(fn)" t nil)

(register-definition-prefixes "tablist" '("tablist-"))

;;;***

;;;### (autoloads nil "tablist-filter" "tablist-filter.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from tablist-filter.el

(register-definition-prefixes "tablist-filter" '("tablist-filter-"))

;;;***

;;;### (autoloads nil nil ("tablist-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tablist-autoloads.el ends here
