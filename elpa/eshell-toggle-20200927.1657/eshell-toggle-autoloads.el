;;; eshell-toggle-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "eshell-toggle" "eshell-toggle.el" (0 0 0 0))
;;; Generated autoloads from eshell-toggle.el

(autoload 'eshell-toggle "eshell-toggle" "\
Show eshell at the bottom of current window and cd to current buffer's path.
\(1) If eshell-toggle'd buffer is already visible in frame for
current buffer then select (toggled) eshell window.
\(2) If current window is (toggled) eshell itself then hide it.
\(3) If KEEP-VISIBLE is non-nil, (toggled) eshell window will stay
visible and will not be hidden.

\(fn &optional KEEP-VISIBLE)" t nil)

(register-definition-prefixes "eshell-toggle" '("eshell-toggle-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eshell-toggle-autoloads.el ends here
