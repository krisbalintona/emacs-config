;;; eshell-syntax-highlighting-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "eshell-syntax-highlighting" "eshell-syntax-highlighting.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from eshell-syntax-highlighting.el

(autoload 'eshell-syntax-highlighting-mode "eshell-syntax-highlighting" "\
Toggle syntax highlighting for Eshell.

If called interactively, enable Eshell-Syntax-Highlighting mode
if ARG is positive, and disable it if ARG is zero or negative.
If called from Lisp, also enable the mode if ARG is omitted or
nil, and toggle it if ARG is `toggle'; disable the mode
otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'eshell-syntax-highlighting-global-mode 'globalized-minor-mode t)

(defvar eshell-syntax-highlighting-global-mode nil "\
Non-nil if Eshell-Syntax-Highlighting-Global mode is enabled.
See the `eshell-syntax-highlighting-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `eshell-syntax-highlighting-global-mode'.")

(custom-autoload 'eshell-syntax-highlighting-global-mode "eshell-syntax-highlighting" nil)

(autoload 'eshell-syntax-highlighting-global-mode "eshell-syntax-highlighting" "\
Toggle Eshell-Syntax-Highlighting mode in all buffers.
With prefix ARG, enable Eshell-Syntax-Highlighting-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Eshell-Syntax-Highlighting mode is enabled in all buffers where
`eshell-syntax-highlighting--global-on' would do it.
See `eshell-syntax-highlighting-mode' for more information on Eshell-Syntax-Highlighting mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "eshell-syntax-highlighting" '("eshell-syntax-highlighting-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eshell-syntax-highlighting-autoloads.el ends here
