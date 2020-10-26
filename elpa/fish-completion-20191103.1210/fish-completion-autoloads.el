;;; fish-completion-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "fish-completion" "fish-completion.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from fish-completion.el

(autoload 'fish-completion-mode "fish-completion" "\
Turn on/off fish shell completion in all future shells or Eshells.

If called interactively, enable Fish-Completion mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

In `shell', completion is replaced by fish completion.
In `eshell', fish completion is only used when `pcomplete' fails.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "fish-completion" '("fish-completion-" "global-fish-completion-mode" "turn-on-fish-completion-mode"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fish-completion-autoloads.el ends here
