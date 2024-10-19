(setopt use-package-always-ensure t
        use-package-expand-minimally t  ; Verbosity of use-package macro
        use-package-always-defer nil)

;; Only be verbose when interpreted, otherwise errors are caught at compile time
(setopt use-package-verbose (not (bound-and-true-p byte-compile-current-file)))

;; Compile statistics to be shown in `use-package-report'
(setopt use-package-compute-statistics t)

(provide 'krisb-use-package)
