;; Initialize package resources
(setopt package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                           ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/"))
        package-archive-priorities '(("gnu-elpa" . 4)
                                     ("melpa" . 3)
                                     ("nongnu" . 2)
                                     ("gnu-elpa-devel" . 1))
        package-install-upgrade-built-in t
        package-pinned-packages nil)

(unless package-archive-contents
  (package-refresh-contents))

;; Although `use-package' is built-in starting Emacs 29.1, I should make sure
;; it's installed just in case I test/use an earlier Emacs version
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Then ensure use-package is available at compile time
(eval-when-compile
  (require 'use-package))

(provide 'krisb-package-management)
