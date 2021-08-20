;;; checking-words-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages relevant to the dictionaries and thesauruses.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)

;;;; Online
;;;;; Define-word
;; See definitions of words within Emacs. Offline version is
;; https://github.com/gromnitsky/wordnut
(use-package define-word
  :general ("C-c d" '(define-word-at-point :which-key "Define-word at point")
            "C-c D" '(define-word :which-key "Define-word lookup"))
  )

;;;;; Powerthesaurus
;; Insert or search words in thesaurus. Offline version is
;; https://github.com/hpdeifel/synosaurus
(use-package powerthesaurus
  :general ("C-c l" '(powerthesaurus-lookup-word-at-point :which-key "Thesaurus at point")
            "C-c L" '(powerthesaurus-lookup-word :which-key "Thesuarus lookup"))
  )

;;;; Offline
;;;;; Wordnut
;; Offline dictionary
(use-package wordnut
  :ensure-system-package (wordnet) ; Make sure English dictionary is also installed
  :config
  (unless (featurep 'define-word)
    (general-define-key
     "C-c d" '(wordnut-lookup-current-word :which-key "Wordnut lookup this word")
     "C-c D" '(wordnut-search :which-key "Wordnut search")))
  )

;;;;; Synosaurus
;; Offline thesaurus
(use-package synosaurus
  :ensure-system-package (wordnet) ; Make sure English dictionary is also installed
  :ghook 'after-init-hook
  :custom
  (synosaurus-backend 'synosaurus-backend-wordnet) ; Offline thesaurus that relies on `wordnet'
  (synosaurus-choose-method 'default)
  :config
  (unless (featurep 'powerthesaurus)
    (general-define-key
     "C-c l" '(synosaurus-choose-and-replace :which-key "Synosaurus at point")
     "C-c L" '(synosaurus-choose-and-insert :which-key "Synosaurus lookup")))
  )

;;; checking-words-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'checking-words-rcp)
