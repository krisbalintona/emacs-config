;;; checking-words-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages relevant to the dictionaries and thesauruses
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'general)
;;;; Online
;;;;; Define-word
;; See definitions of words within Emacs. Offline version is
;; https://github.com/gromnitsky/wordnut
(use-package define-word
  :config
  (general-define-key
   "C-c d" '(define-word-at-point :which-key "Define-word at point")
   "C-c D" '(define-word :which-key "Define-word lookup")
   )
  )

;;;;; Powerthesaurus
;; Insert or search words in thesaurus. Offline version is
;; https://github.com/hpdeifel/synosaurus
(use-package powerthesaurus
  :config
  (general-define-key
   "C-c l" '(powerthesaurus-lookup-word-at-point :which-key "Thesaurus at point")
   "C-c L" '(powerthesaurus-lookup-word :which-key "Thesuarus lookup")
   )
  )

;;;;; Google-translate
;; Translate strings via Google Translate
(use-package google-translate
  :disabled t ; Don't use
  :config
  (general-define-key
   "C-c t" '(google-translate-smooth-translate :which-key "Google translate")
   )
  )

;;;; Offline
;;;;; Wordnut
;; Offline dictionary
(use-package wordnut
  :if (eq system-packages-package-manager 'yay) ; Be on Arch Linux
  :ensure-system-package (wn . wordnet-cli) ; Install alongside `english-wordnet' dependency
  :config
  (unless (featurep 'define-word)
    (general-define-key
     "C-c d" '(wordnut-lookup-current-word :which-key "Wordnut lookup this word")
     "C-c D" '(wordnut-search :which-key "Wordnut search")
     )
    )
  )

;;;;; Synosaurus
;; Offline thesaurus
(use-package synosaurus
  :if (eq system-packages-package-manager 'yay) ; Be on Arch Linux
  :ensure-system-package (wn . wordnet-cli) ; Install alongside `english-wordnet' dependency
  :hook (after-init . synosaurus-mode)
  :custom
  (synosaurus-backend 'synosaurus-backend-wordnet) ; Offline thesaurus that relies on `wordnet'
  (synosaurus-choose-method 'default)
  :config
  (unless (featurep 'powerthesaurus)
    (general-define-key
     "C-c l" '(synosaurus-choose-and-replace :which-key "Synosaurus at point")
     "C-c L" '(synosaurus-choose-and-insert :which-key "Synosaurus lookup")
     )
    )
  )

;;; checking-words-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'checking-words-rcp)
