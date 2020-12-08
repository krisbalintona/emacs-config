;;; checking-words-rcp.el --- Summary
;;
;; Packages relevant to the dictionaries and thesauruses
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Online
;;;;; Emacs themes
;; See definitions of words within Emacs. Offline version is
;; https://github.com/gromnitsky/wordnut
(use-package define-word
  :config
  (general-define-key
   "C-c d" '(define-word-at-point :which-key "Definition at point")
   "C-c D" '(define-word :which-key "Definition lookup")
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
  :config
  (general-define-key
   "C-c t" '(google-translate-smooth-translate :which-key "Google translate")
   )
  )

;;;;; Offline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'checking-words-rcp)
;;; Commentary:
;;
;;; checking-words-rcp.el ends here
