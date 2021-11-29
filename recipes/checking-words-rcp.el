;;; checking-words-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages relevant to the dictionaries and thesauruses.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Online
;;;; Define-word
;; See definitions of words from an online dictionary.
(use-package dictionary
  :gfhook 'hide-mode-line-mode
  :general
  (:keymaps 'dictionary-mode-map
            "q" 'dictionary-close)
  ("C-c d" 'dictionary-lookup-definition
   "C-c D" 'dictionary-search)
  :custom
  (dictionary-use-single-buffer t)      ; Resure dictionary buffers
  ;; (dictionary-default-dictionary "wn")
  (dictionary-default-dictionary "*")
  :config (global-dictionary-tooltip-mode)
  )

;;;; Powerthesaurus
;; Search for synonyms using an online thesaurus.
(use-package powerthesaurus
  :general ("C-c l" '(powerthesaurus-lookup-synonyms-dwim :which-key "Synonyms")
            "C-c L" '(powerthesaurus-lookup-dwim :which-key "Advanced word lookup"))
  )

;;; Offline
;;;; Wordnut
;; Offline dictionary
(use-package wordnut
  :after define-word
  ;; TODO 2021-08-20: Have this changed depending on Linux distribution
  :ensure-system-package (wn . wordnet) ; Make sure English dictionary is also installed
  :config
  (unless (featurep 'define-word)
    (general-define-key
     "C-c d" '(wordnut-lookup-current-word :which-key "Wordnut lookup this word")
     "C-c D" '(wordnut-search :which-key "Wordnut search")))
  )

;;;; Synosaurus
;; Offline thesaurus
(use-package synosaurus
  :after powerthesaurus
  ;; TODO 2021-08-20: Have this changed depending on Linux distribution
  :ensure-system-package (wn . wordnet) ; Make sure English dictionary is also installed
  :custom
  (synosaurus-backend 'synosaurus-backend-wordnet) ; Offline thesaurus that relies on `wordnet'
  (synosaurus-choose-method 'default)
  :config
  (unless (featurep 'powerthesaurus)
    (synosaurus-mode)
    (general-define-key
     "C-c l" '(synosaurus-choose-and-replace :which-key "Synosaurus at point")
     "C-c L" '(synosaurus-choose-and-insert :which-key "Synosaurus lookup")))
  )

;;; checking-words-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'checking-words-rcp)
