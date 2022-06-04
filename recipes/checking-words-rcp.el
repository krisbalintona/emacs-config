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
  :general (:keymaps 'dictionary-mode-map
                     "q" 'dictionary-close)
  :chords (:map text-mode-map
                ("jj" . dictionary-lookup-definition)
                ("JJ" . dictionary-search))
  :custom
  (dictionary-use-single-buffer t))     ; Reuse dictionary buffers

;;;; Powerthesaurus
;; Search for synonyms using an online thesaurus.
(use-package powerthesaurus
  :chords (:map text-mode-map
                ("kk" . powerthesaurus-lookup-synonyms-dwim)
                ("KK" . powerthesaurus-lookup-dwim)))

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
     "C-c d" '(wordnut-lookup-current-word :wk "Wordnut lookup this word")
     "C-c D" '(wordnut-search :wk "Wordnut search")))
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
     "C-c l" '(synosaurus-choose-and-replace :wk "Synosaurus at point")
     "C-c L" '(synosaurus-choose-and-insert :wk "Synosaurus lookup")))
  )

;;; checking-words-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'checking-words-rcp)
