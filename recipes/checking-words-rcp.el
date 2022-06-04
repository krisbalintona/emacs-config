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
  :general  ; `:chords' doesn't have support for multiple keymaps per expression
  (:keymaps 'dictionary-mode-map
            "q" 'dictionary-close)
  (:keymaps 'text-mode-map
            (general-chord "jj") 'dictionary-lookup-definition
            (general-chord "JJ") 'dictionary-search)
  (:keymaps 'prog-mode-map
            (general-chord "jj") '(lambda () ; Only when in comment
                                    (interactive)
                                    (when (nth 4 (syntax-ppss))
                                      (dictionary-lookup-definition)))
            (general-chord "JJ") '(lambda () ; Only when in comment
                                    (interactive)
                                    (when (nth 4 (syntax-ppss))
                                      (dictionary-search))))
  :custom
  (dictionary-use-single-buffer t))     ; Reuse dictionary buffers

;;;; Powerthesaurus
;; Search for synonyms using an online thesaurus.
(use-package powerthesaurus
  :general  ; `:chords' doesn't have support for multiple keymaps per expression
  (:keymaps 'text-mode-map
            (general-chord "kk") 'powerthesaurus-lookup-synonyms-dwim
            (general-chord "KK") 'powerthesaurus-lookup-dwim)
  (:keymaps 'prog-mode-map
            (general-chord "kk") '(lambda () ; Only when in comment
                                    (interactive)
                                    (when (nth 4 (syntax-ppss))
                                      (powerthesaurus-lookup-synonyms-dwim)))
            (general-chord "KK") '(lambda () ; Only when in comment
                                    (interactive)
                                    (when (nth 4 (syntax-ppss))
                                      (powerthesaurus-lookup-dwim)))))

;;; Offline
;;;; Wordnut
;; Offline dictionary
(use-package wordnut
  :after define-word
  ;; TODO 2021-08-20: Have this changed depending on Linux distribution
  ;; Make sure the install a dictnary, in case it is a separate package
  :ensure-system-package (wn . wordnet-cli)
  :config
  (unless (featurep 'define-word)
    (general-define-key
     "C-c d" '(wordnut-lookup-current-word :wk "Wordnut lookup this word")
     "C-c D" '(wordnut-search :wk "Wordnut search"))))

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

;;;; kb/{dictionary,thesaurus}-at-point kb/{dictionary,thesaurus}-lookup
;; Change which packages are used depending on internet connection
(defun kb/internet-up-p (&optional host)
  "Return `t' if the device has internet access, and `nil'
otherwise. Credit to https://emacs.stackexchange.com/a/18515"
  (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
                     (if host host "www.google.com"))))

(defun kb/dictionary-at-point ()
  "Use `dictionary' if online, and `wordnet' if offline."
  (interactive)
  (if (kb/internet-up-p)
      (dictionary-lookup-definition)
    (wordnut-lookup-current-word)))

(defun kb/thesaurus-at-point ()
  "Use `powerthesaurus' if online, and `synosaurus' if offline."
  (interactive)
  (if (kb/internet-up-p)
      (powerthesaurus-lookup-synonyms-dwim)
    (synosaurus-choose-and-replace)))

(defun kb/dictionary-lookup ()
  "Use `dictionary' if online, and `wordnet' if offline."
  (interactive)
  (if (kb/internet-up-p)
      (dictionary-search)
    (wordnut-search)))

(defun kb/dictionary-lookup ()
  "Use `powerthesaurus' if online, and `synosaurus' if offline."
  (interactive)
  (if (kb/internet-up-p)
      (powerthesaurus-lookup-dwim)
    (synosaurus-choose-and-insert)))

;;; checking-words-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'checking-words-rcp)
