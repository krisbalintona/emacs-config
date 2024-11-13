;;; Online
;;;; Dictionary
;; See definitions of words from an online dictionary.
(use-package dictionary
  :ensure nil
  ;; Don't forget to install the following packages from the AUR:
  ;; paru -S dict-wn dict-gcide dict-moby-thesaurus dict-foldoc
  :ensure-system-package (dict . dictd) ; Localhost (offline). Don't forget to enable the systemd service
  :hook (dictionary-mode . hide-mode-line-mode)
  :custom
  (dictionary-use-single-buffer t)
  (dictionary-create-buttons nil)
  (dictionary-read-word-function 'dictionary-read-word-default)
  (dictionary-search-interface nil)
  (dictionary-read-dictionary-function 'dictionary-completing-read-dictionary)
  (dictionary-server nil))              ; "Automatic"

;;;; Powerthesaurus
;; Search for synonyms using an online thesaurus.
(use-package powerthesaurus)

;;; Provide
(provide 'krisb-spelling)
