;;; Online
;;;; Dictionary
;; See definitions of words from an online dictionary.
(use-package dictionary
  :ensure nil
  ;; Don't forget to install the following packages from the AUR:
  ;; paru -S dict-wn dict-gcide dict-moby-thesaurus dict-foldoc
  :ensure-system-package (dict . dictd) ; Localhost (offline). Don't forget to enable the systemd service
  :hook (dictionary-mode . hide-mode-line-mode)
  :bind ( :map embark-region-map
          ("D" . krisb-dictionary-at-point)
          :map embark-identifier-map
          ("D" . krisb-dictionary-at-point))
  :custom
  (dictionary-use-single-buffer t)
  (dictionary-create-buttons nil)
  (dictionary-read-word-function 'dictionary-read-word-default)
  (dictionary-search-interface nil)
  (dictionary-read-dictionary-function 'dictionary-completing-read-dictionary)
  (dictionary-server nil)               ; "Automatic"
  :config
  (defun krisb-dictionary-at-point ()
    "Show dictionary definition for word at point.
If region is active, use the region's contents instead."
    (interactive)
    (let ((word (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (thing-at-point 'word :no-properties))))
      (if word
          (dictionary-search word)
        (message "No word or region selected.")))))

;;;; Powerthesaurus
;; Search for synonyms using an online thesaurus.
(use-package powerthesaurus
  :bind ( :map embark-region-map
          ("t" . powerthesaurus-lookup-dwim)
          :map embark-identifier-map
          ("t" . powerthesaurus-lookup-dwim)))

;;; Provide
(provide 'krisb-spelling)
