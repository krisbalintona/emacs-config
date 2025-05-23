;; -*- lexical-binding: t; -*-

;;; Online
;;;; Dictionary
;; See definitions of words from an online dictionary.
(use-package dictionary
  :commands krisb-dictionary-dwim
  :ensure nil
  ;; Don't forget to install the following packages from the AUR:
  ;; paru -S dict-wn dict-gcide dict-moby-thesaurus dict-foldoc
  ;; :ensure-system-package (dict . dictd) ; Localhost (offline). Don't forget to enable the systemd service
  :hook (dictionary-mode . hide-mode-line-mode)
  :bind ("C-h =" . krisb-dictionary-dwim)
  :custom
  (dictionary-use-single-buffer t)
  (dictionary-create-buttons nil)
  (dictionary-read-word-function 'dictionary-read-word-default)
  (dictionary-search-interface nil)
  (dictionary-read-dictionary-function 'dictionary-completing-read-dictionary)
  (dictionary-server
   (if (string-equal (string-trim (shell-command-to-string "systemctl is-active dictd"))
                     "active")
       "localhost" "dict.org"))
  :init
  ;; We place this here to ensure this package is lazy loaded when one of the
  ;; commands is called from embark keymap
  (with-eval-after-load 'embark
    (bind-keys
     :map embark-region-map
     ("=" . krisb-dictionary-dwim)
     :map embark-identifier-map
     ("=" . krisb-dictionary-dwim)))
  :config
  (defun krisb-dictionary-dwim ()
    "Show dictionary definition for word at point.
If region is active, use the region's contents instead."
    (interactive)
    (if-let ((word (if (use-region-p)
                       (buffer-substring-no-properties (region-beginning) (region-end))
                     (thing-at-point 'word :no-properties))))
        (dictionary-search word)
      (message "No word or region selected."))))

;;;; Powerthesaurus
;; Search for synonyms using an online thesaurus.
(use-package powerthesaurus
  :after embark
  :bind ( :map embark-region-map
          ("t" . powerthesaurus-lookup-synonyms-dwim)
          ("T" . powerthesaurus-lookup-dwim)
          :map embark-identifier-map
          ("t" . powerthesaurus-lookup-synonyms-dwim)
          ("T" . powerthesaurus-lookup-dwim)))

;;; Provide
(provide 'krisb-spelling)
