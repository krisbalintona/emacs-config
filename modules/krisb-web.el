;;; Engine-mode
;; Send arbitrary search engine queries to your browser from within Emacs
(use-package engine-mode
  :custom
  (engine/browser-function 'browse-url-generic)
  :config
  (engine-mode 1)

  ;; My searches
  (defengine amazon
             "https://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s")

  (defengine duckduckgo
             "https://duckduckgo.com/?q=%s"
             :keybinding "d")

  (defengine github
             "https://github.com/search?ref=simplesearch&q=%s"
             :keybinding "g")

  (defengine google
             "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
             :keybinding "G")

  (defengine wikipedia
             "https://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
             :keybinding "w")

  (defengine youtube
             "https://www.youtube.com/results?aq=f&oq=&search_query=%s"
             :keybinding "y"))

;;; Provide
(provide 'krisb-web)
