;;; Engine-mode
;; Send arbitrary search engine queries to your browser from within Emacs
(use-package engine-mode
  :custom
  (engine/browser-function 'browse-url-default-browser)
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

;;; Wombag
(use-package wombag
  :vc (:url "https://github.com/karthink/wombag.git"
            :rev :newest)
  :hook ((wombag-show-mode . org-remark-mode)
         (wombag-show-mode . krisb-wombag-entry-setup))
  :bind ( :map krisb-open-keymap
          ("w" . wombag))
  :custom
  (wombag-dir (no-littering-expand-var-file-name "wombag"))
  (wombag-db-file (no-littering-expand-var-file-name "wombag/wombag.sqlite"))
  (wombag-host "https://app.wallabag.it")
  (wombag-username "krisbalintona")
  (wombag-password (auth-source-pick-first-password :host "app.wallabag.it"))
  (wombag-client-id "23882_1jzdzdd09ikgw4k8o0cog4wggk48cgc0gwk8oos0gsc44gcsco")
  (wombag-client-secret (auth-source-pick-first-password :host "emacs-wombag.el"))
  (wombag-search-filter "")
  :config
  (defun krisb-wombag-entry-setup ()
    "Set up the visual for wombag-entry buffers."
    (setq-local line-spacing 0.08)
    (face-remap-add-relative 'default :height 1.1)
    (when (require 'olivetti nil t)
      (olivetti-mode 1)
      (olivetti-set-width 120))
    (when (require 'mixed-pitch nil t)
      (mixed-pitch-mode 1))
    (visual-line-mode 1))

  ;; Custom wombag org-link
  (defun krisb-wombag-org-store-link ()
    "Stores link to the current wombag entry."
    (when (eq major-mode 'wombag-show-mode)
      (let* ((title (alist-get 'title wombag-show-entry))
             (id (alist-get 'id wombag-show-entry))
             (pt (save-restriction (widen) (point)))
             (url (concat "wombag:" (number-to-string id) "::" (number-to-string pt)))
             (desc (format "%s (at point %s)" title pt)))
        (org-link-store-props
         :type "wombag"
         :link url
         :description desc))))

  (defun krisb-wombag-org-follow-link (path)
    "Open wombag entry.
The PATH is formatted in the following way:
- \"wombag:\"
- a wombag entry ID
- \"::\"
- an optional number that represents the point in the buffer."
    (let* ((option (and (string-match "::\\(.*\\)\\'" path)
                        (match-string 1 path)))
           (id (string-to-number
                (if (not option)
                    path
                  (substring path 0 (match-beginning 0)))))
           (pt (when option
                 (string-to-number (substring path (+ 2 (match-beginning 0))))))
           (entry (car
                   (wombag-db-get-entries
                    `[:select ,(vconcat wombag-search-columns) :from items :where (= id ,id)]
                    wombag-search-columns))))
      (with-current-buffer (wombag-show-entry entry)
        (when pt (goto-char pt)))))

  (org-link-set-parameters
   "wombag"
   :follow #'krisb-wombag-org-follow-link
   :store #'krisb-wombag-org-store-link)

  ;; Glue with `org-remark'. Code based on org-remark-eww.el
  (require 'org-remark)
  (defun krisb-org-remark-wombag-find-file-name ()
    "Return the ID of the entry.
It assumes the buffer is a `wombag-show-mode' buffer and has a
`wombag-show-entry' value.

This function is meant to be set to hook
`org-remark-source-find-file-name-functions'."
    (when (eq major-mode 'wombag-show-mode)
      (concat "wombag:" (number-to-string (alist-get 'id wombag-show-entry)))))

  (defun krisb-org-remark-wombag-highlight-link-to-source (filename point)
    "Return org-link pointing to the source wombag entry (i.e. FILENAME).
It assumes the major mode is `wombag-show-mode'.

 This function is meant to be set to hook
`org-remark-highlight-link-to-source-functions'."
    (when (eq major-mode 'wombag-show-mode)
      (let* ((file-title filename)
             (id (string-to-number (cadr (string-split filename ":"))))
             (title (or (caar (wombag-db-query `[:select title :from items :where (= id ,id)]))
                        "UNTITLED")) ; NOTE 2024-09-24: This is what `wombag' currently titles its untitled notes
             (pt (number-to-string point))
             (desc (format "%s (at point %s)" title point)))
        (concat "[[" file-title "::" pt "][" title " (at point " pt ")" "]]"))))

  (define-minor-mode krisb-org-remark-wombag-mode
    "Enable Org-remark to work with Wombag."
    :global t
    :group 'org-remark-wombag
    (if krisb-org-remark-wombag-mode
        ;; Enable
        (progn
          (add-hook 'wombag-show-mode-hook #'org-remark-auto-on)
          (add-hook 'org-remark-source-find-file-name-functions
                    #'krisb-org-remark-wombag-find-file-name)
          (add-hook 'org-remark-highlight-link-to-source-functions
                    #'krisb-org-remark-wombag-highlight-link-to-source))
      ;; Disable
      (remove-hook 'wombag-show-mode-hook #'org-remark-auto-on)
      (remove-hook 'org-remark-source-find-file-name-functions
                   #'krisb-org-remark-wombag-find-file-name)
      (remove-hook 'org-remark-highlight-link-to-source-functions
                   #'krisb-org-remark-wombag-highlight-link-to-source)))
  (krisb-org-remark-wombag-mode 1))

;;; Provide
(provide 'krisb-web)
