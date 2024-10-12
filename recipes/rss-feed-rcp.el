;;; rss-feed-rcp.el --- RSS                          -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; RSS reader in Emacs.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Elfeed
(use-package elfeed
  :hook ((elfeed-search-mode . (lambda ()
                                 (setq-local display-line-numbers t)
                                 (display-line-numbers-mode 1)
                                 ))
         (elfeed-search-update . elfeed-apply-autotags-now) ; Apply the appropriate autotags to already existing entries
         )
  :bind
  ( :map kb/open-keys
    ("r" . elfeed))
  :custom
  ;; Give time for long updates to complete
  (elfeed-use-curl t)
  (elfeed-curl-timeout 300)
  (url-queue-timeout 300)
  (elfeed-curl-max-connections 300) ; Get a bunch at a time

  (elfeed-search-date-format '("%F %R" 16 :left))
  (elfeed-search-title-max-width 100)
  (elfeed-search-title-min-width 30)

  (elfeed-search-remain-on-entry nil) ; When performing a command, move to next line
  (elfeed-search-clipboard-type 'clipboard) ; Paste to system clipboard

  (elfeed-feeds '())
  (elfeed-search-filter "+unread")
  (elfeed-initial-tags '(unread))
  ;; Tag hooks
  (elfeed-new-entry-hook
   `(;; Status tags
     ,(elfeed-make-tagger :before "3 months ago" ; Mark very old entries as junk
                          :remove 'unread
                          :add 'junk)
     ))
  :config
  ;; Update elfeed every time it is opened
  (advice-add 'elfeed :after #'elfeed-update)
  )

;;;; Elfeed-org
(use-package elfeed-org
  :demand
  :after elfeed
  :custom
  (rmh-elfeed-org-files `(,(concat no-littering-var-directory "elfeed/elfeed-feeds.org")))
  (rmh-elfeed-org-auto-ignore-invalid-feeds nil) ; Appropriately tag failed entries
  :config
  (general-advice-add 'elfeed :after 'elfeed-org nil t))

;;;; Elfeed-goodies
(use-package elfeed-goodies
  :demand t
  :after elfeed ; Can't figure out how to have this work other than this and demanding it
  :custom
  (elfeed-goodies/feed-source-column-width 25)
  (elfeed-goodies/tag-column-width 40)
  (elfeed-goodies/wide-threshold 0.4)

  (elfeed-goodies/entry-pane-position 'bottom)
  (elfeed-goodies/entry-pane-size 0.5)
  :preface
  ;; NOTE 2021-08-20: For some reason the manual `(require popwin)' called by
  ;; `elfeed-goodies' doesn't work. So I call and require it here beforehand
  ;; manually.
  (use-package popwin)
  :config (elfeed-goodies/setup)   ; Immediately load in time for first `elfeed'
  )

;;;; QoL
;; Much is from https://protesilaos.com/dotemacs/#h:0cd8ddab-55d1-40df-b3db-1234850792ba
(with-eval-after-load 'elfeed

;;;;; View in EWW
  (defun prot-elfeed-show-eww (&optional link)
    "Browse current entry's link or optional LINK in `eww'.

Only show the readable part once the website loads.  This can
fail on poorly-designed websites."
    (interactive)
    (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                      elfeed-show-entry
                    (elfeed-search-selected :ignore-region)))
           (link (or link (elfeed-entry-link entry))))
      (eww link)
      (add-hook 'eww-after-render-hook 'eww-readable nil t))
    )
  (bind-key "e" 'prot-elfeed-show-eww 'elfeed-show-mode-map)

  (add-hook 'eww-mode-hook #'(lambda () (olivetti-mode) (mixed-pitch-mode)))

;;;;; Language-detection
  ;; Detects language of current buffer
  ;; Automatically detect then fontify a buffer (that uses `shr', e.g. eww-mode
  ;; and elfeed-show-mode) according to the programming language it appears to
  ;; have.
  (use-package language-detection
    :disabled ; NOTE 2024-09-26: Intrusive. Sometimes there isn't a programming language in the buffer but it still fontifies...
    :init
    (with-eval-after-load 'shr
      (require 'cl-lib)

      (defun kb/language-detection-eww-tag-pre (dom)
        (let ((shr-folding-mode 'none)
              (shr-current-font 'default))
          (shr-ensure-newline)
          (insert (kb/language-detection-eww-fontify-pre dom))
          (shr-ensure-newline)))

      (defun kb/language-detection-eww-fontify-pre (dom)
        (with-temp-buffer
          (shr-generic dom)
          (let ((mode (kb/language-detection-eww-buffer-auto-detect-mode)))
            (when mode
              (kb/language-detection-eww-fontify-buffer mode)))
          (buffer-string)))

      (defun kb/language-detection-eww-fontify-buffer (mode)
        (delay-mode-hooks (funcall mode))
        (font-lock-default-function mode)
        (font-lock-default-fontify-region (point-min)
                                          (point-max)
                                          nil))

      (defun eww-buffer-auto-detect-mode ()
        (let* ((map '((ada ada-mode)
                      (awk awk-mode)
                      (c c-mode)
                      (cpp c++-mode)
                      (clojure clojure-mode lisp-mode)
                      (csharp csharp-mode java-mode)
                      (css css-mode)
                      (dart dart-mode)
                      (delphi delphi-mode)
                      (emacslisp emacs-lisp-mode)
                      (erlang erlang-mode)
                      (fortran fortran-mode)
                      (fsharp fsharp-mode)
                      (go go-mode)
                      (groovy groovy-mode)
                      (haskell haskell-mode)
                      (html html-mode)
                      (java java-mode)
                      (javascript javascript-mode)
                      (json json-mode javascript-mode)
                      (latex latex-mode)
                      (lisp lisp-mode)
                      (lua lua-mode)
                      (matlab matlab-mode octave-mode)
                      (objc objc-mode c-mode)
                      (perl perl-mode)
                      (php php-mode)
                      (prolog prolog-mode)
                      (python python-mode)
                      (r r-mode)
                      (ruby ruby-mode)
                      (rust rust-mode)
                      (scala scala-mode)
                      (shell shell-script-mode)
                      (smalltalk smalltalk-mode)
                      (sql sql-mode)
                      (swift swift-mode)
                      (visualbasic visual-basic-mode)
                      (xml sgml-mode)))
               (language (language-detection-string
                          (buffer-substring-no-properties (point-min) (point-max))))
               (modes (cdr (assoc language map)))
               (mode (cl-loop for mode in modes
                              when (fboundp mode)
                              return mode)))
          (message (format "[language-detection] Detected \"%s\" programming language" language))
          (when (fboundp mode)
            mode)))

      (setq shr-external-rendering-functions
            '((pre . kb/language-detection-eww-tag-pre)))))

;;;;; Custom search completion
  (defun prot-common-crm-exclude-selected-p (input)
    "Filter out INPUT from `completing-read-multiple'.
Hide non-destructively the selected entries from the completion
table, thus avoiding the risk of inputting the same match twice.

To be used as the PREDICATE of `completing-read-multiple'."
    (if-let* ((pos (string-match-p crm-separator input))
              (rev-input (reverse input))
              (element (reverse
                        (substring rev-input 0
                                   (string-match-p crm-separator rev-input))))
              (flag t))
        (progn
          (while pos
            (if (string= (substring input 0 pos) element)
                (setq pos nil)
              (setq input (substring input (1+ pos))
                    pos (string-match-p crm-separator input)
                    flag (when pos t))))
          (not flag))
      t)
    )

  (defun prot-elfeed-search-tag-filter ()
    "Filter Elfeed search buffer by tags using completion.

Completion accepts multiple inputs, delimited by `crm-separator'.
Arbitrary input is also possible, but you may have to exit the
minibuffer with something like `exit-minibuffer'."
    (interactive)
    (unwind-protect
        (elfeed-search-clear-filter)
      (let* ((elfeed-search-filter-active :live)
             (db-tags (elfeed-db-get-all-tags))
             (plus-tags (mapcar (lambda (tag)
                                  (format "+%s" tag))
                                db-tags))
             (minus-tags (mapcar (lambda (tag)
                                   (format "-%s" tag))
                                 db-tags))
             (all-tags (delete-dups (append plus-tags minus-tags)))
             (tags (completing-read-multiple
                    "Apply one or more tags: "
                    all-tags #'prot-common-crm-exclude-selected-p t))
             (input (string-join `(,elfeed-search-filter ,@tags) " ")))
        (setq elfeed-search-filter input))
      (elfeed-search-update :force))
    )
  (bind-key "C-M-s-s" 'prot-elfeed-search-tag-filter 'elfeed-search-mode-map)

;;;;; Toggle custom tag keybinds
  (defun prot-elfeed-toggle-tag (tag)
    "Toggle TAG for the current item.

When the region is active in the `elfeed-search-mode' buffer, all
entries encompassed by it are affected.  Otherwise the item at
point is the target.  For `elfeed-show-mode', the current entry
is always the target.

The list of tags is provided by `prot-elfeed-search-tags'."
    (interactive
     (list
      (intern
       ;; (prot-elfeed--character-prompt prot-elfeed-search-tags))))
       ;; (prot-elfeed--character-prompt prot-elfeed-search-tags)
       (car (completing-read-multiple
             "Apply one or more tags: "
             (delete-dups (append (mapcar (lambda (tag)
                                            (format "%s" tag))
                                          (elfeed-db-get-all-tags))
                                  ))
             #'prot-common-crm-exclude-selected-p t)
            ;; (prot-elfeed-search-tag-filter)
            ))
      ))
    (if (derived-mode-p 'elfeed-show-mode)
        (if (elfeed-tagged-p tag elfeed-show-entry)
            (elfeed-show-untag tag)
          (elfeed-show-tag tag))
      (elfeed-search-toggle-all tag))

    (if (and (derived-mode-p 'elfeed-show-mode) (not elfeed-search-remain-on-entry))
        (elfeed-goodies/split-show-next))
    )
  (bind-keys
   :map elfeed-search-mode-map
   ("u" . (lambda () (interactive) (prot-elfeed-toggle-tag 'unread)))
   ("C-M-s-j" . (lambda () (interactive) (let ((elfeed-search-remain-on-entry t)) (elfeed-search-untag-all 'unread)) (prot-elfeed-toggle-tag 'junk)))
   ("C-M-s-i" . (lambda () (interactive) (let ((elfeed-search-remain-on-entry t)) (elfeed-search-untag-all 'unread) (elfeed-search-untag-all 'junk)) (prot-elfeed-toggle-tag 'input)))
   ("C-M-s-d" . (lambda () (interactive) (let ((elfeed-search-remain-on-entry t)) (elfeed-search-untag-all 'input)) (prot-elfeed-toggle-tag 'done)))
   ("C-M-s-c" . (lambda () (interactive) (let ((elfeed-search-remain-on-entry t)) (elfeed-search-untag-all 'input)) (prot-elfeed-toggle-tag 'cancelled)))
   :map elfeed-show-mode-map
   ([remap elfeed-show-tag--unread] . (lambda () (interactive) (prot-elfeed-toggle-tag 'unread)))
   ("C-M-s-j" . (lambda () (interactive) (prot-elfeed-toggle-tag 'junk)))
   ("C-M-s-i" . (lambda () (interactive) (elfeed-show-untag 'junk) (prot-elfeed-toggle-tag 'input)))
   ("C-M-s-d" . (lambda () (interactive) (elfeed-show-untag 'input) (prot-elfeed-toggle-tag 'done)))
   ("C-M-s-c" . (lambda () (interactive) (elfeed-show-untag 'input) (prot-elfeed-toggle-tag 'cancelled)))))

;;;; Wallabag
(use-package wallabag
  :disabled t
  :vc (:url "https://github.com/chenyanming/wallabag.el.git"
            :rev :newest)
  :hook
  ((wallabag-after-render . wallabag-search-update-and-clear-filter)
   (wallabag-post-html-render . olivetti-mode)
   (wallabag-post-html-render . visual-line-mode))
  :bind ( :map kb/open-keys
          ("w" . wallabag))
  :custom
  (wallabag-db-connector 'sqlite-builtin)
  ;; NOTE 2024-09-23: If sqlite errors are being returned, try recompiling the
  ;; package once the sqlite file path is set by the db variable
  (wallabag-db-file (no-littering-expand-var-file-name "wallabag/wallabag.sqlite"))
  (wallabag-json-file (no-littering-expand-var-file-name "wallabag/wallabag.json"))
  (wallabag-download-dir (expand-file-name "~/Downloads"))
  (wallabag-host "https://app.wallabag.it") ; Wallabag server host name
  (wallabag-username "krisbalintona")       ; Username
  (wallabag-password (auth-source-pick-first-password :host "app.wallabag.it")) ; Password
  (wallabag-clientid "23882_1jzdzdd09ikgw4k8o0cog4wggk48cgc0gwk8oos0gsc44gcsco") ; Created with API clients management
  (wallabag-secret (auth-source-pick-first-password :host "emacs-wombag.el")) ; Created with API clients management
  (wallabag-starred-icon "⭐")
  ;; From
  ;; https://github.com/chenyanming/wallabag.el?tab=readme-ov-file#image-caching:
  ;; Wallabag will not download the images, but using Emacs disk caching
  ;; capability. Setting url-automatic-caching non-nil causes documents to be
  ;; cached automatically.
  (url-automatic-caching t)
  :config
  ;; (run-with-timer 0 3540 'wallabag-request-token) ; Optional, auto refresh token, token should refresh every hour
  )

;;;; Wombag
(use-package wombag
  :vc (:url "https://github.com/karthink/wombag.git"
            :rev :newest)
  :hook
  (wombag-show-mode . org-remark-mode)
  :hook
  (wombag-show-mode . kb/wombag-entry-setup)
  :bind ( :map kb/open-keys
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
  (defun kb/wombag-entry-setup ()
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
  (defun kb/wombag-org-store-link ()
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

  (defun kb/wombag-org-follow-link (path)
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
   :follow #'kb/wombag-org-follow-link
   :store #'kb/wombag-org-store-link)

  ;; Glue with `org-remark'. Code based on org-remark-eww.el
  (require 'org-remark)
  (defun kb/org-remark-wombag-find-file-name ()
    "Return the ID of the entry.
It assumes the buffer is a `wombag-show-mode' buffer and has a
`wombag-show-entry' value.

This function is meant to be set to hook
`org-remark-source-find-file-name-functions'."
    (when (eq major-mode 'wombag-show-mode)
      (concat "wombag:" (number-to-string (alist-get 'id wombag-show-entry)))))

  (defun kb/org-remark-wombag-highlight-link-to-source (filename point)
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

  (define-minor-mode kb/org-remark-wombag-mode
    "Enable Org-remark to work with Wombag."
    :global t
    :group 'org-remark-wombag
    (if kb/org-remark-wombag-mode
        ;; Enable
        (progn
          (add-hook 'wombag-show-mode-hook #'org-remark-auto-on)
          (add-hook 'org-remark-source-find-file-name-functions
                    #'kb/org-remark-wombag-find-file-name)
          (add-hook 'org-remark-highlight-link-to-source-functions
                    #'kb/org-remark-wombag-highlight-link-to-source))
      ;; Disable
      (remove-hook 'wombag-show-mode-hook #'org-remark-auto-on)
      (remove-hook 'org-remark-source-find-file-name-functions
                   #'kb/org-remark-wombag-find-file-name)
      (remove-hook 'org-remark-highlight-link-to-source-functions
                   #'kb/org-remark-wombag-highlight-link-to-source)))
  (kb/org-remark-wombag-mode 1))

;;;; Pocket-reader
;; View my Pocket
(use-package pocket-reader
  :bind
  ( :map kb/open-keys
    ("p" . pocket-reader)
    :map pocket-reader-mode-map
    ("TAB" . kb/pocket-reader-cycle-view)
    ("+" . pocket-reader-more)
    ("o" . pocket-reader-pop-to-url))
  :custom
  (pocket-reader-site-column-max-width 22)
  (pocket-reader-archive-on-open nil)
  (pocket-reader-default-queries (list ":unread"))
  (pocket-reader-open-url-default-function #'org-web-tools-read-url-as-org)
  (pocket-reader-url-open-fn-map '((eww-browse-url "protesilaos.com")))
  :custom-face
  (pocket-reader-unread ((t (:weight bold))))
  (pocket-reader-archived ((t (:strike-through t))))
  :init
  (defun kb/pocket-reader--set-tabulated-list-format ()
    "Set `tabulated-list-format'.

Sets according to the maximum width of items about to be
displayed."
    (when-let* ((added-width 10)
                (domain-width (min pocket-reader-site-column-max-width
                                   (cl-loop for item being the hash-values of pocket-reader-items
                                            maximizing (length (ht-get item 'domain)))))
                (tags-width (cl-loop for item being the hash-values of pocket-reader-items
                                     maximizing (length (string-join (ht-get item 'tags) ","))))
                (title-width (- (window-text-width)
                                5                   ; Idk why this is needed...
                                (+ 1 added-width)   ; Added
                                (+ 2 1)             ; Favorite
                                (+ 3 domain-width)  ; Site
                                (+ 2 tags-width)))) ; Tags
      (setq tabulated-list-format (vector (list "Added" (1+ added-width) pocket-reader-added-column-sort-function)
                                          (list "*" (+ 2 1) t)
                                          (list "Title" (+ 2 title-width) t)
                                          (list "Site" (+ 3 domain-width) t)
                                          (list "Tags" (+ 2 tags-width) t)))))
  (advice-add 'pocket-reader--set-tabulated-list-format
              :override #'kb/pocket-reader--set-tabulated-list-format)

  (defun kb/pocket-reader-cycle-view ()
    "Cycle between showing unread entries and all entries."
    (interactive)
    (let ((all-query ":all")
          (archive-query ":archive")
          (unread-query ":unread"))
      (pcase pocket-reader-queries
        ((pred (member all-query))
         (message "Showing unread")
         (pocket-reader-search unread-query))
        ((pred (member unread-query))
         (message "Showing archived")
         (pocket-reader-search archive-query))
        ((pred (member archive-query))
         (message "Showing all")
         (pocket-reader-search all-query))
        (_
         (message "Showing default")
         (pocket-reader-search pocket-reader-default-queries))))))

(provide 'rss-feed-rcp)
;;; rss-feed-rcp.el ends here
