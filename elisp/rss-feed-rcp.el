;;; rss-feed-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Elfeed RSS reader configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Elfeed
(use-package elfeed
  :hook (elfeed-search-mode . (lambda ()
                                (display-line-numbers-mode 1)
                                (setq-local display-line-numbers t)
                                ))
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
  :config
  ;; Tag hooks
  (setq elfeed-new-entry-hook
        `(;; Status tags
          ,(elfeed-make-tagger :before "3 months ago" ; Archive very old entries
                               :remove 'aging
                               :add 'archive)
          ,(elfeed-make-tagger :before "1 month ago" ; Don't be distracted by old entries
                               :after "3 months ago"
                               :remove 'unread
                               :add 'aging)
          ))

  ;; Update elfeed every time it is opened
  (advice-add 'elfeed :after #'elfeed-update)
  ;; Apply the appropriate autotags to already existing entries
  (add-hook 'elfeed-search-update-hook #'elfeed-apply-autotags-now)

  (general-define-key
   :keymaps 'elfeed-search-mode-map
   :states '(normal visual motion)
   "S" 'elfeed-search-clear-filter
   )

  (general-define-key
   :keymaps '(elfeed-show-mode-map elfeed-search-mode-map)
   :states 'normal
   [remap elfeed-search-tag-all] '(prot-elfeed-toggle-tag :which-key "Add tag")
   "L" '((lambda ()
           (interactive)
           (elfeed-goodies/toggle-logs)
           (other-window 1)
           )
         :which-key "Elfeed logs")
   )

  (kb/leader-keys
    "or" '(elfeed :which-key "Elfeed")
    )
  )

;;;; QoL
;; From https://protesilaos.com/dotemacs/#h:0cd8ddab-55d1-40df-b3db-1234850792ba
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
(general-define-key
 :keymaps 'elfeed-show-mode-map
 :states 'normal
 "e" '(prot-elfeed-show-eww :which-key "Show in EWW")
 )

(add-hook 'eww-mode-hook '(lambda () (visual-fill-column-mode) (variable-pitch-mode)))

;;;;; Language-detection
;; Detects language of current buffer
(use-package language-detection
  :init
  ;; Has proper syntax highlighting in `eww' (and thus `elfeed-show-mode') from
  ;; the languages supported by `language-detection'.
  (require 'cl-lib)

  (defun eww-tag-pre (dom)
    (let ((shr-folding-mode 'none)
          (shr-current-font 'default))
      (shr-ensure-newline)
      (insert (eww-fontify-pre dom))
      (shr-ensure-newline)))

  (defun eww-fontify-pre (dom)
    (with-temp-buffer
      (shr-generic dom)
      (let ((mode (eww-buffer-auto-detect-mode)))
        (when mode
          (eww-fontify-buffer mode)))
      (buffer-string)))

  (defun eww-fontify-buffer (mode)
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
      (message (format "%s" language))
      (when (fboundp mode)
        mode)))

  (setq shr-external-rendering-functions
        '((pre . eww-tag-pre)))
  )

;;;;; Search completion
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

(general-define-key
 :keymaps 'elfeed-search-mode-map
 :states 'normal
 "C-s" '(prot-elfeed-search-tag-filter :which-key "Prot tag completion")
 )

;;;;; Toggle custom tag keybinds
(with-eval-after-load 'elfeed
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

  (general-define-key
   :keymaps 'elfeed-show-mode-map
   :states '(visual normal motion)
   "u" '((lambda () (interactive) (prot-elfeed-toggle-tag 'unread)) :which-key "Toggle unread tag")
   )
  (general-define-key
   :keymaps '(elfeed-search-mode-map elfeed-show-mode-map)
   :states '(visual normal motion)
   "a" '((lambda () (interactive) (prot-elfeed-toggle-tag 'archive)) :which-key "Toggle archive tag")
   "i" '((lambda () (interactive) (prot-elfeed-toggle-tag 'input)) :which-key "Toggle input tag")
   "d" '((lambda () (interactive) (prot-elfeed-toggle-tag 'done)) :which-key "Toggle done tag")
   "c" '((lambda () (interactive) (prot-elfeed-toggle-tag 'cancelled)) :which-key "Toggle cancelled tag")
   )
  )

;;;; Ancillary
;;;;; Elfeed-org
(use-package elfeed-org
  :after elfeed
  :custom
  (rmh-elfeed-org-files `(,(concat no-littering-var-directory "elfeed/elfeed-feeds.org")
                          ))
  (rmh-elfeed-org-auto-ignore-invalid-feeds nil) ; Appropriately tag failed entries
  :config
  (elfeed-org)
  )

;;;;; Elfeed-goodies
(use-package elfeed-goodies
  :after elfeed
  :custom
  (elfeed-goodies/feed-source-column-width 25)
  (elfeed-goodies/tag-column-width 40)
  (elfeed-goodies/wide-threshold 0.4)

  (elfeed-goodies/entry-pane-position 'bottom)
  (elfeed-goodies/entry-pane-size 0.5)
  :config
  (elfeed-goodies/setup)

  (general-define-key
   :keymaps '(elfeed-show-mode-map elfeed-search-mode-map)
   :states 'normal
   "K" 'elfeed-goodies/split-show-prev
   "J" 'elfeed-goodies/split-show-next
   )
  )

;;; rss-feed-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'rss-feed-rcp)
