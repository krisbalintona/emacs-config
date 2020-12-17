;;; completion-ivy-rcp.el --- Summary
;;
;; Ivy completion framework and (agnostic) cousin packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Interface
;;;;; Ivy
;; Ido but more powerful and eye-pleasing
(use-package ivy
  ;; :hook (after-init . ivy-mode)
  :bind (:map ivy-minibuffer-map
              ("TAB" . ivy-alt-done)
              ("C-l" . ivy-alt-done)
              :map ivy-switch-buffer-map
              ("C-l" . ivy-done)
              ("C-d" . ivy-switch-buffer-kill)
              :map ivy-reverse-i-search-map
              ("C-d" . ivy-reverse-i-search-kill))
  :custom
  (ivy-extra-directories nil) ; Remove ./ and ../
  (ivy-use-virtual-buffers nil) ; Bookmarks and recent files in completion buffer
  (ivy-count-format "(%d/%d) ")
  (ivy-use-selectable-prompt t) ; Make prompt line selectable (as a candidate)
  (enable-recursive-minibuffers t) ; Allow minibuffer commands in minibuffer
  (ivy-height 14)
  (ivy-wrap t)
  (ivy-fixed-height-minibuffer t)
  (ivy-magic-slash-non-match-action nil) ; disable magic slash on non-match
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-on-del-error-function #'ignore) ; don't quit minibuffer on delete-error
  (ivy-sort-max-size 7500) ; Default is wayy too high - slows down in big projects
  (ivy-re-builders-alist
   '((counsel-esh-history . ivy--regex-fuzzy) ; Fuzzy findings for counsel-esh-history
     (t . ivy--regex-ignore-order))) ; Default
  :config
  (setf (alist-get 't ivy-format-functions-alist) #'ivy-format-function-line)

  ;; Faces
  (set-face-attribute 'ivy-minibuffer-match-face-1 nil :height 136)

  ;; Counsel changes a lot of ivy's state at startup; to control for that, we
  ;; need to load it as early as possible. Some packages (like `ivy-prescient')
  ;; require this.
  (require 'counsel nil t)
  )

;;;;; Ivy-rich
;; Enhance Ivy interface
(use-package ivy-rich
  :after ivy
  :hook (after-init . ivy-rich-mode)
  :preface
  ;; Taken from https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#ivy
  (defun kb/ivy-rich-describe-variable-value (candidate)
    "Return the value of the variable in a `counsel-describe-variable' session."
    (let* ((symbol (intern candidate))
           (value (and (boundp symbol) (symbol-value symbol)))
           (print-level 3))
      (replace-regexp-in-string
       "[\n\t\^[\^M\^@\^G]" " "
       (cond ((booleanp value)
              (propertize (format "%s" value) 'face (if (null value) 'shadow 'success)))
             ((keymapp value)
              (propertize "<keymap>" 'face 'font-lock-type-face))
             ((listp value)
              (prin1-to-string value))
             ((stringp value)
              (propertize (format "%S" value) 'face 'font-lock-string-face))
             ((symbolp value)
              (propertize (format "'%s" value) 'face 'font-lock-function-name-face))
             ((format "%s" value)))
       t)))
  (defun kb/ivy-rich-switch-buffer-size (candidate)
    "Return the buffer size in a `ivy-switch-buffer' session."
    (with-current-buffer
        (get-buffer candidate)
      (let ((size (buffer-size)))
        (cond
         ((> size 1000000) (format "%.1fM" (/ size 1000000.0)))
         ((> size 1000) (format "%.1fk" (/ size 1000.0)))
         (t (format "%d" size))))))
  :custom
  (ivy-rich-path-style 'abbrev) ; Abbreviate file names
  :config
  (setq-default 
    ivy-rich-display-transformers-list ; Change transformer to show variable values
   (plist-put ivy-rich-display-transformers-list
              'counsel-M-x
              '(:columns
                ((counsel-M-x-transformer (:width .2))
                 (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))
                :delimiter "  "))
   ivy-rich-display-transformers-list
   (plist-put ivy-rich-display-transformers-list
              'counsel-describe-function
              '(:columns
                ((counsel-describe-function-transformer (:width .2))
                 (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))
                :delimiter "  "))
   ivy-rich-display-transformers-list
   (plist-put ivy-rich-display-transformers-list
              'counsel-describe-variable
              '(:columns
                ((counsel-describe-variable-transformer (:width .2))
                 (kb/ivy-rich-describe-variable-value (:width .2))
                 (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face)))
                :delimiter "  "))
   ivy-rich-display-transformers-list
   (plist-put ivy-rich-display-transformers-list
              'ivy-switch-buffer
              '(:columns
                ((ivy-switch-buffer-transformer (:width .2))
                 (kb/ivy-rich-switch-buffer-size (:align left :face shadow :width 8))
                 (ivy-rich-switch-buffer-major-mode (:face warning :width 22))
                 (ivy-rich-switch-buffer-project (:face success :width 34))
                 (ivy-rich-switch-buffer-path))
                :delimiter "  "
                :predicate (lambda (cand) (get-buffer cand))))
   ivy-rich-display-transformers-list
   (plist-put ivy-rich-display-transformers-list
              'package-install
              '(:columns
                ((ivy-rich-candidate (:width .2))
                 (ivy-rich-package-version (:face shadow :width 13))
                 (ivy-rich-package-archive-summary (:face font-lock-builtin-face :width 5))
                 (ivy-rich-package-install-summary (:face font-lock-doc-face)))
                :delimiter "  ")))

  (ivy-rich-set-display-transformer)
  )

;;;;; All-the-icons-ivy-rich
;; Show icons with ivy-rich
(use-package all-the-icons-ivy-rich
  :after (ivy-rich all-the-icons)
  :hook (ivy-rich-mode . all-the-icons-ivy-rich-mode)
  :custom
  (all-the-icons-ivy-rich-icon-size 0.9) ; The icon size
  ;; Slow Rendering
  ;; If you experience a slow down in performance when rendering multiple icons simultaneously,
  ;; you can try setting the following variable
  (inhibit-compacting-font-caches t)
  )

;;;; Candidate selection and sorting
;;;;; Ivy-prescient
;; Ivy with the prescient completion style
(use-package ivy-prescient
  :after ivy
  :hook (after-init . ivy-prescient-mode)
  )

;;;;; Flx
;; Fuzzy finding in Ivy. /Incompatible with presient/ (other Doom flag)
;; Set from setting ivy-re-builders-alist to ivy--regex-fuzzy
(use-package flx
  :defer t ; Ivy loads this when needed
  :custom
  (ivy-flx-limit 10000)
  )

;;;; Cousin packages
;;;;; Counsel
;; Ivy versions for built-in commands alongside other useful commands
(use-package counsel
  :demand t
  :after ivy
  :bind (;; ("M-x" . 'counsel-M-x)
         ;; ("C-x C-f" . 'counsel-find-file)
         ;; ("C-x b" . 'counsel-switch-buffer)
         ;; ("C-h t" . 'counsel-load-theme) ; Replace help-with-tutorial
         ("C-c g" . 'counsel-git)
         ("C-c j" . 'counsel-git-grep)
         ("C-c k" . 'counsel-ag)
         ("C-x l" . 'counsel-locate)
         ;; ("C-h f" . 'counsel-describe-function)
         ;; ("C-h v" . 'counsel-describe-variable)
         ;; ("C-h o" . 'counsel-describe-symbol)
         )
  :custom
  (counsel-describe-function-function #'helpful-function)
  (counsel-describe-symbol-function #'helpful-symbol)
  (counsel-describe-variable-function #'helpful-variable)
  (ivy-initial-inputs-alist nil)         ; Remove "^" when entering a ivy completion buffer
  (counsel-outline-face-style 'org)      ; Have faces match org's
  (counsel-outline-path-separator " / ") ; More distinct outline paths
  :config
  (general-define-key
   :keymaps 'minibuffer-local-map
   "C-r" 'counsel-minibuffer-history
   )

  (kb/leader-keys
    ;; "ff" '(counsel-find-file :which-key "Find file")
    "fF" '(counsel-file-jump :which-key "Fuzzy find file")
    "fr" '(counsel-recentf :which-key "Recent files")

    ;; "hf" '(counsel-describe-function :which-key "Desc func")
    ;; "hv" '(counsel-describe-variable :which-key "Desc var")
    ;; "ho" '(counsel-describe-symbol :which-key "Desc sym")
    ;; "ht" '(counsel-load-theme :which-key "Change theme")

    "bc" '(counsel-switch-buffer :which-key "Counsel switch buffer")

    "ms" '(bookmark-set :which-key "Set bookmark")

    ;; "iy" '(counsel-yank-pop :which-key "Paste")
    )
  )

;;;;; Swiper
;; Word search a buffer with Ivy
(use-package swiper
  :disabled t ; Now use selectrum-swiper
  :custom
  (swiper-goto-start-of-match t)
  :config
  (general-define-key
   "C-s" 'counsel-grep-or-swiper ; Depending on length of file
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-ivy-rcp)
;;; Commentary:
;;
;;; completion-ivy-rcp.el ends here
