;;; Oc (org-cite)
;; Built-in citations in org-mode
(use-package oc
  :ensure nil
  :after org
  :custom
  (org-cite-global-bibliography krisb-bibliography-files)
  (org-cite-csl-locales-dir nil)
  (org-cite-csl-styles-dir (expand-file-name "styles/" krisb-zotero-directory))
  (org-cite-export-processors
   '((md . (csl "chicago-fullnote-bibliography.csl"))   ; Footnote reliant
     (latex biblatex)                                   ; For humanities
     (odt . (csl "chicago-fullnote-bibliography.csl"))  ; Footnote reliant
     (docx . (csl "chicago-fullnote-bibliography.csl")) ; Footnote reliant
     (t . (csl "modern-language-association.csl"))))    ; Fallback
  :custom-face
  ;; Have citation link faces look closer to as they were for `org-ref'
  (org-cite ((t (:foreground "DarkSeaGreen4"))))
  (org-cite-key ((t (:foreground "forest green" :slant italic)))))

;;; Citar
(use-package citar
  :hook (org-mode . citar-capf-setup)
  :bind (("C-c b b" . citar-insert-citation)
         ("C-c b o" . citar-open)
         ("C-c b f" . citar-open-files)
         ("C-c b n" . citar-open-notes)
         :map org-mode-map
         ([remap org-cite-insert] . citar-insert-citation))
  :custom
  (citar-bibliography krisb-bibliography-files)
  (citar-notes-paths (list krisb-notes-directory))
  (citar-open-entry-function #'citar-open-entry-in-file)
  (citar-default-action #'citar-open-files)
  :config
  (with-eval-after-load 'all-the-icons
    ;; Taken from https://github.com/emacs-citar/citar/wiki/Indicators
    (defvar citar-indicator-files-icons
      (citar-indicator-create
       :symbol (all-the-icons-faicon
                "file-o"
                :face 'all-the-icons-green
                :v-adjust -0.1)
       :function #'citar-has-files
       :padding "  " ; Need this because the default padding is too low for these icons
       :tag "has:files"))
    (defvar citar-indicator-links-icons
      (citar-indicator-create
       :symbol (all-the-icons-octicon
                "link"
                :face 'all-the-icons-orange
                :v-adjust 0.01)
       :function #'citar-has-links
       :padding "  "
       :tag "has:links"))
    (defvar citar-indicator-notes-icons
      (citar-indicator-create
       :symbol (all-the-icons-material
                "speaker_notes"
                :face 'all-the-icons-blue
                :v-adjust -0.3)
       :function #'citar-has-notes
       :padding "  "
       :tag "has:notes"))
    (defvar citar-indicator-cited-icons
      (citar-indicator-create
       :symbol (all-the-icons-faicon
                "circle-o"
                :face 'all-the-icon-green)
       :function #'citar-is-cited
       :padding "  "
       :tag "is:cited"))
    (setq citar-indicators
          (list citar-indicator-files-icons
                citar-indicator-links-icons
                citar-indicator-notes-icons
                citar-indicator-cited-icons))))

;;; Citar-org
;; Use `citar' with `org-cite'
(use-package citar-org
  :after oc
  :ensure nil
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-org-styles-format 'short)
  :config
  ;; Faster citation rendered (i.e. activation). Replaces the *VERY SLOW*
  ;; `'org-cite-basic-activate' (which `citar' relies on in
  ;; `citar-org-activate') with a faster version. Practically necessary if I
  ;; want to edit a line with a citation in Org without having to wait several
  ;; seconds for it to render. See for more information on the matter:
  ;; 1. https://www.reddit.com/r/orgmode/comments/td76wz/comment/i0lpg7k/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
  ;; 2. https://list.orgmode.org/87ils5sz8x.fsf@localhost/t/#u
  (defun krisb-citar-basic-activate (citation)
    "Like `org-cite-basic-activate' but faster.
Leverages citar's caching."
    (pcase-let ((`(,beg . ,end) (org-cite-boundaries citation))
                ;; NOTE 2024-09-05: Use `citar' (and its cache) to get all keys
                (keys (let (keys)
                        (maphash (lambda (key value) (push key keys))
                                 (citar-get-entries))
                        keys)))
      (put-text-property beg end 'font-lock-multiline t)
      (add-face-text-property beg end 'org-cite)
      (dolist (reference (org-cite-get-references citation))
        (pcase-let* ((`(,beg . ,end) (org-cite-key-boundaries reference))
                     (key (org-element-property :key reference)))
          ;; Highlight key on mouse over.
          (put-text-property beg end
                             'mouse-face
                             org-cite-basic-mouse-over-key-face)
          (if (member key keys)
              ;; Activate a correct key. Face is `org-cite-key' and `help-echo'
              ;; displays bibliography entry, for reference. <mouse-1> calls
              ;; `org-open-at-point'.
              ;; NOTE 2024-09-05: Use `citar' (and its cache) to create the
              ;; bibliographic entry text used in the help echo
              (let* ((entry (string-trim (citar-format-reference (list key))))
                     (bibliography-entry
                      (org-element-interpret-data entry)))
                (add-face-text-property beg end 'org-cite-key)
                (put-text-property beg end 'help-echo bibliography-entry)
                (org-cite-basic--set-keymap beg end nil))
            ;; Activate a wrong key. Face is `error', `help-echo' displays
            ;; possible suggestions.
            (add-face-text-property beg end 'error)
            (let ((close-keys (org-cite-basic--close-keys key keys)))
              (when close-keys
                (put-text-property beg end 'help-echo
                                   (concat "Suggestions (mouse-1 to substitute): "
                                           (mapconcat #'identity close-keys " "))))
              ;; When the are close know keys, <mouse-1> provides completion to
              ;; fix the current one. Otherwise, call `org-cite-insert'.
              (org-cite-basic--set-keymap beg end (or close-keys 'all))))))))
  (setopt citar-org-activation-functions '(krisb-citar-basic-activate citar-org-activate-keymap)))

;;; Provide
(provide 'krisb-citations)
