;;; Environment
;;;; Olivetti
(use-package olivetti
  :hook ((org-mode Info-mode emacs-news-view-mode org-msg-edit-mode) . olivetti-mode)
  :custom
  (olivetti-lighter nil)
  (olivetti-body-width 0.6)
  (olivetti-minimum-body-width 80)
  (olivetti-margin-width 8)
  (olivetti-style 'fancy)              ; Fancy makes the buffer look like a page
  ;; FIXME 2024-01-11: This is a temporary solution. Olivetti's changing of
  ;; margins and fringes messes with the calculation of
  ;; `mode--line-format-right-align', which determines where the right side of
  ;; the mode line is placed.
  (mode-line-format-right-align
   '(:eval (if (and (bound-and-true-p olivetti-mode)
                    olivetti-style)     ; 'fringes or 'fancy
               (let ((mode-line-right-align-edge 'right-fringe))
                 (mode--line-format-right-align))
             (mode--line-format-right-align))))
  :config
  (krisb-modus-themes-setup-faces
   "olivetti"
   (set-face-attribute 'olivetti-fringe nil
                       :background bg-dim
                       :inherit 'unspecified)))

;;;; Astute.el
(use-package astute
  :hook (org-mode . astute-mode)
  :custom
  (astute-lighter "")
  (astute-prefix-single-quote-exceptions
   '("bout"
     "em"
     "n'"
     "cause"
     "round"
     "twas"
     "tis")))

;;;; Darkroom
(use-package darkroom
  :bind ( :map krisb-toggle-keymap
          ("d" . darkroom-mode)
          ("D" . darkroom-tentative-mode))
  :custom
  (darkroom-text-scale-increase 1.3))

;;; Spell checking
;;;; Jinx
;; JIT spell checker that uses `enchant'. The executable is enchant-2. See the
;; manual for more information:
;; https://abiword.github.io/enchant/src/enchant.html
(use-package jinx
  :ensure-system-package ((enchant-2 . enchant)
                          (pkgconf)
                          ;; Don't forget to install spell checker libraries!
                          (hunspell)
                          ("/usr/share/hunspell/en_US-large.dic" . hunspell-en_us)
                          (hspell)      ; Hebrew
                          (nuspell) ; Newest spell checker to be used by Firefox, Thunderbird, etc.
                          (voikkospell . libvoikko)) ; Finnish
  :diminish
  :bind ( :map jinx-mode-map
          ([remap ispell-word] . jinx-correct)
          ("C-," . jinx-correct)
          ("C-M-$" . jinx-languages))
  :config
  (global-jinx-mode 1)

  ;; Mimic `flyspell-abbrev-p'.  Taken from
  ;; https://github.com/minad/jinx/wiki#save-misspelling-and-correction-as-abbreviation
  (defun krisb-jinx--add-to-abbrev (overlay word)
    "Add abbreviation to `local-abbrev-table'.

The misspelled word is taken from OVERLAY. WORD is the corrected
word."
    (let ((abbrev (buffer-substring-no-properties
                   (overlay-start overlay)
                   (overlay-end overlay))))
      (message "Abbrev: %s -> %s" abbrev word)
      ;; Change this to `global-abbrev-table' if preferred
      (define-abbrev local-abbrev-table abbrev word)))
  (advice-add 'jinx--correct-replace :before #'krisb-jinx--add-to-abbrev)

  ;; Read Ispell's "LocalWords."  Taken from
  ;; https://github.com/minad/jinx/wiki#make-jinx-read-from-localwords
  (defun krisb-jinx-ispell--get-localwords ()
    "Return a string of ispell's local words.

Those are the words following `ispell-words-keyword' (usually
\"LocalWords\") in the current buffer."
    (require 'ispell)
    (save-excursion
      (goto-char (point-min))
      (cl-loop while (search-forward ispell-words-keyword nil t)
               collect (string-trim (buffer-substring-no-properties (point) (line-end-position))) into result
               finally return (mapconcat #'identity result " "))))
  (defun krisb-jinx-ispell-add-localwords ()
    "Add ispell's local words to `jinx-local-words'."
    (let ((ispell-localwords (krisb-jinx-ispell--get-localwords)))
      (setq jinx-local-words (concat jinx-local-words ispell-localwords))
      (setq jinx--session-words (append jinx--session-words (split-string ispell-localwords)))))
  (add-hook 'jinx-mode-hook #'krisb-jinx-ispell-add-localwords)

  ;; Write to buffer's LocalWords instead of populating `jinx-local-words', a
  ;; local variable. Taken from
  ;; https://github.com/minad/jinx/wiki#make-jinx-write-localwords
  (defun krisb-jinx-save-as-ispell-localword (save key word)
    "Save WORD using ispell's `ispell-words-keyword'.
If SAVE is non-nil save, otherwise format candidate given action KEY."
    (if save
        (progn
          (require 'ispell)
          (ispell-add-per-file-word-list word)
          (add-to-list 'jinx--session-words word)
          (setq jinx-local-words
                (string-join
                 (sort (delete-dups
                        (cons word (split-string jinx-local-words)))
                       #'string<)
                 " "))))
    (list key word "File (LocalWords)"))
  ;; NOTE 2023-07-16: Can also directly add to `jinx--save-keys' directly
  (setf (alist-get ?* jinx--save-keys) #'krisb-jinx-save-as-ispell-localword)

  ;; Use Vertico's grid display such that more suggestions fit on the screen and
  ;; enable annotations.  Taken from
  ;; https://github.com/minad/jinx#correcting-misspellings
  (with-eval-after-load 'vertico-multiform
    (add-to-list 'vertico-multiform-categories
                 '(jinx grid
                        (vertico-grid-annotate . 20)
                        (vertico-grid-max-columns . 12)
                        (vertico-grid-separator .
                                                #("    |    " 4 5
                                                  (display (space :width (1)) face (:inherit shadow :inverse-video t))))))))

;;; Provide
(provide 'krisb-prose)
