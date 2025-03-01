;;; checking-spelling-rcp.el --- Spell checking      -*- lexical-binding: t; -*-

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

;; I don't know how to spell...

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Abbrev
;; Automatically correct typed strings (e.g. words). Most useful for correcting
;; spelling mistakes as they are made.
(use-package abbrev
  :ensure nil
  :diminish
  :custom
  (abbrev-file-name (expand-file-name "abbrev/abbrev.el" no-littering-var-directory))
  (save-abbrevs 'silently)
  (abbrev-suggest t)
  (abbrev-suggest-hint-threshold 2)
  :init
  (defun kb/abbrev-todo-keyword--string ()
    "Select a todo keyword."
    ;; OPTIMIZE 2024-10-12: Don't rely on `hl-todo-keyword-faces'
    (completing-read "Keyword: " (split-string (key-description nil hl-todo-keyword-faces))))

  (defun kb/abbrev-todo-keyword ()
    "Insert the a todo keyword."
    (insert (kb/abbrev-todo-keyword--string)))

  (defun kb/abbrev-current-date--string ()
    "Return the current date formatted."
    (format-time-string "%F"))

  (defun kb/abbrev-current-date ()
    "Insert the current date."
    (insert (kb/abbrev-current-date--string)))

  (defun kb/abbrev-todo-keyword-and-date ()
    "Insert a todo keyword followed by the current date and colon."
    (insert (kb/abbrev-todo-keyword--string) " " (kb/abbrev-current-date--string) ":"))
  :config
  ;; Enable the mode globally
  (setq-default abbrev-mode t)

  ;; Allow abbrevs with a prefix colon, semicolon, or underscore. See:
  ;; <https://protesilaos.com/codelog/2024-02-03-emacs-abbrev-mode/>.
  (abbrev-table-put global-abbrev-table :regexp "\\(?:^\\|[\t\s]+\\)\\(?1:[:;_].*\\|.*\\)")

  ;; Predefined abbrevs
  (define-abbrev global-abbrev-table ";t" "" #'kb/abbrev-todo-keyword)
  (define-abbrev global-abbrev-table ";d" "" #'kb/abbrev-current-date)
  (define-abbrev global-abbrev-table ";td" "" #'kb/abbrev-todo-keyword-and-date))

;;; Ispell
(use-package ispell
  :ensure nil
  :ensure-system-package (aspell
                          ("/usr/share/licenses/aspell-en/" . aspell-en))
  :custom
  (ispell-program-name (executable-find "aspell"))
  (ispell-personal-dictionary
   (no-littering-expand-var-file-name "aspell-personal-dict-en_us.pws"))
  (ispell-extra-args (list "--sug-mode=ultra" "--lang=en_US" "--camel-case")))

;;; Flyspell
;; Feature-rich spell-checker
(use-package flyspell
  :disabled                             ; Switched to `jinx'
  :ensure nil
  :diminish
  :hook ((text-mode . (lambda ()             ; Prevent conflicts
                        (unless (featurep 'wucuo)
                          (flyspell-mode))))
         (prog-mode . (lambda ()             ; Prevent conflicts
                        (unless (featurep 'wucuo)
                          (flyspell-prog-mode)))))
  :bind
  ;; Unbind all the keys from the mode-map because they're all annoying...
  (:map flyspell-mode-map
        ("C-," . nil)
        ("C-." . nil)
        ("C-;" . nil)
        ("C-c $" . nil))
  :custom
  (flyspell-use-meta-tab nil)

  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  (flyspell-consider-dash-as-word-delimiter-flag t)

  (flyspell-delay 3)
  (flyspell-sort-corrections t)

  (flyspell-prog-text-faces '(font-lock-string-face
                              font-lock-comment-face
                              font-lock-doc-face
                              tree-sitter-hl-face:string
                              tree-sitter-hl-face:string.special ; For things like regexps
                              tree-sitter-hl-face:comment
                              tree-sitter-hl-face:doc
                              ))

  ;; Personal dictionary
  (flyspell-abbrev-p t)           ; Save changes made by flyspell to abbrev file
  (flyspell-use-global-abbrev-table-p nil)) ; Prefer local to global

;;; Flyspell-correct
;; Suggest correct spelling for words flyspell marks as incorrect
(use-package flyspell-correct
  :after flyspell
  :chords
  (("<<" . flyspell-correct-previous)
   (">>" . flyspell-correct-next))
  :custom
  (flyspell-correct-interface 'flyspell-correct-completing-read))

;;; Wucuo
;; A complete solution to the lag of flyspell
(use-package wucuo
  :disabled
  :diminish
  :after flyspell
  :hook ((text-mode prog-mode) . (lambda ()
                                   (interactive)
                                   ;; `wucuo' is incompatible with `flyspell'
                                   (flyspell-mode -1)
                                   (wucuo-start)))
  :bind
  ([remap flyspell-buffer] . wucuo-spell-check-visible-region)
  :custom
  (wucuo-flyspell-start-mode "normal")
  (wucuo-personal-font-faces-to-check flyspell-prog-text-faces)
  (wucuo-double-check-font-faces '(font-lock-string-face
                                   tree-sitter-hl-face:string
                                   ))
  (wucuo-modes-whose-predicate-ignored nil)
  (wucuo-spell-check-buffer-predicate
   '(lambda ()                           ; Skip spell checking under these conditions
      (not (memq major-mode
                 '(dired-mode
                   log-edit-mode
                   compilation-mode
                   help-mode
                   helpful-mode
                   profiler-report-mode
                   speedbar-mode
                   gud-mode
                   calc-mode
                   Info-mode
                   )))))
  :config
  (defun kb/wucuo-mode-on ()
    "Turn wucuo mode on.  Do not use this; use `wucuo-mode' instead."
    (if flyspell-mode
        (message "Please turn off `flyspell-mode' and `flyspell-prog-mode' before wucuo starts!")
      (wucuo-enhance-flyspell)
      ;; Add to `before-save-hook' instead so it is compatible with `super-save'
      ;; + `eyebrowse-pre-window-switch-hook'
      (add-hook 'before-save-hook #'wucuo-spell-check-buffer nil t)))
  (advice-add 'wucuo-mode-on :override #'kb/wucuo-mode-on))

;;; Jinx
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
                          (voikkospell . libvoikko)) ; Finish
  ;; :ensure (:depth nil
  ;;                 :repo "minad/jinx"
  ;;                 :files (:defaults "jinx-mod.c" "emacs-module.h"))
  :vc (:url "https://github.com/minad/jinx.git"
            :rev :newest)
  :diminish
  :hook (on-first-file . global-jinx-mode)
  :bind
  ( :map jinx-mode-map
    ([remap ispell-word] . jinx-correct)
    ("C-," . jinx-correct)
    ("C-M-$" . jinx-languages))
  :config
  ;; Use Vertico's grid display such that more suggestions fit on the screen and
  ;; enable annotations. Taken from
  ;; https://github.com/minad/jinx#correcting-misspellings
  (with-eval-after-load 'vertico-multiform
    (add-to-list 'vertico-multiform-categories
                 '(jinx grid
                        (vertico-grid-annotate . 20)
                        (vertico-grid-max-columns . 12)
                        (vertico-grid-separator .
                                                #("    |    " 4 5
                                                  (display (space :width (1)) face (:inherit shadow :inverse-video t)))))))

  ;; Mimic `flyspell-abbrev-p'. Taken from
  ;; https://github.com/minad/jinx/wiki#save-misspelling-and-correction-as-abbreviation
  (defun kb/jinx--add-to-abbrev (overlay word)
    "Add abbreviation to `local-abbrev-table'.

The misspelled word is taken from OVERLAY. WORD is the corrected
word."
    (let ((abbrev (buffer-substring-no-properties
                   (overlay-start overlay)
                   (overlay-end overlay))))
      (message "Abbrev: %s -> %s" abbrev word)
      ;; Change this to `global-abbrev-table' if preferred
      (define-abbrev local-abbrev-table abbrev word)))
  (advice-add 'jinx--correct-replace :before #'kb/jinx--add-to-abbrev)

  ;; Read Ispell's "LocalWords." Taken from
  ;; https://github.com/minad/jinx/wiki#make-jinx-read-from-localwords
  (defun kb/jinx-ispell--get-localwords ()
    "Return a string of ispell's local words.

Those are the words following `ispell-words-keyword' (usually
\"LocalWords\") in the current buffer."
    (require 'ispell)
    (save-excursion
      (goto-char (point-min))
      (cl-loop while (search-forward ispell-words-keyword nil t)
               collect (string-trim (buffer-substring-no-properties (point) (line-end-position))) into result
               finally return (mapconcat #'identity result " "))))
  (defun kb/jinx-ispell-add-localwords ()
    "Add ispell's local words to `jinx-local-words'."
    (let ((ispell-localwords (kb/jinx-ispell--get-localwords)))
      (setq jinx-local-words (concat jinx-local-words ispell-localwords))
      (setq jinx--session-words (append jinx--session-words (split-string ispell-localwords)))))
  (add-hook 'jinx-mode-hook #'kb/jinx-ispell-add-localwords)

  ;; Write to buffer's LocalWords instead of populating `jinx-local-words', a
  ;; local variable. Taken from
  ;; https://github.com/minad/jinx/wiki#make-jinx-write-localwords
  (defun kb/jinx-save-as-ispell-localword (save key word)
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
  (setf (alist-get ?* jinx--save-keys) #'kb/jinx-save-as-ispell-localword))

(provide 'checking-spelling-rcp)
;;; checking-spelling-rcp.el ends here
