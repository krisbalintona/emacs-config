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

(provide 'checking-spelling-rcp)
;;; checking-spelling-rcp.el ends here
