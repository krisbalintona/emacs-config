;;; org-general-rcp.el --- Generalized org-mode config  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona(require 'use-package-rcp) <krisbalintona@gmail.com>
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

;; Most of my org-mode config.

;;; Code:
(require 'keybinds-general-rcp)

;;;; Org
;;;;; Itself
(use-package org
  ;; :ensure (:pin t)
  :gfhook
  'variable-pitch-mode
  'visual-line-mode
  '(lambda ()
     (eldoc-mode -1)
     (setq-local line-spacing 0.2))
  :general
  (:keymaps 'org-mode-map
            "C-M-s-s" 'org-store-link
            "C-M-S-s" 'org-id-store-link
            "C-M-<up>" 'org-up-element)
  (kb/note-keys
    "c" 'org-capture)
  :custom
  (org-directory kb/org-dir)
  (org-special-ctrl-a/e t)
  (org-src-window-setup 'current-window) ; Open src block window on current buffer were in the language's major mode

  (org-hide-leading-stars t)
  (org-startup-folded 'nofold)
  (org-ellipsis " ⮷")
  (org-hide-emphasis-markers t)     ; Remove org-mode markup characters
  (org-hide-macro-markers t)
  (org-pretty-entities t)           ; Show as UTF-8 characters (useful for math)
  (org-pretty-entities-include-sub-superscripts t) ; Show superscripts and subscripts? Also see `org-export-with-sub-superscripts'
  (org-use-sub-superscripts '{}) ; Requires brackets to recognize superscripts and subscripts
  (org-hidden-keywords nil)
  (org-ctrl-k-protect-subtree 'error)

  (org-list-allow-alphabetical t)
  (org-list-use-circular-motion t)

  (org-blank-before-new-entry
   '((heading . auto)
     ;; Don't let Emacs make decisions about where to insert newlines
     (plain-list-item . nil)))
  (org-cycle-separator-lines 2)

  (org-return-follows-link t)
  (org-insert-heading-respect-content nil) ; Let M-RET make heading in place

  (org-file-apps
   '((directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . default)
     ("\\.docx\\'" . system)
     ("\\.odt\\'" . system)
     ;; Default to `auto-mode-alist'
     (auto-mode . emacs)))

  (org-fold-catch-invisible-edits 'show)
  (org-edit-timestamp-down-means-later t)

  ;; Org-babel et. al
  (org-confirm-babel-evaluate nil)
  (org-ditaa-jar-path                   ; EAF happens to install it...
   "/home/krisbalintona/.emacs.d/straight/build/eaf/app/markdown-previewer/node_modules/@shd101wyy/mume/dependencies/ditaa/ditaa.jar")
  :custom-face
  (org-ellipsis ((t (:height 1.0)))) ; Don't make line taller because of org-ellipsis
  :config
  (when (bound-and-true-p evil-local-mode)
    (advice-add 'org-ctrl-c-ret :after #'evil-insert-state))) ; Entire insert-state after M-RET

;;;;; Org-num
(use-package org-num
  :ensure nil
  :diminish
  :general (kb/toggle-keys
             :keymaps 'org-mode-map
             "n" 'org-num-mode)
  :custom
  (org-num-face 'fixed-pitch)
  (org-num-skip-commented t)
  (org-num-skip-footnotes t)
  (org-num-skip-unnumbered t))

;;;;; Org-indent
(use-package org-indent
  :disabled
  :ensure nil
  :diminish
  :custom
  (org-indent-indentation-per-level 2)
  (org-indent-mode-turns-off-org-adapt-indentation t)
  ;; This is overridden by `org-superstar'
  (org-startup-indented t)
  (org-indent-mode-turns-on-hiding-stars t)
  :config
  ;; The following to overrides change the character org-indent uses to indent
  ;; headlines from an asterisk to a space, meaning they are not seen even when
  ;; `alpha-background' frame parameter causes the background to become
  ;; transparent, leaving the indent character visible
  (defun kb/org-indent--compute-prefixes ()
    "Compute prefix strings for regular text and headlines."
    (setq org-indent--heading-line-prefixes
          (make-vector org-indent--deepest-level nil))
    (setq org-indent--inlinetask-line-prefixes
          (make-vector org-indent--deepest-level nil))
    (setq org-indent--text-line-prefixes
          (make-vector org-indent--deepest-level nil))
    (when (> org-indent-indentation-per-level 0)
      (dotimes (n org-indent--deepest-level)
        (let ((indentation (if (<= n 1) 0
                             (* (1- org-indent-indentation-per-level)
                                (1- n)))))
          ;; Headlines line prefixes.
          ;; Change from an asterisk (i.e. ?*)
          (let ((heading-prefix (make-string indentation ?\s)))
            (aset org-indent--heading-line-prefixes
                  n
                  (org-add-props heading-prefix nil 'face 'org-indent))
            ;; Inline tasks line prefixes
            (aset org-indent--inlinetask-line-prefixes
                  n
                  (cond ((<= n 1) "")
                        ((bound-and-true-p org-inlinetask-show-first-star)
                         (concat org-indent-inlinetask-first-star
                                 (substring heading-prefix 1)))
                        (t (org-add-props heading-prefix nil 'face 'org-indent)))))
          ;; Text line prefixes.
          (aset org-indent--text-line-prefixes
                n
                (org-add-props
                    (concat (make-string (+ n indentation) ?\s)
                            (and (> n 0)
                                 (char-to-string org-indent-boundary-char)))
                    nil 'face 'org-indent))))))
  (defun kb/org-indent-set-line-properties (level indentation &optional heading)
    "Set prefix properties on current line an move to next one.

LEVEL is the current level of heading.  INDENTATION is the
expected indentation when wrapping line.

When optional argument HEADING is non-nil, assume line is at
a heading.  Moreover, if it is `inlinetask', the first star will
have `org-warning' face."
    (let* ((line (aref (pcase heading
                         (`nil org-indent--text-line-prefixes)
                         (`inlinetask org-indent--inlinetask-line-prefixes)
                         (_ org-indent--heading-line-prefixes))
                       level))
           (wrap
            (org-add-props
                (concat line
                        ;; Change from an asterisk (i.e. ?*)
                        (if heading (concat (make-string level ?\s) " ")
                          (make-string indentation ?\s)))
                nil 'face 'org-indent)))
      ;; Add properties down to the next line to indent empty lines.
      (add-text-properties (line-beginning-position) (line-beginning-position 2)
                           `(line-prefix ,line wrap-prefix ,wrap)))
    (forward-line))
  (advice-add 'org-indent--compute-prefixes :override 'kb/org-indent--compute-prefixes)
  (advice-add 'org-indent-set-line-properties :override 'kb/org-indent-set-line-properties))

;;;;; Org-footnote
(use-package org-footnote
  :ensure nil
  :custom
  (org-footnote-section nil)            ; Don't create footnote headline
  (org-footnote-auto-adjust t)          ; Automatically renumber
  (org-footnote-define-inline t)) ; Write footnote content where you declare rather in a particular section (i.e. `org-footnote-section')?

;;;;; Org-attach
(use-package org-attach
  :demand                               ; Need `org-attach-id-dir' elsewhere
  :ensure nil
  :custom
  (org-attach-preferred-new-method 'id) ; Necessary to add the ATTACH tag
  (org-attach-auto-tag "ATTACH")       ; See `org-roam-db-node-include-function'
  (org-attach-dir-relative nil)        ; Use relative file paths?
  (org-attach-id-dir (expand-file-name "resources" org-directory))
  (org-attach-method 'cp)            ; Attach copies of files
  (org-attach-archive-delete 'query) ; If subtree is deleted or archived, ask user
  (org-attach-id-to-path-function-list
   '(org-attach-id-ts-folder-format
     org-attach-id-uuid-folder-format
     org-attach-id-fallback-folder-format)))

;;;;; Org-id
(use-package org-id
  :ensure nil
  :custom
  (org-clone-delete-id t)
  (org-id-method 'ts)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

;;;;; Org-refile
(use-package org-refile
  :ensure nil
  :custom
  (org-refile-targets
   `((,(car (denote-directory-files "20221011T101254")) . (:maxlevel . 2))
     (nil . (:maxlevel . 2))))
  (org-refile-use-cache nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  :config
  ;; Workaround for orderless issue with `org-refile'. See
  ;; https://github.com/minad/vertico#org-refile
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (when (featurep 'vertico)
    (advice-add #'org-olpath-completing-read :around
                (lambda (&rest args)
                  (minibuffer-with-setup-hook
                      (lambda () (setq-local completion-styles '(basic)))
                    (apply args))))))

;;;;; Org-faces
(use-package org-faces
  :ensure nil
  :custom
  (org-fontify-todo-headline nil)
  (org-fontify-done-headline nil)
  (org-fontify-whole-block-delimiter-line nil)
  (org-fontify-quote-and-verse-blocks nil))

;;;;; Org-src
(use-package org-src
  :ensure nil
  :custom
  (org-src-fontify-natively t)
  (org-src-block-faces nil))

;;;;; Org-archive
(use-package org-archive
  :ensure nil
  :custom
  (org-archive-subtree-save-file-p t)  ; Save archive file always
  :config
  (define-advice org-archive--compute-location
      (:around (orig-fun &rest args) kb/org-archive--compute-location-denote-format-string)
    "Take LOCATION in `org-archive--compute-location' and expand %D.
%D is expanded to the denote identifier."
    ;; Modify LOCATION before normal operations
    (cl-letf (((car args)
               (if (fboundp 'denote-retrieve-filename-identifier)
                   (replace-regexp-in-string "%D"
                                             (denote-retrieve-filename-identifier (buffer-file-name (buffer-base-buffer)))
                                             (car args))
                 (car args))))
      (apply orig-fun args))))

;;;; Org-babel
;;;;; Itself
(use-package ob
  :ensure nil
  :hook (after-init . (lambda ()
                        "Activate languages"
                        (org-babel-do-load-languages
                         'org-babel-load-languages
                         '((emacs-lisp . t)
                           (python . t)
                           (mermaid . t)
                           (ditaa . t))))))

;;;;; Ob-mermaid
(use-package mermaid-mode)
(use-package ob-mermaid
  :after ob
  :custom
  (ob-mermaid-cli-path (executable-find "mmdc"))
  :config
  (defun kb/org-babel-execute:mermaid (body params)
    (let* ((out-file (or (cdr (assoc :file params))
                         (error "mermaid requires a \":file\" header argument")))
           (theme (cdr (assoc :theme params)))
           (width (cdr (assoc :width params)))
           (height (cdr (assoc :height params)))
           (scale (cdr (assoc :scale params)))
           (pdffit (cdr (assoc :scale params)))
           (background-color (cdr (assoc :background-color params)))
           (mermaid-config-file (cdr (assoc :mermaid-config-file params)))
           (css-file (cdr (assoc :css-file params)))
           (pupeteer-config-file (cdr (assoc :pupeteer-config-file params)))
           (temp-file (org-babel-temp-file "mermaid-"))
           (mmdc (or ob-mermaid-cli-path
                     (executable-find "mmdc")
                     (error "`ob-mermaid-cli-path' is not set and mmdc is not in `exec-path'")))
           (cmd (concat (shell-quote-argument (expand-file-name mmdc))
                        " -i " (org-babel-process-file-name temp-file)
                        " -o " (org-babel-process-file-name out-file)
                        (when theme
                          (concat " -t " theme))
                        (when width
                          (concat " -w " width))
                        (when height
                          (concat " -H " height))
                        (when scale   ; Add support for scale
                          (concat " -s " (number-to-string scale)))
                        (when pdffit " -f ") ; Add support for pdffit
                        (when mermaid-config-file
                          (concat " -c " (org-babel-process-file-name mermaid-config-file)))
                        (when css-file
                          (concat " -C " (org-babel-process-file-name css-file)))
                        (when pupeteer-config-file
                          (concat " -p " (org-babel-process-file-name pupeteer-config-file))))))
      (unless (file-executable-p mmdc)
        ;; cannot happen with `executable-find', so we complain about
        ;; `ob-mermaid-cli-path'
        (error "Cannot find or execute %s, please check `ob-mermaid-cli-path'" mmdc))
      (with-temp-file temp-file (insert body))
      (message "%s" cmd)
      (org-babel-eval cmd "")
      nil))
  (advice-add 'org-babel-execute:mermaid :override 'kb/org-babel-execute:mermaid))

;;;; Aesthetics
;;;;; Org-superstar
;; Descendant of (and thus superior to) org-bullets
(use-package org-superstar  ;; Improved version of org-bullets
  :ghook 'org-mode-hook
  :gfhook 'kb/org-superstar-auto-lightweight-mode
  :custom
  ;; Indentation
  ;; The following ensures consistent indentation, overriding `org-indent'
  ;; variables set elsewhere
  (org-hide-leading-stars nil)
  (org-indent-mode-turns-on-hiding-stars nil)
  (org-superstar-remove-leading-stars nil)
  (org-superstar-leading-bullet ?·)

  ;; Headlines
  (org-superstar-headline-bullets-list '("◈" "▷" "◉" "◇" "✳")) ; List inspired from `org-modern'
  (org-n-level-faces 5)
  (org-cycle-level-faces t)
  (org-superstar-cycle-headline-bullets nil) ; Don't repeat bullets in hierarchy

  ;; Todos
  (org-superstar-special-todo-items t)
  ;; Update when I change `org-todo-keywords'
  (org-superstar-todo-bullet-alist
   '(("PROG" . 9744)
     ("ACTIVE" . 9744)
     ("TODO" . 9744)
     ("WAITING" . 9745)
     ("MAYBE" . 9745)
     ("DONE" . 9745)
     ("CANCELLED" . 9745)
     ("[ ]"  . 9744)
     ("[X]"  . 9745)))

  ;; Plain lists
  (org-superstar-prettify-item-bullets t)
  (org-superstar-first-inlinetask-bullet ?▶)
  (org-superstar-item-bullet-alist
   '((?+ . "◦")                         ; List taken from `org-modern'
     (?- . "–")
     (?* . "‣")))
  :custom-face
  ;; Make a good non-distracting foreground color and ensure headlines are
  ;; aligned with headline content
  (org-superstar-leading ((t (:inherit (fixed-pitch org-hide)))))
  :init
  ;; See https://github.com/emacsmirror/org-superstar#fast-plain-list-items
  (defun kb/org-superstar-auto-lightweight-mode ()
    "Start Org Superstar differently depending on the number of lists items."
    (let ((list-items
           (count-matches "^[ \t]*?\\([+-]\\|[ \t]\\*\\)"
                          (point-min) (point-max))))
      (unless (< list-items 100)
        (org-superstar-toggle-lightweight-lists)))))

;;;;; Org-bars
(use-package org-bars
  :disabled                    ; Not much value, and sometimes even distracting
  :ensure (org-bars :type git :host github :repo "tonyaldon/org-bars")
  :ghook 'org-mode-hook
  :init
  ;; Set these in init for some reason
  (setq org-bars-with-dynamic-stars-p nil ; Custom headline stars?
        org-bars-org-indent-mode t
        org-bars-extra-pixels-height 6 ; Use when headline font is larger than 1.0
        org-bars-color-options
        '(:desaturate-level-faces 30
                                  :darken-level-faces 15)))

;;;;; Olivetti
;; Better writing environment
(use-package olivetti
  :hook (((org-mode Info-mode emacs-news-view-mode org-msg-edit-mode) . olivetti-mode)
         (kb/themes . (lambda ()
                        (with-eval-after-load 'olivetti
                          (set-face-attribute 'olivetti-fringe nil
                                              :background (modus-themes-with-colors bg-dim)
                                              :inherit 'unspecified)))))
  :custom
  (olivetti-lighter nil)
  (olivetti-body-width 0.55)
  (olivetti-minimum-body-width 80)
  (olivetti-margin-width 8)
  (olivetti-style 'fancy)  ; NOTE 2024-01-05: "Fancy" works best with org-margin

  ;; FIXME 2024-01-11: This is a temporary solution. Olivetti's changing of
  ;; margins and fringes messes with the calculation of
  ;; `mode--line-format-right-align', which determines where the right side of
  ;; the mode line is placed.
  (mode-line-format-right-align
   '(:eval (if (and (boundp 'olivetti-mode) olivetti-mode)
               (let ((mode-line-right-align-edge 'right-fringe))
                 (mode--line-format-right-align))
             (mode--line-format-right-align)))))

;;;;; Org-appear
;; Show hidden characters (e.g. emphasis markers, link brackets) when point is
;; over enclosed content
(use-package org-appear
  :ghook 'org-mode-hook
  :custom
  (org-appear-delay 0.0)
  (org-appear-trigger 'always)
  (org-appear-autoemphasis t)
  (org-appear-autolinks 'just-brackets)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-inside-latex t))

;;;;; Org-modern
(use-package org-modern
  :disabled              ; NOTE 2024-01-05: Trying-org margin for visual clarity
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-hide-stars nil)           ; Adds extra indentation
  (org-modern-label-border 'auto)
  (org-modern-todo nil)
  (org-modern-keyword nil)
  (org-modern-timestamp t)
  (org-modern-table t)
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 0)
  (org-modern-list nil)
  (org-modern-priority nil))

;;;;; Org-modern-indent
(use-package org-modern-indent
  :disabled              ; NOTE 2024-01-05: Trying-org margin for visual clarity
  :after org
  :hook (org-mode . org-modern-indent-mode)
  :ensure (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent"))

;;;;; Org-extra-emphasis
;; Easier addition and modification of emphasis markers in org. Also has many
;; built-in faces and markup
(use-package org-extra-emphasis
  ;; FIXME 2023-07-12: Too much of a pain with elpaca, poorly maintained; if I
  ;; want to enable again, remove the `ox-odt' dependency manually
  :disabled
  :ensure (:type git :host github :repo "QiangF/org-extra-emphasis")
  :demand
  :after ox-odt
  :custom
  (org-extra-emphasis-alist
   '(("!!" org-extra-emphasis-01)
     ("!@" org-extra-emphasis-02)
     ("!%" org-extra-emphasis-03)
     ("!&" org-extra-emphasis-04)
     ("@!" org-extra-emphasis-05)
     ("@@" org-extra-emphasis-06)
     ("@%" org-extra-emphasis-07)
     ("@&" org-extra-emphasis-08)
     ("%!" org-extra-emphasis-09)
     ("%@" org-extra-emphasis-10)
     ("%%" org-extra-emphasis-11)
     ("%&" org-extra-emphasis-12)
     ("&!" org-extra-emphasis-13)
     ("&@" org-extra-emphasis-14)
     ("&%" org-extra-emphasis-15)
     ("&&" org-extra-emphasis-16)
     ;; My own
     ("&"
      (:box t))))
  :custom-face
  (org-extra-emphasis-01 ((t (:foreground "red" :background unspecified :inherit org-extra-emphasis))))
  :config
  ;; This command isn't autoloaded...
  (org-extra-emphasis-mode 1))

;;;;; Svg-lib
;; Common dependency for Rougier's packages
(use-package svg-lib
  :custom
  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/"))) ; Change cache dir

;;;;; Org-margin
(use-package org-margin
  :disabled
  :ensure (:type git :host github :repo "rougier/org-margin")
  :hook (org-mode . org-margin-mode)
  :custom
  (org-startup-indented nil)            ; Not compatible
  (org-margin-max-level 8)
  (org-margin-headers
   (list (cons 'stars (list (propertize "▸     " 'face '(fixed-pitch org-level-1))
                            (propertize " ▸    " 'face '(fixed-pitch org-level-2))
                            (propertize "  ▸   " 'face '(fixed-pitch org-level-3))
                            (propertize "   ▸  " 'face '(fixed-pitch org-level-4))
                            (propertize "    ▸ " 'face '(fixed-pitch org-level-5))
                            (propertize "     ▸" 'face '(fixed-pitch org-level-6))))
         (cons 'H-txt (list (propertize "    H1" 'face '(fixed-pitch org-level-1))
                            (propertize "    H2" 'face '(fixed-pitch org-level-2))
                            (propertize "    H3" 'face '(fixed-pitch org-level-3))
                            (propertize "    H4" 'face '(fixed-pitch org-level-4))
                            (propertize "    H5" 'face '(fixed-pitch org-level-5))
                            (propertize "    H6" 'face '(fixed-pitch org-level-6))))
         ;; (cons 'H-svg (list (svg-lib-tag "H1" 'org-level-1)
         ;;                    (svg-lib-tag "H2" 'org-level-2)
         ;;                    (svg-lib-tag "H3" 'org-level-3)
         ;;                    (svg-lib-tag "H4" 'org-level-4)
         ;;                    (svg-lib-tag "H5" 'org-level-5)
         ;;                    (svg-lib-tag "H6" 'org-level-6)))
         ))
  (org-margin-headers-set 'stars)
  ;; (org-margin-headers-set 'H-txt)
  ;; (org-margin-headers-set 'H-svg)
  (org-margin-markers
   (list (cons "\\(#\\+begin_src\\)"
               (propertize "     " 'face '(fixed-pitch font-lock-comment-face)))
         (cons "\\(#\\+begin_quote\\)"
               (propertize "     " 'face '(fixed-pitch font-lock-comment-face)))
         (cons "\\(#\\+begin_comment\\)"
               (propertize "    C " 'face '(fixed-pitch font-lock-comment-face)))
         (cons "\\(# TODO\\)"
               (propertize "    T " 'face '(fixed-pitch font-lock-comment-face)))
         (cons "\\(# NOTE\\)"
               (propertize "    N " 'face '(fixed-pitch font-lock-comment-face))))))

;;;; Ancillary functionality
;;;;; Org-web-tools
;; Paste https links with automatic descriptions
(use-package org-web-tools
  :general (kb/yank-keys
             :keymaps 'org-mode-map
             "b" 'org-web-tools-insert-link-for-url)
  :config
  ;; Immediately enter view mode
  (advice-add 'org-web-tools-read-url-as-org :after (lambda (&rest r) (view-mode))))

;;;;; Org-download
;; Insert images and screenshots into select modes
(use-package org-download
  :ensure-system-package (scrot)
  :hook (org-mode . org-download-enable)
  :general (kb/yank-keys
             :keymaps 'org-mode-map
             "i" 'org-download-clipboard)
  :custom
  (org-download-method 'attach)
  (org-download-screenshot-method "scrot -s %s") ; Use scrot
  (org-download-image-dir org-attach-id-dir)
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y-%m-%d_%H-%M-%S_") ; Default
  (org-download-image-html-width 700))

;;;;; Typo-mode
;; Typography stuff for quotations, hyphens, back-ticks, etc.
(use-package typo
  :hook ((typo-mode org-mode) . kb/typo-modify-syntax-table)
  ;; :ghook 'org-mode-hook
  :init
  ;; Add characters (e.g. curly quotes) to syntax table
  (defun kb/typo-modify-syntax-table ()
    "Locally modify the current buffer's syntax table.
Allows our special characters to be recognized as delimiters."
    (modify-syntax-entry (string-to-char "“") "(”")
    (modify-syntax-entry (string-to-char "”") ")“")
    (modify-syntax-entry (string-to-char "‘") "(’")
    (modify-syntax-entry (string-to-char "’") ")‘"))
  :config
  (defun kb/typo-insert-cycle (cycle)
    "Insert the strings in CYCLE"
    (let ((i 0)
          (repeat-key last-input-event)
          repeat-key-str)
      (insert (nth i cycle))
      (setq repeat-key-str (format-kbd-macro (vector repeat-key) nil))
      (while repeat-key

        ;; Don't show the echo messages about cycling when typing in the
        ;; minibuffer
        (unless (minibufferp (current-buffer))
          (message "(Inserted %s; type %s for other options)"
                   (typo-char-name (nth i cycle))
                   repeat-key-str))

        (if (equal repeat-key (read-event))
            (progn
              (clear-this-command-keys t)
              (delete-char (- (length (nth i cycle))))
              (setq i (% (+ i 1)
                         (length cycle)))
              (insert (nth i cycle))
              (setq last-input-event nil))
          (setq repeat-key nil)))
      (when last-input-event
        (clear-this-command-keys t)
        (setq unread-command-events (list last-input-event)))))
  (advice-add 'typo-insert-cycle :override #'kb/typo-insert-cycle)

  ;; My own cycles
  (define-typo-cycle typo-cycle-right-single-quotation-mark
                     "Cycle through the typewriter apostrophe and the right quotation mark.

If used with a numeric prefix argument N, N typewriter
apostrophes will be inserted."
                     ("'" "’")))                          ; Swapped these two

;;;;; Custom org-src-block for paragraphs
;; Also see package `org-edit-indirect'. Does something similar but more
;; versatile, relying on `edit-indirect'. However, this doesn't use native
;; `org-src' infrastructure
(with-eval-after-load 'org
  ;; See `org-edit-indirect-generic-block' from `org-edit-indirect' if I want to
  ;; expand this function to be more versatile
  (defun kb/org-edit-paragraph-block (&optional split)
    "Edit paragraph block at point.

Like `org-edit-comment-block’.

Throw an error when not at a paragraph block."
    (interactive)
    (let* ((element (org-element-at-point))
           (beg (org-element-property :contents-begin element))
           (end (org-element-property :contents-end element)))
      (unless (and (eq (org-element-type element) 'paragraph)
                   (org-src--on-datum-p element))
        (user-error "Not in a paragraph block"))
      (org-src--edit-element
       element
       (org-src--construct-edit-buffer-name (buffer-name) "paragraph")
       'org-mode
       (lambda () (org-escape-code-in-region (point-min) (point-max)))
       (org-unescape-code-in-string (buffer-substring beg end)))
      ;; I don't think I can do this with `org-src-mode-hook' only when
      ;; `kb/org-edit-paragraph-block', so I hardcode it here instead
      (when split (kb/para-split-sentences))
      t))


  ;; Override initial
  (defun kb/org-edit-special (&optional arg)
    "Call a special editor for the element at point.
When at a table, call the formula editor with `org-table-edit-formulas'.
When in a source code block, call `org-edit-src-code'.
When in a fixed-width region, call `org-edit-fixed-width-region'.
When in an export block, call `org-edit-export-block'.
When in a comment block, call `org-edit-comment-block'.
When in a LaTeX environment, call `org-edit-latex-environment'.
When at an INCLUDE, SETUPFILE or BIBLIOGRAPHY keyword, visit the included file.
When at a footnote reference, call `org-edit-footnote-reference'.
When at a planning line call, `org-deadline' and/or `org-schedule'.
When at an active timestamp, call `org-time-stamp'.
When at an inactive timestamp, call `org-time-stamp-inactive'.
On a link, call `ffap' to visit the link at point.

On a paragraph, call `kb/org-edit-paragraph-block’.

Otherwise, return a user error."
    (interactive "P")
    (let ((element (org-element-at-point)))
      (barf-if-buffer-read-only)
      (pcase (org-element-type element)
        (`src-block
         (if (not arg) (org-edit-src-code)
           (let* ((info (org-babel-get-src-block-info))
                  (lang (nth 0 info))
                  (params (nth 2 info))
                  (session (cdr (assq :session params))))
             (if (not session) (org-edit-src-code)
               ;; At a source block with a session and function called
               ;; with an ARG: switch to the buffer related to the
               ;; inferior process.
               (switch-to-buffer
                (funcall (intern (concat "org-babel-prep-session:" lang))
                         session params))))))
        (`keyword
         (unless (member (org-element-property :key element)
                         '("BIBLIOGRAPHY" "INCLUDE" "SETUPFILE"))
           (user-error "No special environment to edit here"))
         (let ((value (org-element-property :value element)))
           (unless (org-string-nw-p value) (user-error "No file to edit"))
           (let ((file (and (string-match "\\`\"\\(.*?\\)\"\\|\\S-+" value)
                            (or (match-string 1 value)
                                (match-string 0 value)))))
             (when (org-url-p file)
               (user-error "Files located with a URL cannot be edited"))
             (org-link-open-from-string
              (format "[[%s]]" (expand-file-name file))))))
        (`table
         (if (eq (org-element-property :type element) 'table.el)
             (org-edit-table.el)
           (call-interactively 'org-table-edit-formulas)))
        ;; Only Org tables contain `table-row' type elements.
        (`table-row (call-interactively 'org-table-edit-formulas))
        (`example-block (org-edit-src-code))
        (`export-block (org-edit-export-block))
        (`comment-block (org-edit-comment-block))
        (`fixed-width (org-edit-fixed-width-region))
        (`latex-environment (org-edit-latex-environment))
        (`planning
         (let ((proplist (cadr element)))
           (mapc #'call-interactively
                 (remq nil
                       (list
                        (when (plist-get proplist :deadline) #'org-deadline)
                        (when (plist-get proplist :scheduled) #'org-schedule))))))
        (_
         ;; No notable element at point.  Though, we may be at a link or
         ;; a footnote reference, which are objects.  Thus, scan deeper.
         (let ((context (org-element-context element)))
           (pcase (org-element-type context)
             (`footnote-reference (org-edit-footnote-reference))
             (`inline-src-block (org-edit-inline-src-code))
             (`latex-fragment (org-edit-latex-fragment))
             (`timestamp (if (eq 'inactive (org-element-property :type context))
                             (call-interactively #'org-time-stamp-inactive)
                           (call-interactively #'org-time-stamp)))
             (`link (call-interactively #'ffap))
             (`paragraph (kb/org-edit-paragraph-block arg))
             (_ (user-error "No special environment to edit here"))))))))
  (advice-add 'org-edit-special :override 'kb/org-edit-special))

;;;;; Org-visibility
;; Persist org headline folded/unfolded states
(use-package org-visibility
  :disabled
  :diminish
  :ghook 'org-mode-hook
  :general
  (:keymaps 'org-visibility-mode-map
            :prefix "C-x"
            "C-v" 'org-visibility-force-save ; Originally bound to `find-alternative-file'
            "M-v" 'org-visibility-remove)
  :custom
  (org-visibility-state-file (no-littering-expand-var-file-name "org/.org-visibility"))
  (org-visibility-include-paths `(,org-directory))
  (org-visibility-include-regexps '("\\.org\\'"))
  (org-visibility-exclude-paths nil)
  (org-visibility-maximum-tracked-files 500)
  (org-visibility-maximum-tracked-days 60)
  (org-visibility-display-messages nil)) ; Annoying echo area updates

(provide 'org-general-rcp)
;;; org-general-rcp.el ends here
