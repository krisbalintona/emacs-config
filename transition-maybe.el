
;;;; Slimmer minibuffer and echo area faces
;; Make minibuffer and echo fonts a little lighter. Taken from
;; https://www.reddit.com/r/emacs/comments/14q399t/comment/jqm6zr3/?utm_source=share&utm_medium=web2x&context=3
(dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                      " *Minibuf-1*" " *Echo Area 1*"))
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (face-remap-add-relative 'bold :weight 'normal)
      (face-remap-add-relative 'default :weight 'light))))

(add-hook 'minibuffer-setup-hook
          '(lambda ()
             (face-remap-add-relative 'bold :weight 'normal)
             (face-remap-add-relative 'default :weight 'light)))

;;;; Web-mode
;; Compatible with most template engines (e.g. handlebars mode, mustache) and
;; proper indentation based on content (i.e. CSS, HTML, JavaScript, or code).
(use-package web-mode
  :ensure-system-package (handlebars . "sudo npm --global install handlebars") ; For
                                        ; ghost
  :mode ("\\.hbs\\'"                    ; For ghost
         "\\.yaml\\'"
         "\\.html\\'")
  :hook
  ((web-mode . display-line-numbers-mode)
   (web-mode . visual-line-mode))
  :custom
  (web-mode-markup-indent-offset 4)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 4)
  (web-mode-comment-style 1)

  (web-mode-engines-alist
   '(("go" . "\\.html\\'")))            ; For hugo

  ;; Features
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-css-colorization t) ; CSS colorization
  (web-mode-enable-block-face t) ; Block face: set blocks background and default
                                        ; foreground
  (web-mode-enable-part-face t) ; Part face: set parts background and default
                                        ; foreground
  (web-mode-enable-comment-interpolation nil) ; Font lock comment keywords
  (web-mode-enable-heredoc-fontification t) ; Heredoc (cf. PHP strings)
                                        ; fontification

  ;; Other
  (flycheck-handlebars-executable (executable-find "handlebars"))
  :config
  (setf (alist-get "handlebars" web-mode-comment-formats nil nil 'string=) '("{{!")))

;;;; Json-mode
(use-package json-mode
  :ensure-system-package (jsonlint . "sudo npm install --global jsonlint")
  :custom
  (flycheck-json-jsonlint-executable (executable-find "jsonlint")))

;;;; Yaml-mode
(use-package yaml-mode
  :ensure-system-package (js-yaml . "sudo npm install --global js-yaml")
  :hook
  ((yaml-mode . display-line-numbers-mode)
   (yaml-mode . visual-line-mode))
  :custom
  (flycheck-yaml-jsyaml-executable (executable-find "js-yaml")))

;;;; Abdridge-diff
(use-package abridge-diff
  :diminish
  :after diff
  :demand
  :config
  (abridge-diff-mode 1))

;;;; Git-gutter-fringe
(use-package git-gutter-fringe
  :after git-gutter
  :custom-face
  ;; Colors taken from `uninspiring-dark-theme'
  (git-gutter-fr:added ((t (:foreground "#98C379" :weight bold :inherit nil))))
  (git-gutter-fr:deleted ((t (:foreground "#E06C75" :weight bold :inherit nil))))
  (git-gutter-fr:modified ((t (:foreground "#D19A66" :weight bold :inherit nil))))
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(top t))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(top t))
  (define-fringe-bitmap 'git-gutter-fr:deleted [240 240 240 240] nil nil '(top t)))

;;;; Diff-hl
;; Diff information in the margins. Also provides commands for navigating and
;; viewing diff info of the current file. Faster and cleaner than git-gutter and
;; git-gutter-fringes
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (conf-mode . diff-hl-mode)
         (vc-dir-mode . diff-hl-dir-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-draw-borders nil)
  (diff-hl-show-staged-changes nil)
  (diff-hl-update-async t)
  (diff-hl-side 'right)
  (diff-hl-flydiff-delay 1)             ; See `diff-hl-flydiff-mode'
  :config
  (global-diff-hl-show-hunk-mouse-mode 1)

  ;; Ensure buffer is widened before calling `diff-hl-stage-dwim' because the
  ;; buffer cannot be narrowed for it to succeed
  (advice-add 'diff-hl-stage-dwim :around (lambda (orig-fun &rest args)
                                            (save-restriction
                                              (widen)
                                              (apply orig-fun args)))))

;;;; Symbol-overlay
;; Mimics functionality of built-in hi-lock but with overlays instead of
;; font-lock. Usefully has `symbol-overlay-rename'. On highlighted regions, the
;; `symbol-overlay-map' is enabled
(use-package symbol-overlay
  :bind
  (([remap highlight-symbol-at-point] . symbol-overlay-put)
   ("M-s h M-n" . symbol-overlay-switch-forward)
   ("M-s h M-p" . symbol-overlay-switch-backward)
   ("M-s h <f7>" . symbol-overlay-mode)
   ("M-s h <f8>" . symbol-overlay-remove-all)))

;;;;; Display-fill-column-indicator
(use-package display-fill-column-indicator
  :ensure nil
  :custom
  (display-fill-column-indicator-character ?│)
  :custom-face
  (fill-column-indicator ((t (:inherit line-number)))))

;;;; Aesthetics
;;;;; Prog-mode
(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . goto-address-prog-mode)
         (window-setup . global-prettify-symbols-mode)
         (org-mode . (lambda () (setq-local prettify-symbols-compose-predicate 'kb/prettify-symbols-compose-p))))
  :init
  (defun kb/prettify-symbols-compose-p (start end _match)
    "Returns nil except on org-mode exceptions.
        Returns nil if the character before and after MATCH isn't a punctuation,
        with the exception of org-emphasis markers."
    (let ((before (char-to-string (or (char-before start) ? )))
          (after (char-to-string (or (char-after end) ? )))
          (org-emphasis-markers (mapcar #'car org-emphasis-alist)))
      (cond
       ((or (member before org-emphasis-markers)
            (member after org-emphasis-markers))
        t)
       ((or (string-match-p (rx (or (any punct))) before)
            (string-match-p (rx (or (any punct))) after))
        nil)
       (t t))))
  :config
  (add-hook 'latex-mode-hook
            (lambda ()
              (add-to-list 'prettify-symbols-alist '("\\Dashv" . ?⫤))
              (add-to-list 'prettify-symbols-alist '("\\DashVDash" . ?⟚))
              (add-to-list 'prettify-symbols-alist '("\\dashVdash" . ?⊢))
              (delete '("--" . 8211 ) prettify-symbols-alist)
              (delete '("---" . 8212) prettify-symbols-alist)
              ;; For `lplfitch'. Slightly higher than `\vdots'. Using the
              ;; `\pline{\vdots}' results in the ellipses not being centered
              ;; on the line.
              (add-to-list 'prettify-symbols-alist '("\\ellipsesline" . ?⋮))
              ;; Circled numbers from the pifont package
              (add-to-list 'prettify-symbols-alist '("\\ding{192}" . ?①))
              (add-to-list 'prettify-symbols-alist '("\\ding{193}" . ?②))
              (add-to-list 'prettify-symbols-alist '("\\ding{194}" . ?③))
              (add-to-list 'prettify-symbols-alist '("\\ding{195}" . ?④))
              (add-to-list 'prettify-symbols-alist '("\\ding{196}" . ?⑤))
              (add-to-list 'prettify-symbols-alist '("\\ding{197}" . ?⑥))
              (add-to-list 'prettify-symbols-alist '("\\ding{198}" . ?⑦))
              (add-to-list 'prettify-symbols-alist '("\\ding{199}" . ?⑧))
              (add-to-list 'prettify-symbols-alist '("\\ding{200}" . ?⑨))
              (add-to-list 'prettify-symbols-alist '("\\ding{201}" . ?⑩))
              ;; Angle brackets for text (non-math)
              (add-to-list 'prettify-symbols-alist '("\\textlangle" . 10216))
              (add-to-list 'prettify-symbols-alist '("\\textrangle" . 10217)))))

;;;;; Ansi-color
;; Apply ANSI terminal color escape codes.
;; <http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html>
(use-package ansi-color
  :ensure nil
  :autoload endless/colorize-compilation
  :hook (compilation-filter . endless/colorize-compilation)
  :config
  (defun endless/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point)))))

;;;;; Fancy-compilation
;; Better compilation buffers. Has support color output,progress updates on a
;; single line (as used by many build systems),scrolling behavior similar to
;; most terminals,and optionally use foreground & background independent of
;; theme colors.
(use-package fancy-compilation
  :after compile
  :demand
  :custom
  (fancy-compilation-override-colors nil)
  ;; Briefer text
  (fancy-compilation-quiet-prelude t)
  (fancy-compilation-quiet-prolog t)
  :config
  (fancy-compilation-mode 1))



;;;; Yasnippet
;; Template-expansion system (doesn't include templates)
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (on-first-buffer . yas-global-mode)
  :custom
  (yas-alias-to-yas/prefix-p nil)
  (yas-also-auto-indent-first-line t)
  (yas-also-indent-empty-lines nil)
  (yas-inhibit-overlay-modification-protection nil)
  (yas-snippet-revival t)
  (yas-triggers-in-field nil)
  :config
  (add-to-list 'prettify-symbols-alist '("->" . ?»)))

;;;;; Minions
(use-package minions
  :hook
  ((elpaca-after-init after-init) . minions-mode)
  :custom
  (minions-mode-line-lighter "…")
  (minions-mode-line-delimiters '("[" . "]"))
  (minions-prominent-modes
   '(kb/lisp-keyword-indent-mode tree-sitter-mode)))

;;;;; Battery
;; Display batter percentage
(use-package battery
  :disabled
  :ensure nil
  :custom
  (battery-load-critical 15)
  (battery-load-low 25)
  (battery-mode-line-limit 95)
  ;; (battery-mode-line-format "%cmAh")
  ;; (battery-mode-line-format "  %p%%")
  (battery-mode-line-format "%b%p%% ")
  :init
  (display-battery-mode 1))


;;;; Gud
(use-package gud
  :ensure nil
  :custom
  (gud-highlight-current-line t))

;;;; Realgud
(use-package realgud
  :hook (realgud-srcbuf-mode . tool-bar-mode)
  :custom
  (realgud-window-split-orientation 'horizontal)
  (realgud-short-key-on-tracing? t))

;;;; Dape
;; Dap-mode but without LSP-mode
(use-package dape
  :diminish dape-breakpoint-global-mode
  :custom
  (dape-key-prefix nil)                 ; I make my own binding
  (dape-buffer-window-arrangement 'right)
  (dape-stepping-granularity 'instruction)
  (dape-info-variable-table-aligned t)
  :config
  (dape-breakpoint-global-mode 1)

  (bind-key "C-c d" dape-global-map 'prog-mode-map)

  ;; Kill created compile buffer on build success
  (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  (defun kb/dape--save-on-start ()
    "Save buffers on startup."
    (save-some-buffers nil t))
  (add-hook 'dape-on-start-hooks 'kb/dape--save-on-start)

  ;; To display info and repl buffers on stopped
  (add-hook 'dape-on-stopped-hooks 'dape-info)
  (add-hook 'dape-on-stopped-hooks 'dape-repl))

;;;;; Org-download
;; Insert images and screenshots into select modes
(use-package org-download
  :ensure-system-package (scrot)
  :hook (org-mode . org-download-enable)
  :bind
  ( :map krisb-yank-keymap
    ("i" . org-download-clipboard))
  :custom
  (org-download-method 'attach)
  (org-download-screenshot-method "scrot -s %s") ; Use scrot
  (org-download-image-dir (progn (require 'org-attach) org-attach-id-dir))
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y-%m-%d_%H-%M-%S_") ; Default
  (org-download-image-html-width 700))

;;;;; Typo-mode
;; Typography stuff for quotations, hyphens, back-ticks, etc.
(use-package typo
  :disabled                             ; NOTE 2024-09-22: Check out `astute.el'
  :hook
  (;; (org-mode . typo-mode)
   ((typo-mode org-mode) . kb/typo-modify-syntax-table))
  :config
  ;; Add characters (e.g. curly quotes) to syntax table
  (defun kb/typo-modify-syntax-table ()
    "Locally modify the current buffer's syntax table.
Allows our special characters to be recognized as delimiters."
    (modify-syntax-entry (string-to-char "“") "(”")
    (modify-syntax-entry (string-to-char "”") ")“")
    (modify-syntax-entry (string-to-char "‘") "(’")
    (modify-syntax-entry (string-to-char "’") ")‘"))

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

;;;; Ox-clip
(use-package ox-clip
  :ensure-system-package ((xclip)
                          (wl-copy . wl-clipboard))
  :bind ( :map krisb-yank-keymap
          ("x" . ox-clip-formatted-copy))
  :custom
  ;; FIXME 2024-10-07: Doesn't work on wayland for some reason. It's just
  ;; pasting the plain text.
  (ox-clip-linux-cmd (if (string-equal (getenv "XDG_SESSION_TYPE") "wayland")
                         "wl-copy -p --type text/html < $%f"
                       "xclip -verbose -i \"%f\" -t text/html -selection clipboard")))

;;;; Auctex-latexmk
;; Quicker insertion and filling-out of macros. Taken from Doom
(use-package auctex-latexmk
  :after tex
  :demand
  :custom
  (TeX-command-default "LatexMk")
  ;; Pass the -pdf flag when TeX-PDF-mode is active.
  (auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  ;; Add LatexMk as a TeX command
  (auctex-latexmk-setup)

  (define-minor-mode kb/auctex-latexmk-mode
    "Compiles on buffer save using LatexMk command."
    :init-value nil
    :lighter " LMk"
    (let ((cmd (lambda () (TeX-command "LatexMk" #'TeX-master-file))))
      (if kb/auctex-latexmk-mode
          (add-hook 'after-save-hook cmd nil t)
        (remove-hook 'after-save-hook cmd t)))))

;;;; Auctex (auctex)
;;;;; This
;; Collection for most of the following packages. **Package name should be
;; loaded as `auctex' for elpaca, according to my testing**
;; NOTE 2023-07-12: If I haven't already, download the `texlive' and
;; `texlive-langs' package groups
(use-package auctex
  ;; NOTE 2023-07-12: I had trouble with the recipe with elpaca, but I found the
  ;; proper one here (and in Doom):
  ;; https://github.com/radian-software/radian/blob/f403244e244ccca695ff6c73c62b1265e521afa7/emacs/radian.el#L3386-L3506
  ;; :ensure (auctex :type git
  ;;                 :host github
  ;;                 :repo "emacs-straight/auctex"
  ;;                 :files ("*.el" "*.info" "dir"
  ;;                         "doc" "etc" "images" "latex" "style")
  ;;                 :pre-build (("chmod" "775" "autogen.sh") ("./autogen.sh")))
  :ensure-system-package biber)

;;;;; Tex
(use-package tex
  :ensure nil
  :after auctex
  :hook ((TeX-mode . (lambda ()
                       ;; Tell Emacs how to parse TeX files
                       (setq ispell-parser 'tex)
                       ;; Don't auto-fill in math blocks
                       (setq fill-nobreak-predicate
                             (cons #'texmathp fill-nobreak-predicate))
                       (visual-line-mode)))
         (TeX-update-style . rainbow-delimiters-mode))
  :custom
  ;; Base settings
  (TeX-engine 'luatex)
  (TeX-command-default "LuaLaTeX")
  (TeX-source-correlate-start-server nil)
  (TeX-master 'dwim)
  (TeX-parse-self t)
  (TeX-auto-save t)
  (TeX-save-query nil)            ; Just save, don't ask before each compilation

  ;; Compilation
  (TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ; Revert PDF after compilation
  (TeX-command-extra-options "-shell-escape") ; Enables system commands? (because we're out of the shell?)
  (TeX-show-compilation nil)

  ;; Other
  (TeX-electric-sub-and-superscript t)
  (TeX-electric-math '("\\(" . ""))
  :config
  ;; Viewing
  (add-to-list 'TeX-view-program-list '("pdf-tools" TeX-pdf-tools-sync-view))
  (add-to-list 'TeX-view-program-selection '(output-pdf "pdf-tools"))

  ;; Command list
  (add-to-list 'TeX-command-list '("LuaLaTeX" "lualatex --synctex=%(mode)% %t" TeX-run-TeX nil t))
  (add-to-list 'TeX-command-list '("View (Evince)" "evince %(O?pdf)" TeX-run-TeX nil t))

  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 -H %s")) ; Set-up chktex (syntax checking utility)

;;;;; Latex
(use-package latex
  :ensure nil
  :after auctex
  :mode ("\\.[tT]e[xX]\\'" . LaTeX-mode)
  :bind
  ( :map LaTeX-mode-map
    ("C-<return>" . LaTeX-insert-item))
  :hook
  (LaTeX-mode . (lambda ()
                  (TeX-PDF-mode)
                  (TeX-source-correlate-mode) ; Minor mode for forward and inverse search.
                  (TeX-fold-mode)
                  (LaTeX-math-mode) ; Math macros
                  (visual-line-mode)
                  (display-line-numbers-mode)
                  (prettify-symbols-mode)
                  (outshine-mode)
                  (flymake-mode)))
  :custom
  ;; Add the TOC entry to the sectioning hooks
  (LaTeX-section-hook
   '(LaTeX-section-heading
     LaTeX-section-title
     LaTeX-section-toc
     LaTeX-section-section
     LaTeX-section-label))
  (LaTeX-fill-break-at-separators nil) ; Don't be afraid to break inline math between lines
  (LaTeX-item-indent 0)
  (LaTeX-math-menu-unicode t)

  ;; Fontification taken from https://tex.stackexchange.com/a/86119/81279, which
  ;; I discovered from Doom
  (font-latex-match-reference-keywords
   '(;; BibLaTeX.
     ("printbibliography" "[{")
     ("addbibresource" "[{")
     ;; Standard commands.
     ("cite" "[{")
     ("citep" "[{")
     ("citet" "[{")
     ("Cite" "[{")
     ("parencite" "[{")
     ("Parencite" "[{")
     ("footcite" "[{")
     ("footcitetext" "[{")
     ;; Style-specific commands.
     ("textcite" "[{")
     ("Textcite" "[{")
     ("smartcite" "[{")
     ("Smartcite" "[{")
     ("cite*" "[{")
     ("parencite*" "[{")
     ("supercite" "[{")
     ;; Qualified citation lists.
     ("cites" "[{")
     ("Cites" "[{")
     ("parencites" "[{")
     ("Parencites" "[{")
     ("footcites" "[{")
     ("footcitetexts" "[{")
     ("smartcites" "[{")
     ("Smartcites" "[{")
     ("textcites" "[{")
     ("Textcites" "[{")
     ("supercites" "[{")
     ;; Style-independent commands.
     ("autocite" "[{")
     ("Autocite" "[{")
     ("autocite*" "[{")
     ("Autocite*" "[{")
     ("autocites" "[{")
     ("Autocites" "[{")
     ;; Text commands.
     ("citeauthor" "[{")
     ("Citeauthor" "[{")
     ("citetitle" "[{")
     ("citetitle*" "[{")
     ("citeyear" "[{")
     ("citedate" "[{")
     ("citeurl" "[{")
     ;; Special commands.
     ("fullcite" "[{")
     ;; Cleveref.
     ("cref" "{")
     ("Cref" "{")
     ("cpageref" "{")
     ("Cpageref" "{")
     ("cpagerefrange" "{")
     ("Cpagerefrange" "{")
     ("crefrange" "{")
     ("Crefrange" "{")
     ("labelcref" "{")))

  (font-latex-match-textual-keywords
   '(;; BibLaTeX brackets.
     ("parentext" "{")
     ("brackettext" "{")
     ("hybridblockquote" "[{")
     ;; Auxiliary commands.
     ("textelp" "{")
     ("textelp*" "{")
     ("textins" "{")
     ("textins*" "{")
     ;; Subcaption.
     ("subcaption" "[{")))

  (font-latex-match-variable-keywords
   '(;; Amsmath.
     ("numberwithin" "{")
     ;; Enumitem.
     ("setlist" "[{")
     ("setlist*" "[{")
     ("newlist" "{")
     ("renewlist" "{")
     ("setlistdepth" "{")
     ("restartlist" "{")
     ("crefname" "{")))
  :config
  ;; Font locking for personal convenience
  (font-lock-add-keywords 'latex-mode
                          '(;; True
                            ("true" 0 '(t :foreground "green") t)
                            ;; False
                            ("false" 0 '(t :foreground "red") t)
                            ;; For table (tabular) columns
                            ("\\\\rowmac" 0 'font-latex-math-face t)
                            ;; For natural deduction tables via `lplfitch'
                            ("\\\\fitchprf" 0 'font-lock-keyword-face t)
                            ("\\\\pline" 0 'font-latex-math-face t)
                            ("\\\\subproof" 0 'font-latex-warning-face t)
                            ("\\\\boxedsubproof" 0 'font-latex-warning-face t)
                            ("\\\\brokenform" 0 'font-latex-warning-face t)
                            ("\\\\formula" 0 'font-latex-math-face t)
                            ("\\\\fitcharg" 0 'font-lock-keyword-face t)
                            ("\\\\fitchctx" 0 'font-lock-keyword-face t)
                            ("\\\\fpline" 0 'font-latex-math-face t)
                            ("\\\\tpline" 0 'font-latex-math-face t)))

  (with-eval-after-load 'eaf
    ;; Using EAF's pdf viewer
    (defun kb/eaf-pdf-synctex-forward-view ()
      "View the PDF file of Tex synchronously."
      (interactive)
      ;; So that the pdf opens in a vsplit
      (split-window-right)
      (windmove-right)
      (let* ((pdf-url (expand-file-name (TeX-active-master (TeX-output-extension))))
             (tex-buffer (window-buffer (minibuffer-selected-window)))
             (tex-file (buffer-file-name tex-buffer))
             (line-num (progn (set-buffer tex-buffer) (line-number-at-pos)))
             (opened-buffer (eaf-pdf--find-buffer pdf-url))
             (synctex-info (eaf-pdf--get-synctex-info tex-file line-num pdf-url)))
        (if (not opened-buffer)
            (eaf-open
             ;; (prin1-to-string pdf-url)    ; This causes an error
             pdf-url                      ; This fixes the error
             "pdf-viewer" (format "synctex_info=%s" synctex-info))
          (pop-to-buffer opened-buffer)
          (eaf-call-sync "call_function_with_args" eaf--buffer-id
                         "jump_to_page_synctex" (format "%s" synctex-info)))))
    (add-to-list 'TeX-view-program-list '("eaf" kb/eaf-pdf-synctex-forward-view))
    (add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))))

;;;;; Tex-fold
(use-package tex-fold
  :ensure nil
  :after auctex
  :hook ((TeX-mode . TeX-fold-mode)
         (mixed-pitch-mode . (lambda ()
                               "Fix folded things invariably getting fixed pitch when using
mixed-pitch. Math faces should stay fixed by the mixed-pitch
blacklist, this is mostly for \\section etc."
                               (when mixed-pitch-mode
                                 ;; Adding to this list makes mixed-pitch clean
                                 ;; the face remaps after us
                                 (add-to-list 'mixed-pitch-fixed-cookie
                                              (face-remap-add-relative
                                               'TeX-fold-folded-face
                                               :family (face-attribute 'variable-pitch :family)
                                               :height (face-attribute 'variable-pitch :height))))))))
