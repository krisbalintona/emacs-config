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

;;;; Aesthetics
;;;;; Prog-mode
(use-package prog-mode
  :ensure nil
  :hook ((window-setup . global-prettify-symbols-mode)
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
