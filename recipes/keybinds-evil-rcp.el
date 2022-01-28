;;; keybinds-evil-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Evil packages!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Evil
;; Emacs vim integration layer
(use-package evil
  :gfhook 'general-evil-setup    ; Set up `general.el' infrastructure for `evil'
  :general
  ([remap undo] 'evil-undo
   [remap undo-redo] 'evil-redo)
  (:states 'insert
           "<escape>" 'evil-normal-state)
  (kb/window-keys
    "w" 'evil-window-mru

    "c" 'evil-window-delete
    "o" 'delete-other-windows

    "L" 'evil-window-move-far-right
    "H" 'evil-window-move-far-left
    "J" 'evil-window-move-very-bottom
    "K" 'evil-window-move-very-top

    "l" 'evil-window-right
    "h" 'evil-window-left
    "j" 'evil-window-down
    "k" 'evil-window-up

    "v" 'evil-window-vsplit
    "s" 'evil-window-split

    "r" 'evil-window-rotate-downwards
    "R" 'evil-window-rotate-upwards
    )
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)    ; Add more keybinds for other modes I don't want

  (evil-want-change-word-to-end nil)    ; `cw' deletes to beginning of next word
  (evil-want-C-u-scroll t)              ; `C-u' scroll up?
  (evil-want-C-u-delete t)       ; `C-u' in insert state deletes to indentation?
  (evil-want-C-i-jump t)         ; `C-i' for evil-jump-backward?
  (evil-want-C-w-delete t)       ; `C-w' deletes word in insert state?

  (evil-move-cursor-back nil)
  (evil-backspace-join-lines t) ; Join lines when deleting newline char from backward delete?
  (evil-respect-visual-line-mode t)     ; Don't skip lines in visual-line-mode

  (evil-want-fine-undo t)               ; More granular undos
  (evil-show-paren-range 0)
  (evil-kill-on-visual-paste nil)       ; Add replaced text to kill ring?

  (evil-esc-mode t)
  (evil-search-wrap nil)                    ; Don't wrap when searching buffer
  (evil-echo-state nil)                     ; Don't echo state in echo area
  :preface (use-package goto-chg :demand t) ; Dependency for `g\;' and `g\,'
  :init                                     ; Has to be init, not config
  (setq evil-normal-state-cursor 'box ; Set these variables since they aren't custom variables
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        evil-emacs-state-cursor 'hbar
        evil-want-Y-yank-to-eol t)      ; Must be set here for some reason
  (evil-mode)
  )

;;; Evil-collection
;; Evil keybinds for many other modes
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer nil)
  (evil-collection-outline-bind-tab-p nil)
  (evil-collection-mode-list
   '(2048-game ag alchemist anaconda-mode apropos arc-mode auto-package-update bm bookmark
               (buff-menu "buff-menu")
               calc calendar cider cmake-mode comint company compile consult
               (custom cus-edit)
               cus-theme dashboard daemons deadgrep debbugs debug devdocs dictionary diff-mode dired dired-sidebar disk-usage distel doc-view docker ebib edbi edebug ediff eglot explain-pause-mode elfeed elisp-mode elisp-refs elisp-slime-nav embark emms epa ert eshell eval-sexp-fu evil-mc eww fanyi finder flycheck flymake free-keys geiser ggtags git-timemachine gnus go-mode grep guix hackernews helm help helpful hg-histedit hungry-delete ibuffer image image-dired image+ imenu imenu-list
               (indent "indent")
               indium info ivy js2-mode leetcode lispy log-edit log-view lsp-ui-imenu lua-mode kotlin-mode macrostep man magit magit-todos monky mpdel mu4e mu4e-conversation neotree newsticker notmuch nov
               (occur replace)
               omnisharp org-present zmusic osx-dictionary outline p4
               (package-menu package)
               pass
               (pdf pdf-view)
               popup proced
               (process-menu simple)
               prodigy profiler python quickrun racer racket-describe realgud reftex restclient rg ripgrep rjsx-mode robe rtags ruby-mode scroll-lock sh-script simple slime sly speedbar tab-bar tablist tabulated-list tar-mode telega
               (term term ansi-term multi-term)
               tetris thread tide timer-list transmission trashed tuareg typescript-mode vc-annotate vc-dir vc-git vdiff view vlf vterm w3m wdired wgrep which-key woman xref youtube-dl
               (ztree ztree-diff)
               xwidget))
  :init
  ;; Load immediately (rather than hook) so other keybind calls won't be
  ;; overridden in config
  (evil-collection-init))

;;; Evil-commentary
;; Comment in evil-mode
(use-package evil-commentary
  :disabled t
  :general
  (:keymaps 'evil-commentary-mode-map
            :states 'normal
            "g/" 'evil-commentary-line)
  :init
  (evil-commentary-mode)
  )

;;; Evil-nerd-commenter
;; Comment in evil-mode
(use-package evil-nerd-commenter
  :after evil
  :general
  (:states 'normal                      ; Evil mode keybindings
           "gcc" 'evilnc-comment-or-uncomment-lines
           "g/" 'evilnc-comment-or-uncomment-lines
           ;; (define-key evil-normal-state-map ",cl" 'evilnc-quick-comment-or-uncomment-to-the-line)
           ;; (define-key evil-normal-state-map ",ll" 'evilnc-quick-comment-or-uncomment-to-the-line)
           ;; (define-key evil-normal-state-map ",cp" 'evilnc-comment-or-uncomment-paragraphs)
           ;; (define-key evil-normal-state-map ",cr" 'comment-or-uncomment-region)
           ;; (define-key evil-normal-state-map ",cv" 'evilnc-toggle-invert-comment-line-by-line)
           "gy" 'evilnc-copy-and-comment-lines)
  (:states 'visual
           "gcc" 'evilnc-comment-or-uncomment-lines
           "g/" 'evilnc-comment-or-uncomment-lines
           "gy" 'evilnc-copy-and-comment-lines)
  (:keymaps 'evil-inner-text-objects-map ; Comment itself is text object
            "c" 'evilnc-inner-commenter
            "c" 'evilnc-outer-commenter)
  :config
  (when evilnc-use-comment-object-setup
    ;; Install operator for evil text objects
    (general-define-key
     :states 'normal
     "g." 'evilnc-copy-and-comment-operator
     "g," 'evilnc-comment-operator)
    (general-define-key
     :states 'visual
     "g." 'evilnc-copy-and-comment-operator
     "g," 'evilnc-comment-operator)))

;;; Evil-org
;; Additional evil keybinds in org-mode
(use-package evil-org
  :ghook 'org-mode-hook
  :general
  (:keymaps 'org-mode-map
            [remap evil-first-non-blank] 'evil-org-beginning-of-line) ; Respect visual-line-mode
  (:keymaps 'org-mode-map
            :states 'insert
            "M-l" 'org-metaright
            "M-h" 'org-metaleft
            "C-<return>" 'evil-org-return
            )
  ;; Since I changed the binding for `evil-outer-text-objects-map' and
  ;; `exchange-point-and-mark', I bind these myself.
  (:keymaps 'evil-outer-text-objects-map
            "e" 'evil-org-an-object
            "E" 'evil-org-an-element
            "r" 'evil-org-a-greater-element
            "R" 'evil-org-a-subtree)
  (:keymaps 'evil-inner-text-objects-map
            "e" 'evil-org-inner-object
            "E" 'evil-org-inner-element
            "r" 'evil-org-inner-greater-element
            "R" 'evil-org-inner-subtree)
  :config
  (evil-org-set-key-theme '(navigation shift todo calendar additional))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)            ; Motion state for org-agenda
  )

;;; Evil-surround
;; Surround a selection with any pair of characters
(use-package evil-surround
  :init
  (global-evil-surround-mode)
  )

;;; Evil-visualstar
;; Situational convenient isearch
(use-package evil-visualstar
  :hook (evil-mode . evil-visualstar-mode)
  :custom
  (evil-visualstar/persistent t) ; Allow visual-mode to remain in affect to allow repeating searches
  )

;;; Evil-matchit
;; Use `%' to jump to matching tags in different major modes (e.g. quotation
;; marks, "<div>" and "</div>" in HTML).
(use-package evil-matchit
  :after evil
  :hook (python-mode . (lambda () (setq-local evilmi-always-simple-jump t)))
  :init
  (global-evil-matchit-mode)
  )

;;; Evil-exchange
;; Swap marked regions
(use-package evil-exchange
  :after evil evil-collection
  :init
  (evil-exchange-install)
  )

;;; Evil-goggles
;; Pulse modified regions.
(use-package evil-goggles
  :after evil
  :custom
  (evil-goggles-duration 0.1)
  :init
  (evil-goggles-mode)
  )

;;; keybinds-evil-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'keybinds-evil-rcp)
