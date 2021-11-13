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
  :demand t
  :ghook 'after-init-hook
  :gfhook 'general-evil-setup    ; Set up `general.el' infrastructure for `evil'
  :general
  (:states '(normal visual motion)
           "K" 'join-line
           "J" '(lambda () (interactive) (join-line 1)))
  (:states 'insert
           "<escape>" 'evil-force-normal-state)
  (:keymaps 'evil-visual-state-map
            "a" 'exchange-point-and-mark
            "o" evil-outer-text-objects-map
            "i" evil-inner-text-objects-map)
  (:states '(normal insert visual motion)
           "C-i" 'evil-jump-backward
           "C-o" 'evil-jump-forward)
  (kb/leader-keys
    "ww" 'evil-window-mru

    "wc" 'evil-window-delete
    "wo" 'delete-other-windows

    "wL" 'evil-window-move-far-right
    "wH" 'evil-window-move-far-left
    "wJ" 'evil-window-move-very-bottom
    "wK" 'evil-window-move-very-top

    "wl" 'evil-window-right
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up

    "wv" 'evil-window-vsplit
    "ws" 'evil-window-split

    "wr" 'evil-window-rotate-downwards
    "wR" 'evil-window-rotate-upwards
    )
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)    ; Add more keybinds for other modes I don't want

  (evil-want-change-word-to-end nil)    ; `cw' deletes to beginning of next word
  (evil-want-C-u-scroll t)              ; `C-u' scroll up?
  (evil-want-C-u-delete t)       ; `C-u' in insert state deletes to indentation?
  (evil-want-C-i-jump t)         ; `C-i' for evil-jump-backward?
  (evil-want-C-w-delete nil)     ; `C-w' deletes word in insert state?
  (evil-want-Y-yank-to-eol t)

  (evil-move-cursor-back nil)
  (evil-backspace-join-lines nil) ; Join lines when deleting newline char in backward delete?
  (evil-respect-visual-line-mode t)     ; Don't skip lines in visual-line-mode

  (evil-want-fine-undo t)               ; More granular undos
  (evil-show-paren-range 3)
  (evil-kill-on-visual-paste nil)       ; Add replaced text to kill ring?

  (evil-esc-mode t)
  (evil-search-wrap nil)                    ; Don't wrap when searching buffer
  (evil-echo-state nil)                     ; Don't echo state in echo area
  :preface (use-package goto-chg :demand t) ; Dependency for `g\;' and `g\,'
  :init
  (setq evil-normal-state-cursor 'box ; Set these variables since they aren't custom variables
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        evil-emacs-state-cursor 'hbar)
  )

;;; Evil-collection
;; Evil keybinds for many other modes
(use-package evil-collection
  :demand t ; Load now or it won't
  :custom
  (evil-collection-setup-minibuffer nil)
  (evil-collection-outline-bind-tab-p nil)
  (evil-collection-mode-list
   '(2048-game ag alchemist ;; anaconda
               -mode apropos arc-mode auto-package-update bm bookmark
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
               xwidget)
   )
  :config
  ;; Load immediately (rather than hook) so other keybind calls won't be overridden in config
  (evil-collection-init)
  )

;;; Evil-commentary
;; Comment in evil-mode
(use-package evil-commentary
  :ghook 'evil-mode-hook
  )

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
  :ghook ('evil-mode-hook 'global-evil-surround-mode)
  )

;;; Evil-visualstar
;; Situational convenient isearch
(use-package evil-visualstar
  :hook (evil-mode . global-evil-visualstar-mode)
  :custom
  (evil-visualstar/persistent t) ; Allow visual-mode to remain in affect to allow repeating searches
  )

;;; Evil-matchit
;; Use `%' to jump to matching tags in different major modes (e.g., "<div>" and
;; "</div>" in HTML).
(use-package evil-matchit
  :hook ((evil-mode . global-evil-matchit-mode)
         (python-mode . (lambda () (setq-local evilmi-always-simple-jump t))))
  )

;;; keybinds-evil-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'keybinds-evil-rcp)
