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

;;;; Evil
;; Emacs vim integration layer
(use-package evil
  :demand t
  :ghook 'after-init-hook
  :gfhook 'general-evil-setup   ; Set up `general.el' infrastructure for `evil'
  :general
  (:states '(normal visual)
           "K" 'join-line
           "J" '(lambda () (interactive) (join-line 1)))
  (:states 'insert
           "<escape>" 'evil-force-normal-state
           "<backtab>" 'evil-delete-back-to-indentation
           "M-P" 'evil-paste-before
           "M-p" 'evil-paste-after)
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
  (evil-want-C-u-scroll t) ; Rebind C-u from universal argument to evil scroll up
  (evil-want-C-i-jump t)   ; C-i keybinding for jumps?
  (evil-want-fine-undo t)               ; More granular undos
  (evil-respect-visual-line-mode t)     ; Don't skip lines in visual-line-mode
  (evil-want-Y-yank-to-eol t)
  (evil-move-cursor-back nil)
  (evil-move-beyond-eol t)
  (evil-normal-state-cursor 'box)
  (evil-insert-state-cursor 'bar)
  (evil-visual-state-cursor 'hollow)
  (evil-emacs-state-cursor 'hbar)
  (evil-echo-state nil)                     ; Don't echo state in echo area
  :preface (use-package goto-chg :demand t) ; Dependency for `g\;' and `g\,'
  )

;;;; Evil-collection
;; Evil keybinds for many other modes
(use-package evil-collection
  :demand t ; Load now or it won't
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
               xwidget)
   )
  :config
  ;; Load immediately (rather than hook) so other keybind calls won't be overridden in config
  (evil-collection-init)
  )

;;;; Evil-commentary
;; Comment in evil-mode
(use-package evil-commentary
  :ghook 'evil-mode-hook
  )

;;;; Evil-surround
;; Surround a selection with any pair of characters
(use-package evil-surround
  :ghook ('evil-mode-hook 'global-evil-surround-mode)
  )

;;;; Evil-visualstar
;; Situational convenient isearch
(use-package evil-visualstar
  :functions global-evil-surround-mode
  :ghook ('evil-mode-hook 'global-evil-visualstar-mode)
  :custom
  (evil-visualstar/persistent t) ; Allow visual-mode to remain in affect to allow repeating searches
  )

;;;; Better-jumper
;; Accompanies `evil-jumper' very well
(use-package better-jumper
  :after evil
  :general (:states '(normal visual normal)
                    [remap evil-jump-backward] '(better-jumper-jump-backward :which-key "Jump backward")
                    [remap evil-jump-forward] '(better-jumper-jump-forward :which-key "Jump forward"))
  :custom
  (better-jumper-max-length 200)
  (better-jumper-use-evil-jump-advice t) ; Add evil-jump jumps
  (better-jumper-add-jump-behavior 'append)

  (better-jumper-context 'buffer)
  (better-jumper-use-savehist t)
  (better-jumper-buffer-savehist-size 50)
  :config
  ;; Add evil navigation commands to the better-jumper jump list
  (general-advice-add '(evil-forward-WORD-end evil-backward-WORD-begin evil-jump-item evil-first-non-blank evil-end-of-visual-line)
                      :after 'better-jumper-set-jump)
  (general-add-hook '(ace-jump-mode-before-jump-hook ace-jump-mode-end-hook) 'better-jumper-set-jump)
  )

;;; keybinds-evil-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'keybinds-evil-rcp)
