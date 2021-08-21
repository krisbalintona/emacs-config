;;; evil-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Evil packages!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)

;;;; Evil
;; Emacs vim integration layer
(use-package evil
  :demand t
  :hook (git-commit-mode . evil-insert-state) ; For magit commits
  :ghook 'after-init-hook
  :general
  (:states 'normal
           "K" 'join-line
           "J" '(lambda () (interactive) (join-line 1)))
  (:states 'insert
           ;; TODO 2021-08-21: For some reason, the second one doesn't load
           "C-P" 'evil-paste-before
           "C-p" 'evil-paste-after)
  (:states '(normal insert visual)
           ;; TODO 2021-08-21: Look into `better-jumper' to see if I can
           ;; ameliorate evil's shortcomings
           "C-:" 'evil-jump-forward)
  (:states '(normal visual motion)
           "zi" 'org-toggle-inline-images)
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
  :init
  ;; A lot of the settings need to be set before evil initializes
  (setq evil-want-integration t
        evil-want-keybinding nil ; Add more keybinds for other modes I don't want
        evil-want-C-u-scroll t ; Rebind C-u from universal argument to evil scroll up
        evil-want-C-i-jump nil
        evil-respect-visual-line-mode t ; Don't skip lines in visual-line-mode
        evil-want-Y-yank-to-eol t
        evil-move-cursor-back nil
        evil-move-beyond-eol t
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-echo-state nil ; Don't echo state in echo area
        evil-undo-system 'undo-fu)
  :config
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'eshell-mode 'insert)
  )

;;;; Evil-collection
;; Evil keybinds for many other modes
(use-package evil-collection
  :demand t ; Load now or it won't
  :requires evil
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
               xwidget)
   )
  :config
  (evil-collection-init) ; Load immediately so other keybind calls won't be overridden in config
  )

;;;; Evil-commentary
;; Comment in evil-mode
(use-package evil-commentary
  :requires evil
  :after evil
  :ghook 'evil-mode-hook
  )

;;;; Evil-org
;; Additional evil keybinds in org-mode
(use-package evil-org
  :requires evil
  :after (evil-collection org)
  :functions evil-org-agenda-set-keys
  :ghook 'org-mode-hook
  :gfhook '(lambda () (evil-org-set-key-theme '(navigation insert textobjects additional calendar)))
  :general
  ;; (:states '(normal visual motion)
  ;;          "ge" 'evil-backward-word-end)
  (:keymaps '(org-mode-map LaTeX-mode-map)
            [remap evil-first-non-blank] 'evil-org-beginning-of-line) ; To respect visual-line-mode
  :init
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  :custom
  (org-special-ctrl-a/e t) ; Make ^ and $ ignore tags and leading stars
  )

;;;; Evil-surround
;; Surround a selection with any pair of characters
(use-package evil-surround
  :requires evil
  :after evil
  :ghook ('evil-mode-hook 'global-evil-surround-mode)
  )

;;;; Evil-visualstar
;; Situational convenient isearch
(use-package evil-visualstar
  :requires evil
  :after evil
  :functions global-evil-surround-mode
  :ghook ('evil-mode-hook 'global-evil-visualstar-mode)
  :custom
  (evil-visualstar/persistent t) ; Allow visual-mode to remain in affect to allow repeating searches
  )

;;; evil-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'evil-rcp)
