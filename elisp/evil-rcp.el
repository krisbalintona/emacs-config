;;; evil-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Evil packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;; Evil
;; Emacs vim integration layer
(use-package evil
  :hook (after-init . evil-mode)
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
        evil-visual-state-cursor 'hollow
        evil-echo-state nil ; Don't echo state in echo area
        evil-undo-system 'undo-fu)
  :config
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'eshell-mode 'insert)
  (add-hook 'git-commit-mode-hook 'evil-insert-state) ; For magit commits

  (evil-define-key 'insert 'global (kbd "C-g") 'evil-escape)
  (evil-define-key '(normal insert visual) 'global (kbd "C-:") 'evil-jump-forward)
  (evil-define-key '(normal visual) 'global (kbd "zi") 'org-toggle-inline-images)

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
  )

;;;; Evil-collection
;; Evil keybinds for many other modes
(use-package evil-collection
  :after evil
  :init (evil-collection-init)
  :custom
  (evil-collection-setup-minibuffer nil)
  (evil-collection-outline-bind-tab-p nil)
  )

;;;; Evil-commentary
;; Comment in evil-mode
(use-package evil-commentary
  :after (evil evil-collection)
  :hook (evil-mode . evil-commentary-mode)
  )

;;;; Evil-org
;; Additional evil keybinds in org-mode
(use-package evil-org
  :after (evil evil-collection org)
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . (lambda ()
                            (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
                            ))
         )
  :init
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  :custom
  (org-special-ctrl-a/e t) ; Make ^ and $ ignore tags and leading stars
  :config
  (general-define-key
   :keymaps '(org-mode-map LaTeX-mode-map)
   [remap evil-first-non-blank] 'evil-org-beginning-of-line ; To respect visual-line-mode
   )
  )

;;;; Evil-surround
;; Surround a selection with a pair of characters
(use-package evil-surround
  :init (global-evil-surround-mode)
  )

;;;; Evil-visualstar
;; Situational convenient isearch
(use-package evil-visualstar
  :custom
  (evil-visualstar/persistent t) ; Allow visual-mode to remain in affect to allow repeating searches
  :init (global-evil-visualstar-mode)
  )

;;; evil-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'evil-rcp)
