;;; keybinds-kakoune-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Kakoune and friends!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Kakoune
(use-package kakoune
  ;; Having a non-chord way to escape is important, since key-chords don't work
  ;; in macros
  :bind ("C-z" . ryo-modal-mode)
  :hook (after-init . my/kakoune-setup)
  :config
  (defun ryo-enter () "Enter normal mode" (interactive) (ryo-modal-mode 1))
  (defun my/kakoune-setup ()
    "Call kakoune-setup-keybinds and then add some personal config."
    (kakoune-setup-keybinds)
    (setq ryo-modal-cursor-type 'box)
    (add-hook 'prog-mode-hook #'ryo-enter)
    (define-key ryo-modal-mode-map (kbd "SPC h") 'help-command)
    ;; Access all C-x bindings easily
    (define-key ryo-modal-mode-map (kbd "z") ctl-x-map)
    (ryo-modal-keys
     ("," save-buffer)
     ("P" counsel-yank-pop)
     ("m" mc/mark-next-like-this)
     ("M" mc/skip-to-next-like-this)
     ("n" mc/mark-previous-like-this)
     ("N" mc/skip-to-previous-like-this)
     ("M-m" mc/edit-lines)
     ("*" mc/mark-all-like-this)
     ("v" er/expand-region)
     ("C-v" set-rectangular-region-anchor)
     ("M-s" m/c/split-region)
     (";" (("q" delete-window)
           ("v" split-window-horizontally)
           ("s" split-window-vertically)))
     ("C-h" windmove-left)
     ("C-j" windmove-down)
     ("C-k" windmove-up)
     ("C-l" windmove-right)
     ("C-u" scroll-down-command :first '(deactivate-mark))
     ("C-d" scroll-up-command :first '(deactivate-mark)))))

;;; Visual-regexp
;; This overrides the default mark-in-region with a prettier-looking one, and
;; provides a couple extra commands
(use-package visual-regexp
  :ryo
  ("s" vr/mc-mark)
  ("?" vr/replace)
  ("M-/" vr/query-replace))

;;; Phi-search
;; Emacs incremental search doesn't work with multiple cursors, but this fixes
;; that
(use-package phi-search
  :bind (("C-s" . phi-search)
         ("C-r" . phi-search-backward)))


;;; keybinds-kakoune-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'keybinds-kakoune-rcp)
