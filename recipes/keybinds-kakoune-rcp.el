;;; keybinds-kakoune-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Kakoune and friends!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Ryo-modal
;; Prerequisite for kakoune. Allows for creation of custom modal editing
;; framework.
(use-package ryo-modal
  :demand t
  :custom
  (ryo-modal-cursor-type 'box)
  )

;;; Kakoune
;; Alternative modal editor to evil.
(use-package kakoune
  :demand t
  :after (ryo-modal consult)
  :ryo
  ;; this works now but I've gotten used to not having it
  (:mc-all t :mode 'prog-mode)
  ("b" kakoune-backward-same-syntax :first '(kakoune-set-mark-here))
  ("B" kakoune-backward-same-syntax :first '(kakoune-set-mark-if-inactive))
  ("w" forward-same-syntax :first '(kakoune-set-mark-here))
  ("W" forward-same-syntax :first '(kakoune-set-mark-if-inactive))
  (:mc-all t)
  ;; Region selectors
  ;; ("M-i" (("w" er/mark-word)
  ;;         ("b" er/mark-inside-pairs)
  ;;         ("'" er/mark-inside-quotes)))
  ;; ("M-a" (("w" er/mark-symbol)
  ;;         ("b" er/mark-outside-pairs)
  ;;         ("'" er/mark-outside-quotes)))
  ;; Basic keybindings
  ("a" forward-char :exit t)
  ("A" move-end-of-line :exit t)
  ("b" backward-word :first '(kakoune-set-mark-here))
  ("B" backward-word :first '(kakoune-set-mark-if-inactive))
  ("c" kakoune-d :exit t)
  ("C" kill-line :exit t)
  ("d" kakoune-d)
  ("D" kill-line)
  ("e" evil-forward-word-end :first '(kakoune-set-mark-here))
  ("E" evil-forward-WORD-end :first '(kakoune-set-mark-if-inactive))
  ("f" kakoune-select-to-char :first '(kakoune-set-mark-here))
  ("F" kakoune-select-to-char :first '(kakoune-set-mark-if-inactive))
  ("g" (("h" beginning-of-line)
        ("<left>" beginning-of-line)
        ("j" end-of-buffer)
        ("<down>" end-of-buffer)
        ("k" beginning-of-buffer)
        ("<up>" beginning-of-buffer)
        ("g" kakoune-gg)
        ("l" end-of-line)
        ("<right>" end-of-line)
        ("i" back-to-indentation))
   :first '(kakoune-deactivate-mark))
  ("G" (("h" beginning-of-line)
        ("<left>" beginning-of-line)
        ("j" end-of-buffer)
        ("<down>" end-of-buffer)
        ("k" beginning-of-buffer)
        ("<up>" beginning-of-buffer)
        ("g" kakoune-gg)
        ("l" end-of-line)
        ("<right>" end-of-line)
        ("i" back-to-indentation))
   :first '(kakoune-set-mark-if-inactive))
  ("g f" find-file-at-point)
  ("g x" kakoune-exchange)
  ("g X" kakoune-exchange-cancel)
  ("h" backward-char :first '(kakoune-deactivate-mark))
  ("H" backward-char :first '(kakoune-set-mark-if-inactive))
  ("i" kakoune-insert-mode)
  ("I" back-to-indentation :exit t)
  ("j" next-line :first '(kakoune-deactivate-mark))
  ("J" next-line :first '(kakoune-set-mark-if-inactive))
  ("k" previous-line :first '(kakoune-deactivate-mark))
  ("K" previous-line :first '(kakoune-set-mark-if-inactive))
  ("l" forward-char :first '(kakoune-deactivate-mark))
  ("L" forward-char :first '(kakoune-set-mark-if-inactive))
  ("o" kakoune-o :exit t)
  ("O" kakoune-O :exit t)
  ("p" kakoune-p)
  ("r" kakoune-replace-char)
  ("R" kakoune-replace-selection)
  ("t" kakoune-select-up-to-char :first '(kakoune-set-mark-here))
  ("T" kakoune-select-up-to-char :first '(kakoune-set-mark-if-inactive))
  ("w" forward-word :first '(kakoune-set-mark-here))
  ("W" forward-word :first '(kakoune-set-mark-if-inactive))
  ("M-w" forward-symbol :first '(kakoune-set-mark-here))
  ("M-W" forward-symbol :first '(kakoune-set-mark-if-inactive))
  ("x" kakoune-x)
  ("X" kakoune-X)
  ("y" kill-ring-save)
  ("Y" kakoune-Y)
  ("." kakoune-select-again :first '(kakoune-set-mark-if-inactive))
  ("M-;" exchange-point-and-mark)
  ("`" kakoune-downcase)
  ("~" kakoune-upcase)
  ("%" mark-whole-buffer)
  ("M-j" kakoune-join)
  ("[ [" backward-paragraph :first '(kakoune-set-mark-here))
  ("] ]" forward-paragraph :first '(kakoune-set-mark-here))
  (">" kakoune-indent-right)
  ("<" kakoune-indent-left)
  ;; Treat arrow keys the same as "hjkl"
  ("<down>" next-line :first '(kakoune-deactivate-mark))
  ("<S-down>" next-line :first '(kakoune-set-mark-if-inactive))
  ("<up>" previous-line :first '(kakoune-deactivate-mark))
  ("<S-up>" previous-line :first '(kakoune-set-mark-if-inactive))
  ("<right>" forward-char :first '(kakoune-deactivate-mark))
  ("<S-right>" forward-char :first '(kakoune-set-mark-if-inactive))
  ("<left>" backward-char :first '(kakoune-deactivate-mark))
  ("<S-left>" backward-char :first '(kakoune-set-mark-if-inactive))
  ;; Numeric arguments
  ("0" "M-0" :norepeat t)
  ("1" "M-1" :norepeat t)
  ("2" "M-2" :norepeat t)
  ("3" "M-3" :norepeat t)
  ("4" "M-4" :norepeat t)
  ("5" "M-5" :norepeat t)
  ("6" "M-6" :norepeat t)
  ("7" "M-7" :norepeat t)
  ("8" "M-8" :norepeat t)
  ("9" "M-9" :norepeat t)
  ("-" "M--" :norepeat t)
  ;; Unimpaired-like functionality
  ("[" (("SPC" kakoune-insert-line-above)
        ("p" kakoune-paste-above)))
  ("]" (("SPC" kakoune-insert-line-below)
        ("p" kakoune-paste-below)))
  ;; Multiple cursors
  ("s" mc/mark-all-in-region)
  ("S" mc/split-region)
  ;; Shell commands
  ("|" kakoune-shell-pipe)
  ("!" kakoune-shell-command)
  ;; put these here because they shouldn't be repeated for all cursors
  ("[ b" previous-buffer)
  ("] b" next-buffer)
  ;; Undoing
  ("u" undo-fu-only-undo)
  ("U" undo-fu-only-redo)
  ;; ("u" undo-tree-undo) ("U" undo-tree-redo) ("SPC u" undo-tree-visualize)
  ;; Access all C-x bindings easily
  ("," save-buffer)
  ("P" consult-yank-pop)
  ("m" mc/mark-next-like-this)
  ("M" mc/skip-to-next-like-this)
  ("n" mc/mark-previous-like-this)
  ("N" mc/skip-to-previous-like-this)
  ("M-m" mc/edit-lines)
  ("*" mc/mark-all-like-this)
  ("v" er/expand-region)
  ("C-v" set-rectangular-region-anchor)
  ("M-s" mc/split-region)
  (";" (("q" delete-window)
        ("v" split-window-horizontally)
        ("s" split-window-vertically)))
  ("C-u" scroll-down-command :first '(deactivate-mark))
  ("C-d" scroll-up-command :first '(deactivate-mark))
  :general
  ("C-=" '(lambda ()
            (interactive)
            (if evil-local-mode
                (progn (evil-local-mode -1)
                       (ryo-modal-mode 1))
              (progn (ryo-modal-mode -1)
                     (evil-local-mode 1))
              ))
   "<escape>" '(lambda ()
                 (interactive)
                 (if ryo-modal-mode
                     (keyboard-escape-quit)
                   (progn (ryo-modal-mode 1)
                          (keyboard-escape-quit)
                          )))
   )
  (:keymaps 'ryo-modal-mode-map
            "z" ctl-x-map
            )
  :custom
  (scroll-preserve-screen-position t) ; Preserve cursor location on screen when scrolling
  (ryo-modal-cursor-color nil)          ; Use default color
  )

;;; Visual-regexp
;; This overrides the default mark-in-region with a prettier-looking one, and
;; provides a couple extra commands
(use-package visual-regexp
  :after kakoune
  :ryo
  ("s" vr/mc-mark)
  ("?" vr/replace)
  ("M-/" vr/query-replace)
  )

;;; Phi-search
;; Emacs incremental search doesn't work with multiple cursors, but this fixes
;; that
(use-package phi-search
  :after kakoune
  :bind (("C-s" . phi-search)
         ("C-r" . phi-search-backward))
  )


;;; keybinds-kakoune-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'keybinds-kakoune-rcp)
