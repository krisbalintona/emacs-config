;;; misc-packages-rcp.el --- Summary
;;
;;; Commentary:
;;
;; This is all the configuration of the org-roam package
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Keyfreq
;; See a heatmap of your keypresses.
;; Use =keyfreq-show= to see how many times you used a command. Use =keyfreq-html= to get the original rendered HTML page. Use =keyfreq-html-v2= to get the keyboard heat map.
(use-package keyfreq
  :straight (keyfreq :type git :host github :repo "KirmTwinty/keyfreq")
  :gfhook 'keyfreq-autosave-mode
  :custom
  (keyfreq-folder (concat no-littering-var-directory "keyfreq"))
  ;; Commands not to be logged
  (keyfreq-excluded-commands '(self-insert-command
                               org-self-insert-command
                               ;; forward-char
                               ;; backward-char
                               ;; previous-line
                               ;; next-line
                               ))
  :config (keyfreq-mode)
  )

;;; Disable-mouse
;; Disable mouse interaction within Emacs
(use-package disable-mouse
  :disabled t ; I actually want to use my mouse when on laptop
  :ghook ('window-setup-hook 'global-disable-mouse-mode)
  :config
  ;; For evil states
  (mapc #'disable-mouse-in-keymap
        (list evil-motion-state-map
              evil-normal-state-map
              evil-visual-state-map
              evil-insert-state-map))
  )

;;; Proced
;; Built in process monitor
(use-package proced
  :straight nil
  :gfhook 'evil-emacs-state
  :custom
  (proced-auto-update-flag t)      ; Update live
  (proced-auto-update-interval 1)
  (proced-descend t)                ; Descending order?
  (proced-filter 'all)      ; Which processes are shown?
  )

;;; Adaptive-wrap
;; Wrap lines as if they were hard newlines (like `fill-paragraph'). In other
;; words, lines preserve indentation.
(use-package adaptive-wrap
  :hook (prog-mode . adaptive-wrap-prefix-mode)
  )

;;; Tmr
;; Timer package/library from Prot
(use-package tmr
  :straight (tmr :type git :host gitlab :repo "protesilaos/tmr.el")
  :general ("C-c T t" '(tmr :which-key "Tmr")
            "C-c T c" '(tmr-cancel :which-key "Tmr cancel"))
  )

;;; Emojify
(use-package emojify
  :hook (window-setup . global-emojify-mode)
  :custom
  (emojify-composed-text-p t)
  (emojify-emoji-styles '(ascii unicode github))
  :config
  ;; NOTE 2021-11-12: This isn't related to emojify but it does relate to how
  ;; emojis are shown in Emacs. Taken from
  ;; https://github.com/alphapapa/ement.el#displaying-symbols-and-emojis
  (setf use-default-font-for-symbols nil)
  (set-fontset-font t 'unicode "Noto Emoji" nil 'append)
  )

;;; Better-jumper
;; Accompanies `evil-jumper' very well. Some of the smart stuff is taken from
;; https://www.reddit.com/r/emacs/comments/ntnhkc/how_i_jump_around_emacs_with_betterjumper/
(use-package better-jumper
  :demand t
  :after (evil consult)
  :general (:states '(normal visual normal insert)
                    [remap evil-jump-backward] 'better-jumper-jump-backward
                    [remap evil-jump-forward] 'better-jumper-jump-forward
                    "C-p" 'better-jumper-jump-toggle)
  :custom
  ;; This is THE key to avoiding conflict with evils' jumping functionality
  (better-jumper-use-evil-jump-advice nil)
  ;; (better-jumper-use-evil-jump-advice t) ; Add evil-jump jumps

  (better-jumper-max-length 200)
  (better-jumper-add-jump-behavior 'append)

  (better-jumper-context 'window)
  (better-jumper-use-savehist t)
  (better-jumper-buffer-savehist-size 50)
  :init
  (defun kb/better-jumper-jump-boundary-advice (oldfun &rest args)
    "This is the key here. This advice makes it so you only set a
     jump point if you move more than one line with whatever
     command you call. For example if you add this advice around
     evil-next-line, you will set a jump point if you do 10 j,
     but not if you just hit j.."
    (let ((old-pos (point)))
      (apply oldfun args)
      (when (> (abs (- (line-number-at-pos old-pos) (line-number-at-pos (point))))
               1)
        (better-jumper-set-jump old-pos))))

  ;; Toggle between two between two points (adapted from evil-jump-backward-swap).
  (evil-define-motion better-jumper-jump-toggle (count)
    (let ((pnt (point)))
      (better-jumper-jump-backward 1)
      (better-jumper-set-jump pnt)))
  :config
  (better-jumper-mode)

  ;; Whenever I want to jump, I should wrap it with `kb/better-jumper-jump-boundary-advice'
  (general-advice-add '(evil-forward-WORD-end evil-backward-WORD-begin
                                              evil-jump-item
                                              evil-first-non-blank evil-end-of-visual-line
                                              evil-goto-first-line evil-goto-line evil-goto-mark evil-goto-definition
                                              consult-line
                                              )
                      :around 'kb/better-jumper-jump-boundary-advice)

  ;; Specifically for ace-jump
  (general-add-hook '(ace-jump-mode-before-jump-hook ace-jump-mode-end-hook) 'better-jumper-set-jump)
  )

;;; Copy-as-format
(use-package copy-as-format
  :custom
  (copy-as-format-default "slack")
  )

;;; Built-in Emacs modes/packages
(use-package emacs
  :straight nil
  :hook (messages-buffer-mode . visual-line-mode)
  :general
  ;; Info-mode
  (:keymaps 'Info-mode-map
            :states '(visual normal motion)
            "SPC" nil ; For my leader key
            [remap evil-ret] 'Info-follow-nearest-node)
  (kb/leader-keys
    "hi" '(info :which-key "Info pages"))
  )

;;; misc-packages-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'misc-packages-rcp)
