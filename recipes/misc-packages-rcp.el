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

;;; Scratch.el
;; Easily create scratch buffers for different modes
(use-package scratch
  ;; :demand t ; For the initial scratch buffer at startup
  :hook (scratch-create-buffer . kb/scratch-buffer-setup)
  :general ("C-c s" '(scratch :wk "Create scratch"))
  :preface
  (defun kb/scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly. Taken from https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore/"
    (let* ((mode (format "%s" major-mode))
           (string (concat "Scratch buffer for: " mode "\n\n")))
      (when scratch-buffer
        (save-excursion
          (insert string)
          (goto-char (point-min))
          (comment-region (point-at-bol) (point-at-eol)))
        (forward-line 2))
      (rename-buffer (concat "*Scratch for " mode "*") t))
    )
  )

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

;;; Tmr
;; Timer package/library from Prot
(use-package tmr
  :straight (tmr :type git :host gitlab :repo "protesilaos/tmr.el")
  :general ("C-c T t" '(tmr :wk "Tmr")
            "C-c T c" '(tmr-cancel :wk "Tmr cancel"))
  )

;;; Emojify
(use-package emojify
  :custom
  (emojify-composed-text-p t)
  (emojify-emoji-styles '(ascii unicode github))
  )

;;; Unicode-fonts
;; NOTE 2022-01-24: See https://github.com/rolandwalker/unicode-fonts#testing
;; for how to test for its success. Also see the very recommended font
;; installations in the same README. Notably, the following are the listed
;; bare-minimum fonts:
;; DejaVu Sans
;; DejaVu Sans Mono
;; Quivira
;; Symbola
;; Noto Sans
;; Noto Sans Symbols
(use-package unicode-fonts
  :hook (emacs-startup . unicode-fonts-setup)
  :init
  ;; Taken from http://xahlee.info/emacs/misc/emacs_macos_emoji.html
  (set-fontset-font                     ; Set font for symbols
   t
   'symbol
   (cond
    ((member "Symbola" (font-family-list)) "Symbola")))
  (set-fontset-font     ; Set font for emoji (should come after setting symbols)
   t
   '(#x1f300 . #x1fad0)
   (cond
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ((member "Symbola" (font-family-list)) "Symbola"))))

;;; Copy-as-format
(use-package copy-as-format
  :custom
  (copy-as-format-default "slack")
  )

;;; Restart-emacs
(use-package restart-emacs
  :general ("C-x Q" '(restart-emacs :wk "Restart emacs"))
  )

;;; Tempel
;; Small and simple snippet/template system compatible with corfu.
(use-package tempel
  :disabled t                           ; Migrate to yasnippet
  :general
  ("M-+" 'tempel-complete               ; List all available templates
   "M-*" 'tempel-insert                 ; Insert typed template
   )
  (:keymaps 'tempel-map
            "C-M-c" 'tempel-done
            )
  :custom
  (tempel-file (no-littering-expand-var-file-name "tempel-templates"))
  )

;;; Ffap
;; Find file at point
(use-package ffap
  :general (:states '(normal motion)
                    "g F" '(ffap-menu :wk "FFAP menu")
                    )
  :config
  (when (featurep 'vertico)
    ;; Use Vertico (and orderless) instead of a completions buffer
    (advice-add #'ffap-menu-ask :around #'(lambda (&rest args)
                                            (cl-letf (((symbol-function #'minibuffer-completion-help)
                                                       #'ignore))
                                              (apply args)))
                ))
  )

;;; Aggressive-fill-paragraph
;; Fill paragraph as I type.
(use-package aggressive-fill-paragraph
  :hook ((text-mode . aggressive-fill-paragraph-mode)
         (prog-mode . aggressive-fill-paragraph-mode)
         (lsp-mode . (lambda () (aggressive-fill-paragraph-mode -1))))
  :custom
  ;; Settle for only doing comments
  (afp-fill-comments-only-mode-list '(emacs-lisp-mode sh-mode python-mode js-mode org-mode)))

;;; All-the-icons-completion
;; Add `all-the-icons' icons to minibuffer completion candidates
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode)
  )

;;; Plantuml-mode
;; Epic diagrams in org-mode
(use-package plantuml-mode
  :ensure-system-package plantuml
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :custom
  (plantuml-executable-path (executable-find "plantuml"))
  (plantuml-default-exec-mode 'executable)
  :config
  ;; Integration with org-mode. See
  ;; https://github.com/skuro/plantuml-mode#integration-with-org-mode
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  )

;;; Blamer
;; Git blame interface (see a line's corresponding commit)
(use-package blamer
  :after magit
  :commands blamer-show-commit-info
  :general (kb/magit-keys
             "b" '(blamer-show-commit-info :wk "Git blame"))
  :custom
  ;; (blamer-author-formatter " ✎ %s ")
  (blamer-author-formatter "")
  (blamer-datetime-formatter "[%s] ")
  (blamer-commit-formatter "%s")
  (blamer-uncommitted-changes-message "N/A")

  (blamer-max-commit-message-length 71) ; Consider length of acceptable length of git commit
  (blamer-max-lines 30)
  (blamer-min-offset 70)

  (blamer-prettify-time-p t)
  (blamer-idle-time 0.3)
  (blamer-type 'overlay-popup)
  (blamer-view 'overlay)
  (blamer--overlay-popup-position 'smart)
  )

;;; Cycle-at-point
(use-package cycle-at-point)

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
  (kb/general-keys
    "u" '(universal-argument :wk "Universal argument")

    "fF" '(find-file-other-window :wk "Find file other window")
    "fS" '(save-some-buffers :wk "Save most buffers")
    "ff" '(find-file :wk "Find file")
    "fs" '(save-buffer :wk "Save buffer")

    "bn" '(next-buffer :wk "Next buffer")
    "bp" '(previous-buffer :wk "Prev buffer")
    "br" '((lambda ()
             (interactive)
             (revert-buffer nil t))
           :wk "Revert buffer")

    "eb" '(eval-buffer :wk "Eval buffer")
    "ee" '(eval-last-sexp :wk "Eval last sexp")
    "ed" '(eval-defun :wk "Eval top-level form")
    "er" '(eval-region :wk "Eval region")

    "hi" '(info :wk "Info pages")

    "oc" '(calc :wk "Open calculator")
    "om" '((lambda ()
             (interactive)
             (pop-to-buffer "*Messages*"))
           :wk "Open *Messages*"))
  (:keymaps 'universal-argument-map     ; Multiple universal arguments
            "u" 'universal-argument-more
            )
  :config
  (general-unbind 'normal help-mode-map "SPC")
  (general-unbind 'normal custom-mode-map "SPC")
  )

;;; misc-packages-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'misc-packages-rcp)
