;;; misc-packages-rcp.el --- Summary
;;
;;; Commentary:
;;
;; This is all the configuration of the org-roam package
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)
(require 'faces-rcp)

;;;; Recentf
;; Enable logging of recent files
(use-package recentf
  :straight nil
  :custom
  (recentf-mode t)
  (recentf-max-saved-items 1000)
  (recentf-max-menu-items 15)
  )

;;;; Abbrev-mode
;; Automatically correct typed strings (e.g. words)
(use-package abbrev-mode
  :straight nil
  :ghook 'text-mode-hook
  )

;;;; Elisp-demos
;; Add example code snippets to some of the help windows
(use-package elisp-demos
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
  )

;;;; Helpful
;; Have more descriptive and helpful function and variable descriptions
(use-package helpful
  :general
  ;; Remap all default help commands to helpful
  ([remap describe-function] 'helpful-callable ; Emacs' describe-function includes both functions and macros
   [remap describe-command] 'helpful-command
   [remap describe-variable] 'helpful-variable
   [remap describe-symbol] 'helpful-symbol
   [remap describe-key] 'helpful-key
   )
  (:states '(visual normal motion)
           "K" 'helpful-at-point
           )
  (kb/leader-keys
    "hk" '(helpful-key :which-key "Desc key")
    )
  )

;;;; Save-place-mode
;; Save and restore the point's location in files
(use-package saveplace
  :straight nil
  :ghook ('after-init-hook 'save-place-mode)
  )

;;;; Dogears
;; Save and return to exact locations when you want, where you want
(use-package dogears
  :disabled t ; For now
  :straight (dogears :type git :host github :repo "alphapapa/dogears.el")
  :ghook 'after-init-hook
  :general
  (:keymaps 'dogears-list-mode-map
            :states '(motion normal visual)
            "dd" 'dogears-list-delete
            "RET" 'dogears-list-go
            )
  (kb/leader-keys
    "bd" '(dogears-go :which-key "Dogears go")
    "bD" '(dogears-list :which-key "Dogears list")
    )
  :custom
  (dogears-limit 200)
  (dogears-line-width 40)
  (dogears-ignore-places-functions
   '(dogears--ignored-mode-p
     minibufferp
     (lambda () (string-match "^/tmp/" (expand-file-name buffer-file-truename)))
     )
   )
  (dogears-ignore-modes
   '(fundamental-mode special-mode helm-major-mode
                      dogears-list-mode messages-buffer-mode custom-mode helpful-mode elfeed-search-mode elfeed-show-mode org-roam-mode embark-collect-mode man-mode flycheck-error-list-mode ledger-report-mode

                      magit-status-mode magit-log-mode magit-wip-mode magit-diff-mode magit-blob-mode magit-refs-mode magit-stash-mode magit-blame-mode magit-reflog-mode magit-cherry-mode magit-proces-mode magit-section-mode magit-stashes-mode magit-repolist-mode magit-revision-mode magit-log-select-mode magit-merge-preview-mode magit-wip-after-save-mode magit-submodule-list-mode magit-blame-read-only-mode magit-wip-after-apply-mode magit-wip-before-apply-mode magit-wip-initial-backup-mode magit-wip-after-save-local-mode unpackaged/magit-log-date-headers-mode

                      )
   )
  (dogears-functions
   '(
     ))
  (dogears-hooks
   '(imenu-after-jump-hook
     change-major-mode-hook
     eyebrowse-pre-window-switch-hook
     window-configuration-change-hook
     ))
  )

;;;; Display-line-numbers-mode
;; Show line numbers on the left fringe
(use-package display-line-numbers
  :straight nil
  :ghook ('prog-mode-hook 'LaTeX-mode-hook)
  ;; :ghook 'prog-mode-hook
  :gfhook 'column-number-mode ; Column number in modeline
  :custom
  (display-line-numbers-type 'relative)
  )

;;;; Which-key
;; Show keybind tooltips
(use-package which-key
  :ghook 'after-init-hook
  :custom
  ;; These variables should be set before which-key-mode is activated
  (which-key-idle-delay 1.6)
  (which-key-idle-secondary-delay 1) ; Delay after which-key has already been shown
  (which-key-show-early-on-C-h t) ; Show which-key help immediately
  (which-key-add-column-padding 0)
  (which-key-max-display-columns nil)
  ;; (which-key-show-transient-maps t) ; Necessary so show embark keybinds with which-key
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'right)
  (which-key-side-window-max-width 0.23)
  :config
  ;; Don't display C-u, digit, and other numeric keybad bindings
  (push '(("^[0-9-]\\|kp-[0-9]\\|kp-subtract\\|C-u$" . nil) . ignore)
        which-key-replacement-alist)
  )

;;;; Beacon
;; Cool version of nav-flash
(use-package beacon
  :ghook 'after-init-hook
  :custom
  (beacon-blink-when-focused t) ; Blink when Emacs comes into focus
  (beacon-blink-delay 0.04)
  (beacon-blink-duration 0.5)
  )

;;;; All-the-icons
;; Provides a bunch of unicode icons which many other packages leverage
(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1.1)
  )

;;;; Hide-mode-line
;; Hide the modeline when you don't want to see it
(use-package hide-mode-line)

;;;; Super-save
;; Automatically save buffers when you do certain things
(use-package super-save
  :ghook 'after-init-hook
  :custom
  (super-save-auto-save-when-idle t) ; Save buffer if Emacs is idle
  (super-save-idle-duration 10) ; Wait 10 seconds for idle trigger
  (super-save-remote-files t) ; Turn on saving of remote files (those pulled from git repo?)
  (super-save-exclude nil) ; Don't exclude anything from being saved
  :config
  (add-to-list 'super-save-triggers 'evil-window-next)
  (add-to-list 'super-save-hook-triggers 'eyebrowse-pre-window-switch-hook)
  )

;;;; Autorevert
;; Automatically update buffers as files are externally modified
(use-package autorevert
  :ghook ('after-init-hook 'global-auto-revert-mode)
  :custom
  (auto-revert-interval 7)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose t)
  )

;;;; Whitespace
;; Remove whitespace on save
(use-package whitespace
  :ghook ('before-save-hook 'whitespace-cleanup)
  :custom
  (whitespace-style '(face empty indentation::space tab))
  )

;;;; Unicode-fonts
;; Support unicode characters
(use-package unicode-fonts
  :custom
  (unicode-fonts-skip-font-groups '(low-quality-glyphs))
  :init
  (defun dw/replace-unicode-font-mapping (block-name old-font new-font)
    "Taken from https://github.com/daviwil/dotfiles/blob/master/Emacs.org#startup-performance"
    (let* ((block-idx (cl-position-if
                       (lambda (i) (string-equal (car i) block-name))
                       unicode-fonts-block-font-mapping))
           (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
           (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
      (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
            `(,updated-block))))
  (defun kb/unicode-setup ()
    "Fix the font mappings to use the right emoji font"
    (mapcar
     (lambda (block-name)
       (dw/replace-unicode-font-mapping block-name "Apple Color Emoji" "Noto Color Emoji"))
     '("Dingbats"
       "Emoticons"
       "Miscellaneous Symbols and Pictographs"
       "Transport and Map Symbols"))
    )
  :config
  (general-add-hook '(server-after-make-frame-hook window-setup-hook) 'kb/unicode-setup)
  )

;;;; Anzu
;; Adds highlight face during replace and regexp
(use-package anzu
  :ghook ('after-init-hook 'global-anzu-mode)
  :general ([remap query-replace] 'anzu-query-replace-regexp)
  :custom
  (anzu-cons-mode-line-p nil)
  )

;;;; Expand-region
;; Incrementally select a region outward
(use-package expand-region
  :general
  (:keymaps '(normal motion)
            "ge" 'er/expand-region)
  :custom
  (expand-region-smart-cursor t)
  (expand-region-skip-whitespace nil)
  (expand-region-subword-enabled t)
  )

;;;; Default-text-scale
;; Text-scale-mode but Emacs-wide
(use-package default-text-scale)

;;;; Goto-line-preview
;; Preview line before you jump to it with =M-x goto-line=
(use-package goto-line-preview
  :general ([remap goto-line] 'goto-line-preview)
  )

;;;; Ace-link
;; Open links easily
(use-package ace-link
  :general (:keymaps '(Info-mode helpful-mode help-mode woman-mode eww-mode compilation-mode mu4e-view-mode custom-mode-map)
                     "M-/" '(ace-link :which-key "Ace-link")
                     )
  )

;;;; Ace-jump
;; Quickly jump to any character
(use-package ace-jump-mode
  :gfhook ('org-mode-hook '(lambda () (face-remap-add-relative 'ace-jump-face-foreground nil :font kb/variable-pitch-font)))
  :general
  ("M-a" '(ace-jump-mode :which-key "Ace-jump"))
  :config
  (setq qace-jump-mode-scope 'window
        ace-jump-mode-case-fold t ; Ignore case?
        ace-jump-mode-gray-background nil ; Don't make text's background gray
        ace-jump-mode-submode-list ; Priority of ace-jump selections
        '(ace-jump-char-mode ace-jump-word-mode ace-jump-line-mode))
  )

;;;; Keyfreq
;; See a heatmap of your keypresses.
;; Use =keyfreq-show= to see how many times you used a command. Use =keyfreq-html= to get the original rendered HTML page. Use =keyfreq-html-v2= to get the keyboard heat map.
(use-package keyfreq
  :straight (keyfreq :type git :host github :repo "KirmTwinty/keyfreq")
  :ghook 'after-init-hook
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
  )

;;;; Git-timemachine
;; Enable in current buffer to iterate through git revision history
(use-package git-timemachine)

;;;; Scratch.el
;; Easily create scratch buffers for different modes
(use-package scratch
  ;; :demand t ; For the initial scratch buffer at startup
  :hook (scratch-create-buffer-hook . kb/scratch-buffer-setup)
  :general ("C-c s" '(scratch :which-key "Create scratch"))
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

;;;; Disable-mouse
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

;;;; Prettify-symbols-mode
;; Turn arbitrary strings into desired unicode characters
(use-package pretty-symbols
  :hook (after-init-hook . global-prettify-symbols-mode)
  :custom
  (prettify-symbols-alist '(("TODO" . "")
                            ("WAIT" . "")
                            ("NOPE" . "")
                            ("DONE" . "")
                            ("[#A]" . "")
                            ("[#B]" . "")
                            ("[#C]" . "")
                            ("[ ]" . "")
                            ("[X]" . "")
                            ("[-]" . "")
                            ("#+BEGIN_SRC" . "")
                            ("#+END_SRC" . "―")
                            (":PROPERTIES:" . "")
                            (":END:" . "―")
                            ("#+STARTUP:" . "")
                            ;; ("#+TITLE: " . "")

                            ("#+RESULTS:" . "")
                            ("#+NAME:" . "")
                            ("#+ROAM_TAGS:" . "")
                            ("#+FILETAGS:" . "")
                            ("#+HTML_HEAD:" . "")
                            ("#+SUBTITLE:" . "")
                            ("#+AUTHOR:" . "")
                            (":Effort:" . "")
                            ("SCHEDULED:" . "")
                            ("DEADLINE:" . "")))
  )

;;; misc-packages-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'misc-packages-rcp)
