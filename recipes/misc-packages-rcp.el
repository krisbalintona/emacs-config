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
    "Add contents to `scratch' buffer and name it accordingly.
 Taken from
 https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore/"
    (let* ((mode (format "%s" major-mode))
           (string (concat "Scratch buffer for: " mode "\n\n")))
      (when scratch-buffer
        (save-excursion
          (insert string)
          (goto-char (point-min))
          (comment-region (point-at-bol) (point-at-eol)))
        (forward-line 2))
      (rename-buffer (concat "*Scratch for " mode "*") t))))

;;; Proced
;; Built in process monitor
(use-package proced
  :straight nil
  :general ("C-c p" '(proced :wk "Proced"))
  :custom
  (proced-auto-update-flag t)           ; Update live
  (proced-auto-update-interval 1)
  (proced-descend t)                    ; Descending order?
  (proced-filter 'all)                  ; Which processes are shown?
  :config
  (with-eval-after-load 'evil
    (add-hook 'proced-mode-hook #'evil-emacs-state)))

;;; Tmr
;; Timer package/library from Prot
(use-package tmr
  :straight (tmr :type git :host gitlab :repo "protesilaos/tmr.el")
  :general ("C-c t" '(tmr-dispatch :wk "Tmr dispatch"))
  :custom
  ;; Useful variables
  (tmr-descriptions-list
   '("Stop working!" "Work time 😄"))
  (tmr-notification-urgency 'normal)
  (tmr-sound-file "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga")
  :init
  (require 'transient)
  (transient-define-prefix tmr-dispatch ()
    "Invoke a transient menu for tmr"
    ["Create or remove timers"
     [("t" "Create a timer" tmr)
      ("T" "Create a timer with description" tmr-with-description)
      ("C" "Clone a timer" tmr-clone)]
     [("r" "Remove finished" tmr-remove-finished)
      ("c" "Cancel timer" tmr-cancel)]]
    ["View timers"
     [("v" "Tabulated view" tmr-tabulated-view)]]))

;;; All-the-icons-completion
;; Add `all-the-icons' icons to minibuffer completion candidates
(use-package all-the-icons-completion
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

;;; Vc-msg
;; See a line's corresponding commit information (like git blame)
(use-package vc-msg
  :general ("H-b" 'vc-msg-show))

;;; Good-scroll
;; Good-enough smooth scrolling
(use-package good-scroll
  :disabled t
  :ghook 'after-init-hook
  :gfhook 'kb/good-scroll-toggle
  :custom
  (good-scroll-step 80)
  :init
  ;; Variables
  (defvar kb/good-scroll--posn-x 0
    "Store the posn-x coordinate.")
  (defvar kb/good-scroll--posn-y 0
    "Store the posn-y coordinate.")
  (defvar kb/good-scroll--inhibit-scroll nil
    "Allow the input of another kb/scroll command?")
  (setq good-scroll-destination 0)      ; Because it's nil at startup

  ;; Functions
  (defun kb/good-scroll--convert-line-to-step (line)
    "Convert number of lines to number of pixels. Credit to
https://github.com/io12/good-scroll.el/issues/28#issuecomment-1117887861"
    (cl-typecase line
      (integer (* line (line-pixel-height)))
      ((or null (member -))
       (- (good-scroll--window-usable-height)
          (* next-screen-context-lines (line-pixel-height))))
      (t (line-pixel-height))))

  (defun kb/good-scroll--move (lines)
    "Scroll FACTOR up or down (down if FACTOR is negative) the screen."
    ;; TODO 2022-05-30: When at the edges of the screen, don't save the x-y
    ;; position (so that when leaving the edges, the non-edge x-y position is
    ;; restored). But make sure that if the point is moved after reaching the
    ;; edge, that x-y position is used instead for the next
    ;; kb/good-scroll--move. Additionally, though you can't good-scroll at the
    ;; edges, still move the point to the edges.
    (let* ((xy (posn-x-y (posn-at-point)))
           (x (car xy))
           (y (cdr xy))
           (down (cl-plusp lines))
           (scroll-pixels (kb/good-scroll--convert-line-to-step lines))
           (distance-to-end-line (- (line-number-at-pos (point-max))
                                    (line-number-at-pos)))
           (too-far-down (and down (>= (/ lines 2) distance-to-end-line))))
      (setq-local kb/good-scroll--posn-x x
                  kb/good-scroll--posn-y y)
      (unless kb/good-scroll--inhibit-scroll
        (if too-far-down
            (forward-line lines)
          (good-scroll-move scroll-pixels)))))
  (defun kb/good-scroll--render ()
    "Render an in-progress scroll.
Update the window's vscroll and position in the buffer based on the scroll
progress. This is called by the timer `good-scroll--timer' every
`good-scroll-render-rate' seconds."
    ;; Check if the window that recieved the scroll event still exists and
    ;; if there is distance to scroll.
    (when (and (window-valid-p good-scroll--window)
               (not (zerop good-scroll-destination)))
      (let ((inhibit-redisplay t))      ; TODO: Does this do anything?
        ;; Switch to the window that recieved the scroll event,
        ;; which might be different from the previously selected window.
        (with-selected-window good-scroll--window
          (let ((position-next-try
                 (funcall good-scroll-algorithm))
                (position-next-actual))
            (cl-assert (<= (abs position-next-try)
                           (abs good-scroll-destination)))
            (when (good-scroll--cached-point-top-dirty-p)
              (setq good-scroll--cached-point-top nil))
            (setq position-next-actual (good-scroll--go-to position-next-try))
            (setq good-scroll-traveled (+ good-scroll-traveled
                                          position-next-actual)
                  good-scroll-destination (- good-scroll-destination
                                             position-next-actual)
                  good-scroll--prev-point (point)
                  good-scroll--prev-window-start (window-start)
                  good-scroll--prev-vscroll (window-vscroll nil t))

            ;; NOTE 2022-06-01: Put this here so that the final non-zero
            ;; good-scroll-destination value is used to update point's posn
            ;; position. If it is put after the liens below (e.g. via :after
            ;; advice), then this function will set the value of
            ;; good-scroll-destination to zero and the point won't be updated
            ;; one more time (leading to the bug where the point is one line
            ;; above where it should be when scrolling down.)
            (goto-char
             (posn-point
              (posn-at-x-y kb/good-scroll--posn-x kb/good-scroll--posn-y)))
            ;; Don't allow for more scrolling commands when in the process of
            ;; scrolling
            (setq-local kb/good-scroll--inhibit-scroll t)

            ;; If we didn't jump the position as much as we wanted,
            ;; then we must be trying to scroll past the edge of the buffer.
            ;; This interrupts the scroll, so reset the destination to zero.
            (when (/= position-next-try position-next-actual)
              (setq good-scroll-destination 0))))))
    ;; NOTE 2022-06-01: Then stop inhibitting scrolling when done
    (unless (not (zerop good-scroll-destination))
      (setq-local kb/good-scroll--inhibit-scroll nil)))

  ;; Keybinds
  (defun kb/good-scroll-up (&optional lines)
    "Scroll up half the screen."
    (interactive)
    (let* ((entire-screen (window-height))
           (half-screen (- (/ entire-screen 2))))
      (kb/good-scroll--move (or lines half-screen))))

  (defun kb/good-scroll-down (&optional lines)
    "Scroll down."
    (interactive)
    (let* ((entire-screen (window-height))
           (half-screen (/ entire-screen 2)))
      (kb/good-scroll--move (or lines half-screen))))

  ;; Setup

  (defun kb/good-scroll-toggle ()
    "Enable or disable my own `good-scroll' functions."
    (interactive)
    (cond (good-scroll-mode
           (when (bound-and-true-p evil-local-mode)
             (advice-add 'evil-scroll-up :override #'kb/good-scroll-up)
             (advice-add 'evil-scroll-down :override #'kb/good-scroll-down))
           (advice-add 'scroll-down-command :override #'kb/good-scroll-up)
           (advice-add 'scroll-up-command :override #'kb/good-scroll-down)
           (advice-add 'good-scroll--render :override #'kb/good-scroll--render))
          (t                   ; When `good-scroll-mode' is nil
           (when (bound-and-true-p evil-local-mode)
             (advice-remove 'evil-scroll-up #'kb/good-scroll-up)
             (advice-remove 'evil-scroll-down #'kb/good-scroll-down))
           (advice-remove 'scroll-down-command #'kb/good-scroll-up)
           (advice-remove 'scroll-up-command #'kb/good-scroll-down)
           (advice-remove 'good-scroll--render #'kb/good-scroll--render)))))

;;; Ctrlf
;; Better `isearch'
(use-package ctrlf
  :init (ctrlf-mode))

;;; Alt-comment-dwim
(use-package alt-comment-dwim
  :straight (alt-comment-dwim :type git
                              :host gitlab
                              :protocol ssh
                              :repo "PreciousPudding/alt-comment-dwim")
  :general
  ([remap comment-dwim] 'alt-comment-dwim
   [remap comment-line] 'alt-comment-dwim-line
   "C-M-;" 'alt-comment-dwim-todo-and-timestamp)
  :custom
  (alt-comment-dwim-keyword-faces
   '(("TODO" . "orange")
     ("HACK" . (error bold))
     ("NOTE" . "cornflower blue")
     ("REVIEW" . "orchid")
     ("FIXME" . (error bold))
     ("OPTIMIZE" . "SandyBrown"))))

;;; Info-variable-pitch
;; Mixed pitch in Info pages
(use-package info-variable-pitch
  :straight (info-variable-pitch :type git :host github :repo "kisaragi-hiu/info-variable-pitch")
  :ghook 'Info-selection-hook)

;;; Whole-line-or-region
(use-package whole-line-or-region
  :hook (after-init . whole-line-or-region-global-mode)
  :init
  ;; Manually redefine the mode map since `whole-line-or-region-yank-handler'
  ;; relies on the mode being active for functionality
  (defvar whole-line-or-region-local-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [remap kill-region] 'whole-line-or-region-kill-region)
      (define-key map [remap kill-ring-save] 'whole-line-or-region-kill-ring-save)
      (define-key map [remap copy-region-as-kill] 'whole-line-or-region-copy-region-as-kill)
      (define-key map [remap delete-region] 'whole-line-or-region-delete-region)
      ;; (define-key map [remap comment-dwim] 'whole-line-or-region-comment-dwim-2)
      (define-key map [remap comment-region] 'whole-line-or-region-comment-region)
      (define-key map [remap uncomment-region] 'whole-line-or-region-uncomment-region)
      map)
    "Minor mode map for `whole-line-or-region-mode'.")
  )

;;; Eldoc
(use-package eldoc
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose)
  (eldoc-print-after-edit nil)
  (eldoc-echo-area-display-truncation-message t)
  (eldoc-idle-delay 0.1)
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)) ; Respects `max-mini-window-height'

;;; Eldoc-box
(use-package eldoc-box
  :general (:keymaps 'eglot-mode-map
                     "H-h" 'eldoc-box-help-at-point)
  :custom
  (eldoc-box-max-pixel-width 650)
  (eldoc-box-max-pixel-height 400)
  (eldoc-box-cleanup-interval 0.5)
  (eldoc-box-only-multi-line t)
  (eldoc-box-fringe-use-same-bg t)
  (eldoc-box-self-insert-command-list '(self-insert-command outshine-self-insert-command))
  :init
  (add-hook 'eldoc-mode-hook 'eldoc-box-hover-mode t))

;;; Pulsar
(use-package pulsar
  :hook
  (consult-after-jump . pulsar-recenter-top)
  (consult-after-jump . pulsar-reveal-entry)
  (imenu-after-jump . pulsar-recenter-top)
  (imenu-after-jump . pulsar-reveal-entry)
  :custom
  (pulsar-iterations 3)
  (pulsar-delay 0.1)
  :init
  (pulsar-global-mode))

;;; Pocket-reader
;; View my Pocket
(use-package pocket-reader
  :straight (pocket-reader :type git
                           :host github
                           :repo "alphapapa/pocket-reader.el")
  :general
  (kb/open-keys
    "p" '(pocket-reader :wk "Open pocket"))
  (general-define-key
   :keymaps 'pocket-reader-mode-map
   "TAB" 'kb/pocket-reader-cycle-view
   "+" 'pocket-reader-more
   "o" 'pocket-reader-pop-to-url)
  :custom
  (pocket-reader-site-column-max-width 22)
  (pocket-reader-archive-on-open nil)
  (pocket-reader-default-queries (list ":unread"))
  (pocket-reader-open-url-default-function #'org-web-tools-read-url-as-org)
  :custom-face
  (pocket-reader-unread ((t (:weight bold))))
  (pocket-reader-archived ((t (:strike-through t))))
  :init
  (defun kb/pocket-reader--set-tabulated-list-format ()
    "Set `tabulated-list-format'.

Sets according to the maximum width of items about to be
displayed."
    (when-let* ((added-width 10)
                (domain-width (min pocket-reader-site-column-max-width
                                   (cl-loop for item being the hash-values of pocket-reader-items
                                            maximizing (length (ht-get item 'domain)))))
                (tags-width (cl-loop for item being the hash-values of pocket-reader-items
                                     maximizing (length (string-join (ht-get item 'tags) ","))))
                (title-width (- (window-text-width)
                                5                   ; Idk why this is needed...
                                (+ 1 added-width)   ; Added
                                (+ 2 1)             ; Favorite
                                (+ 3 domain-width)  ; Site
                                (+ 1 tags-width)))) ; Tags
      (setq tabulated-list-format (vector (list "Added" (1+ added-width) pocket-reader-added-column-sort-function)
                                          (list "*" (+ 2 1) t)
                                          (list "Title" (+ 2 title-width) t)
                                          (list "Site" (+ 3 domain-width) t)
                                          (list "Tags" (+ 1 tags-width) t)))))
  (advice-add 'pocket-reader--set-tabulated-list-format
              :override #'kb/pocket-reader--set-tabulated-list-format)

  (defun kb/pocket-reader-cycle-view ()
    "Cycle between showing unread entries and all entries."
    (interactive)
    (let ((all-query ":all")
          (archive-query ":archive")
          (unread-query ":unread"))
      (pcase pocket-reader-queries
        ((pred (member all-query))
         (message "Showing unread")
         (pocket-reader-search unread-query))
        ((pred (member unread-query))
         (message "Showing archived")
         (pocket-reader-search archive-query))
        ((pred (member archive-query))
         (message "Showing all")
         (pocket-reader-search all-query))
        (_
         (message "Showing default")
         (pocket-reader-search pocket-reader-default-queries))))))

;;; Ansi-color
;; Apply ANSI terminal color escape codes.
;; <http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html>
(use-package ansi-color
  :hook (compilation-filter . endless/colorize-compilation)
  :config
  (defun endless/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point)))))

;;; Compile
(use-package compile
  :custom
  (compilation-scroll-output 'first-error))

;;; Fancy-compilation
(use-package fancy-compilation
  :custom
  (fancy-compilation-override-colors nil)
  (fancy-compilation-quiet-prelude t))

;;; Image-popup
;; Match height of image to line height (?)
(use-package image-popup
  :straight (image-popup :type git :host gitlab :repo "OlMon/image-popup" :branch "master")
  :hook ((eww-after-render nov-post-html-render) . image-popup-reload))

;;; Page breaks as horizontal rules
;; Turn page breaks (i.e. ^L) into horizontal rules. Simply uses the extend face
;; attribute. Taken from https://www.emacswiki.org/emacs/OverlayControlL
(define-minor-mode kb/page-break-horizontal-rule-mode
  "Minor mode for replacing page breaks with horizontal rules"
  :init-value nil
  :lighter ""
  :keymap nil
  (font-lock-add-keywords
   nil
   `((,page-delimiter ;; variable with the regexp (usually "^\f" or "^^L")
      0
      (prog1 nil
        ;; don't display ^L
        (compose-region (match-beginning 0) (match-end 0) "")
        ;; make an overlay (like in hl-line)
        (let ((pdl (make-overlay (line-beginning-position)
                                 (line-beginning-position 2))))
          ;; :background has to be different from the background color
          ;; gray1 here is just a little different from black
          (overlay-put pdl 'face '(:extend t :underline "gray30" :background "gray1"))
          (overlay-put pdl 'modification-hooks
                       ;; these arguments are received from modification-hooks
                       '((lambda (overlay after-p begin end &optional length)
                           (delete-overlay overlay))))
          (overlay-put pdl 'insert-in-front-hooks
                       '((lambda (overlay after-p begin end &optional length)
                           (delete-overlay overlay))))))
      t))))
(dolist (mode '(emacs-lisp-mode-hook
                lisp-mode-hook
                compilation-mode-hook
                outline-mode-hook
                help-mode-hook
                org-mode-hook))
  (add-hook mode 'kb/page-break-horizontal-rule-mode))



;;; Logos
;; Package that encourages focused writing
(use-package logos
  :general
  ([remap narrow-to-region] 'logos-narrow-dwim
   [remap forward-page] 'logos-forward-page-dwim
   [remap backward-page] 'logos-backward-page-dwim)
  :custom
  (logos-outlines-are-pages nil))

;;; Built-in Emacs modes/packages
(use-package emacs
  :straight nil
  :hook (messages-buffer-mode . visual-line-mode)
  :general
  (:keymaps 'global-map
            (general-chord "xf") 'find-file)
  (:keymaps 'Info-mode-map
            :states '(visual normal motion)
            "SPC" nil                   ; For my leader key
            [remap evil-ret] 'Info-follow-nearest-node)
  (:keymaps 'universal-argument-map     ; Multiple universal arguments
            "u" 'universal-argument-more)
  :custom
  (kill-do-not-save-duplicates t)
  :config
  (when (bound-and-true-p evil-local-mode)
    (general-unbind 'normal help-mode-map "SPC")
    (general-unbind 'normal custom-mode-map "SPC"))
  :init
  (global-so-long-mode)
  (electric-pair-mode)
  (repeat-mode))

;;; misc-packages-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'misc-packages-rcp)
