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
  :general (kb/open-keys
             "t" '(tmr-dispatch :wk "Tmr dispatch"))
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
  :general ("H-v" 'vc-msg-show))

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

;;; Iseach
;; Incremental search
(use-package isearch
  :straight nil
  :custom
  (isearch-repeat-on-direction-change t)
  (isearch-allow-scroll t)
  (isearch-allow-motion t)
  (isearch-lazy-count t)
  (isearch-wrap-pause 'no))

;;; Ctrlf
;; Feature-ful `isearch'
(use-package ctrlf
  :disabled
  :init (ctrlf-mode))

;;; Newcomment
(use-package new-comment
  :straight nil
  :custom
  (comment-empty-lines t)
  (comment-fill-column nil)
  (comment-multi-line nil)
  (comment-style 'indent))

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

;;; Info-colors
;; Fontify useful parts of info buffers
(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node))

;;; Whole-line-or-region
(use-package whole-line-or-region
  :diminish whole-line-or-region-local-mode
  :hook (after-init . whole-line-or-region-global-mode)
  :general (:keymaps 'whole-line-or-region-local-mode-map
            [remap kill-region] 'whole-line-or-region-kill-region
            [remap kill-ring-save] 'whole-line-or-region-kill-ring-save
            [remap copy-region-as-kill] 'whole-line-or-region-copy-region-as-kill
            [remap delete-region] 'whole-line-or-region-delete-region
            ;; [remap comment-dwim] 'whole-line-or-region-comment-dwim-2
            [remap comment-dwim] nil
            [remap comment-region] 'whole-line-or-region-comment-region
            [remap uncomment-region] 'whole-line-or-region-uncomment-region))

;;; Eldoc
(use-package eldoc
  :diminish
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose)
  (eldoc-print-after-edit nil)
  (eldoc-echo-area-display-truncation-message t)
  (eldoc-idle-delay 0.1)
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)) ; Respects `max-mini-window-height'

;;; Eldoc-box
(use-package eldoc-box
  :diminish eldoc-box-hover-mode
  :general
  ("H-h" 'eldoc-box-help-at-point)
  (:keymaps 'eglot-mode-map
   [remap eldoc-box-help-at-point] 'eldoc-box-eglot-help-at-point)
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
  (compilation-scroll-output t))        ; Scroll with compile buffer

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

;;; Form-feed
;; Display  fancily. Visit the readme for alternatives and their differences
(use-package form-feed
  :diminish
  :custom
  (form-feed-exclude-modes nil)
  :init
  (global-form-feed-mode))

;;; Logos
;; Package that encourages focused writing
(use-package logos
  :general
  ([remap narrow-to-region] 'logos-narrow-dwim
   [remap backward-page] 'logos-backward-page-dwim
   [remap forward-page] 'logos-forward-page-dwim
   "H-p" 'backward-page
   "H-n" 'forward-page)
  :custom
  (logos-outlines-are-pages t)
  (logos-outline-regexp-alist
   `((emacs-lisp-mode . ,(rx bol (or (literal ";;; ") ?)))
     (org-mode . ,(rx bol (or (literal "* ") ?))))))

;;; Engine-mode
;; Send arbitrary search engine queries to your browser from within Emacs
(use-package engine-mode
  :custom
  (engine/browser-function 'browse-url-generic)
  :init
  (engine-mode)
  :config
  (defengine amazon
    "https://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s")

  (defengine google
    "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")

  (defengine google-images
    "https://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")

  (defengine google-maps
    "https://maps.google.com/maps?q=%s"
    :docstring "Mappin' it up.")

  (defengine project-gutenberg
    "https://www.gutenberg.org/ebooks/search/?query=%s")

  (defengine qwant
    "https://www.qwant.com/?q=%s")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s")

  (defengine twitter
    "https://twitter.com/search?q=%s")

  (defengine wikipedia
    "https://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis.")

  (defengine wiktionary
    "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")

  (defengine wolfram-alpha
    "https://www.wolframalpha.com/input/?i=%s")

  (defengine youtube
    "https://www.youtube.com/results?aq=f&oq=&search_query=%s"))

;;; Electric
(use-package electric
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (electric-pair-inhibit-predicate 'electric-pair-default-inhibit)
  (electric-quote-comment nil)
  (electric-quote-string nil)
  (electric-quote-context-sensitive t)
  (electric-quote-replace-double t)
  (electric-quote-inhibit-functions nil)
  :init
  (electric-pair-mode)
  (electric-quote-mode))                ; For quotes in text mode

;;; Segment
;; Alternative to `sentence-navigation'. Provides sentence navigation commands
;; that respect abbreviations, etc.
(use-package pcre2el)
(use-package segment
  :straight (segment :type git :host codeberg :repo "martianh/segment")
  :commands kb/forward-sentence-function
  :custom
  ;; NOTE 2023-01-18: icu4j has many more rules, but is "too thorough" for my
  ;; tastes, so I stick with the more modest omegat and add more rules
  ;; (segment-ruleset-framework 'icu4j)
  (segment-ruleset-framework 'omegat)
  ;; (segment-ruleset-framework 'okapi-alt)
  (segment-custom-rules-regex-list
   `(("English"
      (("[JS]r\\." "[[:space:]][[:lower:]]" :break nil)
       ("U\\.S\\." "[[:space:]][[:lower:]]" :break nil)
       ("[[:lower:]]+\\.[])}]" "[[:space:]][[:lower:]]" :break nil)
       ("[^\\.][[:space:]][\"“]?[A-Z]\\." "[[:space:]]" :break nil)
       ("[Oo]p\\." "[[:space:]][[:digit:]]" :break nil)
       ;; My regexps
       (,(rx (seq (any space) (or (literal "p.") (literal "pp.")))) ,(rx (any space) (any digit)) :break nil)
       (,(rx (literal "...")) ,(rx (any space) (any lower)) :break nil)
       (,(rx (literal "etc.")) ,(rx (any space) (any lower)) :break nil)
       (,(rx (literal "Rev.")) ,(rx (any space) (any upper)) :break nil)
       (,(rx (literal "Jan.")) ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
       (,(rx (literal "Feb.")) ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
       (,(rx (literal "Mar.")) ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
       (,(rx (literal "Apr.")) ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
       (,(rx (literal "May.")) ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
       (,(rx (literal "Jun.")) ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
       (,(rx (literal "Jul.")) ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
       (,(rx (literal "Aug.")) ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
       (,(rx (literal "Sep.")) ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
       (,(rx (literal "Oct.")) ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
       (,(rx (literal "Nov.")) ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
       (,(rx (literal "Dec.")) ,(rx (seq (any space) (or (any lower) (any digit)))) :break nil)
       (,(rx (literal "Jr.")) ,(rx (any space) (any upper)) :break nil)
       (,(rx (literal "J.")) ,(rx (any space) (any upper)) :break nil)
       (,(rx (literal "Gen.")) ,(rx (any space) (any upper)) :break nil)
       (,(rx (literal "Dist.")) ,(rx (any space) (any upper)) :break nil)
       (,(rx (seq (any punct) (any punct))) ,(rx (any space) (any lower)) :break nil)
       ;; Breaks
       (,(rx (seq (any alnum) (literal ":"))) ,(rx (any space) (any upper)) :break t)))))
  :config
  (defun kb/forward-sentence-function (&optional arg)
    "Move forward to next end of sentence. With argument, repeat.
When ARG is negative, move backward repeatedly to start of sentence.

The variable `sentence-end' is a regular expression that matches ends of
sentences.  Also, every paragraph boundary terminates sentences as well.

Uses `segment’s regexps to ignore common abbreviations (see
`segment-ruleset-framework’) as well as define custom
sentence-breaks (see `segment-custom-rules-regex-list').

Also, when moving forward sentences, will jump the whitespace
between the current and next sentence, i.e,leave the point on the
first character of the next sentence."
    (or arg (setq arg 1))
    (let ((case-fold-search nil)
          (opoint (point))
          (sentence-end (sentence-end)))

      ;; Going backward
      (while (< arg 0)
        (let ((pos (point))
              par-beg par-text-beg
              default-break-point segment-break-point)
          (save-excursion
            (start-of-paragraph-text)
            (setq par-text-beg (point))
            (beginning-of-line)
            (setq par-beg (point)))

          ;; The strategy here is to first find the sentence end based on the
          ;; `sentence-end' regexp then `segment-current-break-rules'.
          ;; Afterwards, we compare them and `goto-char' the one closer to the
          ;; point.
          (setq default-break-point
                (save-excursion
                  (if (and (re-search-backward sentence-end par-beg t)
                           (or (< (match-end 0) pos)
                               (re-search-backward sentence-end par-beg t)))
                      (match-end 0)
                    par-text-beg)))
          (setq segment-break-point
                (let ((closest-break-point most-negative-fixnum))
                  (cl-dolist (reg-pair segment-current-break-rules)
                    (save-excursion
                      (when (and (re-search-backward (rx (seq (regexp (car reg-pair))
                                                              (regexp (cadr reg-pair))))
                                                     (or default-break-point par-beg) t)
                                 (or (< (match-end 0) pos)
                                     (re-search-backward (rx (seq (regexp (car reg-pair))
                                                                  (regexp (cadr reg-pair))))
                                                         (or default-break-point par-beg) t))
                                 (< closest-break-point (match-end 0)))
                        ;; Change the following lines depending on where we want
                        ;; the point to end for our custom line breaks
                        (re-search-forward (rx (any space)))
                        (setq closest-break-point (match-end 0)))))
                  closest-break-point))
          (goto-char (max default-break-point segment-break-point)))

        ;; Added this unless clause. From the `segment' package. This makes it
        ;; so that, if the point moved to a non-valid point, don't increment arg
        (unless (segment--looking-back-forward-map segment-current-language :moving-backward)
          (setq arg (1+ arg))))

      ;; Going forward
      (while (> arg 0)
        (let ((par-end (save-excursion (end-of-paragraph-text) (point)))
              default-break-point segment-break-point)

          (setq default-break-point
                (if (and
                     (progn
                       (re-search-forward sentence-end par-end t)
                       ;; `forward-sentence-default-function' leaves the point
                       ;; at the end of a line when in the middle of a paragraph
                       ;; there is a newline character after a sentence. Without
                       ;; the following line, the point will end after any
                       ;; whitespace, neglecting any newline characters. The
                       ;; only case where this shouldn't be done is when we are
                       ;; at the beginning of the paragraph, in which case this
                       ;; would lead to going backward a line
                       (unless (eq (save-excursion (start-of-paragraph-text) (point))
                                   (point))
                         (skip-chars-backward "\n")))
                     ;; If the point hasn't moved, and the above is non-nil,
                     ;; then we are in between the final full sentence and the
                     ;; end of the paragraph (i.e. ahead of us is an incomplete
                     ;; sentence followed by the end of a paragraph). In this
                     ;; case, then just go to the end of the paragraph
                     (not (eq (point) opoint)))
                    (point)
                  par-end))
          (setq segment-break-point
                (let ((closest-break-point most-positive-fixnum))
                  (cl-dolist (reg-pair segment-current-break-rules)
                    (save-excursion
                      (when (re-search-forward (rx (seq (regexp (car reg-pair))
                                                        (regexp (cadr reg-pair))))
                                               (or default-break-point par-end) t)
                        ;; Change the following lines depending on where we want
                        ;; the point to end
                        (re-search-backward (rx (seq (regexp (car reg-pair))
                                                     (any space))))
                        (setq closest-break-point (match-end 0)))))
                  closest-break-point))
          (goto-char (min default-break-point segment-break-point)))

        (unless (save-excursion
                  ;; Consider point having been moved behind the first character
                  ;; of the sentence by moving back
                  (skip-chars-backward " \t\n")
                  (segment--looking-back-forward-map segment-current-language))
          (setq arg (1- arg))))

      (let ((npoint (constrain-to-field nil opoint t)))
        (not (= npoint opoint)))))
  (setq forward-sentence-function 'kb/forward-sentence-function))

;;; Recursion-indicator
(use-package recursion-indicator
  ;;   :custom
  ;;   (recursion-indicator-general "&")
  ;;   (recursion-indicator-minibuffer "@")
  :init
  (minibuffer-depth-indicate-mode)
  (recursion-indicator-mode)
  :config
  ;; Thanks to Daniel Mendler for this! It removes the square brackets that
  ;; denote recursive edits in the modeline. I do not need them because I am
  ;; using Daniel's `recursion-indicator':
  ;; <https://github.com/minad/recursion-indicator>.
  (setq-default mode-line-modes
                (seq-filter (lambda (s)
                              (not (and (stringp s)
                                        (string-match-p
                                         "^\\(%\\[\\|%\\]\\)$" s))))
                            mode-line-modes)))

;;; Lorem-ipsum
;; Sample text
(use-package lorem-ipsum)

;;; Re-builder
;; Interactively build regexps
(use-package re-builder
  :custom
  (reb-re-syntax 'rx))

;;; Writeroom-mode
(use-package writeroom-mode
  :custom
  (writeroom-major-modes '(org-mode))
  (writeroom-width 100)
  (writeroom-global-effects
   '(writeroom-set-fullscreen
     ;; writeroom-set-alpha
     writeroom-set-menu-bar-lines
     writeroom-set-tool-bar-lines
     writeroom-set-vertical-scroll-bars
     writeroom-set-bottom-divider-width
     )))

;;; Kb/para-split-sentences and kb/para-merge-sentences
;; Modified from https://stackoverflow.com/a/43360152
(defun kb/para-split-sentences ()
  "Separate sentences of paragraph with newlines."
  (interactive)
  (unless buffer-read-only
    (save-excursion
      (start-of-paragraph-text)
      (while (< (point) (point-max))
        (forward-sentence)
        ;; Delete spaces between sentences before making new new line
        (delete-horizontal-space)
        ;; Don't add a new line, if already at the end of the line
        (unless (= (line-end-position) (point))
          (newline 2))))))

(defun kb/para-merge-sentences (&optional beg end)
  "In the active region"
  (interactive "r")
  (replace-regexp "\n" " " nil beg end))

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
  (repeat-mode)
  ;; NOTE 2022-12-30: Adds very useful commands to C-x f, F, k, K, v, V, and l, L
  (find-function-setup-keys))

;;; misc-packages-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'misc-packages-rcp)
