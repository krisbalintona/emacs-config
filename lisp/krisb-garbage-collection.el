;; NOTE 2024-02-11: Please reference https://emacsconf.org/2023/talks/gc/ for a
;; statistically-informed recommendation for GC variables
(setq garbage-collection-messages t)
(setq gc-cons-percentage 0.15)

;;; GCMH
;; Garbage collect on when idle
(use-package gcmh
  :diminish
  :hook ((after-init . gcmh-mode)
         (minibuffer-setup . krisb-gcmh-minibuffer-setup)
         (minibuffer-exit . krisb-gcmh-minibuffer-exit))
  :custom
  ;; For a related discussion, see
  ;; https://www.reddit.com/r/emacs/comments/bg85qm/comment/eln27qh/?utm_source=share&utm_medium=web2x&context=3
  (gcmh-high-cons-threshold (* 16       ; 16 mb, as Doom uses in doom-start.el
                               1024 1024))
  (gcmh-idle-delay 3)
  (gcmh-verbose nil)
  :config
  (setq garbage-collection-messages nil)

  ;; Increase GC threshold when in minibuffer
  (defvar krisb-gc-minibuffer--original gcmh-high-cons-threshold
    "Temporary variable to hold `gcmh-high-cons-threshold'")

  (defun krisb-gcmh-minibuffer-setup ()
    "Temporarily have \"limitless\" `gc-cons-threshold'."
    ;; (message "[krisb-gcmh-minibuffer-setup] Increasing GC threshold")
    (setq gcmh-high-cons-threshold most-positive-fixnum))

  (defun krisb-gcmh-minibuffer-exit ()
    "Restore value of `gc-cons-threshold'."
    ;; (message "[krisb-gcmh-minibuffer-exit] Restoring GC threshold")
    (setq gcmh-high-cons-threshold krisb-gc-minibuffer--original))

  ;; Increase `gc-cons-threshold' while using corfu, like we do for the
  ;; minibuffer
  (with-eval-after-load 'corfu
    (advice-add 'completion-at-point :before 'krisb-gcmh-minibuffer-setup)
    (advice-add 'corfu-quit :before 'krisb-gcmh-minibuffer-exit)
    (advice-add 'corfu-insert :before 'krisb-gcmh-minibuffer-exit)))

;;; Diagnose memory usage
;; See how Emacs is using memory. From
;; https://www.reddit.com/r/emacs/comments/ck4zb3/comment/evji1n7/?utm_source=share&utm_medium=web2x&context=3
(defun krisb-diagnose-garbage-collect ()
  "Run `garbage-collect' and print stats about memory usage."
  (interactive)
  (message (cl-loop for (type size used free) in (garbage-collect)
                    for used = (* used size)
                    for free = (* (or free 0) size)
                    for total = (file-size-human-readable (+ used free))
                    for used = (file-size-human-readable used)
                    for free = (file-size-human-readable free)
                    concat (format "%s: %s + %s = %s\n" type used free total))))

;;; Emacs-gc-stats
;; Collect GC statistics. Requested by someone who'd like GC statistics:
;; https://www.reddit.com/r/emacs/comments/14dej62/please_help_collecting_statistics_to_optimize/.
;; Also see https://elpa.gnu.org/packages/emacs-gc-stats.html
(use-package emacs-gc-stats
  :disabled t                           ; Dont collecting data
  :hook (on-first-input . emacs-gc-stats-mode)
  :custom
  ;; Optionally reset Emacs GC settings to default values (recommended)
  (emacs-gc-stats-gc-defaults 'emacs-defaults)
  (emacs-gc-stats-remind (* 7))  ; Optionally set reminder to upload the stats
  (emacs-gc-stats-inhibit-command-name-logging nil))

(provide 'krisb-garbage-collection)
