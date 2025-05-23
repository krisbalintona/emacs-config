;; -*- lexical-binding: t; -*-

;;; Ibuffer
(use-package ibuffer
  :custom
  (ibuffer-save-with-custom nil)
  (ibuffer-default-sorting-mode 'recency)
  (ibuffer-jump-offer-only-visible-buffers t)
  (ibuffer-old-time 48)
  (ibuffer-expert nil)
  (ibuffer-show-empty-filter-groups t)
  (ibuffer-filter-group-name-face '(:inherit (success bold)))
  ;; Be aware that this value gets overridden by `all-the-icons-ibuffer-formats'
  ;; and `nerd-icons-ibuffer-mode'
  (ibuffer-formats
   '((mark modified read-only locked
           " " (name 18 18 :left :elide)
           " " (krisb-size 9 -1 :right)
           " " (mode 16 16 :right :elide)
           " " filename-and-process)
     (mark " " (name 16 -1) " " filename)))
  :config
  ;; Bespoke size column
  (define-ibuffer-column krisb-size
    (:name "Size"
           :inline t
           :header-mouse-map ibuffer-size-header-map)
    (file-size-human-readable (buffer-size))))

;;; Nerd-icons-ibuffer
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :custom
  (nerd-icons-ibuffer-color-icon t)
  (nerd-icons-ibuffer-icon-size 0.97)
  (nerd-icons-ibuffer-formats           ; See my value for `ibuffer-formats'
   '((mark modified read-only locked
           " " (icon 2 2 :right)
           " " (name 18 18 :left :elide)
           " " (size-h 12 -1 :right)
           " " (mode+ 16 16 :right :elide)
           " " filename-and-process+)
     (mark " " (name 16 -1) " " filename))))

;;; Provide
(provide 'krisb-buffers)
