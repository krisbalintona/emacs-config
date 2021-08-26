;;; completion-text-rcp.el --- Summary
;;
;;; Commentary:
;;
;; Packages that configure the company completion backend.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-frameworks-rcp)
(require 'faces-rcp)

;;;; Company
;;;;; Company-mode
;; Point auto-completion backend
(use-package company
  ;; NOTE 2021-08-26: Keeping this active since it is necessary to keep `:after'
  ;; and `:requires' statements happy.
  :demand t                             ; Necessary for other packages since I don't use it to load it anymore
  :after evil
  ;; :ghook ('after-init-hook 'global-company-mode)
  :gfhook 'evil-normalize-keymaps
  :general
  (:keymaps 'company-active-map
            "<escape>" '(lambda () (interactive) (company-abort) (evil-normal-state))
            "<return>" '(lambda () (interactive) (company-abort) (newline) (indent-according-to-mode))
            "<tab>" 'company-complete-selection
            "C-j" 'company-select-next-or-abort
            "C-k" 'company-select-previous-or-abort
            "C-n" 'company-select-next-or-abort
            "C-p" 'company-select-previous-or-abort)
  :custom
  (company-idle-delay 0.3)
  (company-tooltip-idle-delay 0.2)
  (company-minimum-prefix-length 1)
  (company-require-match 'never)
  (company-selection-wrap-around nil) ; Cycle?
  (company-global-modes '(not shell-mode))

  (company-show-numbers nil)
  (company-tooltip-offset-display 'lines)
  (company-tooltip-minimum-width 85)
  (company-tooltip-maximum-width 85)
  (company-tooltip-width-grow-only t) ; Don't decrease the width?
  (company-tooltip-flip-when-above t)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 12)

  (company-dabbrev-other-buffers nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)

  ;; NOTE 2021-08-22: I've set the initial backends to be minimal, removing a
  ;; lot of the backends that company initially sets. Most notably, I removed
  ;; `company-abbrev' because it slows down the performance significantly and I
  ;; don't use it.
  (company-backends '(company-bbdb company-yasnippet company-files
                                   (company-gtags company-etags company-keywords)
                                   company-capf))
  ;; NOTE 2021-08-25: Uncomment the code below if `company-box' isn't being used
  ;; (company-frontends
  ;;  '(company-preview-common-frontend ; Preview common part of candidates
  ;;    company-preview-if-just-one-frontend ; Always preview if only 1 candidate
  ;;    company-echo-metadata-frontend  ; Show symbol metadata in echo area
  ;;    company-pseudo-tooltip-unless-just-one-frontend-with-delay ; Respect `company-idle-delay'
  ;;    ))
  :init (require 'company-autoloads)    ; Make sure all company-backends are loaded
  :config
  ;; Make `company-backends' buffer-local so that I can configure the enabled
  ;; backends based on major-mode rather than adding every backend to the global
  ;; list.
  (make-variable-buffer-local 'company-backends)

  (set-face-attribute 'company-tooltip nil :font kb/fixed-pitch-font :height 127)
  )

;;;;; Company-box
;; A pretty company autocomplete frontend that also displays candidate
;; documentation
(use-package company-box
  :requires (company all-the-icons)
  :after (company all-the-icons)
  :defines company-box-icons-all-the-icons
  :ghook 'company-mode-hook
  :custom
  (company-box-show-single-candidate 'always)
  (company-box-backends-colors nil)
  (company-box-max-candidates company-tooltip-limit)
  (company-box-doc-delay 0.4)
  (company-box-scrollbar nil)
  (company-box-frame-behavior 'default)   ; Follow point horizontally as I type?
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  (company-box-icons-all-the-icons
   (let ((all-the-icons-scale-factor 0.8))
     `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
       (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
       (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
       (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
       (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
       (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
       (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
       (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
       (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
       (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
       (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
       (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
       (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
       (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
       (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
       (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
       (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
       (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
       (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
       (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
       (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
       (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
       (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
       (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
       (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
       (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
       (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))
       (ElispFunction . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
       (ElispVariable . ,(all-the-icons-material "check_circle"             :face 'all-the-icons-blue))
       (ElispFeature  . ,(all-the-icons-material "stars"                    :face 'all-the-icons-orange))
       (ElispFace     . ,(all-the-icons-material "format_paint"             :face 'all-the-icons-pink)))
     ))
  :config
  ;; (with-no-warnings
  ;;   ;; Prettify icons
  ;;   (defun my-company-box-icons--elisp (candidate)
  ;;     (when (derived-mode-p 'emacs-lisp-mode)
  ;;       (let ((sym (intern candidate)))
  ;;         (cond ((fboundp sym) 'Function)
  ;;               ((featurep sym) 'Module)
  ;;               ((facep sym) 'Color)
  ;;               ((boundp sym) 'Variable)
  ;;               ((symbolp sym) 'Text)
  ;;               (t . nil)))))
  ;;   (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp))
  (delq 'company-echo-metadata-frontend company-frontends) ; Redundant with `compan-box-doc'
  )

;;;; Default `completion-at-point'
;;;;; Corfu
;; Faster, minimal, and more lightweight autocomplete that is more faithful to
;; the Emacs infrastructure
(use-package corfu
  :ghook ('after-init-hook 'corfu-global-mode)
  :general (:keymaps 'corfu-map
                     "<escape>" 'corfu-quit
                     "<tab>" 'corfu-insert
                     "<return>" '(lambda () (interactive) (corfu-quit) (newline) (indent-according-to-mode))
                     "M-d" 'corfu-show-documentation)
  :custom
  (tab-always-indent 'complete)         ; Try to tab and then `complete-at-point'
  
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.05)

  (corfu-count 13)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-cycle t)

  (corfu-echo-documentation t)
  (corfu-quit-at-boundary nil)          ; Necessary for orderless
  (corfu-quit-no-match t)
  (corfu-commit-predicate t)
  )

;;;;; Custom completions
(autoload 'ffap-file-at-point "ffap")
(defun kb/complete-path-at-point ()
  "Return completion data for UNIX path at point."
  (let ((fn (ffap-file-at-point))
        (fap (thing-at-point 'filename)))
    (when (and (or fn (equal "/" fap))
               (save-excursion
                 (search-backward fap (line-beginning-position) t)))
      (list (match-beginning 0)
            (match-end 0)
            #'completion-file-name-table :exclusive 'no))))
(add-to-list 'completion-at-point-functions #'kb/complete-path-at-point)

;;; completion-text-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'completion-text-rcp)
