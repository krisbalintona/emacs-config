;;; uninspiring-dark-theme.el --- My custom theme

;; Copyright 2021-2021 Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalntona@gmail.com>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My version of the Atom One Dark theme.

;;; Code:

(deftheme uninspiring-dark
  "Uninspiring-dark - A slightly customized version of the Atom One Dark theme.")

;;; Define colors
(defvar uninspiring-dark-colors-alist
  (let* ((256color  (eq (display-color-cells (selected-frame)) 256))
         (colors `(("uninspiring-dark-accent"   . "#528BFF")
                   ("uninspiring-dark-fg"       . (if ,256color "color-248" "#ABB2BF"))
                   ("uninspiring-dark-bg"       . (if ,256color "color-235" "#282C34"))
                   ("uninspiring-dark-bg-1"     . (if ,256color "color-234" "#121417"))
                   ("uninspiring-dark-bg-hl"    . (if ,256color "color-236" "#2C323C"))
                   ("uninspiring-dark-gutter"   . (if ,256color "color-239" "#4B5363"))
                   ("uninspiring-dark-mono-1"   . (if ,256color "color-248" "#ABB2BF"))
                   ("uninspiring-dark-mono-2"   . (if ,256color "color-244" "#828997"))
                   ("uninspiring-dark-mono-3"   . (if ,256color "color-240" "#7C828C"))
                   ("uninspiring-dark-cyan"     . "#55C2BE")
                   ("uninspiring-dark-blue"     . "#61AFEF")
                   ("uninspiring-dark-purple"   . "#C678DD")
                   ("uninspiring-dark-green"    . "#98C379")
                   ("uninspiring-dark-red-1"    . "#E06C75")
                   ("uninspiring-dark-red-2"    . "#BE5046")
                   ("uninspiring-dark-orange-1" . "#D19A66")
                   ("uninspiring-dark-orange-2" . "#E5C07B")
                   ("uninspiring-dark-gray"     . (if ,256color "color-237" "#3E4451"))
                   ("uninspiring-dark-silver"   . (if ,256color "color-247" "#9DA5B4"))
                   ("uninspiring-dark-black"    . (if ,256color "color-233" "#21252B"))
                   ("uninspiring-dark-border"   . (if ,256color "color-232" "#181A1F"))
                   )))
    colors)
  "List of Atom One Dark colors.")
(defmacro uninspiring-dark-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@ (mapcar (lambda (cons)
                      (list (intern (car cons)) (cdr cons)))
                    uninspiring-dark-colors-alist))
     ,@body))

;;; Define font families
(defvar uninspiring-dark-default
  "Iosevka Term SS04"
  )
(defvar uninspiring-dark-fixed-pitch
  "Hack Nerd Font Mono"
  )
(defvar uninspiring-dark-variable-pitch
  ;; "LiterationSerif Nerd Font"           ; Variable
  ;; "Latin Modern Mono Prop"              ; Monospace
  ;; "Sans Serif"
  "Open Sans"
  )

;;; Bind variables to colors
(uninspiring-dark-with-color-variables
  (custom-theme-set-faces
   'uninspiring-dark

;;;; Standard faces broad
   `(default ((t (:font ,uninspiring-dark-default :height 136 :foreground ,uninspiring-dark-fg :background ,uninspiring-dark-bg))))
   `(fixed-pitch ((t (:font ,uninspiring-dark-fixed-pitch :height 140))))
   `(variable-pitch ((t (:font ,uninspiring-dark-variable-pitch :height 160))))
   `(success ((t (:foreground ,uninspiring-dark-green))))
   `(warning ((t (:foreground ,uninspiring-dark-orange-2))))
   `(error ((t (:foreground ,uninspiring-dark-red-1 :weight bold))))
   `(link ((t (:foreground ,uninspiring-dark-blue :slant italic :underline t))))
   `(link-visited ((t (:inherit link))))
   `(cursor ((t (:background ,uninspiring-dark-accent))))
   `(fringe ((t (:background ,uninspiring-dark-bg))))
   `(region ((t (:background ,uninspiring-dark-gray :distant-foreground ,uninspiring-dark-mono-2))))
   `(highlight ((t (:background ,uninspiring-dark-gray :distant-foreground ,uninspiring-dark-mono-2))))
   `(hl-line ((t (:background "#1F2228" :distant-foreground nil))))
   `(header-line ((t (:background ,uninspiring-dark-black))))
   `(vertical-border ((t (:background ,uninspiring-dark-border :foreground ,uninspiring-dark-border))))
   `(secondary-selection ((t (:background ,uninspiring-dark-bg-1))))
   `(query-replace ((t (:inherit (isearch)))))
   `(minibuffer-prompt ((t (:foreground ,uninspiring-dark-silver))))
   `(tooltip ((t (:foreground ,uninspiring-dark-fg :background ,uninspiring-dark-bg-1 :inherit variable-pitch))))

   `(font-lock-builtin-face ((t (:foreground ,uninspiring-dark-cyan))))
   `(font-lock-comment-face ((t (:foreground ,uninspiring-dark-mono-3))))
   `(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
   `(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
   `(font-lock-function-name-face ((t (:foreground ,uninspiring-dark-blue))))
   `(font-lock-keyword-face ((t (:foreground ,uninspiring-dark-purple :weight normal))))
   `(font-lock-preprocessor-face ((t (:foreground ,uninspiring-dark-mono-2))))
   `(font-lock-string-face ((t (:foreground ,uninspiring-dark-green))))
   `(font-lock-type-face ((t (:foreground ,uninspiring-dark-orange-2))))
   `(font-lock-constant-face ((t (:foreground ,uninspiring-dark-cyan))))
   `(font-lock-variable-name-face ((t (:foreground ,uninspiring-dark-red-1))))
   `(font-lock-warning-face ((t (:foreground ,uninspiring-dark-mono-3 :bold t))))
   `(font-lock-negation-char-face ((t (:foreground ,uninspiring-dark-cyan :bold t))))

;;;; Mode-line
   ;; Heavily based on mood-one theme
   `(mode-line ((t (:family "JetBrainsMono Nerd Font" :height 113 :background "#1c2024" :foreground "#c0c4d2" :box (:line-width 1 :color "#1c2024")))))
   `(mode-line-inactive ((t (:inherit mode-line :background "#212428" :foreground "#5b6265"))))
   ;; `(mode-line-emphasis ((t (:foreground "#51afef"))))
   `(mode-line-highlight ((t (:inherit 'highlight))))
   `(mode-line-buffer-id ((t (:foreground "#a9a1e1" :weight bold))))
   ;; `(header-line ((t (:inherit 'mode-line))))

;;;; Window-divider
   `(window-divider ((t (:foreground ,uninspiring-dark-border))))
   `(window-divider-first-pixel ((t (:foreground ,uninspiring-dark-border))))
   `(window-divider-last-pixel ((t (:foreground ,uninspiring-dark-border))))

;;;; Custom
   `(custom-state ((t (:foreground ,uninspiring-dark-green))))

;;;; Ace-jump
   `(ace-jump-face-background ((t (:foreground ,uninspiring-dark-mono-3 :background ,uninspiring-dark-bg-1 :inverse-video nil))))
   `(ace-jump-face-foreground ((t (:foreground ,uninspiring-dark-red-1 :background ,uninspiring-dark-bg-1 :inverse-video nil))))

;;;; Outshine
   `(outshine-level-4 ((t :inherit outline-5)))
   `(outshine-level-5 ((t :inherit outline-6)))
   `(outshine-level-6 ((t :inherit outline-8)))
   `(outshine-level-8 ((t :inherit outline-7)))

;;;; Company-mode
   `(company-tooltip ((t (:font ,uninspiring-dark-fixed-pitch :height 127 :foreground ,uninspiring-dark-fg :background ,uninspiring-dark-bg-1))))
   `(company-tooltip-annotation ((t (:foreground ,uninspiring-dark-mono-2 :background ,uninspiring-dark-bg-1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,uninspiring-dark-mono-2 :background ,uninspiring-dark-gray))))
   `(company-tooltip-selection ((t (:foreground ,uninspiring-dark-fg :background ,uninspiring-dark-gray))))
   `(company-tooltip-mouse ((t (:background ,uninspiring-dark-gray))))
   `(company-tooltip-common ((t (:foreground ,uninspiring-dark-orange-2 :background ,uninspiring-dark-bg-1))))
   `(company-tooltip-common-selection ((t (:foreground ,uninspiring-dark-orange-2 :background ,uninspiring-dark-gray))))
   `(company-preview ((t (:background ,uninspiring-dark-bg))))
   `(company-preview-common ((t (:foreground ,uninspiring-dark-orange-2 :background ,uninspiring-dark-bg))))
   `(company-scrollbar-fg ((t (:background ,uninspiring-dark-mono-1))))
   `(company-scrollbar-bg ((t (:background ,uninspiring-dark-bg-1))))
   `(company-template-field ((t (:inherit highlight))))

;;;; Doom-modeline
   `(doom-modeline-bar ((t (:background ,uninspiring-dark-accent))))

;;;; Flyspell
   `(flyspell-duplicate ((t (:underline (:color "#D19A66" :style line)))))
   `(flyspell-incorrect ((t (:underline (:color "red2" :style wave)))))

;;;; Flycheck
   `(flycheck-error ((t (:underline (:color ,uninspiring-dark-red-1 :style wave)))))
   `(flycheck-info ((t (:underline (:color ,uninspiring-dark-green :style wave)))))
   `(flycheck-warning ((t (:underline (:color ,uninspiring-dark-orange-1 :style wave)))))

;;;; Compilation
   `(compilation-face ((t (:foreground ,uninspiring-dark-fg))))
   `(compilation-line-number ((t (:foreground ,uninspiring-dark-mono-2))))
   `(compilation-column-number ((t (:foreground ,uninspiring-dark-mono-2))))
   `(compilation-mode-line-exit ((t (:inherit compilation-info :weight bold))))
   `(compilation-mode-line-fail ((t (:inherit compilation-error :weight bold))))

;;;; Isearch
   `(isearch ((t (:foreground ,uninspiring-dark-bg :background ,uninspiring-dark-purple))))
   `(isearch-fail ((t (:foreground ,uninspiring-dark-red-2 :background nil))))
   `(lazy-highlight ((t (:foreground ,uninspiring-dark-purple :background ,uninspiring-dark-bg-1 :underline ,uninspiring-dark-purple))))

;;;; Diff-hl (https://github.com/dgutov/diff-hl)
   '(diff-hl-change ((t (:foreground "#E9C062" :background "#8b733a"))))
   '(diff-hl-delete ((t (:foreground "#CC6666" :background "#7a3d3d"))))
   '(diff-hl-insert ((t (:foreground "#A8FF60" :background "#547f30"))))

;;;; Dired-mode
   '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
   '(dired-flagged ((t (:inherit (diff-hl-delete)))))
   '(dired-symlink ((t (:foreground "#FD5FF1"))))

;;;; Dired-async
   `(dired-async-failures ((t (:inherit error))))
   `(dired-async-message ((t (:inherit success))))
   `(dired-async-mode-message ((t (:foreground ,uninspiring-dark-orange-1))))

;;;; Ivy
   `(ivy-confirm-face ((t (:inherit minibuffer-prompt :foreground ,uninspiring-dark-green))))
   `(ivy-current-match ((t (:background ,uninspiring-dark-gray :weight normal))))
   `(ivy-highlight-face ((t (:inherit font-lock-builtin-face))))
   `(ivy-match-required-face ((t (:inherit minibuffer-prompt :foreground ,uninspiring-dark-red-1))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,uninspiring-dark-bg-hl :height 136))))
   `(ivy-minibuffer-match-face-2 ((t (:inherit ivy-minibuffer-match-face-1 :background ,uninspiring-dark-black :foreground ,uninspiring-dark-purple :weight semi-bold))))
   `(ivy-minibuffer-match-face-3 ((t (:inherit ivy-minibuffer-match-face-2 :background ,uninspiring-dark-black :foreground ,uninspiring-dark-green :weight semi-bold))))
   `(ivy-minibuffer-match-face-4 ((t (:inherit ivy-minibuffer-match-face-2 :background ,uninspiring-dark-black :foreground ,uninspiring-dark-orange-2 :weight semi-bold))))
   `(ivy-minibuffer-match-highlight ((t (:inherit ivy-current-match))))
   `(ivy-modified-buffer ((t (:inherit default :foreground ,uninspiring-dark-orange-1))))
   `(ivy-virtual ((t (:inherit font-lock-builtin-face :slant italic))))

;;;; Counsel
   `(counsel-key-binding ((t (:foreground ,uninspiring-dark-orange-2 :weight bold))))

;;;; Git-commit
   `(git-commit-comment-action  ((t (:foreground ,uninspiring-dark-green :weight bold))))
   `(git-commit-comment-branch  ((t (:foreground ,uninspiring-dark-blue :weight bold))))
   `(git-commit-comment-heading ((t (:foreground ,uninspiring-dark-orange-2 :weight bold))))

;;;; Git-gutter
   `(git-gutter:added ((t (:foreground ,uninspiring-dark-green :weight bold))))
   `(git-gutter:deleted ((t (:foreground ,uninspiring-dark-red-1 :weight bold))))
   `(git-gutter:modified ((t (:foreground ,uninspiring-dark-orange-1 :weight bold))))

;;;; Eshell
   `(eshell-ls-archive ((t (:foreground ,uninspiring-dark-purple :weight bold))))
   `(eshell-ls-backup ((t (:foreground ,uninspiring-dark-orange-2))))
   `(eshell-ls-clutter ((t (:foreground ,uninspiring-dark-red-2 :weight bold))))
   `(eshell-ls-directory ((t (:foreground ,uninspiring-dark-blue :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,uninspiring-dark-green :weight bold))))
   `(eshell-ls-missing ((t (:foreground ,uninspiring-dark-red-1 :weight bold))))
   `(eshell-ls-product ((t (:foreground ,uninspiring-dark-orange-2))))
   `(eshell-ls-special ((t (:foreground "#FD5FF1" :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,uninspiring-dark-cyan :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,uninspiring-dark-mono-1))))
   `(eshell-prompt ((t (:inherit minibuffer-prompt))))

;;;; Man
   `(Man-overstrike ((t (:inherit font-lock-type-face :weight bold))))
   `(Man-underline ((t (:inherit font-lock-keyword-face :slant italic :weight bold))))

;;;; Woman
   `(woman-bold ((t (:inherit font-lock-type-face :weight bold))))
   `(woman-italic ((t (:inherit font-lock-keyword-face :slant italic :weight bold))))

;;;; EWW
   `(eww-form-checkbox ((t (:inherit eww-form-submit))))
   `(eww-form-file ((t (:inherit eww-form-submit))))
   `(eww-form-select ((t (:inherit eww-form-submit))))
   `(eww-form-submit ((t (:background ,uninspiring-dark-gray :foreground ,uninspiring-dark-fg :box (:line-width 2 :color ,uninspiring-dark-border :style released-button)))))
   `(eww-form-text ((t (:inherit widget-field :box (:line-width 1 :color ,uninspiring-dark-border)))))
   `(eww-form-textarea ((t (:inherit eww-form-text))))
   `(eww-invalid-certificate ((t (:foreground ,uninspiring-dark-red-1))))
   `(eww-valid-certificate ((t (:foreground ,uninspiring-dark-green))))

;;;; JS2-mode
   `(js2-error ((t (:underline (:color ,uninspiring-dark-red-1 :style wave)))))
   `(js2-external-variable ((t (:foreground ,uninspiring-dark-cyan))))
   `(js2-warning ((t (:underline (:color ,uninspiring-dark-orange-1 :style wave)))))
   `(js2-function-call ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param ((t (:foreground ,uninspiring-dark-mono-1))))
   `(js2-jsdoc-tag ((t (:foreground ,uninspiring-dark-purple))))
   `(js2-jsdoc-type ((t (:foreground ,uninspiring-dark-orange-2))))
   `(js2-jsdoc-value((t (:foreground ,uninspiring-dark-red-1))))
   `(js2-object-property ((t (:foreground ,uninspiring-dark-red-1))))

;;;; Ediff
   `(ediff-fine-diff-Ancestor                ((t (:background "#885555"))))
   `(ediff-fine-diff-A                       ((t (:background "#885555"))))
   `(ediff-fine-diff-B                       ((t (:background "#558855"))))
   `(ediff-fine-diff-C                       ((t (:background "#555588"))))
   `(ediff-current-diff-Ancestor             ((t (:background "#663333"))))
   `(ediff-current-diff-A                    ((t (:background "#663333"))))
   `(ediff-current-diff-B                    ((t (:background "#336633"))))
   `(ediff-current-diff-C                    ((t (:background "#333366"))))
   `(ediff-even-diff-Ancestor                ((t (:background "#181a1f"))))
   `(ediff-even-diff-A                       ((t (:background "#181a1f"))))
   `(ediff-even-diff-B                       ((t (:background "#181a1f"))))
   `(ediff-even-diff-C                       ((t (:background "#181a1f"))))
   `(ediff-odd-diff-Ancestor                 ((t (:background "#181a1f"))))
   `(ediff-odd-diff-A                        ((t (:background "#181a1f"))))
   `(ediff-odd-diff-B                        ((t (:background "#181a1f"))))
   `(ediff-odd-diff-C                        ((t (:background "#181a1f"))))

;;;; Magit
   `(magit-section-highlight ((t (:background ,uninspiring-dark-bg-hl))))
   `(magit-section-heading ((t (:foreground ,uninspiring-dark-orange-2 :weight bold))))
   `(magit-section-heading-selection ((t (:foreground ,uninspiring-dark-fg :weight bold))))
   `(magit-diff-file-heading ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,uninspiring-dark-gray :weight bold))))
   `(magit-diff-file-heading-selection ((t (:foreground ,uninspiring-dark-orange-2 :background ,uninspiring-dark-bg-hl :weight bold))))
   `(magit-diff-hunk-heading ((t (:foreground ,uninspiring-dark-mono-2 :background ,uninspiring-dark-gray))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,uninspiring-dark-mono-1 :background ,uninspiring-dark-mono-3))))
   `(magit-diff-hunk-heading-selection ((t (:foreground ,uninspiring-dark-purple :background ,uninspiring-dark-mono-3))))
   `(magit-diff-context ((t (:foreground ,uninspiring-dark-fg))))
   `(magit-diff-context-highlight ((t (:background ,uninspiring-dark-bg-1 :foreground ,uninspiring-dark-fg))))
   `(magit-diffstat-added ((t (:foreground ,uninspiring-dark-green))))
   `(magit-diffstat-removed ((t (:foreground ,uninspiring-dark-red-1))))
   `(magit-process-ok ((t (:foreground ,uninspiring-dark-green))))
   `(magit-process-ng ((t (:foreground ,uninspiring-dark-red-1))))
   `(magit-log-author ((t (:foreground ,uninspiring-dark-orange-2))))
   `(magit-log-date ((t (:foreground ,uninspiring-dark-mono-2))))
   `(magit-log-graph ((t (:foreground ,uninspiring-dark-silver))))
   `(magit-sequence-pick ((t (:foreground ,uninspiring-dark-orange-2))))
   `(magit-sequence-stop ((t (:foreground ,uninspiring-dark-green))))
   `(magit-sequence-part ((t (:foreground ,uninspiring-dark-orange-1))))
   `(magit-sequence-head ((t (:foreground ,uninspiring-dark-blue))))
   `(magit-sequence-drop ((t (:foreground ,uninspiring-dark-red-1))))
   `(magit-sequence-done ((t (:foreground ,uninspiring-dark-mono-2))))
   `(magit-sequence-onto ((t (:foreground ,uninspiring-dark-mono-2))))
   `(magit-bisect-good ((t (:foreground ,uninspiring-dark-green))))
   `(magit-bisect-skip ((t (:foreground ,uninspiring-dark-orange-1))))
   `(magit-bisect-bad ((t (:foreground ,uninspiring-dark-red-1))))
   `(magit-blame-heading ((t (:background ,uninspiring-dark-bg-1 :foreground ,uninspiring-dark-mono-2))))
   `(magit-blame-hash ((t (:background ,uninspiring-dark-bg-1 :foreground ,uninspiring-dark-purple))))
   `(magit-blame-name ((t (:background ,uninspiring-dark-bg-1 :foreground ,uninspiring-dark-orange-2))))
   `(magit-blame-date ((t (:background ,uninspiring-dark-bg-1 :foreground ,uninspiring-dark-mono-3))))
   `(magit-blame-summary ((t (:background ,uninspiring-dark-bg-1 :foreground ,uninspiring-dark-mono-2))))
   `(magit-dimmed ((t (:foreground ,uninspiring-dark-mono-2))))
   `(magit-hash ((t (:foreground ,uninspiring-dark-purple))))
   `(magit-tag  ((t (:foreground ,uninspiring-dark-orange-1 :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,uninspiring-dark-green :weight bold))))
   `(magit-branch-local   ((t (:foreground ,uninspiring-dark-blue :weight bold))))
   `(magit-branch-current ((t (:foreground ,uninspiring-dark-blue :weight bold :box t))))
   `(magit-head           ((t (:foreground ,uninspiring-dark-blue :weight bold))))
   `(magit-refname        ((t (:background ,uninspiring-dark-bg :foreground ,uninspiring-dark-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,uninspiring-dark-bg :foreground ,uninspiring-dark-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,uninspiring-dark-bg :foreground ,uninspiring-dark-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,uninspiring-dark-green))))
   `(magit-signature-bad       ((t (:foreground ,uninspiring-dark-red-1))))
   `(magit-signature-untrusted ((t (:foreground ,uninspiring-dark-orange-1))))
   `(magit-cherry-unmatched    ((t (:foreground ,uninspiring-dark-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,uninspiring-dark-purple))))
   `(magit-reflog-commit       ((t (:foreground ,uninspiring-dark-green))))
   `(magit-reflog-amend        ((t (:foreground ,uninspiring-dark-purple))))
   `(magit-reflog-merge        ((t (:foreground ,uninspiring-dark-green))))
   `(magit-reflog-checkout     ((t (:foreground ,uninspiring-dark-blue))))
   `(magit-reflog-reset        ((t (:foreground ,uninspiring-dark-red-1))))
   `(magit-reflog-rebase       ((t (:foreground ,uninspiring-dark-purple))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,uninspiring-dark-green))))
   `(magit-reflog-remote       ((t (:foreground ,uninspiring-dark-cyan))))
   `(magit-reflog-other        ((t (:foreground ,uninspiring-dark-cyan))))

;;;; Message
   `(message-cited-text ((t (:foreground ,uninspiring-dark-green))))
   `(message-header-cc ((t (:foreground ,uninspiring-dark-orange-1 :weight bold))))
   `(message-header-name ((t (:foreground ,uninspiring-dark-purple))))
   `(message-header-newsgroups ((t (:foreground ,uninspiring-dark-orange-2 :weight bold :slant italic))))
   `(message-header-other ((t (:foreground ,uninspiring-dark-red-1))))
   `(message-header-subject ((t (:foreground ,uninspiring-dark-blue))))
   `(message-header-to ((t (:foreground ,uninspiring-dark-orange-2 :weight bold))))
   `(message-header-xheader ((t (:foreground ,uninspiring-dark-silver))))
   `(message-mml ((t (:foreground ,uninspiring-dark-purple))))
   `(message-separator ((t (:foreground ,uninspiring-dark-mono-3 :slant italic))))

;;;; Notmuch
   `(notmuch-crypto-decryption ((t (:foreground ,uninspiring-dark-purple :background ,uninspiring-dark-black))))
   `(notmuch-crypto-signature-bad ((t (:foreground ,uninspiring-dark-red-1 :background ,uninspiring-dark-black))))
   `(notmuch-crypto-signature-good ((t (:foreground ,uninspiring-dark-green :background ,uninspiring-dark-black))))
   `(notmuch-crypto-signature-good-key ((t (:foreground ,uninspiring-dark-green :background ,uninspiring-dark-black))))
   `(notmuch-crypto-signature-unknown ((t (:foreground ,uninspiring-dark-orange-1 :background ,uninspiring-dark-black))))
   `(notmuch-hello-logo-background ((t (:inherit default))))
   `(notmuch-message-summary-face ((t (:background ,uninspiring-dark-black))))
   `(notmuch-search-count ((t (:inherit default :foreground ,uninspiring-dark-silver))))
   `(notmuch-search-date ((t (:inherit default :foreground ,uninspiring-dark-purple))))
   `(notmuch-search-matching-authors ((t (:inherit default :foreground ,uninspiring-dark-orange-2))))
   `(notmuch-search-non-matching-authors ((t (:inherit font-lock-comment-face :slant italic))))
   `(notmuch-tag-added ((t (:underline t))))
   `(notmuch-tag-deleted ((t (:strike-through ,uninspiring-dark-red-2))))
   `(notmuch-tag-face ((t (:foreground ,uninspiring-dark-green))))
   `(notmuch-tag-unread ((t (:foreground ,uninspiring-dark-red-1))))
   `(notmuch-tree-match-author-face ((t (:inherit notmuch-search-matching-authors))))
   `(notmuch-tree-match-date-face ((t (:inherit notmuch-search-date))))
   `(notmuch-tree-match-face ((t (:weight semi-bold))))
   `(notmuch-tree-match-tag-face ((t (:inherit notmuch-tag-face))))
   `(notmuch-tree-no-match-face ((t (:slant italic :weight light :inherit font-lock-comment-face))))

;;;; Elfeed
   `(elfeed-log-debug-level-face ((t (:background ,uninspiring-dark-black :foreground ,uninspiring-dark-green))))
   `(elfeed-log-error-level-face ((t (:background ,uninspiring-dark-black :foreground ,uninspiring-dark-red-1))))
   `(elfeed-log-info-level-face ((t (:background ,uninspiring-dark-black :foreground ,uninspiring-dark-blue))))
   `(elfeed-log-warn-level-face ((t (:background ,uninspiring-dark-black :foreground ,uninspiring-dark-orange-1))))
   `(elfeed-search-date-face ((t (:foreground ,uninspiring-dark-purple))))
   `(elfeed-search-feed-face ((t (:foreground ,uninspiring-dark-orange-2))))
   `(elfeed-search-tag-face ((t (:foreground ,uninspiring-dark-green))))
   `(elfeed-search-title-face ((t (:foreground ,uninspiring-dark-mono-1))))
   `(elfeed-search-unread-count-face ((t (:foreground ,uninspiring-dark-silver))))

;;;; Rainbow-delimites
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,uninspiring-dark-blue))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,uninspiring-dark-green))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,uninspiring-dark-orange-1))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,uninspiring-dark-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,uninspiring-dark-purple))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,uninspiring-dark-orange-2))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,uninspiring-dark-blue))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,uninspiring-dark-green))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,uninspiring-dark-orange-1))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,uninspiring-dark-cyan))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,uninspiring-dark-purple))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,uninspiring-dark-orange-2))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,uninspiring-dark-red-1 :weight bold))))

;;;; Show-paren
   `(show-paren-match ((t (:foreground ,uninspiring-dark-purple :inherit bold :underline t))))
   `(show-paren-mismatch ((t (:foreground ,uninspiring-dark-red-1 :inherit bold :underline t))))

;;;; Sh-mode
   `(sh-heredoc ((t (:inherit font-lock-string-face :slant italic))))

;;;; Smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,uninspiring-dark-red-1 :background ,uninspiring-dark-gray :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,uninspiring-dark-gray :weight bold))))

;;;; Web-mode
   `(web-mode-doctype-face ((t (:inherit font-lock-comment-face))))
   `(web-mode-error-face ((t (:background ,uninspiring-dark-black :foreground ,uninspiring-dark-red-1))))
   `(web-mode-html-attr-equal-face ((t (:inherit default))))
   `(web-mode-html-attr-name-face ((t (:foreground ,uninspiring-dark-orange-1))))
   `(web-mode-html-tag-bracket-face ((t (:inherit default))))
   `(web-mode-html-tag-face ((t (:foreground ,uninspiring-dark-red-1))))
   `(web-mode-symbol-face ((t (:foreground ,uninspiring-dark-orange-1))))

;;;; Display-line-numbers
   ;; Native line numbers
   `(line-number ((t (:foreground ,uninspiring-dark-gutter :background ,uninspiring-dark-bg))))
   `(line-number-current-line ((t (:weight bold :foreground ,uninspiring-dark-fg :background ,uninspiring-dark-bg-hl))))

;;;; Regexp-builder
   `(reb-match-0 ((t (:background ,uninspiring-dark-gray))))
   `(reb-match-1 ((t (:background ,uninspiring-dark-black :foreground ,uninspiring-dark-purple :weight semi-bold))))
   `(reb-match-2 ((t (:background ,uninspiring-dark-black :foreground ,uninspiring-dark-green :weight semi-bold))))
   `(reb-match-3 ((t (:background ,uninspiring-dark-black :foreground ,uninspiring-dark-orange-2 :weight semi-bold))))

;;;; Desktop-entry
   `(desktop-entry-deprecated-keyword-face ((t (:inherit font-lock-warning-face))))
   `(desktop-entry-group-header-face ((t (:inherit font-lock-type-face))))
   `(desktop-entry-locale-face ((t (:inherit font-lock-string-face))))
   `(desktop-entry-unknown-keyword-face ((t (:underline (:color ,uninspiring-dark-red-1 :style wave) :inherit font-lock-keyword-face))))
   `(desktop-entry-value-face ((t (:inherit default))))

;;;; Latex-mode
   `(font-latex-sectioning-0-face ((t (:foreground ,uninspiring-dark-blue :height 1.0))))
   `(font-latex-sectioning-1-face ((t (:foreground ,uninspiring-dark-blue :height 1.0))))
   `(font-latex-sectioning-2-face ((t (:foreground ,uninspiring-dark-blue :height 1.0))))
   `(font-latex-sectioning-3-face ((t (:foreground ,uninspiring-dark-blue :height 1.0))))
   `(font-latex-sectioning-4-face ((t (:foreground ,uninspiring-dark-blue :height 1.0))))
   `(font-latex-sectioning-5-face ((t (:foreground ,uninspiring-dark-blue :height 1.0))))
   `(font-latex-bold-face ((t (:foreground ,uninspiring-dark-green :weight bold))))
   `(font-latex-italic-face ((t (:foreground ,uninspiring-dark-green))))
   `(font-latex-warning-face ((t (:foreground ,uninspiring-dark-red-1))))
   `(font-latex-doctex-preprocessor-face ((t (:foreground ,uninspiring-dark-cyan))))
   `(font-latex-script-char-face ((t (:foreground ,uninspiring-dark-mono-2))))

;;;; Org-mode
   `(org-date ((t (:foreground ,uninspiring-dark-cyan))))
   `(org-document-info ((t (:foreground ,uninspiring-dark-mono-2))))
   `(org-document-info-keyword ((t (:inherit org-meta-line :underline t))))
   `(org-document-title ((t (:weight bold :height 1.7 :foreground "goldenrod"))) t)
   `(org-footnote ((t (:foreground ,uninspiring-dark-cyan))))
   `(org-sexp-date ((t (:foreground ,uninspiring-dark-cyan))))
   `(org-ellipsis ((t (:inherit nil :height 0.7))) t)

   `(org-level-1 ((t (:inherit outline-1 :height 210 :font ,uninspiring-dark-variable-pitch))) t)
   `(org-level-2 ((t (:inherit outline-2 :height 198 :font ,uninspiring-dark-variable-pitch))) t)
   `(org-level-3 ((t (:inherit outline-3 :height 185 :font ,uninspiring-dark-variable-pitch))) t)
   `(org-level-4 ((t (:inherit outline-4 :height 170 :foreground "medium aquamarine" :font ,uninspiring-dark-variable-pitch))) t)
   `(org-level-5 ((t (:inherit outline-5 :height 165 :foreground "light sea green" :font ,uninspiring-dark-variable-pitch))) t)

   `(org-block ((t (:foreground nil :inherit fixed-pitch :background "#232635" :extend t))) t)
   `(org-quote ((t (:inherit org-block :height 143))) t)
   `(org-code ((t (:inherit (shadow fixed-pitch)))) t)
   `(org-verbatim ((t (:inherit (shadow fixed-pitch)))) t)
   `(org-table ((t (:inherit (shadow fixed-pitch)))) t)
   `(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))) t)
   `(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))) t)
   `(org-checkbox ((t (:inherit fixed-pitch))) t)
   `(org-tag ((t (:height 153 :bold t :italic t))) t)

;;;; Calendar
   `(diary ((t (:inherit warning))))
   `(holiday ((t (:foreground ,uninspiring-dark-green))))

;;;; Gud
   `(breakpoint-disabled ((t (:foreground ,uninspiring-dark-orange-1))))
   `(breakpoint-enabled ((t (:foreground ,uninspiring-dark-red-1 :weight bold))))

;;;; Realgud
   `(realgud-overlay-arrow1        ((t (:foreground ,uninspiring-dark-green))))
   `(realgud-overlay-arrow3        ((t (:foreground ,uninspiring-dark-orange-1))   `(realgud-overlay-arrow2        ((t (:foreground ,uninspiring-dark-orange-2))))
                                    ))
   '(realgud-bp-enabled-face       ((t (:inherit (error)))))
   `(realgud-bp-disabled-face      ((t (:inherit (secondary-selection)))))
   `(realgud-bp-line-enabled-face  ((t (:box (:color ,uninspiring-dark-red-1)))))
   `(realgud-bp-line-disabled-face ((t (:box (:color ,uninspiring-dark-gray)))))
   `(realgud-line-number           ((t (:foreground ,uninspiring-dark-mono-2))))
   `(realgud-backtrace-number      ((t (:inherit (secondary-selection)))))

;;;; Fill-column-indicator
   `(fci-rule-color ,uninspiring-dark-gray)

;;;; Vertico
   `(vertico-current ((t (:inherit success :background "#3A3F5A"))))

;;;; Ledger
   `(ledger-font-xact-highlight-face ((t (:inherit nil))))

;;;; Org-transclusion
   ;; NOTE 2021-08-31: Don't know if this is correctly written. Haven't tested
   ;; yet.
   `(org-transclusion-source-fringe ((t (:foreground (face-background default) :background (face-background default))))) ; Make fringe in referenced node invisible

;;;; Selectrum
   `(selectrum-current-candidate ((t (:background "#3E4451" :extend t :weight semi-bold))))
   `(selectrum-completion-annotation ((t (:inherit success))))
   `(selectrum-prescient-primary-highlight ((t (:foreground "#dc85f7"))))
   `(selectrum-prescient-secondary-highlight ((t (:foreground "#E5C07B"))))

;;;; Selectrum-prescient
   `(selectrum-prescient-primary-highlight ((t (:foreground "#dc85f7"))))
   `(selectrum-prescient-secondary-highlight ((t (:foreground "#E5C07B"))))

;;;; Lsp
   `(lsp-face-semhl-comment ((t (:inherit font-lock-comment-face :underline t))))
   `(lsp-face-highlight-read ((t (:inherit nil :box (:line-width -1 :style nil)))))
   `(lsp-face-highlight-write ((t (:inherit nil :underline t))))
   `(lsp-face-semhl-property ((t (:inherit font-lock-builtin-face :foreground "#E05D68"))))

;;;; Tree-sitter
   `(tree-sitter-hl-face:constant.builtin ((t (:inherit font-lock-builtin-face :foreground "#EFA161"))))
   `(tree-sitter-hl-face:number ((t (:inherit tree-sitter-hl-face:constant.builtin))))

;;;; Solaire
   ;; From mood-one theme
   `(solaire-default-face ((t (:inherit default :background "#212428"))))
   `(solaire-hl-line-face ((t (:inherit hl-line :background "#282c30"))))
   `(solaire-line-number-face ((t (:inherit solaire-default-face))))

;;;; Eyebrowse
   `(eyebrowse-mode-line-inactive ((t (:inherit mood-line-unimportant))))
   `(eyebrowse-mode-line-delimiters ((t (:inherit mood-line-unimportant))))

;;;; Marginalia
   `(marginalia-documentation ((t (:inherit font-lock-string-face :slant italic))))

;;;; End
   ))
;;; Force faces for certain modes
(defvar uninspiring-dark-theme-force-faces-for-mode t
  "If t, uninspiring-dark-theme will use Face Remapping to alter the theme faces for
the current buffer based on its mode in an attempt to mimick the Atom One Dark
Theme from Atom.io as best as possible.
The reason this is required is because some modes (html-mode, jyaml-mode, ...)
do not provide the necessary faces to do theming without conflicting with other
modes.
Current modes, and their faces, impacted by this variable:
* js2-mode: font-lock-constant-face, font-lock-doc-face, font-lock-variable-name-face
* html-mode: font-lock-function-name-face, font-lock-variable-name-face
")

;; Many modes in Emacs do not define their own faces and instead use standard Emacs faces when it comes to theming.
;; That being said, to have a real "Atom One Dark Theme" for Emacs, we need to work around this so that these themes look
;; as much like "Atom One Dark Theme" as possible.  This means using per-buffer faces via "Face Remapping":
;;
;;   http://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Remapping.html
;;
;; Of course, this might be confusing to some when in one mode they see keywords highlighted in one face and in another
;; mode they see a different face.  That being said, you can set the `uninspiring-dark-theme-force-faces-for-mode` variable to
;; `nil` to disable this feature.
(defun uninspiring-dark-theme-change-faces-for-mode ()
  (interactive)
  (when (or uninspiring-dark-theme-force-faces-for-mode (called-interactively-p))
    (uninspiring-dark-with-color-variables
      (cond
       ((member major-mode '(js2-mode))
        (face-remap-add-relative 'font-lock-constant-face :foreground uninspiring-dark-orange-1)
        (face-remap-add-relative 'font-lock-doc-face '(:inherit (font-lock-comment-face)))
        (face-remap-add-relative 'font-lock-variable-name-face :foreground uninspiring-dark-mono-1))
       ((member major-mode '(html-mode))
        (face-remap-add-relative 'font-lock-function-name-face :foreground uninspiring-dark-red-1)
        (face-remap-add-relative 'font-lock-variable-name-face :foreground uninspiring-dark-orange-1))))))

(add-hook 'after-change-major-mode-hook 'uninspiring-dark-theme-change-faces-for-mode)

;;; Provide
(provide-theme 'uninspiring-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; uninspiring-dark-theme.el ends here
