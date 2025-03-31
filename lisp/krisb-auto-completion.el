;;; krisb-auto-completion.el --- Implementation of Oblique Strategies  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Kristoffer Balintona
;;
;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords: text, convenience
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Bespoke auto-completion configuration with external packages.

;;; Code:

;;;###autoload
(define-minor-mode krisb-auto-completion-mode
  "Minor mode for my bespoke auto-completion setup.
This minor mode enables functionality to enable an auto-completion
setup.
- Sets relevant variables completion-related packages.
- Modifies `completion-at-point-functions' appropriately.
- Modifies relevant minor mode keybindings.
- DOES NOT enable or disable any modes.  The user must do so separately.

Notably, this minor mode is designed to work in harmony with
`completion-preview-mode' previews.

NOTE: Disabling this minor mode does not revert these changes.

See also
https://github.com/minad/corfu?tab=readme-ov-file#orderless-completion
for recommended corfu settings and usage with orderless."
  :lighter " Auto-C"
  ;; There is a notable, behavior with `corfu-separator': if the corfu
  ;; completions candidates are not shown yet, only through calling
  ;; `corfu-insert-separator' will they be shown.  They will not be shown by
  ;; inserting the character corresponding to the value of `corfu-separator'.
  ;; However, after the first invocation, regular insertions of that character
  ;; will be recognized as separators.
  (if krisb-auto-completion-mode
      (let ((sep " ")
            (min-symbol-length 2))
        (require 'completion-preview)
        (require 'corfu)
        (require 'orderless)

        (setq-local corfu-auto t
                    corfu-auto-delay 0.25
                    corfu-auto-prefix min-symbol-length
                    corfu-separator (string-to-char sep)
                    corfu-quit-at-boundary 'separator
                    corfu-quit-no-match 'separator
                    ;; This setup (particularly its keybinds) is designed to work in
                    ;; conjunction with `completion-preview-mode' in order to take advantage
                    ;; of its candidate previews
                    completion-preview-minimum-symbol-length min-symbol-length)

        ;; TODO 2025-03-26: Is there a more elegant solution?  Overwrite
        ;; `corfu-map' bindings buffer-locally.
        ;; Overwrite `corfu-map' `minibuffer-local-filename-syntax'
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map corfu-map)
          (bind-keys :map map
                     ;; `corfu-map' remaps `next-line' and `previous-line'; I
                     ;; undo the remapping
                     ([remap next-line] . nil)
                     ([remap previous-line] . nil)
                     ;; Have TAB do nothing
                     ("TAB" . nil)
                     ;; Rely on RET to insert and expand (`corfu-complete').  I
                     ;; prefer to use this and C-j to do the usual RET behavior
                     ("RET" . corfu-complete)
                     ;; Behave like "M-i" in
                     ;; `completion-preview-active-mode-map'
                     ("M-i" . corfu-expand))
          (setq-local corfu-map map))

        ;; Ensure `orderless-component-separator' matches `corfu-separator'
        (when (member 'orderless completion-styles)
          (setq-local orderless-component-separator sep))

        ;; These capfs are annoying with corfu auto-completion since there will
        ;; always be candidates, no matter what I type.  Ensure these are not in
        ;; the global value of `completion-at-point-functions'.
        (dolist (capf '(krisb-cape-super-capf--dict-dabbrev
                        cape-dabbrev))
          (remove-hook 'completion-at-point-functions capf t)))
    ;; TODO 2025-03-27: Can we do more to revert options, modes, etc. when
    ;; disabling?
    (dolist (var '(corfu-auto
                   corfu-auto-prefix
                   corfu-auto-delay
                   corfu-separator
                   corfu-quit-at-boundary
                   corfu-quit-no-match
                   corfu-map))
      (kill-local-variable var))))

;;; Provide
(provide 'krisb-auto-completion)
;;; krisb-auto-completion.el ends here
