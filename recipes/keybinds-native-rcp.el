;;; keybinds-native-rcp.el --- Summary
;;
;;; Commentary:
;;
;; General.el setup and very broad keybindings.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Commands
;;;; Inserting empty lines
(defun kb/open-line-above-goto ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current
mode. Credit to
https://emacsredux.com/blog/2013/06/15/open-line-above/"
  (interactive)
  (end-of-line 0)
  (newline)
  (indent-according-to-mode))

(defun kb/open-line-below-goto ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current
mode. Credit to
https://emacsredux.com/blog/2013/03/26/smarter-open-line/"
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun kb/open-line-above-insert ()
  "Insert an empty line above the current one without going to it."
  (interactive)
  (save-mark-and-excursion (kb/open-line-above-goto)))

(defun kb/open-line-below-insert ()
  "Insert an empty line above the current one without going to it."
  (interactive)
  (save-excursion (kb/open-line-below-goto)))

;;;; Join lines
(defun kb/join-line-above ()
  "Join the current line with the line above."
  (interactive)
  (let ((emptyp (save-excursion
                  (previous-line)
                  (beginning-of-line)
                  (looking-at-p "[[:blank:]]*$"))))
    (save-excursion
      (join-line))
    (when emptyp
      (funcall indent-line-function))))

(defun kb/join-line-below ()
  "Join the current line with the line below."
  (interactive)
  (let ((emptyp (save-excursion
                  (next-line)
                  (beginning-of-line)
                  (looking-at-p "[[:blank:]]*$"))))
    (save-excursion
      (join-line 1))
    (when (bolp)
      (funcall indent-line-function))))

;;;; Scrolling
(defun kb/scroll-up-command ()
  (interactive)
  (let ((lines (round (/ (window-height) 2))))
    (scroll-up-command lines)))

(defun kb/scroll-down-command ()
  (interactive)
  (let ((lines (round (/ (window-height) 2))))
    (scroll-down-command lines)))

(with-eval-after-load 'pulsar
  (add-to-list 'pulsar-pulse-functions 'kb/scroll-up-command)
  (add-to-list 'pulsar-pulse-functions 'kb/scroll-down-command))

(general-define-key
 [remap scroll-up-command] 'kb/scroll-up-command
 [remap scroll-down-command] 'kb/scroll-down-command)

;;; Text editing
(general-define-key
 "C-S-p" 'kb/open-line-above-goto
 "C-S-n" 'kb/open-line-below-goto
 "C-S-k" 'kb/join-line-above
 "C-S-j" 'kb/join-line-below
 (general-chord "[ ") 'kb/open-line-above-insert
 (general-chord "] ") 'kb/open-line-below-insert)

;;; Other
(general-define-key
 (general-chord "xx") 'save-buffer
 "<f10>" 'restart-emacs
 "C-x x R" 'rename-visited-file)

;;; keybinds-native-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'keybinds-native-rcp)
