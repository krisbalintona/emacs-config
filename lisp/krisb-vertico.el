;;; krisb-vertico.el --- Vertico extensions          -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Extension to Vertico.

;;; Code:
(require 'vertico)

;;; Tab for tramp paths
;; Exception for TAB behavior when completing TRAMP paths. See
;; https://github.com/minad/vertico/wiki#restore-old-tab-behavior-when-completing-tramp-paths
(defun krisb-vertico-insert-unless-tramp ()
  "Insert current candidate in minibuffer, except for tramp."
  (interactive)
  (if (vertico--remote-p (vertico--candidate))
      (minibuffer-complete)
    (vertico-insert)))
(bind-key [remap vertico-insert] #'krisb-vertico-insert-unless-tramp 'vertico-map)

;;; Exceptions for org commands
;; Special for `org-agenda-filter' and `org-tags-view'. See
;; https://github.com/minad/vertico?tab=readme-ov-file#org-agenda-filter-and-org-tags-view
(defun krisb-vertico-org-enforce-basic-completion (&rest args)
  (minibuffer-with-setup-hook
      (:append
       (lambda ()
         (let ((map (make-sparse-keymap)))
           (define-key map [tab] #'minibuffer-complete)
           (use-local-map (make-composed-keymap (list map) (current-local-map))))
         (setq-local completion-styles (cons 'basic completion-styles)
                     vertico-preselect 'prompt)))
    (apply args)))
(with-eval-after-load 'org
  (declare-function org-make-tags-matcher "org")
  (advice-add #'org-make-tags-matcher :around #'krisb-vertico-org-enforce-basic-completion))
(with-eval-after-load 'org-agenda
  (declare-function org-agenda-filter "org-agenda")
  (advice-add #'org-agenda-filter :around #'krisb-vertico-org-enforce-basic-completion))

;;; Truncate long filenames
;; Left-truncate filename candidates. Taken from
;; https://github.com/minad/vertico/wiki#left-truncate-recentf-filename-candidates-eg-for-consult-buffer
(defun krisb-vertico-truncate-filename-candidates (args)
  (if-let ((arg (car args))
           (type (get-text-property 0 'multi-category arg))
           ((eq (car-safe type) 'file))
           (w (max 30 (- (window-width) 38)))
           (l (length arg))
           ((> l w)))
      (setcar args (concat "…" (truncate-string-to-width arg l (- l w)))))
  args)
(advice-add #'vertico--format-candidate :filter-args #'krisb-vertico-truncate-filename-candidates)

;;; Provide
(provide 'krisb-vertico)
;;; krisb-vertico.el ends here
