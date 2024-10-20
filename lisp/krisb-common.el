;;; krisb-common.el --- Common variables and functions  -*- lexical-binding: t; -*-

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

;; Variables and functions I use throughout my Emacs config.

;;; Code:

;;; Variables

;;;; Me
(setq user-full-name "Kristoffer Balintona"
      user-mail-address "krisbalintona@gmail.com")

;;;; System
(defconst krisb-system-win-p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst krisb-system-mac-p
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst krisb-system-linux-p
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst krisb-linux-distribution
  (when krisb-system-linux-p (shell-command-to-string "printf %s \"$(lsb_release -sd)\""))
  "An escaped string that has the name of my Linux distribution.")

(defconst krisb-linux-ubuntu-p
  (integerp (string-match "Ubuntu" krisb-linux-distribution))
  "Is this Ubuntu?")

(defconst krisb-linux-fedora-p
  (integerp (string-match "Fedora" krisb-linux-distribution))
  "Is this Fedora?")

(defconst krisb-linux-arch-p
  (integerp (string-match "Arch" krisb-linux-distribution))
  "Is this Arch Linux?")

;;;; Org
(defvar krisb-org-directory (expand-file-name "org-database" "~/Documents")
  "The directory holding my org files.
Meant to be used as the value of `org-directory'.")

(defvar krisb-notes-directory (expand-file-name "notes" krisb-org-directory)
  "My notes directory.")

(defvar krisb-blog-directory (expand-file-name "blog" krisb-notes-directory)
  "The directory for my pre-export blog files.")

(defvar krisb-org-agenda-directory (expand-file-name "agenda" krisb-org-directory)
  "The directory holding my main org-agenda files.")

(defvar krisb-org-agenda-main-file (expand-file-name "todo.org" krisb-org-agenda-directory)
  "My main org-agenda file.")

(defvar krisb-org-agenda-directory-files (cl-remove-if
                                          (lambda (f)
                                            (string-match-p (rx "archive.org") f))
                                          (directory-files-recursively krisb-org-agenda-directory ".org$"))
  "A list of all org and org_archive files in `krisb-org-directory'.")

(defvar krisb-bibliography-files (list (expand-file-name "master-lib.bib" krisb-org-directory))
  "A list of my bibliography (.bib) files.")

;;;; Other
(defvar krisb-zotero-directory (expand-file-name "Zotero" "~")
  "The directory for everything Zotero.
Useful for some citation-related configurations.")

(defvar krisb-email-directory (expand-file-name "~/Documents/emails/")
  "Directory that houses my local email files.")

;;; Functions
;; Thanks to
;; https://emacs.stackexchange.com/questions/24657/unadvise-a-function-remove-all-advice-from-it
(defun krisb-advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props)
                 (advice-remove sym advice))
               sym))
;;; Macros
(defmacro krisb-evaluate-when-internet (interval &rest body)
  "Asynchronously evaluate BODY once internet connection is available.
Retries every INTERVAL seconds."
  (declare (indent 0))
  `(let ((url "https://google.com"))
     (cl-labels ((check-connection (status)
                   (if (plist-get status :error)
                       (progn
                         (message "No internet. Retrying in %s seconds..." interval)
                         (run-at-time interval nil
                                      (lambda () (url-retrieve url #'check-connection))))
                     (progn ,@body))))  ; Execute BODY on success.
       (url-retrieve url #'check-connection))))

(defmacro krisb-modus-themes-setup-faces (label &rest body)
  "Set up faces using modus-themes.
Does several things:
- Defines a function whose name begins with \"krisb-modus-themes-\",
  followed by LABEL and \"-setup-faces\".
- This function evaluates BODY within a `modus-themes-with-colors' block
  if the enabled theme's name begins with \"modus-\".
- Adds that function to `enable-theme-functions'.
- Calls the function immediately to apply the changes."
  (let ((name (intern (concat "krisb-modus-themes-" label "-setup-faces"))))
    `(progn
       ;; Define the function
       (defun ,name (theme)
         ,(concat "Set up faces for " label ".
This function was generated by the macro
`krisb-modus-themes-setup-faces'.")
         (when (string-match "^modus-" (symbol-name theme))
           (modus-themes-with-colors
             ,@body)))
       (,name (car custom-enabled-themes))
       (add-hook 'enable-theme-functions #',name))))

;; A useful macro for executing stuff in other windows. Taken from
;; https://karthinks.com/software/emacs-window-management-almanac/#with-other-window-an-elisp-helper
(defmacro krisb-with-other-window (&rest body)
  "Execute forms in BODY in the other window."
  `(unless (one-window-p)
     (with-selected-window (other-window-for-scrolling)
       ,@body)))

;;; Keymaps
(defvar-keymap krisb-note-keymap
  :doc "Prefix for my note-taking needs.")
(bind-key "C-c n" krisb-note-keymap 'global-map)

(defvar-keymap krisb-lsp-keymap
  :doc "Prefix for lsp-related commands.")
(with-eval-after-load 'lsp-mode
  (bind-key "C-c l" krisb-lsp-keymap 'lsp-mode-map))

(defvar-keymap krisb-file-keymap
  :doc "Prefix for file-related commands.")
(bind-key "C-c f" krisb-file-keymap 'global-map)

(defvar-keymap krisb-yank-keymap
  :doc "Prefix for yanking stuff.")
(bind-key "C-c i" krisb-yank-keymap 'global-map)

(defvar-keymap krisb-open-keymap
  :doc "Prefix for opening various hings.")
(bind-key "C-c o" krisb-open-keymap 'global-map)

(defvar-keymap krisb-toggle-keymap
  :doc "Prefix for toggling stuff.")
(bind-key "C-M-s-t" krisb-toggle-keymap 'global-map)


;;; Provide
(provide 'krisb-common)
;;; krisb-common.el ends here
