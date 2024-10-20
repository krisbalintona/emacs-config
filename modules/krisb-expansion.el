;;; Abbrev
;; Automatically correct typed strings (e.g. words).  Most useful for correcting
;; spelling mistakes as they are made.
(use-package abbrev
  :ensure nil
  :diminish
  :custom
  (save-abbrevs 'silently)
  (abbrev-suggest t)
  (abbrev-suggest-hint-threshold 2)
  :config
  (defun krisb-abbrev-todo-keyword--string ()
    "Select a todo keyword."
    ;; OPTIMIZE 2024-10-12: Don't rely on `hl-todo-keyword-faces'
    (completing-read "Keyword: " (split-string (key-description nil hl-todo-keyword-faces))))

  (defun krisb-abbrev-todo-keyword ()
    "Insert the a todo keyword."
    (insert (krisb-abbrev-todo-keyword--string)))

  (defun krisb-abbrev-current-date--string ()
    "Return the current date formatted."
    (format-time-string "%F"))

  (defun krisb-abbrev-current-date ()
    "Insert the current date."
    (insert (krisb-abbrev-current-date--string)))

  (defun krisb-abbrev-todo-keyword-and-date ()
    "Insert a todo keyword followed by the current date and colon."
    (insert (krisb-abbrev-todo-keyword--string) " " (krisb-abbrev-current-date--string) ":"))
  :config
  ;; Enable the mode globally
  (setq-default abbrev-mode t)

  ;; Allow abbrevs with a prefix colon, semicolon, or underscore. See:
  ;; <https://protesilaos.com/codelog/2024-02-03-emacs-abbrev-mode/>.
  (abbrev-table-put global-abbrev-table :regexp "\\(?:^\\|[\t\s]+\\)\\(?1:[:;_].*\\|.*\\)")

  ;; Predefined abbrevs
  (define-abbrev global-abbrev-table ";t" "" #'krisb-abbrev-todo-keyword)
  (define-abbrev global-abbrev-table ";d" "" #'krisb-abbrev-current-date)
  (define-abbrev global-abbrev-table ";td" "" #'krisb-abbrev-todo-keyword-and-date))

;;; Provide
(provide 'krisb-expansion)
