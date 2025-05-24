;; -*- lexical-binding: t; -*-

;;; Jump

;;;; Intra-file

;;;;; Occur
(use-package replace
  :ensure nil
  :config
  (with-eval-after-load 'krisb-reveal
    (defun kris-reveal-occur-find-information ()
      "Return information required by `krisb-reveal-fold-commands'.
See the docstring of `krisb-reveal-fold-commands'."
      (save-window-excursion
        (save-excursion
          (occur-mode-goto-occurrence)
          (cons (point) (current-buffer)))))
    (dolist (command '(next-error-no-select
                       previous-error-no-select
                       occur-mode-display-occurrence
                       occur-mode-goto-occurrence
                       occur-mode-goto-occurrence-other-window))
      (add-to-list 'krisb-reveal-fold-commands
                   (list :command command
                         :location #'kris-reveal-occur-find-information
                         :predicate (lambda () (eq major-mode 'occur-mode)))))))

;;;; Inter-file

;;; Provide
(provide 'krisb-navigation)
