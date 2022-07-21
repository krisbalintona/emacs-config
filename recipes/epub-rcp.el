;;; epub-rcp.el --- Summary
;;
;;; Commentary:
;;
;; For reading epub files
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;; Nov-mode
;; EPub reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook ((nov-mode . visual-line-mode)
         (nov-mode . visual-fill-column-mode))
  :config
  (with-eval-after-load 'eaf
    (add-to-list 'eaf-find-file-ext-blacklist "epub")))

;;; Justify-kp
;; Advanced justification of text with the Knuth/Plass algorithm
(use-package justify-kp
  :after nov
  :straight (justify-kp :type git :host github :repo "Fuco1/justify-kp")
  :hook (nov-post-html-render . kb/nov-post-html-render-hook)
  :config
  (defun kb/nov-window-configuration-change-hook ()
    (kb/nov-post-html-render-hook)
    (remove-hook 'window-configuration-change-hook 'kb/nov-window-configuration-change-hook t))

  (defun kb/nov-post-html-render-hook ()
    (if (get-buffer-window)
        (let ((max-width (pj-line-width))
              buffer-read-only)
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (not (looking-at "^[[:space:]]*$"))
                (goto-char (line-end-position))
                (when (> (shr-pixel-column) max-width)
                  (goto-char (line-beginning-position))
                  (pj-justify)))
              (forward-line 1))))
      (add-hook 'window-configuration-change-hook 'kb/nov-window-configuration-change-hook nil t))))

;;; epub-rcp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'epub-rcp)
