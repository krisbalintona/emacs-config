;;; Window
(use-package window
  :ensure nil
  :bind* ("M-o" . other-window)
  :bind (([remap other-window] . krisb-other-window-mru)
         :repeat-map other-window-repeat-map
         ("o" . krisb-other-window-mru))
  :custom
  (split-width-threshold (ceiling (* 0.6 (frame-width))))
  (split-height-threshold 80)
  (window-sides-vertical t)
  (window-resize-pixelwise t)
  (window-combination-resize t) ; Allow to resize existing windows when splitting?
  (fit-window-to-buffer-horizontally t)

  (switch-to-buffer-obey-display-actions t) ; As per suggestion of Mastering Emacs
  (switch-to-buffer-in-dedicated-window 'pop)
  :config
  ;; Modified version of "other-window-mru" taken from
  ;; https://karthinks.com/software/emacs-window-management-almanac/#the-back-and-forth-method
  ;; that accepts a prefix arg
  (defun krisb-other-window-mru (&optional arg)
    "Select the most recently used window on this frame."
    (interactive "p")
    (when-let ((windows-by-mru              ; Used `get-mru-window' as a reference
                (sort (delq nil
                            (mapcar
                             (lambda (win)
                               (when (and (not (eq win (selected-window)))
                                          (not (window-no-other-p win)))
                                 (cons (window-use-time win) win)))
                             (window-list-1 nil 'nomini nil)))
                      :lessp #'>
                      :key #'car)))
      (select-window (cdr (nth (1- (min (length windows-by-mru) (or arg 1))) windows-by-mru))))))

;;; Tab-bar
(use-package tab-bar
  :ensure nil
  :bind ( :map tab-prefix-map
          ("w" . tab-bar-move-window-to-tab)
          :repeat-map krisb-tab-bar-repeat-map
          ("C-c <left>" . tab-bar-history-back)
          ("C-c <right>" . tab-bar-history-forward))
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice 'clone)
  (tab-bar-close-last-tab-choice 'delete-frame)
  (tab-bar-select-tab-modifiers '(meta))
  (tab-bar-tab-hints t)
  (tab-bar-show t)
  (tab-bar-separator " ")
  (tab-bar-format
   '(tab-bar-format-tabs-groups
     tab-bar-separator
     tab-bar-format-align-right
     tab-bar-format-global))
  :config
  (tab-bar-mode 1)
  (tab-bar-history-mode 1))

;;;; Display-buffer-alist
(with-eval-after-load 'window
;;;;; Messages
  (setq display-buffer-alist
        `((,(rx (literal messages-buffer-name))
           (display-buffer-in-side-window)
           (window-height . 0.36)
           (side . top)
           (slot . 1)
           (post-command-select-window . t))
;;;;; Org-mime
          ("OrgMimeMailBody"
           (display-buffer-same-window))

;;;;; Diff-mode
          ((major-mode . diff-mode)
           (display-buffer-same-window))

;;;;; VC
          ((or . ((major-mode . vc-dir-mode)
                  (major-mode . vc-git-log-view-mode)

                  (major-mode . vc-git-region-history-mode)))
           (display-buffer-same-window))
          ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . 20)
           (dedicated . t)
           (preserve-size . (t . t)))
          ("\\*vc-log\\*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (dedicated . t))

;;;;; Help
          ((major-mode . help-mode)
           (display-buffer-reuse-window display-buffer-pop-up-window display-buffer-below-selected)
           (window-height . shrink-window-if-larger-than-buffer))

;;;;; Eldoc
          ("^\\*eldoc"
           (display-buffer-at-bottom)
           (post-command-select-window . t)
           (window-height . shrink-window-if-larger-than-buffer)
           (window-parameters . ((mode-line-format . none))))

;;;;; Org and calendar
          ("\\*\\(?:Org Select\\|Agenda Commands\\)\\*"
           (display-buffer-in-side-window)
           (window-height . fit-window-to-buffer)
           (side . top)
           (slot . -2)
           (preserve-size . (nil . t))
           (window-parameters . ((mode-line-format . none)))
           (post-command-select-window . t))
          ("\\*Calendar\\*"
           (display-buffer-below-selected)
           (window-height . fit-window-to-buffer))
          ))

;;;;; Xref
  (with-eval-after-load 'xref
    (add-to-list 'display-buffer-alist
                 `((or (major-mode . xref--xref-buffer-mode)
                       (,(rx (literal xref-buffer-name))))
                   (display-buffer-below-selected display-buffer-at-bottom)
                   (window-height . 0.25)))))

;;; Provide
(provide 'krisb-windows)
