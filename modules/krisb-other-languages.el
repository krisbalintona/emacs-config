;;; Hyprlang-ts-mode
;; Treesitter mode for hyprland config files
(use-package hyprlang-ts-mode
  :init
  ;; 2025-03-31: You can install this grammar with:
  ;;     M-x treesit-install-language-grammar RET hyprlang
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(hyprlang "https://github.com/tree-sitter-grammars/tree-sitter-hyprlang"))))

;;; Provide
(provide 'krisb-other-languages)
