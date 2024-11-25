;;; Hyprlang-ts-mode
;; Treesitter mode for hyprland config files
(use-package hyprlang-ts-mode
  :vc ( :url "https://github.com/Nathan-Melaku/hyprlang-ts-mode.git"
        :rev :newest)
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(hyprlang "https://github.com/tree-sitter-grammars/tree-sitter-hyprlang"))))

;;; Provide
(provide 'krisb-other-languages)
