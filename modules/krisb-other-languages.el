;;; LaTeX
;;;; Auctex
(use-package auctex
  :config
  ;; Add lualatex to commands list
  (with-eval-after-load 'tex
    (add-to-list 'TeX-command-list
                 '("LuaLaTeX" "lualatex --interaction=nonstopmode %t"
                   TeX-run-TeX nil LaTeX-mode :help "Run LuaLaTeX"))))

;;;; Auctex-cont-latexmk
;; Automatically compile a latex file on-save with the latexmk program.  This is
;; a more maintained version of auctex-latexmk
;; (https://github.com/tom-tan/auctex-latexmk/).
(use-package auctex-cont-latexmk
  ;; For AUR:
  ;; :ensure-system-package (latexmk . texlive-binextra)
  :after latex
  :bind ( :map LaTeX-mode-map
          ("C-c t k" . auctex-cont-latexmk-toggle)))

;;; Hyprlang-ts-mode
;; Treesitter mode for hyprland config files
(use-package hyprlang-ts-mode
  :init
  ;; 2025-03-31: You can install this grammar with:
  ;;     M-x treesit-install-language-grammar RET hyprlang
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(hyprlang "https://github.com/tree-sitter-grammars/tree-sitter-hyprlang"))
    (unless (treesit-grammar-location 'hyprlang)
      (treesit-install-language-grammar 'hyprlang))))

;;; Nix
(use-package nix-ts-mode)

;;; Provide
(provide 'krisb-other-languages)
