;;; completion-inline-rcp.el --- Inline completion   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords:

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

;; Packages that configure the vanilla `completion-at-point' functionality.

;;; Code:
(require 'use-package-rcp)
(require 'keybinds-general-rcp)

;;;; Kind-icon
;; Icons for corfu!
(use-package kind-icon
  :after corfu
  :demand
  :hook (kb/themes . (lambda ()
                       "Add hook to reset cache so the icon colors match my theme"
                       (call-interactively 'kind-icon-reset-cache)))
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; To unify background color
  (kind-icon-blend-background nil)  ; Mix foreground and background ("blended")?
  (kind-icon-blend-frac 0.08)
  (kind-icon-default-style ; FIXME 2024-02-03: Keyword annotations get cut off because icons too wide
   '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 1.0))
  ;; Use VSCode's icons. Taken from
  ;; https://github.com/jdtsmith/kind-icon/wiki#using-vs-code-icons-as-an-alternative
  (kind-icon-mapping
   '((array          "a"   :icon "symbol-array"       :face font-lock-type-face              :collection "nerd-fonts-codicons")
     (boolean        "b"   :icon "symbol-boolean"     :face font-lock-builtin-face           :collection "nerd-fonts-codicons")
     (color          "#"   :icon "symbol-color"       :face success                          :collection "nerd-fonts-codicons")
     (command        "cm"  :icon "chevron-right"      :face default                          :collection "nerd-fonts-codicons")
     (constant       "co"  :icon "symbol-constant"    :face font-lock-constant-face          :collection "nerd-fonts-codicons")
     (class          "c"   :icon "symbol-class"       :face font-lock-type-face              :collection "nerd-fonts-codicons")
     (constructor    "cn"  :icon "symbol-method"      :face font-lock-function-name-face     :collection "nerd-fonts-codicons")
     (enum           "e"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "nerd-fonts-codicons")
     (enummember     "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "nerd-fonts-codicons")
     (enum-member    "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "nerd-fonts-codicons")
     (event          "ev"  :icon "symbol-event"       :face font-lock-warning-face           :collection "nerd-fonts-codicons")
     (field          "fd"  :icon "symbol-field"       :face font-lock-variable-name-face     :collection "nerd-fonts-codicons")
     (file           "f"   :icon "symbol-file"        :face font-lock-string-face            :collection "nerd-fonts-codicons")
     (folder         "d"   :icon "folder"             :face font-lock-doc-face               :collection "nerd-fonts-codicons")
     (function       "f"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "nerd-fonts-codicons")
     (interface      "if"  :icon "symbol-interface"   :face font-lock-type-face              :collection "nerd-fonts-codicons")
     (keyword        "kw"  :icon "symbol-keyword"     :face font-lock-keyword-face           :collection "nerd-fonts-codicons")
     (macro          "mc"  :icon "lambda"             :face font-lock-keyword-face)
     (magic          "ma"  :icon "lightbulb-autofix"  :face font-lock-builtin-face           :collection "nerd-fonts-codicons")
     (method         "m"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "nerd-fonts-codicons")
     (module         "{"   :icon "file-code-outline"  :face font-lock-preprocessor-face)
     (numeric        "nu"  :icon "symbol-numeric"     :face font-lock-builtin-face           :collection "nerd-fonts-codicons")
     (operator       "op"  :icon "symbol-operator"    :face font-lock-comment-delimiter-face :collection "nerd-fonts-codicons")
     (param          "pa"  :icon "gear"               :face default                          :collection "nerd-fonts-codicons")
     (property       "pr"  :icon "symbol-property"    :face font-lock-variable-name-face     :collection "nerd-fonts-codicons")
     (reference      "rf"  :icon "library"            :face font-lock-variable-name-face     :collection "nerd-fonts-codicons")
     (snippet        "S"   :icon "symbol-snippet"     :face font-lock-string-face            :collection "nerd-fonts-codicons")
     (string         "s"   :icon "symbol-string"      :face font-lock-string-face            :collection "nerd-fonts-codicons")
     (struct         "%"   :icon "symbol-structure"   :face font-lock-variable-name-face     :collection "nerd-fonts-codicons")
     (text           "tx"  :icon "symbol-key"         :face font-lock-doc-face               :collection "nerd-fonts-codicons")
     (typeparameter  "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "nerd-fonts-codicons")
     (type-parameter "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "nerd-fonts-codicons")
     (unit           "u"   :icon "symbol-ruler"       :face font-lock-constant-face          :collection "nerd-fonts-codicons")
     (value          "v"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "nerd-fonts-codicons")
     (variable       "va"  :icon "symbol-variable"    :face font-lock-variable-name-face     :collection "nerd-fonts-codicons")
     (t              "."   :icon "question"           :face font-lock-warning-face           :collection "nerd-fonts-codicons")))
  :init
  (require 'svg-lib)
  (add-to-list 'svg-lib-icon-collections
               '("nerd-fonts-codicons" . "https://github.com/microsoft/vscode-codicons/raw/HEAD/src/icons/%s.svg"))
  :config
  ;; TODO 2022-05-24: See if I can use the cooler icons from
  ;; `lsp-bridge-icon--icons' without requiring the package
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;;; Nerd-icons-corfu
;; Use nerd-icons with corfu
(use-package nerd-icons-corfu
  :disabled ; Deprecated because I've created a setup with kind-icon that pulls nerd-icon icons
  :after corfu
  :demand
  :autoload nerd-icons-corfu-formatter
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
(provide 'completion-inline-rcp)
;;; completion-inline-rcp.el ends here
