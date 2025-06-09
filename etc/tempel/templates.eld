;; Several of these templates are taken from tempel-collection:
;; https://github.com/Crandel/tempel-collection

fundamental-mode

(t (p (if (require 'hl-todo nil t)
          (split-string (key-description nil hl-todo-keyword-faces))
        '("TODO" "FIXME" "NOTE" "REVIEW"))
      possible-todo-keywords noinsert)
   (completing-read "Keyword: " possible-todo-keywords) (just-one-space))
(d (format-time-string "%F"))
(td (p (if (equal major-mode 'emacs-lisp-mode) ";;" comment-start) comment-delimiter noinsert)
    (when (and comment-delimiter (not (nth 4 (syntax-ppss))))
      (concat (string-trim-right comment-delimiter)))
    (just-one-space) (include t) (include d) ":" (just-one-space))

emacs-lisp-mode

(al & ";;;###autoload")
(adv "(defun " (p "adv" name) " (&rest app)" n> p n> "(apply app))" n>
     "(advice-add #'" (p "fun") " " (p ":around") " #'" (s name) ")")
(prv "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
     ";;;" (file-name-nondirectory (or (buffer-file-name) (buffer-name))) " ends here" n)
(lg "(message \"LOG %s " (s sexp) ": %S"
    "\" (format-time-string \"%X\") " sexp ")")
(dv "(defvar " p "\n  \"" p "\")")
(dc "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")
(df "(defface " p " '((t :inherit " p "))\n  \"" p "\")")
(dg "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
(dm "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(da "(defalias '" p " '" p ")")
(dfn "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(package-header ";;;" (p (file-name-base (or (buffer-file-name) (buffer-name))) file) ".el --- " (p "summary" summary) " -*- lexical-binding: t -*-" n n

                ";; Author:" (p (user-full-name)) n
                ";; Version:" (p "version") n
                ";; Package-Requires:" (p "dependencies") n
                ";; Homepage:" (p "homepage") n
                ";; Keywords:" (p "keywords") n n

                ";; This file is not part of GNU Emacs" n n

                (p (concat
                    ";; This program is free software: you can redistribute it and/or modify\n"
                    ";; it under the terms of the GNU General Public License as published by\n"
                    ";; the Free Software Foundation, either version 3 of the License, or\n"
                    ";; (at your option) any later version.\n\n"

                    ";; This program is distributed in the hope that it will be useful,\n"
                    ";; but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
                    ";; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
                    ";; GNU General Public License for more details.\n\n"

                    ";; You should have received a copy of the GNU General Public License\n"
                    ";; along with this program.  If not, see <https://www.gnu.org/licenses/>."))

                n n

                ";;; Commentary:" n n

                ";;" (p "commentary") n n

                ";;; Code:" n n

                (p "(message \"Hello, World\")") n n

                "(provide '" (s file) ")" n
                ";;;" (s file) ".el ends here")

org-mode

(cid (if (fboundp 'krisb-org-create-dedicated-target)
         (krisb-org-ext-create-dedicated-target)
       (user-error "Function \“krisb-org-ext-create-dedicated-target\” is not defined")))
