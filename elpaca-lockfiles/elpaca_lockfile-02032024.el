((elpaca :source
   "lockfile" :date (26045 56636 173380 125000) :recipe
   (:source nil :protocol https :inherit t :depth 1 :repo
            "https://github.com/progfolio/elpaca.git" :ref
            "f16b80fe68de88003098086bdb317e5b90e5d0fe" :files
            (:defaults (:exclude "extensions")) :build
            (:not elpaca--activate-package) :package "elpaca"))
 (elpaca-use-package :source "lockfile" :date (26045 56636 172058 44000) :recipe
                     (:package "elpaca-use-package" :repo
                               "https://github.com/progfolio/elpaca.git" :files
                               ("extensions/elpaca-use-package.el") :main
                               "extensions/elpaca-use-package.el" :build
                               (:not elpaca--compile-info) :source
                               "Elpaca extensions" :protocol https :inherit t
                               :depth 1 :ref
                               "f16b80fe68de88003098086bdb317e5b90e5d0fe"))
 (use-package :source "lockfile"
   :date (26045 56636 169521 271000) :recipe
   (:package "use-package" :fetcher github :repo "jwiegley/use-package" :files
             (:defaults
              (:exclude "bind-key.el" "bind-chord.el" "use-package-chords.el"
                        "use-package-ensure-system-package.el"))
             :source "MELPA" :protocol https :inherit t :depth 1 :ref
             "a6e856418d2ebd053b34e0ab2fda328abeba731c"))
 (exec-path-from-shell :source "lockfile" :date (26045 56636 167999 986000)
                       :recipe
                       (:package "exec-path-from-shell" :fetcher github :repo
                                 "purcell/exec-path-from-shell" :files
                                 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                  "*.texinfo" "doc/dir" "doc/*.info"
                                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                  (:exclude ".dir-locals.el" "test.el"
                                            "tests.el" "*-test.el" "*-tests.el"
                                            "LICENSE" "README*" "*-pkg.el"))
                                 :source "MELPA" :protocol https :inherit t
                                 :depth 1 :ref
                                 "6c10a9d4a38425f2b494013b6bdff91537a6b6da"))
 (system-packages :source "lockfile" :date (26045 56636 165257 910000) :recipe
                  (:package "system-packages" :fetcher gitlab :repo
                            "jabranham/system-packages" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                             "doc/*.texinfo" "lisp/*.el"
                             (:exclude ".dir-locals.el" "test.el" "tests.el"
                                       "*-test.el" "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t :depth 1
                            :ref "c087d2c6e598f85fc2760324dce20104ea442fa3"))
 (gcmh :source "lockfile" :date (26045 56636 162796 258000) :recipe
       (:package "gcmh" :repo "koral/gcmh" :fetcher gitlab :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth 1 :ref
                 "0089f9c3a6d4e9a310d0791cf6fa8f35642ecfd9"))
 (no-littering :source "lockfile" :date (26045 56636 160335 297000) :recipe
               (:package "no-littering" :fetcher github :repo
                         "emacscollective/no-littering" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                                    "*-test.el" "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t :depth 1
                         :ref "dab372c82338ba10d9810151623b3fbd9e3a78e8"))
 (compat :source "lockfile" :date (26045 56636 157850 541000) :recipe
         (:package "compat" :repo "https://github.com/emacs-compat/compat"
                   :local-repo "compat" :files ("*" (:exclude ".git")) :source
                   "GNU-devel ELPA" :protocol https :inherit t :depth 1 :ref
                   "eb8fbfa5582a8e5880e2eaa66d15d498bca6a45a"))
 (general :source "lockfile" :date (26045 56636 155342 332000) :recipe
          (:package "general" :fetcher github :repo "noctuid/general.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref
                    "833dea2c4a60e06fcd552b653dfc8960935c9fb4"))
 (use-package-chords :source "lockfile" :date (26045 56636 152712 946000)
                     :recipe
                     (:package "use-package-chords" :repo "jwiegley/use-package"
                               :fetcher github :files ("use-package-chords.el")
                               :source "MELPA" :protocol https :inherit t :depth
                               1 :ref "a6e856418d2ebd053b34e0ab2fda328abeba731c"))
 (which-key :source "lockfile" :date (26045 56636 151265 49000) :recipe
            (:package "which-key" :repo "justbur/emacs-which-key" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "4d20bc852545a2e602f59084a630f888542052b1"))
 (all-the-icons :source "lockfile" :date (26045 56636 148757 891000) :recipe
                (:package "all-the-icons" :repo "domtronn/all-the-icons.el"
                          :fetcher github :files (:defaults "data") :source
                          "MELPA" :protocol https :inherit t :depth 1 :ref
                          "ee414384938ccf2ce93c77d717b85dc5538a257d"))
 (mixed-pitch :source "lockfile" :date (26045 56636 146289 256000) :recipe
              (:package "mixed-pitch" :fetcher gitlab :repo
                        "jabranham/mixed-pitch" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref
                        "519e05f74825abf04b7d2e0e38ec040d013a125a"))
 (default-text-scale :source "lockfile" :date (26045 56636 143850 927000)
                     :recipe
                     (:package "default-text-scale" :fetcher github :repo
                               "purcell/default-text-scale" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el"
                                (:exclude ".dir-locals.el" "test.el" "tests.el"
                                          "*-test.el" "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :protocol https :inherit t :depth
                               1 :ref "bfc0987c37e93742255d3b23d86c17096fda8e7e"))
 (emojify :source "lockfile" :date (26045 56636 141279 209000) :recipe
          (:package "emojify" :fetcher github :repo "iqbalansari/emacs-emojify"
                    :files (:defaults "data" "images") :source "MELPA" :protocol
                    https :inherit t :depth 1 :ref
                    "1b726412f19896abf5e4857d4c32220e33400b55"))
 (unicode-fonts :source "lockfile" :date (26045 56636 139038 319000) :recipe
                (:package "unicode-fonts" :repo "rolandwalker/unicode-fonts"
                          :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth 1
                          :ref "6245b97d8ddaeaf1de4dbe2cd85ca0f3b20ef81b"))
 (ligature :source "lockfile" :date (26045 56636 136344 623000) :recipe
           (:package "ligature" :fetcher github :repo "mickeynp/ligature.el"
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :type
                     git :host github :ref
                     "6ac1634612dbd42f7eb81ecaf022bd239aabb954"))
 (nano-theme :source "lockfile" :date (26045 56636 133623 195000) :recipe
             (:package "nano-theme" :repo
                       "https://github.com/rougier/nano-theme" :local-repo
                       "nano-theme" :files ("*" (:exclude ".git")) :source
                       "GNU-devel ELPA" :protocol https :inherit t :depth 1 :ref
                       "9d64dc167883835a6dc3a6d286c98dbbf7e95a96"))
 (modus-themes :source "lockfile" :date (26045 56636 131326 842000) :recipe
               (:package "modus-themes" :fetcher sourcehut :repo
                         "protesilaos/modus-themes" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                                    "*-test.el" "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t :depth 1
                         :ref "64823c7767710207cdf443492e0f712876dc4ee4"))
 (bind-chord :source "lockfile" :date (26045 56636 129839 50000) :recipe
             (:package "bind-chord" :repo "jwiegley/use-package" :fetcher github
                       :files ("bind-chord.el") :source "MELPA" :protocol https
                       :inherit t :depth 1 :ref
                       "a6e856418d2ebd053b34e0ab2fda328abeba731c"))
 (key-chord :source "lockfile" :date (26045 56636 127208 763000) :recipe
            (:package "key-chord" :fetcher github :repo
                      "emacsorphanage/key-chord" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "dbf91fefdad58b1c2f07c92e658ce81490837c60"))
 (ht :source "lockfile" :date (26045 56636 124417 354000) :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :source "MELPA" :protocol https :inherit t :depth 1 :ref
               "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (dash :source "lockfile" :date (26045 56636 122071 448000) :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi") :source "MELPA" :protocol https
                 :inherit t :depth 1 :ref
                 "13f9fcd09d40132e9e1346a69e7b293d835a85f2"))
 (font-utils :source "lockfile" :date (26045 56636 119163 701000) :recipe
             (:package "font-utils" :repo "rolandwalker/font-utils" :fetcher
                       github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref
                       "abc572eb0dc30a26584c0058c3fe6c7273a10003"))
 (ucs-utils :source "lockfile" :date (26045 56636 116426 93000) :recipe
            (:package "ucs-utils" :repo "rolandwalker/ucs-utils" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "91b9e0207fff5883383fd39c45ad5522e9b90e65"))
 (list-utils :source "lockfile" :date (26045 56636 114098 862000) :recipe
             (:package "list-utils" :repo "rolandwalker/list-utils" :fetcher
                       github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref
                       "f02dcef36330871855346f9eab97eef58d323d9a"))
 (persistent-soft :source "lockfile" :date (26045 56636 111557 190000) :recipe
                  (:package "persistent-soft" :repo
                            "rolandwalker/persistent-soft" :fetcher github
                            :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                             "doc/*.texinfo" "lisp/*.el"
                             (:exclude ".dir-locals.el" "test.el" "tests.el"
                                       "*-test.el" "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t :depth 1
                            :ref "a1e0ddf2a12a6f18cab565dee250f070384cbe02"))
 (pcache :source "lockfile" :date (26045 56636 108981 274000) :recipe
         (:package "pcache" :repo "sigma/pcache" :fetcher github :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth 1 :ref
                   "507230d094cc4a5025fe09b62569ad60c71c4226"))
 (fontaine :source "lockfile" :date (26045 56636 106332 171000) :recipe
           (:package "fontaine" :repo "https://git.sr.ht/~protesilaos/fontaine"
                     :local-repo "fontaine" :files
                     ("*" (:exclude ".git" "COPYING" "doclicense.texi")) :source
                     "GNU-devel ELPA" :protocol https :inherit t :depth 1 :ref
                     "8d5ba4ac64658da197b9eba41bf4e345a714f782"))
 (hide-mode-line :source "lockfile" :date (26045 56636 103768 869000) :recipe
                 (:package "hide-mode-line" :repo
                           "hlissner/emacs-hide-mode-line" :fetcher github
                           :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                            "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                            "lisp/*.el"
                            (:exclude ".dir-locals.el" "test.el" "tests.el"
                                      "*-test.el" "*-tests.el" "LICENSE"
                                      "README*" "*-pkg.el"))
                           :source "MELPA" :protocol https :inherit t :depth 1
                           :ref "bc5d293576c5e08c29e694078b96a5ed85631942"))
 (lin :source "lockfile" :date (26045 56636 101043 844000) :recipe
      (:package "lin" :repo "https://git.sr.ht/~protesilaos/lin" :local-repo
                "lin" :files ("*" (:exclude ".git" "COPYING" "doclicense.texi"))
                :source "GNU-devel ELPA" :protocol https :inherit t :depth 1
                :ref "75983a770d70b57c090db8cd503a340c32f51827"))
 (nerd-icons :source "lockfile" :date (26045 56636 98471 384000) :recipe
             (:package "nerd-icons" :repo "rainstormstudio/nerd-icons.el"
                       :fetcher github :files (:defaults "data") :source "MELPA"
                       :protocol https :inherit t :depth 1 :ref
                       "c6a4acf19454b415cba1c43daf4bfca8fccdd9ba"))
 (minions :source "lockfile" :date (26045 56636 95807 294000) :recipe
          (:package "minions" :fetcher github :repo "tarsius/minions" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref
                    "07caa8c30b12c35f3fe563a036f9823c4a6a5f01"))
 (diminish :source "lockfile" :date (26045 56636 93463 31000) :recipe
           (:package "diminish" :fetcher github :repo "myrjola/diminish.el"
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref
                     "fbd5d846611bad828e336b25d2e131d1bc06b83d"))
 (marginalia :source "lockfile" :date (26045 56636 90763 644000) :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher github
                       :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref
                       "ea356ebb1ddb8d6da78574b517155475cf52d46f"))
 (vertico :source "lockfile" :date (26045 56636 88334 753000) :recipe
          (:package "vertico" :repo "minad/vertico" :files
                    (:defaults "extensions/*") :fetcher github :source "MELPA"
                    :protocol https :inherit t :depth 1 :ref
                    "4a7da56b02c6aefff8f6b4574a530a7cb54bc21a"))
 (vertico-truncate :source "lockfile" :date (26045 56636 85686 792000) :recipe
                   (:source nil :protocol https :inherit t :depth 1 :type git
                            :host github :repo "jdtsmith/vertico-truncate"
                            :package "vertico-truncate" :ref
                            "7803be7f5b791b9e12be1bf3e0d381fd532d117a"))
 (orderless :source "lockfile" :date (26045 56636 82981 554000) :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "b24748093b00b37c3a572c4909f61c08fa27504f"))
 (flx-rs :source "lockfile" :date (26045 56636 80779 698000) :recipe
         (:source nil :protocol https :inherit t :depth 1 :repo
                  "jcs-elpa/flx-rs" :fetcher github :files (:defaults "bin")
                  :package "flx-rs" :ref
                  "538688482536f5a051ddc9eed0990ddc6b8181c1"))
 (liquidmetal :source "lockfile" :date (26045 56636 78325 199000) :recipe
              (:package "liquidmetal" :repo "jcs-elpa/liquidmetal" :fetcher
                        github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref
                        "5d100f4371e0d10656a2bd23c0461781c3c1884b"))
 (fuz-bin :source "lockfile" :date (26045 56636 75777 25000) :recipe
          (:source nil :protocol https :inherit t :depth 1 :repo
                   "jcs-elpa/fuz-bin" :fetcher github :files (:defaults "bin")
                   :package "fuz-bin" :ref
                   "41c38f9ac8eb0f203ebab6d47148b4ff80a70d28"))
 (fzf-native :source "lockfile" :date (26045 56636 73371 718000) :recipe
             (:source nil :protocol https :inherit t :depth 1 :repo
                      "dangduc/fzf-native" :host github :files (:defaults "bin")
                      :package "fzf-native" :ref
                      "ba06f5149e23c57510de2e67d5c46aee96356dba"))
 (sublime-fuzzy :source "lockfile" :date (26045 56636 70739 887000) :recipe
                (:source nil :protocol https :inherit t :depth 1 :repo
                         "jcs-elpa/sublime-fuzzy" :fetcher github :files
                         (:defaults "bin") :package "sublime-fuzzy" :ref
                         "a0f1c86601e3287e96956fd79cc46252f21ef8aa"))
 (hotfuzz :source "lockfile" :date (26045 56636 67988 754000) :recipe
          (:package "hotfuzz" :repo "axelf4/hotfuzz" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref
                    "0d89041ca494432d79e85b0454f21a75c6e21925"))
 (corfu :source "lockfile" :date (26045 56636 65364 908000) :recipe
        (:package "corfu" :repo "minad/corfu" :files (:defaults "extensions/*")
                  :fetcher github :source "MELPA" :protocol https :inherit t
                  :depth 1 :ref "b48d3017a47706198e04440cc1b3483bdf646771"))
 (kind-icon :source "lockfile" :date (26045 56636 62874 913000) :recipe
            (:package "kind-icon" :repo "https://github.com/jdtsmith/kind-icon"
                      :local-repo "kind-icon" :files ("*" (:exclude ".git"))
                      :source "GNU-devel ELPA" :protocol https :inherit t :depth
                      1 :ref "724f1facb580cd54057bdae1ee9942c385c8d92e"))
 (cape :source "lockfile" :date (26045 56636 60440 692000) :recipe
       (:package "cape" :repo "minad/cape" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth 1 :ref
                 "a687fbeddc0f1dcdc7e4c2d2a69c96243587317d"))
 (company :source "lockfile" :date (26045 56636 58012 532000) :recipe
          (:package "company" :fetcher github :repo "company-mode/company-mode"
                    :files
                    (:defaults "icons" ("images/small" "doc/images/small/*.png"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref
                    "84b111f26663c65cb9e465a028c9558332b4951b"))
 (company-box :source "lockfile" :date (26045 56636 55877 891000) :recipe
              (:package "company-box" :fetcher github :repo
                        "sebastiencs/company-box" :files (:defaults "images")
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref
                        "b6f53e26adf948aca55c3ff6c22c21a6a6614253"))
 (company-prescient :source "lockfile" :date (26045 56636 53278 872000) :recipe
                    (:package "company-prescient" :fetcher github :repo
                              "radian-software/prescient.el" :files
                              ("company-prescient.el") :source "MELPA" :protocol
                              https :inherit t :depth 1 :ref
                              "4b875be52e75f7b81e68a16b62cfbb2f2584042c"))
 (company-shell :source "lockfile" :date (26045 56636 51690 382000) :recipe
                (:package "company-shell" :repo "Alexander-Miller/company-shell"
                          :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth 1
                          :ref "5f959a63a6e66eb0cbdac3168cad523a62cc2ccd"))
 (ivy :source "lockfile" :date (26045 56636 49341 570000) :recipe
      (:package "ivy" :repo "abo-abo/swiper" :fetcher github :files
                (:defaults "doc/ivy-help.org"
                           (:exclude "swiper.el" "counsel.el" "ivy-hydra.el"
                                     "ivy-avy.el"))
                :source "MELPA" :protocol https :inherit t :depth 1 :ref
                "8c30f4cab5948aa8d942a3b2bbf5fb6a94d9441d"))
 (ivy-rich :source "lockfile" :date (26045 56636 47951 481000) :recipe
           (:package "ivy-rich" :repo "Yevgnen/ivy-rich" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref
                     "aff9b6bd53e0fdcf350ab83c90e64e651b47dba4"))
 (all-the-icons-ivy-rich :source "lockfile" :date (26045 56636 45393 249000)
                         :recipe
                         (:package "all-the-icons-ivy-rich" :fetcher github
                                   :repo "seagle0128/all-the-icons-ivy-rich"
                                   :files
                                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                    "*.texinfo" "doc/dir" "doc/*.info"
                                    "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                    (:exclude ".dir-locals.el" "test.el"
                                              "tests.el" "*-test.el"
                                              "*-tests.el" "LICENSE" "README*"
                                              "*-pkg.el"))
                                   :source "MELPA" :protocol https :inherit t
                                   :depth 1 :ref
                                   "f4d77b63c226dc744d1cd3e44dafda787dd71c7b"))
 (ivy-prescient :source "lockfile" :date (26045 56636 43134 907000) :recipe
                (:package "ivy-prescient" :fetcher github :repo
                          "radian-software/prescient.el" :files
                          ("ivy-prescient.el") :source "MELPA" :protocol https
                          :inherit t :depth 1 :ref
                          "4b875be52e75f7b81e68a16b62cfbb2f2584042c"))
 (flx :source "lockfile" :date (26045 56636 41620 415000) :recipe
      (:package "flx" :repo "lewang/flx" :fetcher github :files ("flx.el")
                :source "MELPA" :protocol https :inherit t :depth 1 :ref
                "7b44a5abb254bbfbeca7a29336f7f4ebd8aabbf2"))
 (counsel :source "lockfile" :date (26045 56636 39356 823000) :recipe
          (:package "counsel" :repo "abo-abo/swiper" :fetcher github :files
                    ("counsel.el") :source "MELPA" :protocol https :inherit t
                    :depth 1 :ref "8c30f4cab5948aa8d942a3b2bbf5fb6a94d9441d"))
 (helm :source "lockfile" :date (26045 56636 37894 669000) :recipe
       (:package "helm" :fetcher github :repo "emacs-helm/helm" :files
                 ("*.el" "emacs-helm.sh"
                  (:exclude "helm-lib.el" "helm-source.el" "helm-multi-match.el"
                            "helm-core.el"))
                 :source "MELPA" :protocol https :inherit t :depth 1 :ref
                 "f817c7920dcfcaa24a258917ff9b7a3a4023fd91"))
 (transpose-frame :source "lockfile" :date (26045 56636 36456 571000) :recipe
                  (:package "transpose-frame" :fetcher github :repo
                            "emacsorphanage/transpose-frame" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                             "doc/*.texinfo" "lisp/*.el"
                             (:exclude ".dir-locals.el" "test.el" "tests.el"
                                       "*-test.el" "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t :depth 1
                            :ref "94c87794d53883a2358d13da264ad8dab9a52daa"))
 (ace-window :source "lockfile" :date (26045 56636 33729 241000) :recipe
             (:package "ace-window" :repo "abo-abo/ace-window" :fetcher github
                       :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref
                       "77115afc1b0b9f633084cf7479c767988106c196"))
 (popper :source "lockfile" :date (26045 56636 31249 786000) :recipe
         (:package "popper" :fetcher github :repo "karthink/popper" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth 1 :ref
                   "570b0820f884a9c0e3d9cb07e7f7f523b39b836f"))
 (burly :source "lockfile" :date (26045 56636 28671 335000) :recipe
        (:package "burly" :fetcher github :repo "alphapapa/burly.el" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                   "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth 1 :branch
                  "wip/readablep" :ref
                  "df333960d5229f1ff75d8e5e6d65db3db08123aa"))
 (perfect-margin :source "lockfile" :date (26045 56636 26146 695000) :recipe
                 (:package "perfect-margin" :repo "mpwang/perfect-margin"
                           :fetcher github :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                            "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                            "lisp/*.el"
                            (:exclude ".dir-locals.el" "test.el" "tests.el"
                                      "*-test.el" "*-tests.el" "LICENSE"
                                      "README*" "*-pkg.el"))
                           :source "MELPA" :protocol https :inherit t :depth 1
                           :ref "bd15823219c93261536966b4dc65c1ae45a404cf"))
 (undo-fu :source "lockfile" :date (26045 56636 23405 851000) :recipe
          (:package "undo-fu" :fetcher codeberg :repo "ideasman42/emacs-undo-fu"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref
                    "0e74116fd5c7797811a91ba4eadef50d67523eb6"))
 (undo-fu-session :source "lockfile" :date (26045 56636 20823 92000) :recipe
                  (:package "undo-fu-session" :fetcher codeberg :repo
                            "ideasman42/emacs-undo-fu-session" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                             "doc/*.texinfo" "lisp/*.el"
                             (:exclude ".dir-locals.el" "test.el" "tests.el"
                                       "*-test.el" "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t :depth 1
                            :ref "9147a7223ee8136769cf42239c7d9a8577edfaff"))
 (puni :source "lockfile" :date (26045 56636 18047 122000) :recipe
       (:package "puni" :repo "AmaiKinono/puni" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth 1 :ref
                 "72e091ef30e0c9299dbcd0bc4669ab9bb8fb6e47"))
 (avy :source "lockfile" :date (26045 56636 15696 858000) :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :source "MELPA" :protocol https :inherit t :depth 1 :ref
                "be612110cb116a38b8603df367942e2bb3d9bdbe"))
 (imenu-list :source "lockfile" :date (26045 56636 13156 47000) :recipe
             (:package "imenu-list" :repo "bmag/imenu-list" :fetcher github
                       :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref
                       "76f2335ee6f2f066d87fe4e4729219d70c9bc70d"))
 (flyspell-correct :source "lockfile" :date (26045 56636 10814 330000) :recipe
                   (:package "flyspell-correct" :repo
                             "d12frosted/flyspell-correct" :fetcher github
                             :files
                             ("flyspell-correct.el" "flyspell-correct-ido.el")
                             :source "MELPA" :protocol https :inherit t :depth 1
                             :ref "1e7a5a56362dd875dddf848b9a9e25d1395b9d37"))
 (jinx :source "lockfile" :date (26045 56636 8426 696000) :recipe
       (:package "jinx" :repo "minad/jinx" :files
                 (:defaults "jinx-mod.c" "emacs-module.h") :fetcher github
                 :source "MELPA" :protocol https :inherit t :depth nil :ref
                 "50dfdcdbdeb320fe0cf02006c90d087eb7a9787d"))
 (flymake-languagetool :source "lockfile" :date (26045 56636 5750 672000)
                       :recipe
                       (:package "flymake-languagetool" :repo
                                 "emacs-languagetool/flymake-languagetool"
                                 :fetcher github :files
                                 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                  "*.texinfo" "doc/dir" "doc/*.info"
                                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                  (:exclude ".dir-locals.el" "test.el"
                                            "tests.el" "*-test.el" "*-tests.el"
                                            "LICENSE" "README*" "*-pkg.el"))
                                 :source "MELPA" :protocol https :inherit t
                                 :depth 1 :ref
                                 "12db8f943f5f856d43b6ca78819234e697276162"))
 (lsp-grammarly :source "lockfile" :date (26045 56636 3033 322000) :recipe
                (:package "lsp-grammarly" :repo "emacs-grammarly/lsp-grammarly"
                          :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth 1
                          :ref "e1eabc08b670b578cfdedeb2bcc43a3c5c5a7729"))
 (powerthesaurus :source "lockfile" :date (26045 56636 189 986000) :recipe
                 (:package "powerthesaurus" :repo
                           "SavchenkoValeriy/emacs-powerthesaurus" :fetcher
                           github :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                            "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                            "lisp/*.el"
                            (:exclude ".dir-locals.el" "test.el" "tests.el"
                                      "*-test.el" "*-tests.el" "LICENSE"
                                      "README*" "*-pkg.el"))
                           :source "MELPA" :protocol https :inherit t :depth 1
                           :ref "4b97797cf789aaba411c61a85fe23474ebc5bedc"))
 (le-thesaurus :source "lockfile" :date (26045 56635 997499 686000) :recipe
               (:package "le-thesaurus" :repo "AnselmC/le-thesaurus.el" :fetcher
                         github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                                    "*-test.el" "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t :depth 1
                         :ref "83e8df8957a3b8167cc2bf97849a1eca555ce9a6"))
 (synosaurus :source "lockfile" :date (26045 56635 994798 175000) :recipe
             (:package "synosaurus" :repo "hpdeifel/synosaurus" :fetcher github
                       :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref
                       "14d34fc92a77c3a916b4d58400424c44ae99cd81"))
 (org :source "lockfile" :date (26045 56635 992191 362000) :recipe
      (:package "org" :local-repo "org" :repo
                "https://git.savannah.gnu.org/git/emacs/org-mode.git" :pre-build
                (progn (require 'elpaca-menu-org) (elpaca-menu-org--build))
                :autoloads "org-loaddefs.el" :build
                (:not elpaca--generate-autoloads-async) :files
                (:defaults ("etc/styles/" "etc/styles/*" "doc/*.texi")) :source
                "Org" :protocol https :inherit t :depth 1 :ref
                "5e98599f50c8a959638d653f8d4874bb8461f145"))
 (mermaid-mode :source "lockfile" :date (26045 56635 989426 102000) :recipe
               (:package "mermaid-mode" :repo "abrochard/mermaid-mode" :fetcher
                         github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                                    "*-test.el" "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t :depth 1
                         :ref "d8bfb8c819cda9ead19c871842f6b0b8d56c56c0"))
 (ob-mermaid :source "lockfile" :date (26045 56635 986990 398000) :recipe
             (:package "ob-mermaid" :repo "arnm/ob-mermaid" :fetcher github
                       :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref
                       "b4ce25699e3ebff054f523375d1cf5a17bd0dbaf"))
 (org-superstar :source "lockfile" :date (26045 56635 984349 29000) :recipe
                (:package "org-superstar" :fetcher github :repo
                          "integral-dw/org-superstar-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth 1
                          :ref "54c81c27dde2a6dc461bb064e79a8b2089093a2e"))
 (olivetti :source "lockfile" :date (26045 56635 981980 0) :recipe
           (:package "olivetti" :fetcher github :repo "rnkn/olivetti" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref
                     "1f8b3d5cb155f7497083001037a09a972befab21"))
 (org-appear :source "lockfile" :date (26045 56635 979112 329000) :recipe
             (:package "org-appear" :fetcher github :repo "awth13/org-appear"
                       :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref
                       "81eba5d7a5b74cdb1bad091d85667e836f16b997"))
 (svg-lib :source "lockfile" :date (26045 56635 976783 916000) :recipe
          (:package "svg-lib" :repo "https://github.com/rougier/svg-lib"
                    :local-repo "svg-lib" :files ("*" (:exclude ".git")) :source
                    "GNU-devel ELPA" :protocol https :inherit t :depth 1 :ref
                    "beafc98077401643f2db98158833f012e713a6c2"))
 (org-margin :source "lockfile" :date (26045 56635 974222 467000) :recipe
             (:source nil :protocol https :inherit t :depth 1 :type git :host
                      github :repo "rougier/org-margin" :package "org-margin"
                      :ref "4013b59ff829903a7ab86b95593be71aa5c9b87d"))
 (org-web-tools :source "lockfile" :date (26045 56635 971443 0) :recipe
                (:package "org-web-tools" :fetcher github :repo
                          "alphapapa/org-web-tools" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth 1
                          :ref "7a6498f442fc7f29504745649948635c7165d847"))
 (org-download :source "lockfile" :date (26045 56635 968724 27000) :recipe
               (:package "org-download" :repo "abo-abo/org-download" :fetcher
                         github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                                    "*-test.el" "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t :depth 1
                         :ref "19e166f0a8c539b4144cfbc614309d47a9b2a9b7"))
 (typo :source "lockfile" :date (26045 56635 965897 552000) :recipe
       (:package "typo" :fetcher github :repo "jorgenschaefer/typoel" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth 1 :ref
                 "173ebe4fc7ac38f344b16e6eaf41f79e38f20d57"))
 (org-contrib :source "lockfile" :date (26045 56635 963328 920000) :recipe
              (:package "org-contrib" :local-repo "org-contrib" :repo
                        "https://git.sr.ht/~bzg/org-contrib" :files (:defaults)
                        :source "Org" :protocol https :inherit t :depth 1 :ref
                        "0d5abf4a2e214a03606ca4b43adec5be3066f363"))
 (ox-pandoc :source "lockfile" :date (26045 56635 960914 696000) :recipe
            (:package "ox-pandoc" :repo "emacsorphanage/ox-pandoc" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "399d787b6e2124bd782615338b845c3724a47718"))
 (citar :source "lockfile" :date (26045 56635 958594 318000) :recipe
        (:package "citar" :repo "emacs-citar/citar" :fetcher github :files
                  (:defaults (:exclude "citar-embark.el")) :old-names
                  (bibtex-actions) :source "MELPA" :protocol https :inherit t
                  :depth 1 :ref "885b86f6733fd70f42c32dd7791d3447f93db990"))
 (citar-embark :source "lockfile" :date (26045 56635 957153 194000) :recipe
               (:package "citar-embark" :repo "emacs-citar/citar" :fetcher
                         github :files ("citar-embark.el") :source "MELPA"
                         :protocol https :inherit t :depth 1 :ref
                         "885b86f6733fd70f42c32dd7791d3447f93db990"))
 (ox-hugo :source "lockfile" :date (26045 56635 954605 901000) :recipe
          (:package "ox-hugo" :fetcher github :repo "kaushalmodi/ox-hugo" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref
                    "cb1b6cfd7b080e889352150416c1725f11ba937a"))
 (denote :source "lockfile" :date (26045 56635 951767 905000) :recipe
         (:package "denote" :repo "emacs-straight/denote" :local-repo "denote"
                   :files ("*" (:exclude ".git")) :source "GNU-devel ELPA"
                   :protocol https :inherit t :depth nil :type git :host github
                   :ref "7ca5234d035d6f5612eea3a41353b5ad6802b753"))
 (denote-menu :source "lockfile" :date (26045 56635 948882 641000) :recipe
              (:package "denote-menu" :repo "namilus/denote-menu" :local-repo
                        "denote-menu" :files ("*" (:exclude ".git" "COPYING"))
                        :source "GNU-devel ELPA" :protocol https :inherit t
                        :depth 1 :type git :host github :ref
                        "6d97b6be0511420dca27b294844bdaa5fa72f753"))
 (citar-denote :source "lockfile" :date (26045 56635 946291 807000) :recipe
               (:package "citar-denote" :repo "pprevos/citar-denote" :fetcher
                         github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                                    "*-test.el" "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t :depth 1
                         :ref "0c68a4f74f5dcfc23ef03b211658a2ccc5d9aa3b"))
 (pdf-tools :source "lockfile" :date (26045 56635 943786 42000) :recipe
            (:package "pdf-tools" :fetcher github :repo "vedang/pdf-tools"
                      :files
                      (:defaults "README" ("build" "Makefile")
                                 ("build" "server"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "c69e7656a4678fe25afbd29f3503dd19ee7f9896"))
 (saveplace-pdf-view :source "lockfile" :date (26045 56635 941270 960000)
                     :recipe
                     (:package "saveplace-pdf-view" :fetcher github :repo
                               "nicolaisingh/saveplace-pdf-view" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el"
                                (:exclude ".dir-locals.el" "test.el" "tests.el"
                                          "*-test.el" "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :protocol https :inherit t :depth
                               1 :ref "abfb5e1f463cffc18218a0f7f2fa141a271b1813"))
 (org-noter :source "lockfile" :date (26045 56635 938689 153000) :recipe
            (:package "org-noter" :fetcher github :repo "org-noter/org-noter"
                      :files
                      ("*.el" "modules"
                       (:exclude "*-test-utils.el" "*-devel.el"))
                      :source "MELPA" :protocol ssh :inherit t :depth 1 :remotes
                      ("remote" :repo "krisbalintona/org-noter") :ref
                      "cbf52dd436a8a6c417e4b760342acbea7d7f3157"))
 (zotxt :source "lockfile" :date (26045 56635 935470 535000) :recipe
        (:package "zotxt" :fetcher gitlab :repo "egh/zotxt-emacs" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                   "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth 1 :ref
                  "96a132d6b39f6bc19a58913b761d42efc198f8a4"))
 (org-remark :source "lockfile" :date (26045 56635 932240 687000) :recipe
             (:package "org-remark" :repo "https://github.com/nobiot/org-remark"
                       :local-repo "org-remark" :files ("*" (:exclude ".git"))
                       :source "GNU-devel ELPA" :protocol https :inherit t
                       :depth 1 :ref "1948980f26bd4635acfc230d777e2c57c06fe00b"))
 (org-transclusion :source "lockfile" :date (26045 56635 929957 28000) :recipe
                   (:package "org-transclusion" :repo
                             "https://github.com/nobiot/org-transclusion"
                             :local-repo "org-transclusion" :files
                             ("*" (:exclude ".git")) :source "GNU-devel ELPA"
                             :protocol https :inherit t :depth 1 :ref
                             "e65cd19167f89126e25ac50320be0d59c765eee6"))
 (websocket :source "lockfile" :date (26045 56635 927404 966000) :recipe
            (:package "websocket" :repo "ahyatt/emacs-websocket" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "40c208eaab99999d7c1e4bea883648da24c03be3"))
 (simple-httpd :source "lockfile" :date (26045 56635 924911 434000) :recipe
               (:package "simple-httpd" :repo "skeeto/emacs-web-server" :fetcher
                         github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                                    "*-test.el" "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t :depth 1
                         :ref "3982c55e9061475038a3ccd61aecb2de3d407cec"))
 (f :source "lockfile" :date (26045 56635 922464 269000) :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                         "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth 1 :ref
              "4f03d7bb724a9049b0ab9ef86127694756f99656"))
 (org-roam-ui :source "lockfile" :date (26045 56635 920157 536000) :recipe
              (:package "org-roam-ui" :fetcher github :repo
                        "org-roam/org-roam-ui" :files ("*.el" "out") :source
                        "MELPA" :protocol https :inherit t :depth 1 :host github
                        :branch "main" :ref
                        "5ac74960231db0bf7783c2ba7a19a60f582e91ab"))
 (lister :source "lockfile" :date (26045 56635 917561 131000) :recipe
         (:package "lister" :repo "publicimageltd/lister" :fetcher github :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth 1 :ref
                   "84fbba7450ac02cbb844727a28b6f245f553df7b"))
 (org-super-agenda :source "lockfile" :date (26045 56635 915031 653000) :recipe
                   (:package "org-super-agenda" :fetcher github :repo
                             "alphapapa/org-super-agenda" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                              "doc/*.texinfo" "lisp/*.el"
                              (:exclude ".dir-locals.el" "test.el" "tests.el"
                                        "*-test.el" "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit t :depth 1
                             :ref "ee3379ae92b90c084717fb2a7614060ce12283cb"))
 (work-timer :source "lockfile" :date (26045 56635 912590 719000) :recipe
             (:source nil :protocol ssh :inherit t :depth nil :host github :repo
                      "krisbalintona/work-timer" :files (:defaults "*.mp3")
                      :package "work-timer" :ref
                      "a60f8f6ec6ac5f1f340ab025a2fd50d8f5c17fbf"))
 (org-edna :source "lockfile" :date (26045 56635 911390 375000) :recipe
           (:package "org-edna" :repo "git://git.sv.gnu.org/emacs/elpa"
                     :local-repo "org-edna" :branch "externals/org-edna" :files
                     ("*" (:exclude ".git")) :source "GNU-devel ELPA" :protocol
                     https :inherit t :depth 1 :ref
                     "8258a4dfa00aa522249cdf9aeea5be4de97bd7c1"))
 (org-timeblock :source "lockfile" :date (26045 56635 909004 43000) :recipe
                (:package "org-timeblock" :fetcher github :repo
                          "ichernyshovvv/org-timeblock" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth 1
                          :type git :host github :ref
                          "7b39e19d511536835a3891f1cfbcdff4983b29b6"))
 (auctex :source "lockfile" :date (26045 56635 906996 851000) :recipe
         (:package "auctex" :repo "emacs-straight/auctex" :local-repo "auctex"
                   :files
                   ("*.el" "*.info" "dir" "doc" "etc" "images" "latex" "style")
                   :source "GNU-devel ELPA" :protocol https :inherit t :depth 1
                   :type git :host github :pre-build
                   (("chmod" "775" "autogen.sh") ("./autogen.sh")) :ref
                   "86b2397abdc20a638e5751251026727bc6282022"))
 (cdlatex :source "lockfile" :date (26045 56635 904554 274000) :recipe
          (:package "cdlatex" :fetcher github :repo "cdominik/cdlatex" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref
                    "33770dec73138909714711b05a63e79da5a19ccd"))
 (auctex-latexmk :source "lockfile" :date (26045 56635 901775 349000) :recipe
                 (:package "auctex-latexmk" :fetcher github :repo
                           "emacsmirror/auctex-latexmk" :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                            "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                            "lisp/*.el"
                            (:exclude ".dir-locals.el" "test.el" "tests.el"
                                      "*-test.el" "*-tests.el" "LICENSE"
                                      "README*" "*-pkg.el"))
                           :source "MELPA" :protocol https :inherit t :depth 1
                           :ref "b00a95e6b34c94987fda5a57c20cfe2f064b1c7a"))
 (popweb :source "lockfile" :date (26045 56635 898999 689000) :recipe
         (:source nil :protocol https :inherit t :depth 1 :type git :host github
                  :repo "manateelazycat/popweb" :files
                  (:defaults "*.py" "*.js" "extension/*/*") :package "popweb"
                  :ref "ca6262b0f0a44076526457e57056ffb92340e984"))
 (markdown-mode :source "lockfile" :date (26045 56635 896512 830000) :recipe
                (:package "markdown-mode" :fetcher github :repo
                          "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth 1
                          :ref "e096bb97a91fcd4dc2b46d8b6e093194b03b7364"))
 (markdown-xwidget :source "lockfile" :date (26045 56635 893958 574000) :recipe
                   (:source nil :protocol https :inherit t :depth 1 :type git
                            :host github :repo "cfclrk/markdown-xwidget" :files
                            (:defaults "resources") :package "markdown-xwidget"
                            :ref "f7f6f74729c9604a8a845ca73929822e0066c0ea"))
 (esup :source "lockfile" :date (26045 56635 891363 933000) :recipe
       (:package "esup" :fetcher github :repo "jschaf/esup" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth 1 :ref
                 "4b49c8d599d4cc0fbf994e9e54a9c78e5ab62a5f"))
 (explain-pause-mode :source "lockfile" :date (26045 56635 888916 718000)
                     :recipe
                     (:source nil :protocol https :inherit t :depth 1 :type git
                              :host github :repo
                              "lastquestion/explain-pause-mode" :package
                              "explain-pause-mode" :ref
                              "2356c8c3639cbeeb9751744dbe737267849b4b51"))
 (bug-hunter :source "lockfile" :date (26045 56635 886176 344000) :recipe
             (:package "bug-hunter" :repo
                       "https://github.com/Malabarba/elisp-bug-hunter"
                       :local-repo "bug-hunter" :files ("*" (:exclude ".git"))
                       :source "GNU-devel ELPA" :protocol https :inherit t
                       :depth 1 :ref "31a2da8fd5825f0938a1cce976baf39805b13e9f"))
 (hl-todo :source "lockfile" :date (26045 56635 883323 391000) :recipe
          (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref
                    "f1fef158f99a70746926ef52c59f4863a29b7ed7"))
 (rainbow-mode :source "lockfile" :date (26045 56635 880853 62000) :recipe
               (:package "rainbow-mode" :repo "git://git.sv.gnu.org/emacs/elpa"
                         :local-repo "rainbow-mode" :branch
                         "externals/rainbow-mode" :files ("*" (:exclude ".git"))
                         :source "GNU-devel ELPA" :protocol https :inherit t
                         :depth 1 :ref
                         "f7db3b5919f70420a91eb199f8663468de3033f3"))
 (highlight-defined :source "lockfile" :date (26045 56635 877690 880000) :recipe
                    (:package "highlight-defined" :fetcher github :repo
                              "Fanael/highlight-defined" :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                               "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el"
                               (:exclude ".dir-locals.el" "test.el" "tests.el"
                                         "*-test.el" "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "MELPA" :protocol https :inherit t :depth
                              1 :ref "4420bdda419875dacb065468aafe273b2022580e"))
 (highlight-quoted :source "lockfile" :date (26045 56635 875316 561000) :recipe
                   (:package "highlight-quoted" :fetcher github :repo
                             "Fanael/highlight-quoted" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                              "doc/*.texinfo" "lisp/*.el"
                              (:exclude ".dir-locals.el" "test.el" "tests.el"
                                        "*-test.el" "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit t :depth 1
                             :ref "24103478158cd19fbcfb4339a3f1fa1f054f1469"))
 (adaptive-wrap :source "lockfile" :date (26045 56635 872835 862000) :recipe
                (:package "adaptive-wrap" :repo
                          "git://git.sv.gnu.org/emacs/elpa" :local-repo
                          "adaptive-wrap" :branch "externals/adaptive-wrap"
                          :files ("*" (:exclude ".git")) :source
                          "GNU-devel ELPA" :protocol https :inherit t :depth 1
                          :ref "cb759c0ad5a3203464687c09dbe0e56464c2126e"))
 (consult :source "lockfile" :date (26045 56635 869732 390000) :recipe
          (:package "consult" :repo "minad/consult" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref
                    "42aaed5cc85dfc21c9c31a43ccbf3db5b347e593"))
 (embark :source "lockfile" :date (26045 56635 867162 526000) :recipe
         (:package "embark" :repo "oantolin/embark" :fetcher github :files
                   ("embark.el" "embark-org.el" "embark.texi") :source "MELPA"
                   :protocol https :inherit t :depth 1 :ref
                   "e0ee1c78620c7cdc81bd786fb44bf0e2ee918e31"))
 (embark-consult :source "lockfile" :date (26045 56635 865969 125000) :recipe
                 (:package "embark-consult" :repo "oantolin/embark" :fetcher
                           github :files ("embark-consult.el") :source "MELPA"
                           :protocol https :inherit t :depth 1 :ref
                           "e0ee1c78620c7cdc81bd786fb44bf0e2ee918e31"))
 (sudo-edit :source "lockfile" :date (26045 56635 863678 452000) :recipe
            (:package "sudo-edit" :repo "nflath/sudo-edit" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "74eb1e6986461baed9a9269566ff838530b4379b"))
 (vimrc-mode :source "lockfile" :date (26045 56635 861060 437000) :recipe
             (:package "vimrc-mode" :fetcher github :repo "mcandre/vimrc-mode"
                       :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref
                       "13bc150a870d5d4a95f1111e4740e2b22813c30e"))
 (outshine :source "lockfile" :date (26045 56635 858621 257000) :recipe
           (:package "outshine" :fetcher github :repo "alphapapa/outshine"
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref
                     "bf1eed10dd7a89b63d0fc014944033db397c1e23"))
 (all-the-icons-dired :source "lockfile" :date (26045 56635 856185 282000)
                      :recipe
                      (:package "all-the-icons-dired" :repo
                                "wyuenho/all-the-icons-dired" :fetcher github
                                :files
                                ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                                 "doc/*.texinfo" "lisp/*.el"
                                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                                           "*-test.el" "*-tests.el" "LICENSE"
                                           "README*" "*-pkg.el"))
                                :source "MELPA" :protocol https :inherit t
                                :depth 1 :ref
                                "e157f0668f22ed586aebe0a2c0186ab07702986c"))
 (dired-open :source "lockfile" :date (26045 56635 853520 69000) :recipe
             (:package "dired-open" :fetcher github :repo "Fuco1/dired-hacks"
                       :files ("dired-open.el") :source "MELPA" :protocol https
                       :inherit t :depth 1 :ref
                       "874449d6fc98aee565e1715ec18acec3c1c2cafb"))
 (dired-hide-dotfiles :source "lockfile" :date (26045 56635 851911 272000)
                      :recipe
                      (:package "dired-hide-dotfiles" :fetcher github :repo
                                "mattiasb/dired-hide-dotfiles" :files
                                ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                                 "doc/*.texinfo" "lisp/*.el"
                                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                                           "*-test.el" "*-tests.el" "LICENSE"
                                           "README*" "*-pkg.el"))
                                :source "MELPA" :protocol https :inherit t
                                :depth 1 :ref
                                "6a379f23f64045f5950d229254ce6f32dbbf5364"))
 (dired-hist :source "lockfile" :date (26045 56635 849423 401000) :recipe
             (:source nil :protocol https :inherit t :depth 1 :host github :repo
                      "karthink/dired-hist" :package "dired-hist" :ref
                      "94b09260ac964e3d856c018d66af3214915dd826"))
 (consult-dir :source "lockfile" :date (26045 56635 846904 0) :recipe
              (:package "consult-dir" :fetcher github :repo
                        "karthink/consult-dir" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref
                        "3f5f4b71ebe819392cb090cda71bd39a93bd830a"))
 (affe :source "lockfile" :date (26045 56635 844569 696000) :recipe
       (:package "affe" :repo "minad/affe" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth 1 :ref
                 "1fd5732afa5d68b120fd2e949702b1abde0466d7"))
 (dwim-shell-command :source "lockfile" :date (26045 56635 841970 837000)
                     :recipe
                     (:package "dwim-shell-command" :fetcher github :repo
                               "xenodium/dwim-shell-command" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el"
                                (:exclude ".dir-locals.el" "test.el" "tests.el"
                                          "*-test.el" "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :protocol https :inherit t :depth
                               1 :ref "7a2c298424466d2bff7c050e01fb85b5f882dbc3"))
 (counsel-projectile :source "lockfile" :date (26045 56635 839561 412000)
                     :recipe
                     (:package "counsel-projectile" :fetcher github :repo
                               "ericdanan/counsel-projectile" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el"
                                (:exclude ".dir-locals.el" "test.el" "tests.el"
                                          "*-test.el" "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :protocol https :inherit t :depth
                               1 :ref "40d1e1d4bb70acb00fddd6f4df9778bf2c52734b"))
 (project :source "lockfile" :date (26045 56635 836772 358000) :recipe
          (:package "project" :repo "https://github.com/emacs-mirror/emacs"
                    :local-repo "project" :branch "master" :files
                    ("lisp/progmodes/project.el" (:exclude ".git")) :source
                    "GNU-devel ELPA" :protocol https :inherit t :depth 1 :ref
                    "a470dfb7f8a0f6d561b1f7c9665408d73b578e18"))
 (xref :source "lockfile" :date (26045 56635 833567 476000) :recipe
       (:package "xref" :repo "https://github.com/emacs-mirror/emacs"
                 :local-repo "xref" :branch "master" :files
                 ("lisp/progmodes/xref.el" (:exclude ".git")) :source
                 "GNU-devel ELPA" :protocol https :inherit t :depth 1 :ref
                 "a470dfb7f8a0f6d561b1f7c9665408d73b578e18"))
 (dumb-jump :source "lockfile" :date (26045 56635 830754 36000) :recipe
            (:package "dumb-jump" :repo "jacktasia/dumb-jump" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "ede6a04187e79a29ef31d14760ac0d8d4c5f4cc5"))
 (seq :source "lockfile" :date (26045 56635 828216 732000) :recipe
      (:package "seq" :repo "https://git.savannah.gnu.org/git/emacs/elpa.git"
                :local-repo "seq" :branch "externals/seq" :files
                ("*" (:exclude ".git")) :source "GNU-devel ELPA" :protocol https
                :inherit t :depth 1 :type git :host nil :ref
                "9d9f51b0e3ca59e0a488801064512f4878ac910b"))
 (magit :source "lockfile" :date (26045 56635 825593 237000) :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-rebase.el" "docs/magit.texi"
                   "docs/AUTHORS.md" "LICENSE" "Documentation/magit.texi"
                   "Documentation/AUTHORS.md"
                   (:exclude "lisp/magit-libgit.el" "lisp/magit-libgit-pkg.el"
                             "lisp/magit-section.el" "lisp/magit-section-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth 1 :ref
                  "54d37dc14c3f715dd0328a70bc65d63c54ee9613"))
 (magit-lfs :source "lockfile" :date (26045 56635 824429 952000) :recipe
            (:package "magit-lfs" :fetcher github :repo "Ailrun/magit-lfs"
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "cd9f46e1840270be27e2c2d9dcf036ff0781f66d"))
 (forge :source "lockfile" :date (26045 56635 821909 360000) :recipe
        (:package "forge" :fetcher github :repo "magit/forge" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                   "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth 1 :ref
                  "c50204185734f1ac0f480323ad542a4bf3348ed5"))
 (abridge-diff :source "lockfile" :date (26045 56635 818936 782000) :recipe
               (:package "abridge-diff" :repo "jdtsmith/abridge-diff" :fetcher
                         github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                                    "*-test.el" "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t :depth 1
                         :ref "31e0ccaa9d0bd4ad257f5de25cc3c0b3395fafa1"))
 (keychain-environment :source "lockfile" :date (26045 56635 816265 187000)
                       :recipe
                       (:package "keychain-environment" :repo
                                 "tarsius/keychain-environment" :fetcher github
                                 :files
                                 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                  "*.texinfo" "doc/dir" "doc/*.info"
                                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                  (:exclude ".dir-locals.el" "test.el"
                                            "tests.el" "*-test.el" "*-tests.el"
                                            "LICENSE" "README*" "*-pkg.el"))
                                 :source "MELPA" :protocol https :inherit t
                                 :depth 1 :ref
                                 "d3643196de6dc79ea77f9f4805028350fd76100b"))
 (git-gutter :source "lockfile" :date (26045 56635 813566 632000) :recipe
             (:package "git-gutter" :repo "emacsorphanage/git-gutter" :fetcher
                       github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref
                       "4cfcc01063b5ee69527c6a78e405d7988740ffa4"))
 (git-gutter-fringe :source "lockfile" :date (26045 56635 811540 975000) :recipe
                    (:package "git-gutter-fringe" :repo
                              "emacsorphanage/git-gutter-fringe" :fetcher github
                              :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                               "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el"
                               (:exclude ".dir-locals.el" "test.el" "tests.el"
                                         "*-test.el" "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "MELPA" :protocol https :inherit t :depth
                              1 :ref "648cb5b57faec55711803cdc9434e55a733c3eba"))
 (diff-hl :source "lockfile" :date (26045 56635 808766 768000) :recipe
          (:package "diff-hl" :fetcher github :repo "dgutov/diff-hl" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref
                    "b8b2727a72fdf64ac98e6cfa136a43cb0cacf72f"))
 (git-timemachine :source "lockfile" :date (26045 56635 806102 908000) :recipe
                  (:package "git-timemachine" :fetcher codeberg :repo
                            "pidu/git-timemachine" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                             "doc/*.texinfo" "lisp/*.el"
                             (:exclude ".dir-locals.el" "test.el" "tests.el"
                                       "*-test.el" "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t :depth 1
                            :ref "ac933e5cd29583c131401f3bd991d98129c316df"))
 (deadgrep :source "lockfile" :date (26045 56635 803292 844000) :recipe
           (:package "deadgrep" :repo "Wilfred/deadgrep" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref
                     "38abe362997d2f18633a75d04c09da751bf8085e"))
 (flycheck :source "lockfile" :date (26045 56635 800212 154000) :recipe
           (:package "flycheck" :repo "flycheck/flycheck" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref
                     "e56e30d8c66ffc9776d07740658d3b542c1a8e21"))
 (consult-flycheck :source "lockfile" :date (26045 56635 797683 477000) :recipe
                   (:package "consult-flycheck" :fetcher github :repo
                             "minad/consult-flycheck" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                              "doc/*.texinfo" "lisp/*.el"
                              (:exclude ".dir-locals.el" "test.el" "tests.el"
                                        "*-test.el" "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit t :depth 1
                             :ref "d83f87581af74f7a2739d8b1b90c37da5ae3d310"))
 (flymake :source "lockfile" :date (26045 56635 795048 230000) :recipe
          (:package "flymake" :repo "https://github.com/emacs-mirror/emacs"
                    :local-repo "flymake" :branch "master" :files
                    ("lisp/progmodes/flymake.el" (:exclude ".git")) :source
                    "GNU-devel ELPA" :protocol https :inherit t :depth 1 :ref
                    "a470dfb7f8a0f6d561b1f7c9665408d73b578e18"))
 (flymake-collection :source "lockfile" :date (26045 56635 792177 933000)
                     :recipe
                     (:package "flymake-collection" :fetcher github :repo
                               "mohkale/flymake-collection" :files
                               (:defaults "src/*.el" "src/checkers/*.el")
                               :old-names (flymake-rest) :source "MELPA"
                               :protocol https :inherit t :depth 1 :ref
                               "852d47f7b4cac7345d40bd16067842e095aee13b"))
 (flymake-flycheck :source "lockfile" :date (26045 56635 789504 135000) :recipe
                   (:package "flymake-flycheck" :fetcher github :repo
                             "purcell/flymake-flycheck" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                              "doc/*.texinfo" "lisp/*.el"
                              (:exclude ".dir-locals.el" "test.el" "tests.el"
                                        "*-test.el" "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit t :depth 1
                             :ref "6c13e1c6ff6790222facf37439cafcacba513322"))
 (package-lint-flymake :source "lockfile" :date (26045 56635 786610 184000)
                       :recipe
                       (:package "package-lint-flymake" :fetcher github :repo
                                 "purcell/package-lint" :files
                                 ("package-lint-flymake.el") :source "MELPA"
                                 :protocol https :inherit t :depth 1 :ref
                                 "a6a74293dbc9c69c568c79c509d2aa3b67bc6769"))
 (breadcrumb :source "lockfile" :date (26045 56635 785030 531000) :recipe
             (:package "breadcrumb" :repo
                       "https://github.com/joaotavora/breadcrumb" :local-repo
                       "breadcrumb" :files ("*" (:exclude ".git")) :source
                       "GNU-devel ELPA" :protocol https :inherit t :depth 1 :ref
                       "dcb6e2e82de2432d8eb75be74c8d6215fc97a2d3"))
 (apheleia :source "lockfile" :date (26045 56635 782310 104000) :recipe
           (:package "apheleia" :fetcher github :repo "radian-software/apheleia"
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref
                     "96a9805ecb75aac2adde7568d26b3e3b3ffc19af"))
 (devdocs-browser :source "lockfile" :date (26045 56635 779542 931000) :recipe
                  (:package "devdocs-browser" :fetcher github :repo
                            "blahgeek/emacs-devdocs-browser" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                             "doc/*.texinfo" "lisp/*.el"
                             (:exclude ".dir-locals.el" "test.el" "tests.el"
                                       "*-test.el" "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t :depth 1
                            :ref "afc460e687bec4eb947ab85d207778fc3b9b3bbc"))
 (dash-docs :source "lockfile" :date (26045 56635 777028 409000) :recipe
            (:package "dash-docs" :repo "dash-docs-el/dash-docs" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "29848b6b347ac520f7646c200ed2ec36cea3feda"))
 (treesit-auto :source "lockfile" :date (26045 56635 774661 545000) :recipe
               (:package "treesit-auto" :fetcher github :repo
                         "renzmann/treesit-auto" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                                    "*-test.el" "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t :depth 1
                         :ref "16ac5316a80954f2633f0a2fb1d87fef108278ad"))
 (eglot :source "lockfile" :date (26045 56635 772545 248000) :recipe
        (:package "eglot" :repo "https://github.com/emacs-mirror/emacs"
                  :local-repo "eglot" :branch "master" :files
                  ("lisp/progmodes/eglot.el" "doc/emacs/doclicense.texi"
                   "doc/emacs/docstyle.texi" "doc/misc/eglot.texi"
                   "etc/EGLOT-NEWS" (:exclude ".git"))
                  :source "GNU-devel ELPA" :protocol https :inherit t :depth 1
                  :ref "c5a4366b3f3c6ee4178d954e58eb226441d1d2ee"))
 (eglot-booster :source "lockfile" :date (26045 56635 771210 32000) :recipe
                (:source nil :protocol https :inherit t :depth 1 :type git :host
                         github :repo "jdtsmith/eglot-booster" :package
                         "eglot-booster" :ref
                         "bb05a99385990d738cee4c321cbac6bb13078227"))
 (eglot-java :source "lockfile" :date (26045 56635 768370 172000) :recipe
             (:package "eglot-java" :repo "yveszoundi/eglot-java" :fetcher
                       github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref
                       "d533ea5f0b2cb314c6ce0de7614cb120c5e64edf"))
 (consult-eglot :source "lockfile" :date (26045 56635 765700 982000) :recipe
                (:package "consult-eglot" :fetcher github :repo
                          "mohkale/consult-eglot" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth 1
                          :ref "049c6319b8a48ff66189d49592c7759f0b356596"))
 (lsp-mode :source "lockfile" :date (26045 56635 763185 258000) :recipe
           (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher github
                     :files (:defaults "clients/*.el") :source "MELPA" :protocol
                     https :inherit t :depth 1 :ref
                     "820dc172a5218d72d13cd0a48e846fdb52a78e9a"))
 (lsp-ui :source "lockfile" :date (26045 56635 760590 767000) :recipe
         (:package "lsp-ui" :repo "emacs-lsp/lsp-ui" :fetcher github :files
                   (:defaults "lsp-ui-doc.html" "resources") :source "MELPA"
                   :protocol https :inherit t :depth 1 :ref
                   "bc58c6664577d1d79060c6b32b7ad20e70ee19d0"))
 (consult-lsp :source "lockfile" :date (26045 56635 758252 85000) :recipe
              (:package "consult-lsp" :fetcher github :repo "gagbo/consult-lsp"
                        :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref
                        "f8db3252c0daa41225ba4ed1c0d178b281cd3e90"))
 (lsp-treemacs :source "lockfile" :date (26045 56635 755759 485000) :recipe
               (:package "lsp-treemacs" :repo "emacs-lsp/lsp-treemacs" :fetcher
                         github :files (:defaults "icons") :source "MELPA"
                         :protocol https :inherit t :depth 1 :ref
                         "e54e74deb8150964e3c3024e1ec14295a34e2a3b"))
 (lsp-bridge :source "lockfile" :date (26045 56635 753013 531000) :recipe
             (:source nil :protocol https :inherit t :depth 1 :type git :host
                      github :repo "manateelazycat/lsp-bridge" :files
                      (:defaults "*.py" "langserver" "acm") :package
                      "lsp-bridge" :ref
                      "5baf390b5fb2a7d821ba73cf401c6c8ec419cb4c"))
 (realgud :source "lockfile" :date (26045 56635 750529 507000) :recipe
          (:package "realgud" :fetcher github :repo "realgud/realgud" :files
                    ("realgud.el" "realgud/.nosearch"
                     "realgud-recursive-autoloads.el"
                     ("realgud/common" "realgud/common/*.el")
                     ("realgud/common/buffer" "realgud/common/buffer/*.el")
                     ("realgud/debugger/bashdb" "realgud/debugger/bashdb/*.el")
                     ("realgud/debugger/gdb" "realgud/debugger/gdb/*.el")
                     ("realgud/debugger/gub" "realgud/debugger/gub/*.el")
                     ("realgud/debugger/ipdb" "realgud/debugger/ipdb/*.el")
                     ("realgud/debugger/jdb" "realgud/debugger/jdb/*.el")
                     ("realgud/debugger/kshdb" "realgud/debugger/kshdb/*.el")
                     ("realgud/debugger/nodejs" "realgud/debugger/nodejs/*.el")
                     ("realgud/debugger/pdb" "realgud/debugger/pdb/*.el")
                     ("realgud/debugger/perldb" "realgud/debugger/perldb/*.el")
                     ("realgud/debugger/rdebug" "realgud/debugger/rdebug/*.el")
                     ("realgud/debugger/remake" "realgud/debugger/remake/*.el")
                     ("realgud/debugger/trepan" "realgud/debugger/trepan/*.el")
                     ("realgud/debugger/trepan.pl"
                      "realgud/debugger/trepan.pl/*.el")
                     ("realgud/debugger/trepan2" "realgud/debugger/trepan2/*.el")
                     ("realgud/debugger/trepan3k"
                      "realgud/debugger/trepan3k/*.el")
                     ("realgud/debugger/trepanjs"
                      "realgud/debugger/trepanjs/*.el")
                     ("realgud/debugger/zshdb" "realgud/debugger/zshdb/*.el")
                     ("realgud/lang" "realgud/lang/*.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref
                    "365063ea8ce8ec6a852cb388088d84147421c3c2"))
 (dap-mode :source "lockfile" :date (26045 56635 748003 674000) :recipe
           (:package "dap-mode" :repo "emacs-lsp/dap-mode" :fetcher github
                     :files (:defaults "icons") :source "MELPA" :protocol https
                     :inherit t :depth 1 :ref
                     "2f0c5b28578ce65ec746e4084ba72ba5c652ea79"))
 (dape :source "lockfile" :date (26045 56635 745210 282000) :recipe
       (:source "Init file" :protocol https :inherit t :depth 1 :type git :host
                github :repo "svaante/dape" :package "dape" :ref
                "b7fce202149b6cbbab78818d5448d6647a6b7e5e"))
 (eros :source "lockfile" :date (26045 56635 742207 177000) :recipe
       (:package "eros" :fetcher github :repo "xiongtx/eros" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth 1 :ref
                 "a9a92bdc6be0521a6a06eb464be55ed61946639c"))
 (rainbow-delimiters :source "lockfile" :date (26045 56635 739657 270000)
                     :recipe
                     (:package "rainbow-delimiters" :fetcher github :repo
                               "Fanael/rainbow-delimiters" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el"
                                (:exclude ".dir-locals.el" "test.el" "tests.el"
                                          "*-test.el" "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :protocol https :inherit t :depth
                               1 :ref "f40ece58df8b2f0fb6c8576b527755a552a5e763"))
 (help-find :source "lockfile" :date (26045 56635 736970 877000) :recipe
            (:package "help-find" :fetcher github :repo "duncanburke/help-find"
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "ef7266fc480367c12bff64817c875af940d0c9c0"))
 (elisp-demos :source "lockfile" :date (26045 56635 734361 960000) :recipe
              (:package "elisp-demos" :fetcher github :repo
                        "xuchunyang/elisp-demos" :files (:defaults "*.org")
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref
                        "1a108d1c5011f9ced58be2ca98bea1fbd4130a2f"))
 (suggest :source "lockfile" :date (26045 56635 731629 140000) :recipe
          (:package "suggest" :repo "Wilfred/suggest.el" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref
                    "999ab73a3e546bd8d30f0de2408c511abe43135a"))
 (ssh-config-mode :source "lockfile" :date (26045 56635 729254 711000) :recipe
                  (:package "ssh-config-mode" :fetcher github :repo
                            "jhgorrell/ssh-config-mode-el" :files
                            (:defaults "*.txt") :source "MELPA" :protocol https
                            :inherit t :depth 1 :ref
                            "d560a0876a93ad4130baf33dae1b9405ad37a405"))
 (lua-mode :source "lockfile" :date (26045 56635 726855 686000) :recipe
           (:package "lua-mode" :repo "immerrr/lua-mode" :fetcher github :files
                     (:defaults (:exclude "init-tryout.el")) :source "MELPA"
                     :protocol https :inherit t :depth 1 :ref
                     "d074e4134b1beae9ed4c9b512af741ca0d852ba3"))
 (company-lua :source "lockfile" :date (26045 56635 724394 895000) :recipe
              (:package "company-lua" :fetcher github :repo "ptrv/company-lua"
                        :files (:defaults "lua") :source "MELPA" :protocol https
                        :inherit t :depth 1 :ref
                        "29f6819de4d691e5fd0b62893a9f4fbc1c6fcb52"))
 (web-mode :source "lockfile" :date (26045 56635 722040 824000) :recipe
           (:package "web-mode" :repo "fxbois/web-mode" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref
                     "82847071ce93293bdb7945db08d970f13fd883cf"))
 (js2-mode :source "lockfile" :date (26045 56635 719298 677000) :recipe
           (:package "js2-mode" :repo "mooz/js2-mode" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref
                     "ca0af32eea0281322a9ce268d98f01fbb88bbb7a"))
 (json-mode :source "lockfile" :date (26045 56635 716753 989000) :recipe
            (:package "json-mode" :fetcher github :repo "json-emacs/json-mode"
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "a93a0c76784376fbb9105719f25c7489991056a1"))
 (yaml-mode :source "lockfile" :date (26045 56635 714147 135000) :recipe
            (:package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "5b58248ab255dff6cfa4c4057a191bc4446ee5b6"))
 (python :source "lockfile" :date (26045 56635 711886 109000) :recipe
         (:package "python" :repo "https://github.com/emacs-mirror/emacs"
                   :local-repo "python" :branch "master" :files
                   ("lisp/progmodes/python.el" (:exclude ".git")) :source
                   "GNU-devel ELPA" :protocol https :inherit t :depth 1 :ref
                   "a470dfb7f8a0f6d561b1f7c9665408d73b578e18"))
 (lsp-pyright :source "lockfile" :date (26045 56635 709279 856000) :recipe
              (:package "lsp-pyright" :repo "emacs-lsp/lsp-pyright" :fetcher
                        github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref
                        "2f2631ae242d5770dbe6cb924e44c1ee5671789d"))
 (python-pytest :source "lockfile" :date (26045 56635 706736 661000) :recipe
                (:package "python-pytest" :fetcher github :repo
                          "wbolster/emacs-python-pytest" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth 1
                          :ref "bdfb3e81eedc6b76ed0c5f77079e7cc8adff7b00"))
 (lsp-java :source "lockfile" :date (26045 56635 704229 994000) :recipe
           (:package "lsp-java" :repo "emacs-lsp/lsp-java" :fetcher github
                     :files (:defaults "icons") :source "MELPA" :protocol https
                     :inherit t :depth 1 :ref
                     "c962a3b3ac2beabdf1ce83b815396d6c38e3cefa"))
 (helm-lsp :source "lockfile" :date (26045 56635 701432 755000) :recipe
           (:package "helm-lsp" :repo "emacs-lsp/helm-lsp" :fetcher github
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref
                     "c2c6974dadfac459b1a69a1217441283874cea92"))
 (gdb-bp-session :source "lockfile" :date (26045 56635 698725 804000) :recipe
                 (:source nil :protocol https :inherit t :depth 1 :type git
                          :host github :repo "emacsmirror/gdb-bp-session"
                          :package "gdb-bp-session" :ref
                          "5c566b00407dd527bb8f95634aec11e490ea5b75"))
 (rustic :source "lockfile" :date (26045 56635 696387 61000) :recipe
         (:package "rustic" :repo "brotzeit/rustic" :fetcher github :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth 1 :ref
                   "39423d1cf4fa054c36bf9577356451f4c06ee148"))
 (haskell-mode :source "lockfile" :date (26045 56635 693997 13000) :recipe
               (:package "haskell-mode" :repo "haskell/haskell-mode" :fetcher
                         github :files (:defaults "NEWS" "logo.svg") :source
                         "MELPA" :protocol https :inherit t :depth 1 :ref
                         "43b4036bf02b02de75643a1a2a31e28efac1c50b"))
 (lsp-haskell :source "lockfile" :date (26045 56635 691588 330000) :recipe
              (:package "lsp-haskell" :repo "emacs-lsp/lsp-haskell" :fetcher
                        github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref
                        "89d16370434e9a247e95b8b701f524f5abfc884b"))
 (topsy :source "lockfile" :date (26045 56635 688937 423000) :recipe
        (:package "topsy" :fetcher github :repo "alphapapa/topsy.el" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                   "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth 1 :ref
                  "8b6c6d5026ac72b4c3704ed7bb8fafe1ea343699"))
 (fish-mode :source "lockfile" :date (26045 56635 686256 250000) :recipe
            (:package "fish-mode" :fetcher github :repo "wwwjfy/emacs-fish"
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "2526b1803b58cf145bc70ff6ce2adb3f6c246f89"))
 (shrink-path :source "lockfile" :date (26045 56635 683413 506000) :recipe
              (:package "shrink-path" :fetcher gitlab :repo
                        "bennya/shrink-path.el" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref
                        "c14882c8599aec79a6e8ef2d06454254bb3e1e41"))
 (eshell-syntax-highlighting :source "lockfile" :date
                             (26045 56635 681150 254000) :recipe
                             (:package "eshell-syntax-highlighting" :fetcher
                                       github :repo
                                       "akreisher/eshell-syntax-highlighting"
                                       :files
                                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                        "*.texinfo" "doc/dir" "doc/*.info"
                                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                        (:exclude ".dir-locals.el" "test.el"
                                                  "tests.el" "*-test.el"
                                                  "*-tests.el" "LICENSE"
                                                  "README*" "*-pkg.el"))
                                       :source "MELPA" :protocol https :inherit
                                       t :depth 1 :ref
                                       "4ac27eec6595ba116a6151dfaf0b0e0440101e10"))
 (pcmpl-args :source "lockfile" :date (26045 56635 678533 893000) :recipe
             (:package "pcmpl-args" :fetcher github :repo
                       "JonWaltman/pcmpl-args.el" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref
                       "43229e1096f89c277190f09a3d794781f8fa0015"))
 (vterm :source "lockfile" :date (26045 56635 676090 454000) :recipe
        (:package "vterm" :fetcher github :repo "akermu/emacs-libvterm" :files
                  ("CMakeLists.txt" "elisp.c" "elisp.h" "emacs-module.h" "etc"
                   "utf8.c" "utf8.h" "vterm.el" "vterm-module.c"
                   "vterm-module.h")
                  :source "MELPA" :protocol https :inherit t :depth 1 :ref
                  "c3a3a23a5eace137947524c93644204bf6b56cff"))
 (notmuch-indicator :source "lockfile" :date (26045 56635 673687 341000) :recipe
                    (:package "notmuch-indicator" :repo
                              "https://git.sr.ht/~protesilaos/notmuch-indicator"
                              :local-repo "notmuch-indicator" :files
                              ("*" (:exclude ".git" "COPYING" "doclicense.texi"))
                              :source "GNU-devel ELPA" :protocol https :inherit
                              t :depth 1 :ref
                              "72abc677a01008010c3d1ceafed824be253bd7e7"))
 (mu4e-column-faces :source "lockfile" :date (26045 56635 671052 445000) :recipe
                    (:package "mu4e-column-faces" :repo
                              "Alexander-Miller/mu4e-column-faces" :fetcher
                              github :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                               "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el"
                               (:exclude ".dir-locals.el" "test.el" "tests.el"
                                         "*-test.el" "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "MELPA" :protocol https :inherit t :depth
                              1 :ref "1bbb646ea07deb1bd2daa4c6eb36e0f65aac40b0"))
 (org-msg :source "lockfile" :date (26045 56635 668222 544000) :recipe
          (:package "org-msg" :repo "jeremy-compostella/org-msg" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :type
                    git :host github :ref
                    "0b65f0f77a7a71881ddfce19a8cdc60465bda057"))
 (mu4e-send-delay :source "lockfile" :date (26045 56635 665681 193000) :recipe
                  (:source nil :protocol ssh :inherit t :depth nil :type git
                           :host github :repo "krisbalintona/mu4e-send-delay"
                           :package "mu4e-send-delay" :ref
                           "e09019480a946d617f452f13460229552c364300"))
 (elfeed :source "lockfile" :date (26045 56635 663161 633000) :recipe
         (:package "elfeed" :repo "skeeto/elfeed" :fetcher github :files
                   (:defaults "README.md") :source "MELPA" :protocol https
                   :inherit t :depth 1 :ref
                   "55fb162fa27e71b88effa59a83c57842e262b00f"))
 (elfeed-org :source "lockfile" :date (26045 56635 660519 393000) :recipe
             (:package "elfeed-org" :repo "remyhonig/elfeed-org" :fetcher github
                       :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref
                       "d62d23e25c5e3be3d70b7fbe1eaeb6e43f93a061"))
 (popwin :source "lockfile" :date (26045 56635 658095 170000) :recipe
         (:package "popwin" :fetcher github :repo "emacsorphanage/popwin" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth 1 :ref
                   "f4bf2e4cbda328359b06d89e233c951cba30363e"))
 (elfeed-goodies :source "lockfile" :date (26045 56635 656072 989000) :recipe
                 (:package "elfeed-goodies" :repo "jeetelongname/elfeed-goodies"
                           :fetcher github :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                            "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                            "lisp/*.el"
                            (:exclude ".dir-locals.el" "test.el" "tests.el"
                                      "*-test.el" "*-tests.el" "LICENSE"
                                      "README*" "*-pkg.el"))
                           :source "MELPA" :protocol https :inherit t :depth 1
                           :ref "544ef42ead011d960a0ad1c1d34df5d222461a6b"))
 (language-detection :source "lockfile" :date (26045 56635 653282 682000)
                     :recipe
                     (:package "language-detection" :fetcher github :repo
                               "andreasjansson/language-detection.el" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el"
                                (:exclude ".dir-locals.el" "test.el" "tests.el"
                                          "*-test.el" "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :protocol https :inherit t :depth
                               1 :ref "54a6ecf55304fba7d215ef38a4ec96daff2f35a4"))
 (wombag :source "lockfile" :date (26045 56635 650289 236000) :recipe
         (:source nil :protocol https :inherit t :depth 1 :host github :repo
                  "karthink/wombag" :package "wombag" :ref
                  "4e6251f8cc7b02e2fec595ca66380f224d5c0663"))
 (nov :source "lockfile" :date (26045 56635 647707 199000) :recipe
      (:package "nov" :fetcher git :url "https://depp.brause.cc/nov.el.git"
                :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :source "MELPA" :protocol https :inherit t :depth 1 :ref
                "cc31ce0356226c3a2128119b08de6107e38fdd17"))
 (justify-kp :source "lockfile" :date (26045 56635 645303 84000) :recipe
             (:source nil :protocol https :inherit t :depth 1 :type git :host
                      github :repo "Fuco1/justify-kp" :package "justify-kp" :ref
                      "33a186e297c0359547820088669486afd7b5fddb"))
 (plz :source "lockfile" :date (26045 56635 642969 80000) :recipe
      (:package "plz" :repo "alphapapa/plz.el" :local-repo "plz" :files
                ("*" (:exclude ".git" "LICENSE")) :source "GNU-devel ELPA"
                :protocol https :inherit t :depth 1 :type git :host github :ref
                "f402bcc93446fe1629dd2e64d7e147e22fe034e8"))
 (ement :source "lockfile" :date (26045 56635 640342 319000) :recipe
        (:package "ement" :repo "alphapapa/ement.el" :local-repo "ement" :files
                  ("*" (:exclude ".git")) :source "GNU-devel ELPA" :protocol
                  https :inherit t :depth 1 :type git :host github :ref
                  "5ef7417ee5e8d4e691a362282bce4172fce64dea"))
 (ledger-mode :source "lockfile" :date (26045 56635 637924 789000) :recipe
              (:package "ledger-mode" :fetcher github :repo "ledger/ledger-mode"
                        :files ("ledger-*.el" "doc/*.texi") :old-names
                        (ldg-mode) :source "MELPA" :protocol https :inherit t
                        :depth 1 :ref "11e748d4838d51772f531a75849349ed8cd939ed"))
 (flycheck-ledger :source "lockfile" :date (26045 56635 635176 171000) :recipe
                  (:package "flycheck-ledger" :fetcher github :repo
                            "purcell/flycheck-ledger" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                             "doc/*.texinfo" "lisp/*.el"
                             (:exclude ".dir-locals.el" "test.el" "tests.el"
                                       "*-test.el" "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t :depth 1
                            :ref "628e25ba66604946085571652a94a54f4d1ad96f"))
 (yasnippet :source "lockfile" :date (26045 56635 632747 79000) :recipe
            (:package "yasnippet" :repo "joaotavora/yasnippet" :fetcher github
                      :files ("yasnippet.el" "snippets") :source "MELPA"
                      :protocol https :inherit t :depth 1 :ref
                      "297546f0853a6a51f5b05e954d0c6aea8caa5ec2"))
 (consult-yasnippet :source "lockfile" :date (26045 56635 630662 532000) :recipe
                    (:package "consult-yasnippet" :fetcher github :repo
                              "mohkale/consult-yasnippet" :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                               "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el"
                               (:exclude ".dir-locals.el" "test.el" "tests.el"
                                         "*-test.el" "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "MELPA" :protocol https :inherit t :depth
                              1 :ref "ae0450889484f23dc4ec37518852a2c61b89f184"))
 (scratch :source "lockfile" :date (26045 56635 628235 855000) :recipe
          (:package "scratch" :fetcher codeberg :repo "emacs-weirdware/scratch"
                    :files ("scratch.el") :source "MELPA" :protocol https
                    :inherit t :depth 1 :ref
                    "f000648c9663833a76a8de9b1e78c99a9d698e48"))
 (tmr :source "lockfile" :date (26045 56635 625801 664000) :recipe
      (:package "tmr" :repo "protesilaos/tmr.el" :local-repo "tmr" :files
                ("*" (:exclude ".git" "COPYING" "doclicense.texi" "Makefile"))
                :source "GNU-devel ELPA" :protocol https :inherit t :depth 1
                :type git :host gitlab :ref
                "3321b498573e87fe60ab8fda4d62244758dfb8d0"))
 (all-the-icons-completion :source "lockfile" :date (26045 56635 623862 368000)
                           :recipe
                           (:package "all-the-icons-completion" :repo
                                     "iyefrat/all-the-icons-completion" :fetcher
                                     github :files
                                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                      "*.texinfo" "doc/dir" "doc/*.info"
                                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                      (:exclude ".dir-locals.el" "test.el"
                                                "tests.el" "*-test.el"
                                                "*-tests.el" "LICENSE" "README*"
                                                "*-pkg.el"))
                                     :source "MELPA" :protocol https :inherit t
                                     :depth 1 :ref
                                     "4c8bcad8033f5d0868ce82ea3807c6cd46c4a198"))
 (vc-msg :source "lockfile" :date (26045 56635 621142 82000) :recipe
         (:package "vc-msg" :fetcher github :repo "redguardtoo/vc-msg" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth 1 :ref
                   "5b0f27307402442b062448d548c29da02ba92295"))
 (alt-comment-dwim :source "lockfile" :date (26045 56635 618551 679000) :recipe
                   (:source nil :protocol ssh :inherit t :depth nil :type git
                            :host gitlab :repo
                            "PreciousPudding/alt-comment-dwim" :package
                            "alt-comment-dwim" :ref
                            "0a1e1e298d3b2dd746c4da130cb500d9e66b7b9d"))
 (info-variable-pitch :source "lockfile" :date (26045 56635 616048 599000)
                      :recipe
                      (:source nil :protocol https :inherit t :depth 1 :type git
                               :host github :repo
                               "kisaragi-hiu/info-variable-pitch" :package
                               "info-variable-pitch" :ref
                               "e18e8dfb5dbea304fcf2312eb6cc8a0736e6eda0"))
 (info-colors :source "lockfile" :date (26045 56635 613474 16000) :recipe
              (:package "info-colors" :fetcher github :repo
                        "ubolonton/info-colors" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref
                        "2e237c301ba62f0e0286a27c1abe48c4c8441143"))
 (inform :source "lockfile" :date (26045 56635 610898 951000) :recipe
         (:package "inform" :fetcher github :repo "dieter-wilhelm/inform" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth 1 :ref
                   "8ff0a19a9f40cfa8283da8ed73de94c35a327423"))
 (eldoc :source "lockfile" :date (26045 56635 608374 752000) :recipe
        (:package "eldoc" :repo "https://github.com/emacs-mirror/emacs"
                  :local-repo "eldoc" :branch "master" :files
                  ("lisp/emacs-lisp/eldoc.el" (:exclude ".git")) :source
                  "GNU-devel ELPA" :protocol https :inherit t :depth 1 :ref
                  "a470dfb7f8a0f6d561b1f7c9665408d73b578e18"))
 (pulsar :source "lockfile" :date (26045 56635 605802 944000) :recipe
         (:package "pulsar" :repo "https://git.sr.ht/~protesilaos/pulsar"
                   :local-repo "pulsar" :files
                   ("*" (:exclude ".git" "COPYING" "doclicense.texi")) :source
                   "GNU-devel ELPA" :protocol https :inherit t :depth 1 :ref
                   "1ca4941f2cf68771c0fd9561e1631d2f701f6b31"))
 (pocket-reader :source "lockfile" :date (26045 56635 603342 784000) :recipe
                (:package "pocket-reader" :fetcher github :repo
                          "alphapapa/pocket-reader.el" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth 1
                          :type git :host github :ref
                          "ef6b6892ef13eff3479d79c7f6bc918dd0444e88"))
 (fancy-compilation :source "lockfile" :date (26045 56635 600792 797000) :recipe
                    (:package "fancy-compilation" :fetcher codeberg :repo
                              "ideasman42/emacs-fancy-compilation" :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                               "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el"
                               (:exclude ".dir-locals.el" "test.el" "tests.el"
                                         "*-test.el" "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "MELPA" :protocol https :inherit t :depth
                              1 :ref "74833d618024cee47c24aabdc6e9daa4009d0690"))
 (image-popup :source "lockfile" :date (26045 56635 598295 328000) :recipe
              (:source nil :protocol https :inherit t :depth 1 :type git :host
                       gitlab :repo "OlMon/image-popup" :branch "master"
                       :package "image-popup" :ref
                       "8d8e86d1ac08738ad744af6283abd48db1d6858d"))
 (form-feed :source "lockfile" :date (26045 56635 595765 628000) :recipe
            (:package "form-feed" :fetcher git :url
                      "https://depp.brause.cc/form-feed.git" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "ac1f0ef30a11979f5dfe12d8c05a666739e486ff"))
 (engine-mode :source "lockfile" :date (26045 56635 593957 938000) :recipe
              (:package "engine-mode" :repo "hrs/engine-mode" :fetcher github
                        :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref
                        "19fbf8e21df8f45083a904fdba2fac85b3b02dd0"))
 (pcre2el :source "lockfile" :date (26045 56635 591503 219000) :recipe
          (:package "pcre2el" :fetcher github :repo "joddie/pcre2el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref
                    "018531ba0cf8e2b28d1108136a0e031b6a45f1c1"))
 (sentex :source "lockfile" :date (26045 56635 588907 165000) :recipe
         (:package "sentex" :fetcher codeberg :repo "martianh/sentex" :files
                   ("*") :source "MELPA" :protocol https :inherit t :depth 1
                   :type git :host codeberg :ref
                   "b08c48bfab391a259d93e182b9115b2aba275d82"))
 (recursion-indicator :source "lockfile" :date (26045 56635 585643 293000)
                      :recipe
                      (:package "recursion-indicator" :repo
                                "minad/recursion-indicator" :fetcher github
                                :files
                                ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                                 "doc/*.texinfo" "lisp/*.el"
                                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                                           "*-test.el" "*-tests.el" "LICENSE"
                                           "README*" "*-pkg.el"))
                                :source "MELPA" :protocol https :inherit t
                                :depth 1 :ref
                                "548838df2ef15fdd8e9d904d0a74182297e3383f"))
 (lorem-ipsum :source "lockfile" :date (26045 56635 582895 596000) :recipe
              (:package "lorem-ipsum" :fetcher github :repo
                        "jschaf/emacs-lorem-ipsum" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref
                        "4e87a899868e908a7a9e1812831d76c8d072f885"))
 (writeroom-mode :source "lockfile" :date (26045 56635 580218 441000) :recipe
                 (:package "writeroom-mode" :fetcher github :repo
                           "joostkremers/writeroom-mode" :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                            "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                            "lisp/*.el"
                            (:exclude ".dir-locals.el" "test.el" "tests.el"
                                      "*-test.el" "*-tests.el" "LICENSE"
                                      "README*" "*-pkg.el"))
                           :source "MELPA" :protocol https :inherit t :depth 1
                           :ref "f4d035e91d20bf1dd3f2857b9cc344f844979a78"))
 (clipetty :source "lockfile" :date (26045 56635 577276 350000) :recipe
           (:package "clipetty" :repo "spudlyo/clipetty" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref
                     "01b39044b9b65fa4ea7d3166f8b1ffab6f740362"))
 (goto-chg :source "lockfile" :date (26045 56635 574880 641000) :recipe
           (:package "goto-chg" :repo "emacs-evil/goto-chg" :fetcher github
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref
                     "278cd3e6d5107693aa2bb33189ca503f22f227d0"))
 (fontify-patch :source "lockfile" :date (26045 56635 572275 480000) :recipe
                (:source nil :protocol https :inherit t :depth 1 :type git :host
                         github :repo "whame/fontify-patch" :package
                         "fontify-patch" :ref
                         "9c13c4109505c84bbab3ca6ee8ff0fb391ed6dec"))
 (smog :source "lockfile" :date (26045 56635 569365 500000) :recipe
       (:package "smog" :repo "zzkt/smog" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth 1 :ref
                 "2fc5fef0f5000027b3550495259a65966c68ec52"))
 (reverso :source "lockfile" :date (26045 56635 566516 93000) :recipe
          (:source nil :protocol https :inherit t :depth 1 :host github :repo
                   "SqrtMinusOne/reverso.el" :package "reverso" :ref
                   "c4658784f024850ef54d9d0908e28089fef3a849"))
 (frame-local :source "lockfile" :date (26045 56635 564442 506000) :recipe
              (:package "frame-local" :fetcher github :repo
                        "sebastiencs/frame-local" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref
                        "7ee1106c3bcd4022f48421f8cb1ef4f995da816e"))
 (prescient :source "lockfile" :date (26045 56635 562032 800000) :recipe
            (:package "prescient" :fetcher github :repo
                      "radian-software/prescient.el" :files ("prescient.el")
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "4b875be52e75f7b81e68a16b62cfbb2f2584042c"))
 (swiper :source "lockfile" :date (26045 56635 559479 396000) :recipe
         (:package "swiper" :repo "abo-abo/swiper" :fetcher github :files
                   ("swiper.el") :source "MELPA" :protocol https :inherit t
                   :depth 1 :ref "8c30f4cab5948aa8d942a3b2bbf5fb6a94d9441d"))
 (helm-core :source "lockfile" :date (26045 56635 556626 282000) :recipe
            (:package "helm-core" :repo "emacs-helm/helm" :fetcher github :files
                      ("helm-core.el" "helm-lib.el" "helm-source.el"
                       "helm-multi-match.el")
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "f817c7920dcfcaa24a258917ff9b7a3a4023fd91"))
 (wfnames :source "lockfile" :date (26045 56635 553644 788000) :recipe
          (:package "wfnames" :fetcher github :repo "thierryvolpiatto/wfnames"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref
                    "19b452fb698a5ba3b0f1d6e7d69a5e19af2c83e7"))
 (popup :source "lockfile" :date (26045 56635 551066 167000) :recipe
        (:package "popup" :fetcher github :repo "auto-complete/popup-el" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                   "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth 1 :ref
                  "4d6f6c22a5abf130fe8359171cb9d6b00dc41c0b"))
 (async :source "lockfile" :date (26045 56635 548590 428000) :recipe
        (:package "async" :repo "jwiegley/emacs-async" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                   "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth 1 :ref
                  "d040f72cb0be5265d50ac541ddb09ebbc68b7908"))
 (grammarly :source "lockfile" :date (26045 56635 546108 929000) :recipe
            (:package "grammarly" :repo "emacs-grammarly/grammarly" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "45fb69f48b3fdcfcee990410cdd860f257cf4f54"))
 (request :source "lockfile"
   :date (26045 56635 543654 680000) :recipe
   (:package "request" :repo "tkf/emacs-request" :fetcher github :files
             ("request.el") :source "MELPA" :protocol https :inherit t :depth 1
             :ref "01e338c335c07e4407239619e57361944a82cb8a"))
 (s :source "lockfile" :date (26045 56635 540994 376000) :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                         "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth 1 :ref
              "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (spinner :source "lockfile" :date (26045 56635 538607 745000) :recipe
          (:package "spinner" :repo "https://github.com/Malabarba/spinner.el"
                    :local-repo "spinner" :files ("*" (:exclude ".git")) :source
                    "GNU-devel ELPA" :protocol https :inherit t :depth 1 :ref
                    "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (lv :source "lockfile" :date (26045 56635 536089 857000) :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files ("lv.el")
               :source "MELPA" :protocol https :inherit t :depth 1 :ref
               "317e1de33086637579a7aeb60f77ed0405bf359b"))
 (jeison :source "lockfile" :date (26045 56635 534572 39000) :recipe
         (:package "jeison" :repo "SavchenkoValeriy/jeison" :fetcher github
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth 1 :ref
                   "19a51770f24eaa7b538c7be6a8a5c25d154b641f"))
 (esxml :source "lockfile" :date (26045 56635 532118 31000) :recipe
        (:package "esxml" :fetcher github :repo "tali713/esxml" :files
                  ("esxml.el" "esxml-query.el") :source "MELPA" :protocol https
                  :inherit t :depth 1 :ref
                  "225693096a587492d76bf696d1f0c25c61f7d531"))
 (kv :source "lockfile" :date (26045 56635 529746 367000) :recipe
     (:package "kv" :fetcher github :repo "nicferrier/emacs-kv" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :source "MELPA" :protocol https :inherit t :depth 1 :ref
               "721148475bce38a70e0b678ba8aa923652e8900e"))
 (parsebib :source "lockfile" :date (26045 56635 527349 25000) :recipe
           (:package "parsebib" :fetcher github :repo "joostkremers/parsebib"
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref
                     "ace9df707108b17759c004c7387655277122d4c1"))
 (citeproc :source "lockfile" :date (26045 56635 525043 605000) :recipe
           (:package "citeproc" :fetcher github :repo
                     "andras-simonyi/citeproc-el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref
                     "c61c98b9d230ea28b2ca49498134803e1f8ea526"))
 (queue :source "lockfile" :date (26045 56635 522548 731000) :recipe
        (:package "queue" :repo "git://git.sv.gnu.org/emacs/elpa" :local-repo
                  "queue" :branch "externals/queue" :files
                  ("*" (:exclude ".git")) :source "GNU-devel ELPA" :protocol
                  https :inherit t :depth 1 :ref
                  "f986fb68e75bdae951efb9e11a3012ab6bd408ee"))
 (string-inflection :source "lockfile" :date (26045 56635 520104 140000) :recipe
                    (:package "string-inflection" :fetcher github :repo
                              "akicho8/string-inflection" :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                               "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el"
                               (:exclude ".dir-locals.el" "test.el" "tests.el"
                                         "*-test.el" "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "MELPA" :protocol https :inherit t :depth
                              1 :ref "50ad54970b3cc79b6b83979bde9889ad9a9e1a9c"))
 (tomelr :source "lockfile" :date (26045 56635 517486 436000) :recipe
         (:package "tomelr" :repo "https://github.com/kaushalmodi/tomelr"
                   :local-repo "tomelr" :files ("*" (:exclude ".git" "LICENSE"))
                   :source "GNU-devel ELPA" :protocol https :inherit t :depth 1
                   :ref "670e0a08f625175fd80137cf69e799619bf8a381"))
 (tablist :source "lockfile" :date (26045 56635 514823 778000) :recipe
          (:package "tablist" :fetcher github :repo "emacsorphanage/tablist"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref
                    "fcd37147121fabdf003a70279cf86fbe08cfac6f"))
 (deferred :source "lockfile" :date (26045 56635 512350 33000) :recipe
           (:package "deferred" :repo "kiwanami/emacs-deferred" :fetcher github
                     :files ("deferred.el") :source "MELPA" :protocol https
                     :inherit t :depth 1 :ref
                     "2239671d94b38d92e9b28d4e12fd79814cfb9c16"))
 (org-roam :source "lockfile" :date (26045 56635 509690 300000) :recipe
           (:package "org-roam" :fetcher github :repo "org-roam/org-roam" :files
                     (:defaults "extensions/*") :source "MELPA" :protocol https
                     :inherit t :depth 1 :ref
                     "8667e441876cd2583fbf7282a65796ea149f0e5f"))
 (emacsql :source "lockfile" :date (26045 56635 506990 994000) :recipe
          (:package "emacsql" :fetcher github :repo "magit/emacsql" :files
                    (:defaults "sqlite") :source "MELPA" :protocol https
                    :inherit t :depth 1 :ref
                    "5aba772e562f0b22e36e34c9b4c256ffe439654c"))
 (magit-section :source "lockfile" :date (26045 56635 504657 952000) :recipe
                (:package "magit-section" :fetcher github :repo "magit/magit"
                          :files
                          ("lisp/magit-section.el" "lisp/magit-section-pkg.el"
                           "docs/magit-section.texi"
                           "Documentation/magit-section.texi")
                          :source "MELPA" :protocol https :inherit t :depth 1
                          :ref "54d37dc14c3f715dd0328a70bc65d63c54ee9613"))
 (ts :source "lockfile" :date (26045 56635 503163 528000) :recipe
     (:package "ts" :fetcher github :repo "alphapapa/ts.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :source "MELPA" :protocol https :inherit t :depth 1 :ref
               "552936017cfdec89f7fc20c254ae6b37c3f22c5b"))
 (mustache :source "lockfile" :date (26045 56635 500492 113000) :recipe
           (:package "mustache" :fetcher github :repo "Wilfred/mustache.el"
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref
                     "229e01f0f0a5684499bcc6a11a5bf8dbe14fd4e8"))
 (outorg :source "lockfile" :date (26045 56635 497941 995000) :recipe
         (:package "outorg" :fetcher github :repo "alphapapa/outorg" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth 1 :ref
                   "ef0f86f4b893b30be8bcf8b43a5ec357a6c70f07"))
 (dired-hacks-utils :source "lockfile" :date (26045 56635 495361 741000) :recipe
                    (:package "dired-hacks-utils" :fetcher github :repo
                              "Fuco1/dired-hacks" :files
                              ("dired-hacks-utils.el") :source "MELPA" :protocol
                              https :inherit t :depth 1 :ref
                              "874449d6fc98aee565e1715ec18acec3c1c2cafb"))
 (projectile :source "lockfile" :date (26045 56635 492444 327000) :recipe
             (:package "projectile" :fetcher github :repo "bbatsov/projectile"
                       :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref
                       "f7e60843bfada2eee89595580786a4468fd3f881"))
 (git-commit :source "lockfile" :date (26045 56635 489880 333000) :recipe
             (:package "git-commit" :fetcher github :repo "magit/magit" :files
                       ("lisp/git-commit.el" "lisp/git-commit-pkg.el")
                       :old-names (git-commit-mode) :source "MELPA" :protocol
                       https :inherit t :depth 1 :ref
                       "54d37dc14c3f715dd0328a70bc65d63c54ee9613"))
 (with-editor :source "lockfile"
   :date (26045 56635 486767 133000) :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :source "MELPA" :protocol https :inherit t :depth 1 :ref
             "d43db3c58c34d4dbc3ce6f68ec24fecf3452b20e"))
 (closql :source "lockfile" :date (26045 56635 484105 747000) :recipe
         (:package "closql" :fetcher github :repo "magit/closql" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth 1 :ref
                   "dc8cacbafc4d99ac25280c973a754a5ee5dbe2b0"))
 (ghub :source "lockfile" :date (26045 56635 481588 500000) :recipe
       (:package "ghub" :fetcher github :repo "magit/ghub" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth 1 :ref
                 "ba56fe223fbabab89fee577a1d3b0704d7c4a0df"))
 (yaml :source "lockfile" :date (26045 56635 479073 498000) :recipe
       (:package "yaml" :repo "zkry/yaml.el" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth 1 :ref
                 "70c4fcead97e9bd6594e418c922ae769818f4245"))
 (treepy :source "lockfile" :date (26045 56635 476299 312000) :recipe
         (:package "treepy" :repo "volrath/treepy.el" :fetcher github :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth 1 :ref
                   "75fe3ec37e6f9b2bdfd6d0584efd984d0c00a43e"))
 (fringe-helper :source "lockfile" :date (26045 56635 473937 256000) :recipe
                (:package "fringe-helper" :fetcher github :repo
                          "nschum/fringe-helper.el" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth 1
                          :ref "ef4a9c023bae18ec1ddd7265f1f2d6d2e775efdd"))
 (pkg-info :source "lockfile" :date (26045 56635 470159 322000) :recipe
           (:package "pkg-info" :repo "emacsorphanage/pkg-info" :fetcher github
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref
                     "76ba7415480687d05a4353b27fea2ae02b8d9d61"))
 (epl :source "lockfile" :date (26045 56635 466193 928000) :recipe
      (:package "epl" :repo "cask/epl" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :source "MELPA" :protocol https :inherit t :depth 1 :ref
                "78ab7a85c08222cd15582a298a364774e3282ce6"))
 (package-lint :source "lockfile" :date (26045 56635 462213 387000) :recipe
               (:package "package-lint" :fetcher github :repo
                         "purcell/package-lint" :files
                         (:defaults "data" (:exclude "*flymake.el")) :source
                         "MELPA" :protocol https :inherit t :depth 1 :ref
                         "a6a74293dbc9c69c568c79c509d2aa3b67bc6769"))
 (treemacs :source "lockfile" :date (26045 56635 458182 360000) :recipe
           (:package "treemacs" :fetcher github :repo
                     "Alexander-Miller/treemacs" :files
                     (:defaults "Changelog.org" "icons" "src/elisp/treemacs*.el"
                                "src/scripts/treemacs*.py"
                                (:exclude "src/extra/*"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref
                     "df26b6ab9a0f467e5ff99f7ed97551ccf756e06c"))
 (pfuture :source "lockfile" :date (26045 56635 453788 786000) :recipe
          (:package "pfuture" :repo "Alexander-Miller/pfuture" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref
                    "19b53aebbc0f2da31de6326c495038901bffb73c"))
 (hydra :source "lockfile" :date (26045 56635 449668 753000) :recipe
        (:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
                  (:defaults (:exclude "lv.el")) :source "MELPA" :protocol https
                  :inherit t :depth 1 :ref
                  "317e1de33086637579a7aeb60f77ed0405bf359b"))
 (cfrs :source "lockfile" :date (26045 56635 445619 913000) :recipe
       (:package "cfrs" :repo "Alexander-Miller/cfrs" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth 1 :ref
                 "f3a21f237b2a54e6b9f8a420a9da42b4f0a63121"))
 (posframe :source "lockfile" :date (26045 56635 441803 738000) :recipe
           (:package "posframe" :fetcher github :repo "tumashu/posframe" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref
                     "017deece88360c7297265680d78a0bb316470716"))
 (load-relative :source "lockfile" :date (26045 56635 437748 256000) :recipe
                (:package "load-relative" :fetcher github :repo
                          "rocky/emacs-load-relative" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth 1
                          :ref "b7987c265a64435299d6b02f960ed2c894c4a145"))
 (loc-changes :source "lockfile" :date (26045 56635 433417 98000) :recipe
              (:package "loc-changes" :fetcher github :repo
                        "rocky/emacs-loc-changes" :files ("loc-changes.el")
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref
                        "622371e432f50626aaac82f8ee2841f71685b0fb"))
 (test-simple :source "lockfile" :date (26045 56635 429552 343000) :recipe
              (:package "test-simple" :fetcher github :repo
                        "rocky/emacs-test-simple" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref
                        "8b191842318bb05da74052025192d32ebebb033a"))
 (bui :source "lockfile" :date (26045 56635 425841 716000) :recipe
      (:package "bui" :repo "alezost/bui.el" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :source "MELPA" :protocol https :inherit t :depth 1 :ref
                "f3a137628e112a91910fd33c0cff0948fa58d470"))
 (lsp-docker :source "lockfile" :date (26045 56635 421940 1000) :recipe
             (:package "lsp-docker" :repo "emacs-lsp/lsp-docker" :fetcher github
                       :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref
                       "60e1103ac7c8e30d036ea65fad489210682d6259"))
 (loop :source "lockfile" :date (26045 56635 417065 738000) :recipe
       (:package "loop" :repo "Wilfred/loop.el" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth 1 :ref
                 "9db6372791bbd0cf3fa907ed0ae3e6b7bcf6cc57"))
 (json-snatcher :source "lockfile" :date (26045 56635 413120 382000) :recipe
                (:package "json-snatcher" :fetcher github :repo
                          "Sterlingg/json-snatcher" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth 1
                          :ref "b28d1c0670636da6db508d03872d96ffddbc10f2"))
 (rust-mode :source "lockfile" :date (26045 56635 408530 951000) :recipe
            (:package "rust-mode" :repo "rust-lang/rust-mode" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "9c26dc1195ec05309ee15c014631fb9abd6cf5d2"))
 (xterm-color :source "lockfile" :date (26045 56635 404701 361000) :recipe
              (:package "xterm-color" :repo "atomontage/xterm-color" :fetcher
                        github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref
                        "2ad407c651e90fff2ea85d17bf074cee2c022912"))
 (htmlize :source "lockfile" :date (26045 56635 400159 399000) :recipe
          (:package "htmlize" :fetcher github :repo "hniksic/emacs-htmlize"
                    :version-regexp "release/\\(.*\\)" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref
                    "dd27bc3f26efd728f2b1f01f9e4ac4f61f2ffbf9"))
 (powerline :source "lockfile" :date (26045 56635 396198 3000) :recipe
            (:package "powerline" :fetcher github :repo "milkypostman/powerline"
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "c35c35bdf5ce2d992882c1f06f0f078058870d4a"))
 (link-hint :source "lockfile" :date (26045 56635 392285 498000) :recipe
            (:package "link-hint" :fetcher github :repo "noctuid/link-hint.el"
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref
                      "9153eafc776549376bb85d9ff555fef83aca8285"))
 (persist :source "lockfile" :date (26045 56635 387980 529000) :recipe
          (:package "persist" :repo "https://gitlab.com/phillord/persist"
                    :local-repo "persist" :files ("*" (:exclude ".git")) :source
                    "GNU-devel ELPA" :protocol https :inherit t :depth 1 :ref
                    "01e5001da9fbe2dd7dc085f28be43ec4f4be886e"))
 (taxy :source "lockfile" :date (26045 56635 383143 616000) :recipe
       (:package "taxy" :repo "https://github.com/alphapapa/taxy.el.git"
                 :local-repo "taxy" :files ("*" (:exclude ".git" "images"))
                 :source "GNU-devel ELPA" :protocol https :inherit t :depth 1
                 :ref "b27fa67ecf3f8954ce0d5c2747d1de4dc94ff09f"))
 (taxy-magit-section :source "lockfile" :date (26045 56635 378853 65000) :recipe
                     (:package "taxy-magit-section" :repo
                               "https://github.com/alphapapa/taxy.el.git"
                               :local-repo "taxy-magit-section" :branch
                               "package/taxy-magit-section" :files
                               ("*" (:exclude ".git")) :source "GNU-devel ELPA"
                               :protocol https :inherit t :depth 1 :ref
                               "9d79793edaa98a8a06674723ad5a53ebc4fc237c"))
 (peg :source "lockfile" :date (26045 56635 373824 293000) :recipe
      (:package "peg" :repo "git://git.sv.gnu.org/emacs/elpa" :local-repo "peg"
                :branch "externals/peg" :files ("*" (:exclude ".git" "COPYING"))
                :source "GNU-devel ELPA" :protocol https :inherit t :depth 1
                :ref "f55ca24ba1d51d80e920d611217140e1013e4f3d"))
 (pocket-lib :source "lockfile" :date (26045 56635 369492 163000) :recipe
             (:package "pocket-lib" :fetcher github :repo
                       "alphapapa/pocket-lib.el" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref
                       "f794e3e619e1f6cad25bbfd5fe019a7e62820bf4"))
 (ov :source "lockfile" :date (26045 56635 365359 126000) :recipe
     (:package "ov" :fetcher github :repo "emacsorphanage/ov" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :source "MELPA" :protocol https :inherit t :depth 1 :ref
               "e2971ad986b6ac441e9849031d34c56c980cf40b"))
 (rainbow-identifiers :source "lockfile" :date (26045 56635 361484 923000)
                      :recipe
                      (:package "rainbow-identifiers" :fetcher github :repo
                                "Fanael/rainbow-identifiers" :files
                                ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                                 "doc/*.texinfo" "lisp/*.el"
                                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                                           "*-test.el" "*-tests.el" "LICENSE"
                                           "README*" "*-pkg.el"))
                                :source "MELPA" :protocol https :inherit t
                                :depth 1 :ref
                                "19fbfded1baa98d12335f26f6d7b20e5ae44ce2e"))
 (visual-fill-column :source "lockfile" :date (26045 56635 357119 752000)
                     :recipe
                     (:package "visual-fill-column" :fetcher codeberg :repo
                               "joostkremers/visual-fill-column" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el"
                                (:exclude ".dir-locals.el" "test.el" "tests.el"
                                          "*-test.el" "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :protocol https :inherit t :depth
                               1 :ref "db7c7c236555c9c684e1294a277efefdc25fa5c4")))
