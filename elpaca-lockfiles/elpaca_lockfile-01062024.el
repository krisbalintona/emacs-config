((elpaca :source
         "lockfile" :date (26009 6537 740542 740000) :recipe
                                                     (:source nil :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1
                                                      :repo
                                                      "https://github.com/progfolio/elpaca.git"
                                                      :ref
                                                      "64e2ef42168e3e133db2d4c3b78fb4381c6f8562"
                                                      :files
                                                      (:defaults
                                                       (:exclude "extensions"))
                                                      :build
                                                      (:not
                                                       elpaca--activate-package)
                                                      :package "elpaca"))
 (elpaca-use-package :source "lockfile" :date (26009 6537 738994 70000) :recipe
                                                                        (:package
                                                                         "elpaca-use-package"
                                                                         :repo
                                                                         "https://github.com/progfolio/elpaca.git"
                                                                         :files
                                                                         ("extensions/elpaca-use-package.el")
                                                                         :main
                                                                         "extensions/elpaca-use-package.el"
                                                                         :build
                                                                         (:not
                                                                          elpaca--compile-info)
                                                                         :source
                                                                         "Elpaca extensions"
                                                                         :protocol
                                                                         https
                                                                         :inherit
                                                                         t
                                                                         :depth
                                                                         1 :ref
                                                                           "64e2ef42168e3e133db2d4c3b78fb4381c6f8562"))
 (use-package :source "lockfile"
              :date (26009 6537 736989 594000) :recipe
                                               (:package "use-package" :fetcher
                                                                       github
                                                :repo "jwiegley/use-package"
                                                :files
                                                (:defaults
                                                 (:exclude "bind-key.el"
                                                  "bind-chord.el"
                                                  "use-package-chords.el"
                                                  "use-package-ensure-system-package.el"))
                                                :source "MELPA" :protocol https
                                                :inherit t :depth 1 :ref
                                                                    "a6e856418d2ebd053b34e0ab2fda328abeba731c"))
 (exec-path-from-shell :source "lockfile" :date (26009 6537 735438 158000)
                       :recipe
                       (:package "exec-path-from-shell" :fetcher github :repo
                                                                        "purcell/exec-path-from-shell"
                        :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref
                                                                            "6c10a9d4a38425f2b494013b6bdff91537a6b6da"))
 (system-packages :source "lockfile" :date (26009 6537 733588 833000) :recipe
                                                                      (:package
                                                                       "system-packages"
                                                                       :fetcher
                                                                       gitlab
                                                                       :repo
                                                                       "jabranham/system-packages"
                                                                       :files
                                                                       ("*.el"
                                                                        "*.el.in"
                                                                        "dir"
                                                                        "*.info"
                                                                        "*.texi"
                                                                        "*.texinfo"
                                                                        "doc/dir"
                                                                        "doc/*.info"
                                                                        "doc/*.texi"
                                                                        "doc/*.texinfo"
                                                                        "lisp/*.el"
                                                                        (:exclude
                                                                         ".dir-locals.el"
                                                                         "test.el"
                                                                         "tests.el"
                                                                         "*-test.el"
                                                                         "*-tests.el"
                                                                         "LICENSE"
                                                                         "README*"
                                                                         "*-pkg.el"))
                                                                       :source
                                                                       "MELPA"
                                                                       :protocol
                                                                       https
                                                                       :inherit
                                                                       t :depth
                                                                         1 :ref
                                                                           "c087d2c6e598f85fc2760324dce20104ea442fa3"))
 (gcmh :source "lockfile" :date (26009 6537 731850 917000) :recipe
                                                           (:package "gcmh"
                                                            :repo "koral/gcmh"
                                                            :fetcher gitlab
                                                            :files
                                                            ("*.el" "*.el.in"
                                                             "dir" "*.info"
                                                             "*.texi"
                                                             "*.texinfo"
                                                             "doc/dir"
                                                             "doc/*.info"
                                                             "doc/*.texi"
                                                             "doc/*.texinfo"
                                                             "lisp/*.el"
                                                             (:exclude
                                                              ".dir-locals.el"
                                                              "test.el"
                                                              "tests.el"
                                                              "*-test.el"
                                                              "*-tests.el"
                                                              "LICENSE"
                                                              "README*"
                                                              "*-pkg.el"))
                                                            :source "MELPA"
                                                            :protocol https
                                                            :inherit t :depth 1
                                                            :ref
                                                            "0089f9c3a6d4e9a310d0791cf6fa8f35642ecfd9"))
 (no-littering :source "lockfile" :date (26009 6537 730153 17000) :recipe
                                                                  (:package
                                                                   "no-littering"
                                                                   :fetcher
                                                                   github :repo
                                                                          "emacscollective/no-littering"
                                                                   :files
                                                                   ("*.el"
                                                                    "*.el.in"
                                                                    "dir"
                                                                    "*.info"
                                                                    "*.texi"
                                                                    "*.texinfo"
                                                                    "doc/dir"
                                                                    "doc/*.info"
                                                                    "doc/*.texi"
                                                                    "doc/*.texinfo"
                                                                    "lisp/*.el"
                                                                    (:exclude
                                                                     ".dir-locals.el"
                                                                     "test.el"
                                                                     "tests.el"
                                                                     "*-test.el"
                                                                     "*-tests.el"
                                                                     "LICENSE"
                                                                     "README*"
                                                                     "*-pkg.el"))
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "775f06c2d85bbf77e6fc1dc315ddb35f74ec58db"))
 (compat :source "lockfile" :date (26009 6537 729183 174000) :recipe
                                                             (:package "compat"
                                                              :repo
                                                              "https://github.com/emacs-compat/compat"
                                                              :local-repo
                                                              "compat" :files
                                                                       ("*"
                                                                        (:exclude
                                                                         ".git"))
                                                              :source
                                                              "GNU-devel ELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "ea8de2ea18cf7c348aadb6eb2aeb2a9d840bd064"))
 (general :source "lockfile" :date (26009 6537 727312 169000) :recipe
                                                              (:package
                                                               "general"
                                                               :fetcher github
                                                               :repo
                                                               "noctuid/general.el"
                                                               :files
                                                               ("*.el" "*.el.in"
                                                                "dir" "*.info"
                                                                "*.texi"
                                                                "*.texinfo"
                                                                "doc/dir"
                                                                "doc/*.info"
                                                                "doc/*.texi"
                                                                "doc/*.texinfo"
                                                                "lisp/*.el"
                                                                (:exclude
                                                                 ".dir-locals.el"
                                                                 "test.el"
                                                                 "tests.el"
                                                                 "*-test.el"
                                                                 "*-tests.el"
                                                                 "LICENSE"
                                                                 "README*"
                                                                 "*-pkg.el"))
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "833dea2c4a60e06fcd552b653dfc8960935c9fb4"))
 (use-package-chords :source "lockfile" :date (26009 6537 725937 125000) :recipe
                                                                         (:package
                                                                          "use-package-chords"
                                                                          :repo
                                                                          "jwiegley/use-package"
                                                                          :fetcher
                                                                          github
                                                                          :files
                                                                          ("use-package-chords.el")
                                                                          :source
                                                                          "MELPA"
                                                                          :protocol
                                                                          https
                                                                          :inherit
                                                                          t
                                                                          :depth
                                                                          1 :ref
                                                                            "a6e856418d2ebd053b34e0ab2fda328abeba731c"))
 (which-key :source "lockfile" :date (26009 6537 724320 217000) :recipe
                                                                (:package
                                                                 "which-key"
                                                                 :repo
                                                                 "justbur/emacs-which-key"
                                                                 :fetcher github
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "4d20bc852545a2e602f59084a630f888542052b1"))
 (all-the-icons :source "lockfile" :date (26009 6537 722542 566000) :recipe
                                                                    (:package
                                                                     "all-the-icons"
                                                                     :repo
                                                                     "domtronn/all-the-icons.el"
                                                                     :fetcher
                                                                     github
                                                                     :files
                                                                     (:defaults
                                                                      "data")
                                                                     :source
                                                                     "MELPA"
                                                                     :protocol
                                                                     https
                                                                     :inherit t
                                                                     :depth 1
                                                                     :ref
                                                                     "be9d5dcda9c892e8ca1535e288620eec075eb0be"))
 (mixed-pitch :source "lockfile" :date (26009 6537 721154 278000) :recipe
                                                                  (:package
                                                                   "mixed-pitch"
                                                                   :fetcher
                                                                   gitlab :repo
                                                                          "jabranham/mixed-pitch"
                                                                   :files
                                                                   ("*.el"
                                                                    "*.el.in"
                                                                    "dir"
                                                                    "*.info"
                                                                    "*.texi"
                                                                    "*.texinfo"
                                                                    "doc/dir"
                                                                    "doc/*.info"
                                                                    "doc/*.texi"
                                                                    "doc/*.texinfo"
                                                                    "lisp/*.el"
                                                                    (:exclude
                                                                     ".dir-locals.el"
                                                                     "test.el"
                                                                     "tests.el"
                                                                     "*-test.el"
                                                                     "*-tests.el"
                                                                     "LICENSE"
                                                                     "README*"
                                                                     "*-pkg.el"))
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "519e05f74825abf04b7d2e0e38ec040d013a125a"))
 (default-text-scale :source "lockfile" :date (26009 6537 719540 64000) :recipe
                                                                        (:package
                                                                         "default-text-scale"
                                                                         :fetcher
                                                                         github
                                                                         :repo
                                                                         "purcell/default-text-scale"
                                                                         :files
                                                                         ("*.el"
                                                                          "*.el.in"
                                                                          "dir"
                                                                          "*.info"
                                                                          "*.texi"
                                                                          "*.texinfo"
                                                                          "doc/dir"
                                                                          "doc/*.info"
                                                                          "doc/*.texi"
                                                                          "doc/*.texinfo"
                                                                          "lisp/*.el"
                                                                          (:exclude
                                                                           ".dir-locals.el"
                                                                           "test.el"
                                                                           "tests.el"
                                                                           "*-test.el"
                                                                           "*-tests.el"
                                                                           "LICENSE"
                                                                           "README*"
                                                                           "*-pkg.el"))
                                                                         :source
                                                                         "MELPA"
                                                                         :protocol
                                                                         https
                                                                         :inherit
                                                                         t
                                                                         :depth
                                                                         1 :ref
                                                                           "bfc0987c37e93742255d3b23d86c17096fda8e7e"))
 (emojify :source "lockfile" :date (26009 6537 717617 581000) :recipe
                                                              (:package
                                                               "emojify"
                                                               :fetcher github
                                                               :repo
                                                               "iqbalansari/emacs-emojify"
                                                               :files
                                                               (:defaults "data"
                                                                "images")
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "1b726412f19896abf5e4857d4c32220e33400b55"))
 (unicode-fonts :source "lockfile" :date (26009 6537 716244 421000) :recipe
                                                                    (:package
                                                                     "unicode-fonts"
                                                                     :repo
                                                                     "rolandwalker/unicode-fonts"
                                                                     :fetcher
                                                                     github
                                                                     :files
                                                                     ("*.el"
                                                                      "*.el.in"
                                                                      "dir"
                                                                      "*.info"
                                                                      "*.texi"
                                                                      "*.texinfo"
                                                                      "doc/dir"
                                                                      "doc/*.info"
                                                                      "doc/*.texi"
                                                                      "doc/*.texinfo"
                                                                      "lisp/*.el"
                                                                      (:exclude
                                                                       ".dir-locals.el"
                                                                       "test.el"
                                                                       "tests.el"
                                                                       "*-test.el"
                                                                       "*-tests.el"
                                                                       "LICENSE"
                                                                       "README*"
                                                                       "*-pkg.el"))
                                                                     :source
                                                                     "MELPA"
                                                                     :protocol
                                                                     https
                                                                     :inherit t
                                                                     :depth 1
                                                                     :ref
                                                                     "6245b97d8ddaeaf1de4dbe2cd85ca0f3b20ef81b"))
 (ligature :source "lockfile" :date (26009 6537 714575 335000) :recipe
                                                               (:package
                                                                "ligature"
                                                                :fetcher github
                                                                :repo
                                                                "mickeynp/ligature.el"
                                                                :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :type
                                                                         git
                                                                :host github
                                                                :ref
                                                                "6ac1634612dbd42f7eb81ecaf022bd239aabb954"))
 (nano-theme :source "lockfile" :date (26009 6537 713178 149000) :recipe
                                                                 (:package
                                                                  "nano-theme"
                                                                  :repo
                                                                  "https://github.com/rougier/nano-theme"
                                                                  :local-repo
                                                                  "nano-theme"
                                                                  :files
                                                                  ("*"
                                                                   (:exclude
                                                                    ".git"))
                                                                  :source
                                                                  "GNU-devel ELPA"
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "9d64dc167883835a6dc3a6d286c98dbbf7e95a96"))
 (modus-themes :source "lockfile" :date (26009 6537 711789 460000) :recipe
                                                                   (:package
                                                                    "modus-themes"
                                                                    :fetcher
                                                                    sourcehut
                                                                    :repo
                                                                    "protesilaos/modus-themes"
                                                                    :files
                                                                    ("*.el"
                                                                     "*.el.in"
                                                                     "dir"
                                                                     "*.info"
                                                                     "*.texi"
                                                                     "*.texinfo"
                                                                     "doc/dir"
                                                                     "doc/*.info"
                                                                     "doc/*.texi"
                                                                     "doc/*.texinfo"
                                                                     "lisp/*.el"
                                                                     (:exclude
                                                                      ".dir-locals.el"
                                                                      "test.el"
                                                                      "tests.el"
                                                                      "*-test.el"
                                                                      "*-tests.el"
                                                                      "LICENSE"
                                                                      "README*"
                                                                      "*-pkg.el"))
                                                                    :source
                                                                    "MELPA"
                                                                    :protocol
                                                                    https
                                                                    :inherit t
                                                                    :depth 1
                                                                    :ref
                                                                    "64823c7767710207cdf443492e0f712876dc4ee4"))
 (bind-chord :source "lockfile" :date (26009 6537 710643 156000) :recipe
                                                                 (:package
                                                                  "bind-chord"
                                                                  :repo
                                                                  "jwiegley/use-package"
                                                                  :fetcher
                                                                  github :files
                                                                         ("bind-chord.el")
                                                                  :source
                                                                  "MELPA"
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "a6e856418d2ebd053b34e0ab2fda328abeba731c"))
 (key-chord :source "lockfile" :date (26009 6537 709256 9000) :recipe
                                                              (:package
                                                               "key-chord"
                                                               :fetcher github
                                                               :repo
                                                               "emacsorphanage/key-chord"
                                                               :files
                                                               ("*.el" "*.el.in"
                                                                "dir" "*.info"
                                                                "*.texi"
                                                                "*.texinfo"
                                                                "doc/dir"
                                                                "doc/*.info"
                                                                "doc/*.texi"
                                                                "doc/*.texinfo"
                                                                "lisp/*.el"
                                                                (:exclude
                                                                 ".dir-locals.el"
                                                                 "test.el"
                                                                 "tests.el"
                                                                 "*-test.el"
                                                                 "*-tests.el"
                                                                 "LICENSE"
                                                                 "README*"
                                                                 "*-pkg.el"))
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "e724def60fdf6473858f2962ae276cf4413473eb"))
 (ht :source "lockfile" :date (26009 6537 708138 149000) :recipe
                                                         (:package "ht" :fetcher
                                                                        github
                                                          :repo "Wilfred/ht.el"
                                                          :files
                                                          ("*.el" "*.el.in"
                                                           "dir" "*.info"
                                                           "*.texi" "*.texinfo"
                                                           "doc/dir"
                                                           "doc/*.info"
                                                           "doc/*.texi"
                                                           "doc/*.texinfo"
                                                           "lisp/*.el"
                                                           (:exclude
                                                            ".dir-locals.el"
                                                            "test.el" "tests.el"
                                                            "*-test.el"
                                                            "*-tests.el"
                                                            "LICENSE" "README*"
                                                            "*-pkg.el"))
                                                          :source "MELPA"
                                                          :protocol https
                                                          :inherit t :depth 1
                                                          :ref
                                                          "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (dash :source "lockfile" :date (26009 6537 706541 549000) :recipe
                                                           (:package "dash"
                                                            :fetcher github
                                                            :repo
                                                            "magnars/dash.el"
                                                            :files
                                                            ("dash.el"
                                                             "dash.texi")
                                                            :source "MELPA"
                                                            :protocol https
                                                            :inherit t :depth 1
                                                            :ref
                                                            "e32a70ca636bad42232b6c79f1491dc86802a721"))
 (font-utils :source "lockfile" :date (26009 6537 705526 301000) :recipe
                                                                 (:package
                                                                  "font-utils"
                                                                  :repo
                                                                  "rolandwalker/font-utils"
                                                                  :fetcher
                                                                  github :files
                                                                         ("*.el"
                                                                          "*.el.in"
                                                                          "dir"
                                                                          "*.info"
                                                                          "*.texi"
                                                                          "*.texinfo"
                                                                          "doc/dir"
                                                                          "doc/*.info"
                                                                          "doc/*.texi"
                                                                          "doc/*.texinfo"
                                                                          "lisp/*.el"
                                                                          (:exclude
                                                                           ".dir-locals.el"
                                                                           "test.el"
                                                                           "tests.el"
                                                                           "*-test.el"
                                                                           "*-tests.el"
                                                                           "LICENSE"
                                                                           "README*"
                                                                           "*-pkg.el"))
                                                                  :source
                                                                  "MELPA"
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "abc572eb0dc30a26584c0058c3fe6c7273a10003"))
 (ucs-utils :source "lockfile" :date (26009 6537 703289 96000) :recipe
                                                               (:package
                                                                "ucs-utils"
                                                                :repo
                                                                "rolandwalker/ucs-utils"
                                                                :fetcher github
                                                                :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "91b9e0207fff5883383fd39c45ad5522e9b90e65"))
 (list-utils :source "lockfile" :date (26009 6537 702158 662000) :recipe
                                                                 (:package
                                                                  "list-utils"
                                                                  :repo
                                                                  "rolandwalker/list-utils"
                                                                  :fetcher
                                                                  github :files
                                                                         ("*.el"
                                                                          "*.el.in"
                                                                          "dir"
                                                                          "*.info"
                                                                          "*.texi"
                                                                          "*.texinfo"
                                                                          "doc/dir"
                                                                          "doc/*.info"
                                                                          "doc/*.texi"
                                                                          "doc/*.texinfo"
                                                                          "lisp/*.el"
                                                                          (:exclude
                                                                           ".dir-locals.el"
                                                                           "test.el"
                                                                           "tests.el"
                                                                           "*-test.el"
                                                                           "*-tests.el"
                                                                           "LICENSE"
                                                                           "README*"
                                                                           "*-pkg.el"))
                                                                  :source
                                                                  "MELPA"
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "f02dcef36330871855346f9eab97eef58d323d9a"))
 (persistent-soft :source "lockfile" :date (26009 6537 700846 887000) :recipe
                                                                      (:package
                                                                       "persistent-soft"
                                                                       :repo
                                                                       "rolandwalker/persistent-soft"
                                                                       :fetcher
                                                                       github
                                                                       :files
                                                                       ("*.el"
                                                                        "*.el.in"
                                                                        "dir"
                                                                        "*.info"
                                                                        "*.texi"
                                                                        "*.texinfo"
                                                                        "doc/dir"
                                                                        "doc/*.info"
                                                                        "doc/*.texi"
                                                                        "doc/*.texinfo"
                                                                        "lisp/*.el"
                                                                        (:exclude
                                                                         ".dir-locals.el"
                                                                         "test.el"
                                                                         "tests.el"
                                                                         "*-test.el"
                                                                         "*-tests.el"
                                                                         "LICENSE"
                                                                         "README*"
                                                                         "*-pkg.el"))
                                                                       :source
                                                                       "MELPA"
                                                                       :protocol
                                                                       https
                                                                       :inherit
                                                                       t :depth
                                                                         1 :ref
                                                                           "a1e0ddf2a12a6f18cab565dee250f070384cbe02"))
 (pcache :source "lockfile" :date (26009 6537 699331 470000) :recipe
                                                             (:package "pcache"
                                                              :repo
                                                              "sigma/pcache"
                                                              :fetcher github
                                                              :files
                                                              ("*.el" "*.el.in"
                                                               "dir" "*.info"
                                                               "*.texi"
                                                               "*.texinfo"
                                                               "doc/dir"
                                                               "doc/*.info"
                                                               "doc/*.texi"
                                                               "doc/*.texinfo"
                                                               "lisp/*.el"
                                                               (:exclude
                                                                ".dir-locals.el"
                                                                "test.el"
                                                                "tests.el"
                                                                "*-test.el"
                                                                "*-tests.el"
                                                                "LICENSE"
                                                                "README*"
                                                                "*-pkg.el"))
                                                              :source "MELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "507230d094cc4a5025fe09b62569ad60c71c4226"))
 (fontaine :source "lockfile" :date (26009 6537 698354 534000) :recipe
                                                               (:package
                                                                "fontaine" :repo
                                                                           "https://git.sr.ht/~protesilaos/fontaine"
                                                                :local-repo
                                                                "fontaine"
                                                                :files
                                                                ("*"
                                                                 (:exclude
                                                                  ".git"
                                                                  "COPYING"
                                                                  "doclicense.texi"))
                                                                :source
                                                                "GNU-devel ELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "8d5ba4ac64658da197b9eba41bf4e345a714f782"))
 (hide-mode-line :source "lockfile" :date (26009 6537 696744 318000) :recipe
                                                                     (:package
                                                                      "hide-mode-line"
                                                                      :repo
                                                                      "hlissner/emacs-hide-mode-line"
                                                                      :fetcher
                                                                      github
                                                                      :files
                                                                      ("*.el"
                                                                       "*.el.in"
                                                                       "dir"
                                                                       "*.info"
                                                                       "*.texi"
                                                                       "*.texinfo"
                                                                       "doc/dir"
                                                                       "doc/*.info"
                                                                       "doc/*.texi"
                                                                       "doc/*.texinfo"
                                                                       "lisp/*.el"
                                                                       (:exclude
                                                                        ".dir-locals.el"
                                                                        "test.el"
                                                                        "tests.el"
                                                                        "*-test.el"
                                                                        "*-tests.el"
                                                                        "LICENSE"
                                                                        "README*"
                                                                        "*-pkg.el"))
                                                                      :source
                                                                      "MELPA"
                                                                      :protocol
                                                                      https
                                                                      :inherit t
                                                                      :depth 1
                                                                      :ref
                                                                      "bc5d293576c5e08c29e694078b96a5ed85631942"))
 (lin :source "lockfile" :date (26009 6537 694914 350000) :recipe
                                                          (:package "lin" :repo
                                                                          "https://git.sr.ht/~protesilaos/lin"
                                                           :local-repo "lin"
                                                           :files
                                                           ("*"
                                                            (:exclude ".git"
                                                             "COPYING"
                                                             "doclicense.texi"))
                                                           :source
                                                           "GNU-devel ELPA"
                                                           :protocol https
                                                           :inherit t :depth 1
                                                           :ref
                                                           "75983a770d70b57c090db8cd503a340c32f51827"))
 (nerd-icons :source "lockfile" :date (26009 6537 693518 206000) :recipe
                                                                 (:package
                                                                  "nerd-icons"
                                                                  :repo
                                                                  "rainstormstudio/nerd-icons.el"
                                                                  :fetcher
                                                                  github :files
                                                                         (:defaults
                                                                          "data")
                                                                  :source
                                                                  "MELPA"
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "d53c5a1e0e8837735310d9ebff53d072a947872a"))
 (minions :source "lockfile" :date (26009 6537 691793 976000) :recipe
                                                              (:package
                                                               "minions"
                                                               :fetcher github
                                                               :repo
                                                               "tarsius/minions"
                                                               :files
                                                               ("*.el" "*.el.in"
                                                                "dir" "*.info"
                                                                "*.texi"
                                                                "*.texinfo"
                                                                "doc/dir"
                                                                "doc/*.info"
                                                                "doc/*.texi"
                                                                "doc/*.texinfo"
                                                                "lisp/*.el"
                                                                (:exclude
                                                                 ".dir-locals.el"
                                                                 "test.el"
                                                                 "tests.el"
                                                                 "*-test.el"
                                                                 "*-tests.el"
                                                                 "LICENSE"
                                                                 "README*"
                                                                 "*-pkg.el"))
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "07caa8c30b12c35f3fe563a036f9823c4a6a5f01"))
 (diminish :source "lockfile" :date (26009 6537 689941 755000) :recipe
                                                               (:package
                                                                "diminish"
                                                                :fetcher github
                                                                :repo
                                                                "myrjola/diminish.el"
                                                                :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "fbd5d846611bad828e336b25d2e131d1bc06b83d"))
 (marginalia :source "lockfile" :date (26009 6537 688599 32000) :recipe
                                                                (:package
                                                                 "marginalia"
                                                                 :repo
                                                                 "minad/marginalia"
                                                                 :fetcher github
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "ea356ebb1ddb8d6da78574b517155475cf52d46f"))
 (vertico :source "lockfile" :date (26009 6537 686927 812000) :recipe
                                                              (:package
                                                               "vertico" :repo
                                                                         "minad/vertico"
                                                               :files
                                                               (:defaults
                                                                "extensions/*")
                                                               :fetcher github
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "357c2503a7787193d50dafd8212f2b1cc165ae16"))
 (vertico-truncate :source "lockfile" :date (26009 6537 686019 84000) :recipe
                                                                      (:source
                                                                       nil
                                                                       :protocol
                                                                       https
                                                                       :inherit
                                                                       t :depth
                                                                         1 :type
                                                                           git
                                                                       :host
                                                                       github
                                                                       :repo
                                                                       "jdtsmith/vertico-truncate"
                                                                       :package
                                                                       "vertico-truncate"
                                                                       :ref
                                                                       "7803be7f5b791b9e12be1bf3e0d381fd532d117a"))
 (orderless :source "lockfile" :date (26009 6537 685111 8000) :recipe
                                                              (:package
                                                               "orderless" :repo
                                                                           "oantolin/orderless"
                                                               :fetcher github
                                                               :files
                                                               ("*.el" "*.el.in"
                                                                "dir" "*.info"
                                                                "*.texi"
                                                                "*.texinfo"
                                                                "doc/dir"
                                                                "doc/*.info"
                                                                "doc/*.texi"
                                                                "doc/*.texinfo"
                                                                "lisp/*.el"
                                                                (:exclude
                                                                 ".dir-locals.el"
                                                                 "test.el"
                                                                 "tests.el"
                                                                 "*-test.el"
                                                                 "*-tests.el"
                                                                 "LICENSE"
                                                                 "README*"
                                                                 "*-pkg.el"))
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "b24748093b00b37c3a572c4909f61c08fa27504f"))
 (flx-rs :source "lockfile" :date (26009 6537 684076 785000) :recipe
                                                             (:source nil
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :repo
                                                                           "jcs-elpa/flx-rs"
                                                              :fetcher github
                                                              :files
                                                              (:defaults "bin")
                                                              :package "flx-rs"
                                                              :ref
                                                              "e1f10261bf2207e720b0551bac44c1c25735cdce"))
 (liquidmetal :source "lockfile" :date (26009 6537 682626 510000) :recipe
                                                                  (:package
                                                                   "liquidmetal"
                                                                   :repo
                                                                   "jcs-elpa/liquidmetal"
                                                                   :fetcher
                                                                   github :files
                                                                          ("*.el"
                                                                           "*.el.in"
                                                                           "dir"
                                                                           "*.info"
                                                                           "*.texi"
                                                                           "*.texinfo"
                                                                           "doc/dir"
                                                                           "doc/*.info"
                                                                           "doc/*.texi"
                                                                           "doc/*.texinfo"
                                                                           "lisp/*.el"
                                                                           (:exclude
                                                                            ".dir-locals.el"
                                                                            "test.el"
                                                                            "tests.el"
                                                                            "*-test.el"
                                                                            "*-tests.el"
                                                                            "LICENSE"
                                                                            "README*"
                                                                            "*-pkg.el"))
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "5d100f4371e0d10656a2bd23c0461781c3c1884b"))
 (fuz-bin :source "lockfile" :date (26009 6537 681041 732000) :recipe
                                                              (:source nil
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1
                                                               :repo
                                                               "jcs-elpa/fuz-bin"
                                                               :fetcher github
                                                               :files
                                                               (:defaults "bin")
                                                               :package
                                                               "fuz-bin" :ref
                                                                         "41c38f9ac8eb0f203ebab6d47148b4ff80a70d28"))
 (fzf-native :source "lockfile" :date (26009 6537 679335 335000) :recipe
                                                                 (:source nil
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1
                                                                  :repo
                                                                  "dangduc/fzf-native"
                                                                  :host github
                                                                  :files
                                                                  (:defaults
                                                                   "bin")
                                                                  :package
                                                                  "fzf-native"
                                                                  :ref
                                                                  "ba06f5149e23c57510de2e67d5c46aee96356dba"))
 (sublime-fuzzy :source "lockfile" :date (26009 6537 678009 835000) :recipe
                                                                    (:source nil
                                                                     :protocol
                                                                     https
                                                                     :inherit t
                                                                     :depth 1
                                                                     :repo
                                                                     "jcs-elpa/sublime-fuzzy"
                                                                     :fetcher
                                                                     github
                                                                     :files
                                                                     (:defaults
                                                                      "bin")
                                                                     :package
                                                                     "sublime-fuzzy"
                                                                     :ref
                                                                     "a0f1c86601e3287e96956fd79cc46252f21ef8aa"))
 (hotfuzz :source "lockfile" :date (26009 6537 676902 754000) :recipe
                                                              (:package
                                                               "hotfuzz" :repo
                                                                         "axelf4/hotfuzz"
                                                               :fetcher github
                                                               :files
                                                               ("*.el" "*.el.in"
                                                                "dir" "*.info"
                                                                "*.texi"
                                                                "*.texinfo"
                                                                "doc/dir"
                                                                "doc/*.info"
                                                                "doc/*.texi"
                                                                "doc/*.texinfo"
                                                                "lisp/*.el"
                                                                (:exclude
                                                                 ".dir-locals.el"
                                                                 "test.el"
                                                                 "tests.el"
                                                                 "*-test.el"
                                                                 "*-tests.el"
                                                                 "LICENSE"
                                                                 "README*"
                                                                 "*-pkg.el"))
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "2a4d9d32b3e4ae41b78bdf538352b85b88b39187"))
 (corfu :source "lockfile" :date (26009 6537 675531 327000) :recipe
                                                            (:package "corfu"
                                                             :repo "minad/corfu"
                                                             :files
                                                             (:defaults
                                                              "extensions/*")
                                                             :fetcher github
                                                             :source "MELPA"
                                                             :protocol https
                                                             :inherit t :depth 1
                                                             :ref
                                                             "adfccae7be2dff68ecbded1b47d5029e4e86dc00"))
 (kind-icon :source "lockfile" :date (26009 6537 674404 579000) :recipe
                                                                (:package
                                                                 "kind-icon"
                                                                 :repo
                                                                 "https://github.com/jdtsmith/kind-icon"
                                                                 :local-repo
                                                                 "kind-icon"
                                                                 :files
                                                                 ("*"
                                                                  (:exclude
                                                                   ".git"))
                                                                 :source
                                                                 "GNU-devel ELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "f8c13af0746d70401e5c6d6a8395bca7c7195af8"))
 (cape :source "lockfile" :date (26009 6537 672905 132000) :recipe
                                                           (:package "cape"
                                                            :repo "minad/cape"
                                                            :fetcher github
                                                            :files
                                                            ("*.el" "*.el.in"
                                                             "dir" "*.info"
                                                             "*.texi"
                                                             "*.texinfo"
                                                             "doc/dir"
                                                             "doc/*.info"
                                                             "doc/*.texi"
                                                             "doc/*.texinfo"
                                                             "lisp/*.el"
                                                             (:exclude
                                                              ".dir-locals.el"
                                                              "test.el"
                                                              "tests.el"
                                                              "*-test.el"
                                                              "*-tests.el"
                                                              "LICENSE"
                                                              "README*"
                                                              "*-pkg.el"))
                                                            :source "MELPA"
                                                            :protocol https
                                                            :inherit t :depth 1
                                                            :ref
                                                            "86c5bdfc5c3ea1148a827702224190a70250c656"))
 (company :source "lockfile" :date (26009 6537 671022 404000) :recipe
                                                              (:package
                                                               "company"
                                                               :fetcher github
                                                               :repo
                                                               "company-mode/company-mode"
                                                               :files
                                                               (:defaults
                                                                "icons"
                                                                ("images/small"
                                                                 "doc/images/small/*.png"))
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "d04e665d8ca01a5baa42b5e2dad88d5bbe5a9b8f"))
 (company-box :source "lockfile" :date (26009 6537 669547 422000) :recipe
                                                                  (:package
                                                                   "company-box"
                                                                   :fetcher
                                                                   github :repo
                                                                          "sebastiencs/company-box"
                                                                   :files
                                                                   (:defaults
                                                                    "images")
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "b6f53e26adf948aca55c3ff6c22c21a6a6614253"))
 (company-prescient :source "lockfile" :date (26009 6537 667959 408000) :recipe
                                                                        (:package
                                                                         "company-prescient"
                                                                         :fetcher
                                                                         github
                                                                         :repo
                                                                         "radian-software/prescient.el"
                                                                         :files
                                                                         ("company-prescient.el")
                                                                         :source
                                                                         "MELPA"
                                                                         :protocol
                                                                         https
                                                                         :inherit
                                                                         t
                                                                         :depth
                                                                         1 :ref
                                                                           "864b352e7ecc649cd13ff23172c9cc8039129cc9"))
 (company-shell :source "lockfile" :date (26009 6537 666113 279000) :recipe
                                                                    (:package
                                                                     "company-shell"
                                                                     :repo
                                                                     "Alexander-Miller/company-shell"
                                                                     :fetcher
                                                                     github
                                                                     :files
                                                                     ("*.el"
                                                                      "*.el.in"
                                                                      "dir"
                                                                      "*.info"
                                                                      "*.texi"
                                                                      "*.texinfo"
                                                                      "doc/dir"
                                                                      "doc/*.info"
                                                                      "doc/*.texi"
                                                                      "doc/*.texinfo"
                                                                      "lisp/*.el"
                                                                      (:exclude
                                                                       ".dir-locals.el"
                                                                       "test.el"
                                                                       "tests.el"
                                                                       "*-test.el"
                                                                       "*-tests.el"
                                                                       "LICENSE"
                                                                       "README*"
                                                                       "*-pkg.el"))
                                                                     :source
                                                                     "MELPA"
                                                                     :protocol
                                                                     https
                                                                     :inherit t
                                                                     :depth 1
                                                                     :ref
                                                                     "5f959a63a6e66eb0cbdac3168cad523a62cc2ccd"))
 (ivy :source "lockfile" :date (26009 6537 664711 856000) :recipe
                                                          (:package "ivy" :repo
                                                                          "abo-abo/swiper"
                                                           :fetcher github
                                                           :files
                                                           (:defaults
                                                            "doc/ivy-help.org"
                                                            (:exclude
                                                             "swiper.el"
                                                             "counsel.el"
                                                             "ivy-hydra.el"
                                                             "ivy-avy.el"))
                                                           :source "MELPA"
                                                           :protocol https
                                                           :inherit t :depth 1
                                                           :ref
                                                           "8c30f4cab5948aa8d942a3b2bbf5fb6a94d9441d"))
 (ivy-rich :source "lockfile" :date (26009 6537 663699 323000) :recipe
                                                               (:package
                                                                "ivy-rich" :repo
                                                                           "Yevgnen/ivy-rich"
                                                                :fetcher github
                                                                :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "aff9b6bd53e0fdcf350ab83c90e64e651b47dba4"))
 (all-the-icons-ivy-rich :source "lockfile" :date (26009 6537 662619 765000)
                         :recipe
                         (:package "all-the-icons-ivy-rich" :fetcher github
                          :repo "seagle0128/all-the-icons-ivy-rich" :files
                                                                    ("*.el"
                                                                     "*.el.in"
                                                                     "dir"
                                                                     "*.info"
                                                                     "*.texi"
                                                                     "*.texinfo"
                                                                     "doc/dir"
                                                                     "doc/*.info"
                                                                     "doc/*.texi"
                                                                     "doc/*.texinfo"
                                                                     "lisp/*.el"
                                                                     (:exclude
                                                                      ".dir-locals.el"
                                                                      "test.el"
                                                                      "tests.el"
                                                                      "*-test.el"
                                                                      "*-tests.el"
                                                                      "LICENSE"
                                                                      "README*"
                                                                      "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth 1
                          :ref "f4d77b63c226dc744d1cd3e44dafda787dd71c7b"))
 (ivy-prescient :source "lockfile" :date (26009 6537 661220 595000) :recipe
                                                                    (:package
                                                                     "ivy-prescient"
                                                                     :fetcher
                                                                     github
                                                                     :repo
                                                                     "radian-software/prescient.el"
                                                                     :files
                                                                     ("ivy-prescient.el")
                                                                     :source
                                                                     "MELPA"
                                                                     :protocol
                                                                     https
                                                                     :inherit t
                                                                     :depth 1
                                                                     :ref
                                                                     "864b352e7ecc649cd13ff23172c9cc8039129cc9"))
 (flx :source "lockfile" :date (26009 6537 659739 773000) :recipe
                                                          (:package "flx" :repo
                                                                          "lewang/flx"
                                                           :fetcher github
                                                           :files ("flx.el")
                                                           :source "MELPA"
                                                           :protocol https
                                                           :inherit t :depth 1
                                                           :ref
                                                           "7b44a5abb254bbfbeca7a29336f7f4ebd8aabbf2"))
 (counsel :source "lockfile" :date (26009 6537 658745 434000) :recipe
                                                              (:package
                                                               "counsel" :repo
                                                                         "abo-abo/swiper"
                                                               :fetcher github
                                                               :files
                                                               ("counsel.el")
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "8c30f4cab5948aa8d942a3b2bbf5fb6a94d9441d"))
 (helm :source "lockfile" :date (26009 6537 657097 738000) :recipe
                                                           (:package "helm"
                                                            :fetcher github
                                                            :repo
                                                            "emacs-helm/helm"
                                                            :files
                                                            ("*.el"
                                                             "emacs-helm.sh"
                                                             (:exclude
                                                              "helm-lib.el"
                                                              "helm-source.el"
                                                              "helm-multi-match.el"
                                                              "helm-core.el"))
                                                            :source "MELPA"
                                                            :protocol https
                                                            :inherit t :depth 1
                                                            :ref
                                                            "f7ca648c2c3b308b90739f1862b4f750585ee362"))
 (transpose-frame :source "lockfile" :date (26009 6537 655219 960000) :recipe
                                                                      (:package
                                                                       "transpose-frame"
                                                                       :fetcher
                                                                       github
                                                                       :repo
                                                                       "emacsorphanage/transpose-frame"
                                                                       :files
                                                                       ("*.el"
                                                                        "*.el.in"
                                                                        "dir"
                                                                        "*.info"
                                                                        "*.texi"
                                                                        "*.texinfo"
                                                                        "doc/dir"
                                                                        "doc/*.info"
                                                                        "doc/*.texi"
                                                                        "doc/*.texinfo"
                                                                        "lisp/*.el"
                                                                        (:exclude
                                                                         ".dir-locals.el"
                                                                         "test.el"
                                                                         "tests.el"
                                                                         "*-test.el"
                                                                         "*-tests.el"
                                                                         "LICENSE"
                                                                         "README*"
                                                                         "*-pkg.el"))
                                                                       :source
                                                                       "MELPA"
                                                                       :protocol
                                                                       https
                                                                       :inherit
                                                                       t :depth
                                                                         1 :ref
                                                                           "94c87794d53883a2358d13da264ad8dab9a52daa"))
 (ace-window :source "lockfile" :date (26009 6537 653949 983000) :recipe
                                                                 (:package
                                                                  "ace-window"
                                                                  :repo
                                                                  "abo-abo/ace-window"
                                                                  :fetcher
                                                                  github :files
                                                                         ("*.el"
                                                                          "*.el.in"
                                                                          "dir"
                                                                          "*.info"
                                                                          "*.texi"
                                                                          "*.texinfo"
                                                                          "doc/dir"
                                                                          "doc/*.info"
                                                                          "doc/*.texi"
                                                                          "doc/*.texinfo"
                                                                          "lisp/*.el"
                                                                          (:exclude
                                                                           ".dir-locals.el"
                                                                           "test.el"
                                                                           "tests.el"
                                                                           "*-test.el"
                                                                           "*-tests.el"
                                                                           "LICENSE"
                                                                           "README*"
                                                                           "*-pkg.el"))
                                                                  :source
                                                                  "MELPA"
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "77115afc1b0b9f633084cf7479c767988106c196"))
 (popper :source "lockfile" :date (26009 6537 652413 416000) :recipe
                                                             (:package "popper"
                                                              :fetcher github
                                                              :repo
                                                              "karthink/popper"
                                                              :files
                                                              ("*.el" "*.el.in"
                                                               "dir" "*.info"
                                                               "*.texi"
                                                               "*.texinfo"
                                                               "doc/dir"
                                                               "doc/*.info"
                                                               "doc/*.texi"
                                                               "doc/*.texinfo"
                                                               "lisp/*.el"
                                                               (:exclude
                                                                ".dir-locals.el"
                                                                "test.el"
                                                                "tests.el"
                                                                "*-test.el"
                                                                "*-tests.el"
                                                                "LICENSE"
                                                                "README*"
                                                                "*-pkg.el"))
                                                              :source "MELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "570b0820f884a9c0e3d9cb07e7f7f523b39b836f"))
 (bufler :source "lockfile" :date (26009 6537 651474 121000) :recipe
                                                             (:package "bufler"
                                                              :fetcher github
                                                              :repo
                                                              "alphapapa/bufler.el"
                                                              :files
                                                              (:defaults
                                                               (:exclude
                                                                "helm-bufler.el"))
                                                              :source "MELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "ff0d371b853a65943ccd3546fe947d407cd2e28a"))
 (burly :source "lockfile" :date (26009 6537 649802 299000) :recipe
                                                            (:package "burly"
                                                             :fetcher github
                                                             :repo
                                                             "alphapapa/burly.el"
                                                             :files
                                                             ("*.el" "*.el.in"
                                                              "dir" "*.info"
                                                              "*.texi"
                                                              "*.texinfo"
                                                              "doc/dir"
                                                              "doc/*.info"
                                                              "doc/*.texi"
                                                              "doc/*.texinfo"
                                                              "lisp/*.el"
                                                              (:exclude
                                                               ".dir-locals.el"
                                                               "test.el"
                                                               "tests.el"
                                                               "*-test.el"
                                                               "*-tests.el"
                                                               "LICENSE"
                                                               "README*"
                                                               "*-pkg.el"))
                                                             :source "MELPA"
                                                             :protocol https
                                                             :inherit t :depth 1
                                                             :branch
                                                             "wip/readablep"
                                                             :ref
                                                             "df333960d5229f1ff75d8e5e6d65db3db08123aa"))
 (undo-fu :source "lockfile" :date (26009 6537 647975 476000) :recipe
                                                              (:package
                                                               "undo-fu"
                                                               :fetcher codeberg
                                                               :repo
                                                               "ideasman42/emacs-undo-fu"
                                                               :files
                                                               ("*.el" "*.el.in"
                                                                "dir" "*.info"
                                                                "*.texi"
                                                                "*.texinfo"
                                                                "doc/dir"
                                                                "doc/*.info"
                                                                "doc/*.texi"
                                                                "doc/*.texinfo"
                                                                "lisp/*.el"
                                                                (:exclude
                                                                 ".dir-locals.el"
                                                                 "test.el"
                                                                 "tests.el"
                                                                 "*-test.el"
                                                                 "*-tests.el"
                                                                 "LICENSE"
                                                                 "README*"
                                                                 "*-pkg.el"))
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "0e74116fd5c7797811a91ba4eadef50d67523eb6"))
 (undo-fu-session :source "lockfile" :date (26009 6537 646479 124000) :recipe
                                                                      (:package
                                                                       "undo-fu-session"
                                                                       :fetcher
                                                                       codeberg
                                                                       :repo
                                                                       "ideasman42/emacs-undo-fu-session"
                                                                       :files
                                                                       ("*.el"
                                                                        "*.el.in"
                                                                        "dir"
                                                                        "*.info"
                                                                        "*.texi"
                                                                        "*.texinfo"
                                                                        "doc/dir"
                                                                        "doc/*.info"
                                                                        "doc/*.texi"
                                                                        "doc/*.texinfo"
                                                                        "lisp/*.el"
                                                                        (:exclude
                                                                         ".dir-locals.el"
                                                                         "test.el"
                                                                         "tests.el"
                                                                         "*-test.el"
                                                                         "*-tests.el"
                                                                         "LICENSE"
                                                                         "README*"
                                                                         "*-pkg.el"))
                                                                       :source
                                                                       "MELPA"
                                                                       :protocol
                                                                       https
                                                                       :inherit
                                                                       t :depth
                                                                         1 :ref
                                                                           "9147a7223ee8136769cf42239c7d9a8577edfaff"))
 (puni :source "lockfile" :date (26009 6537 645528 498000) :recipe
                                                           (:package "puni"
                                                            :repo
                                                            "AmaiKinono/puni"
                                                            :fetcher github
                                                            :files
                                                            ("*.el" "*.el.in"
                                                             "dir" "*.info"
                                                             "*.texi"
                                                             "*.texinfo"
                                                             "doc/dir"
                                                             "doc/*.info"
                                                             "doc/*.texi"
                                                             "doc/*.texinfo"
                                                             "lisp/*.el"
                                                             (:exclude
                                                              ".dir-locals.el"
                                                              "test.el"
                                                              "tests.el"
                                                              "*-test.el"
                                                              "*-tests.el"
                                                              "LICENSE"
                                                              "README*"
                                                              "*-pkg.el"))
                                                            :source "MELPA"
                                                            :protocol https
                                                            :inherit t :depth 1
                                                            :ref
                                                            "72e091ef30e0c9299dbcd0bc4669ab9bb8fb6e47"))
 (avy :source "lockfile" :date (26009 6537 643871 605000) :recipe
                                                          (:package "avy" :repo
                                                                          "abo-abo/avy"
                                                           :fetcher github
                                                           :files
                                                           ("*.el" "*.el.in"
                                                            "dir" "*.info"
                                                            "*.texi" "*.texinfo"
                                                            "doc/dir"
                                                            "doc/*.info"
                                                            "doc/*.texi"
                                                            "doc/*.texinfo"
                                                            "lisp/*.el"
                                                            (:exclude
                                                             ".dir-locals.el"
                                                             "test.el"
                                                             "tests.el"
                                                             "*-test.el"
                                                             "*-tests.el"
                                                             "LICENSE" "README*"
                                                             "*-pkg.el"))
                                                           :source "MELPA"
                                                           :protocol https
                                                           :inherit t :depth 1
                                                           :ref
                                                           "be612110cb116a38b8603df367942e2bb3d9bdbe"))
 (link-hint :source "lockfile" :date (26009 6537 642049 371000) :recipe
                                                                (:package
                                                                 "link-hint"
                                                                 :fetcher github
                                                                 :repo
                                                                 "noctuid/link-hint.el"
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "9153eafc776549376bb85d9ff555fef83aca8285"))
 (imenu-list :source "lockfile" :date (26009 6537 640566 93000) :recipe
                                                                (:package
                                                                 "imenu-list"
                                                                 :repo
                                                                 "bmag/imenu-list"
                                                                 :fetcher github
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "76f2335ee6f2f066d87fe4e4729219d70c9bc70d"))
 (flyspell-correct :source "lockfile" :date (26009 6537 638951 429000) :recipe
                                                                       (:package
                                                                        "flyspell-correct"
                                                                        :repo
                                                                        "d12frosted/flyspell-correct"
                                                                        :fetcher
                                                                        github
                                                                        :files
                                                                        ("flyspell-correct.el"
                                                                         "flyspell-correct-ido.el")
                                                                        :source
                                                                        "MELPA"
                                                                        :protocol
                                                                        https
                                                                        :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "1e7a5a56362dd875dddf848b9a9e25d1395b9d37"))
 (jinx :source "lockfile" :date (26009 6537 638033 114000) :recipe
                                                           (:package "jinx"
                                                            :repo "minad/jinx"
                                                            :files
                                                            (:defaults
                                                             "jinx-mod.c"
                                                             "emacs-module.h")
                                                            :fetcher github
                                                            :source "MELPA"
                                                            :protocol https
                                                            :inherit t :depth
                                                                       nil :ref
                                                                           "d2caef25a0d7a3eaa55da43c928ec944e0d09450"))
 (flymake-languagetool :source "lockfile" :date (26009 6537 636034 809000)
                       :recipe
                       (:package "flymake-languagetool" :repo
                                                        "emacs-languagetool/flymake-languagetool"
                        :fetcher github :files
                                        ("*.el" "*.el.in" "dir" "*.info"
                                         "*.texi" "*.texinfo" "doc/dir"
                                         "doc/*.info" "doc/*.texi"
                                         "doc/*.texinfo" "lisp/*.el"
                                         (:exclude ".dir-locals.el" "test.el"
                                          "tests.el" "*-test.el" "*-tests.el"
                                          "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref
                                                                            "bd2d2c05949e9d892ca1313d1012aff39528218e"))
 (lsp-grammarly :source "lockfile" :date (26009 6537 634592 108000) :recipe
                                                                    (:package
                                                                     "lsp-grammarly"
                                                                     :repo
                                                                     "emacs-grammarly/lsp-grammarly"
                                                                     :fetcher
                                                                     github
                                                                     :files
                                                                     ("*.el"
                                                                      "*.el.in"
                                                                      "dir"
                                                                      "*.info"
                                                                      "*.texi"
                                                                      "*.texinfo"
                                                                      "doc/dir"
                                                                      "doc/*.info"
                                                                      "doc/*.texi"
                                                                      "doc/*.texinfo"
                                                                      "lisp/*.el"
                                                                      (:exclude
                                                                       ".dir-locals.el"
                                                                       "test.el"
                                                                       "tests.el"
                                                                       "*-test.el"
                                                                       "*-tests.el"
                                                                       "LICENSE"
                                                                       "README*"
                                                                       "*-pkg.el"))
                                                                     :source
                                                                     "MELPA"
                                                                     :protocol
                                                                     https
                                                                     :inherit t
                                                                     :depth 1
                                                                     :ref
                                                                     "7b3597e19c50452124f532c3c47f40c0e33f6e91"))
 (powerthesaurus :source "lockfile" :date (26009 6537 632924 394000) :recipe
                                                                     (:package
                                                                      "powerthesaurus"
                                                                      :repo
                                                                      "SavchenkoValeriy/emacs-powerthesaurus"
                                                                      :fetcher
                                                                      github
                                                                      :files
                                                                      ("*.el"
                                                                       "*.el.in"
                                                                       "dir"
                                                                       "*.info"
                                                                       "*.texi"
                                                                       "*.texinfo"
                                                                       "doc/dir"
                                                                       "doc/*.info"
                                                                       "doc/*.texi"
                                                                       "doc/*.texinfo"
                                                                       "lisp/*.el"
                                                                       (:exclude
                                                                        ".dir-locals.el"
                                                                        "test.el"
                                                                        "tests.el"
                                                                        "*-test.el"
                                                                        "*-tests.el"
                                                                        "LICENSE"
                                                                        "README*"
                                                                        "*-pkg.el"))
                                                                      :source
                                                                      "MELPA"
                                                                      :protocol
                                                                      https
                                                                      :inherit t
                                                                      :depth 1
                                                                      :ref
                                                                      "4b97797cf789aaba411c61a85fe23474ebc5bedc"))
 (synosaurus :source "lockfile" :date (26009 6537 631404 678000) :recipe
                                                                 (:package
                                                                  "synosaurus"
                                                                  :repo
                                                                  "hpdeifel/synosaurus"
                                                                  :fetcher
                                                                  github :files
                                                                         ("*.el"
                                                                          "*.el.in"
                                                                          "dir"
                                                                          "*.info"
                                                                          "*.texi"
                                                                          "*.texinfo"
                                                                          "doc/dir"
                                                                          "doc/*.info"
                                                                          "doc/*.texi"
                                                                          "doc/*.texinfo"
                                                                          "lisp/*.el"
                                                                          (:exclude
                                                                           ".dir-locals.el"
                                                                           "test.el"
                                                                           "tests.el"
                                                                           "*-test.el"
                                                                           "*-tests.el"
                                                                           "LICENSE"
                                                                           "README*"
                                                                           "*-pkg.el"))
                                                                  :source
                                                                  "MELPA"
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "14d34fc92a77c3a916b4d58400424c44ae99cd81"))
 (org :source "lockfile" :date (26009 6537 629933 223000) :recipe
                                                          (:package "org"
                                                           :local-repo "org"
                                                           :repo
                                                           "https://git.savannah.gnu.org/git/emacs/org-mode.git"
                                                           :pre-build
                                                           (progn
                                                             (require
                                                              'elpaca-menu-org)
                                                             (elpaca-menu-org--build))
                                                           :autoloads
                                                           "org-loaddefs.el"
                                                           :build
                                                           (:not
                                                            elpaca--generate-autoloads-async)
                                                           :files
                                                           (:defaults
                                                            ("etc/styles/"
                                                             "etc/styles/*"
                                                             "doc/*.texi"))
                                                           :source "Org"
                                                           :protocol https
                                                           :inherit t :depth 1
                                                           :ref
                                                           "4279b84a1fc76138dee6b9eb4f14ea2da53b57f0"))
 (org-visibility :source "lockfile" :date (26009 6537 628481 355000) :recipe
                                                                     (:package
                                                                      "org-visibility"
                                                                      :repo
                                                                      "nullman/emacs-org-visibility"
                                                                      :fetcher
                                                                      github
                                                                      :files
                                                                      ("*.el"
                                                                       "*.el.in"
                                                                       "dir"
                                                                       "*.info"
                                                                       "*.texi"
                                                                       "*.texinfo"
                                                                       "doc/dir"
                                                                       "doc/*.info"
                                                                       "doc/*.texi"
                                                                       "doc/*.texinfo"
                                                                       "lisp/*.el"
                                                                       (:exclude
                                                                        ".dir-locals.el"
                                                                        "test.el"
                                                                        "tests.el"
                                                                        "*-test.el"
                                                                        "*-tests.el"
                                                                        "LICENSE"
                                                                        "README*"
                                                                        "*-pkg.el"))
                                                                      :source
                                                                      "MELPA"
                                                                      :protocol
                                                                      https
                                                                      :inherit t
                                                                      :depth 1
                                                                      :ref
                                                                      "afa4b6f8ff274df87eb11f1afd0321084a45a2ab"))
 (mermaid-mode :source "lockfile" :date (26009 6537 626421 695000) :recipe
                                                                   (:package
                                                                    "mermaid-mode"
                                                                    :repo
                                                                    "abrochard/mermaid-mode"
                                                                    :fetcher
                                                                    github
                                                                    :files
                                                                    ("*.el"
                                                                     "*.el.in"
                                                                     "dir"
                                                                     "*.info"
                                                                     "*.texi"
                                                                     "*.texinfo"
                                                                     "doc/dir"
                                                                     "doc/*.info"
                                                                     "doc/*.texi"
                                                                     "doc/*.texinfo"
                                                                     "lisp/*.el"
                                                                     (:exclude
                                                                      ".dir-locals.el"
                                                                      "test.el"
                                                                      "tests.el"
                                                                      "*-test.el"
                                                                      "*-tests.el"
                                                                      "LICENSE"
                                                                      "README*"
                                                                      "*-pkg.el"))
                                                                    :source
                                                                    "MELPA"
                                                                    :protocol
                                                                    https
                                                                    :inherit t
                                                                    :depth 1
                                                                    :ref
                                                                    "4cdc76e459d184fc241b607e9da131ebce9f4ce2"))
 (ob-mermaid :source "lockfile" :date (26009 6537 625046 270000) :recipe
                                                                 (:package
                                                                  "ob-mermaid"
                                                                  :repo
                                                                  "arnm/ob-mermaid"
                                                                  :fetcher
                                                                  github :files
                                                                         ("*.el"
                                                                          "*.el.in"
                                                                          "dir"
                                                                          "*.info"
                                                                          "*.texi"
                                                                          "*.texinfo"
                                                                          "doc/dir"
                                                                          "doc/*.info"
                                                                          "doc/*.texi"
                                                                          "doc/*.texinfo"
                                                                          "lisp/*.el"
                                                                          (:exclude
                                                                           ".dir-locals.el"
                                                                           "test.el"
                                                                           "tests.el"
                                                                           "*-test.el"
                                                                           "*-tests.el"
                                                                           "LICENSE"
                                                                           "README*"
                                                                           "*-pkg.el"))
                                                                  :source
                                                                  "MELPA"
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "b4ce25699e3ebff054f523375d1cf5a17bd0dbaf"))
 (org-superstar :source "lockfile" :date (26009 6537 623483 113000) :recipe
                                                                    (:package
                                                                     "org-superstar"
                                                                     :fetcher
                                                                     github
                                                                     :repo
                                                                     "integral-dw/org-superstar-mode"
                                                                     :files
                                                                     ("*.el"
                                                                      "*.el.in"
                                                                      "dir"
                                                                      "*.info"
                                                                      "*.texi"
                                                                      "*.texinfo"
                                                                      "doc/dir"
                                                                      "doc/*.info"
                                                                      "doc/*.texi"
                                                                      "doc/*.texinfo"
                                                                      "lisp/*.el"
                                                                      (:exclude
                                                                       ".dir-locals.el"
                                                                       "test.el"
                                                                       "tests.el"
                                                                       "*-test.el"
                                                                       "*-tests.el"
                                                                       "LICENSE"
                                                                       "README*"
                                                                       "*-pkg.el"))
                                                                     :source
                                                                     "MELPA"
                                                                     :protocol
                                                                     https
                                                                     :inherit t
                                                                     :depth 1
                                                                     :ref
                                                                     "54c81c27dde2a6dc461bb064e79a8b2089093a2e"))
 (olivetti :source "lockfile" :date (26009 6537 621609 582000) :recipe
                                                               (:package
                                                                "olivetti"
                                                                :fetcher github
                                                                :repo
                                                                "rnkn/olivetti"
                                                                :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "1f8b3d5cb155f7497083001037a09a972befab21"))
 (org-appear :source "lockfile" :date (26009 6537 620288 340000) :recipe
                                                                 (:package
                                                                  "org-appear"
                                                                  :fetcher
                                                                  github :repo
                                                                         "awth13/org-appear"
                                                                  :files
                                                                  ("*.el"
                                                                   "*.el.in"
                                                                   "dir"
                                                                   "*.info"
                                                                   "*.texi"
                                                                   "*.texinfo"
                                                                   "doc/dir"
                                                                   "doc/*.info"
                                                                   "doc/*.texi"
                                                                   "doc/*.texinfo"
                                                                   "lisp/*.el"
                                                                   (:exclude
                                                                    ".dir-locals.el"
                                                                    "test.el"
                                                                    "tests.el"
                                                                    "*-test.el"
                                                                    "*-tests.el"
                                                                    "LICENSE"
                                                                    "README*"
                                                                    "*-pkg.el"))
                                                                  :source
                                                                  "MELPA"
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "81eba5d7a5b74cdb1bad091d85667e836f16b997"))
 (svg-lib :source "lockfile" :date (26009 6537 619234 890000) :recipe
                                                              (:package
                                                               "svg-lib" :repo
                                                                         "https://github.com/rougier/svg-lib"
                                                               :local-repo
                                                               "svg-lib" :files
                                                                         ("*"
                                                                          (:exclude
                                                                           ".git"))
                                                               :source
                                                               "GNU-devel ELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "913d86b0ec91e84366f9ef1e870bb5e3f51ffe65"))
 (org-margin :source "lockfile" :date (26009 6537 617874 984000) :recipe
                                                                 (:source nil
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1
                                                                  :type git
                                                                  :host github
                                                                  :repo
                                                                  "rougier/org-margin"
                                                                  :package
                                                                  "org-margin"
                                                                  :ref
                                                                  "5fda5e29c7813079072c9f33255bdb185bef1365"))
 (org-web-tools :source "lockfile" :date (26009 6537 616243 58000) :recipe
                                                                   (:package
                                                                    "org-web-tools"
                                                                    :fetcher
                                                                    github :repo
                                                                           "alphapapa/org-web-tools"
                                                                    :files
                                                                    ("*.el"
                                                                     "*.el.in"
                                                                     "dir"
                                                                     "*.info"
                                                                     "*.texi"
                                                                     "*.texinfo"
                                                                     "doc/dir"
                                                                     "doc/*.info"
                                                                     "doc/*.texi"
                                                                     "doc/*.texinfo"
                                                                     "lisp/*.el"
                                                                     (:exclude
                                                                      ".dir-locals.el"
                                                                      "test.el"
                                                                      "tests.el"
                                                                      "*-test.el"
                                                                      "*-tests.el"
                                                                      "LICENSE"
                                                                      "README*"
                                                                      "*-pkg.el"))
                                                                    :source
                                                                    "MELPA"
                                                                    :protocol
                                                                    https
                                                                    :inherit t
                                                                    :depth 1
                                                                    :ref
                                                                    "7a6498f442fc7f29504745649948635c7165d847"))
 (org-download :source "lockfile" :date (26009 6537 614374 757000) :recipe
                                                                   (:package
                                                                    "org-download"
                                                                    :repo
                                                                    "abo-abo/org-download"
                                                                    :fetcher
                                                                    github
                                                                    :files
                                                                    ("*.el"
                                                                     "*.el.in"
                                                                     "dir"
                                                                     "*.info"
                                                                     "*.texi"
                                                                     "*.texinfo"
                                                                     "doc/dir"
                                                                     "doc/*.info"
                                                                     "doc/*.texi"
                                                                     "doc/*.texinfo"
                                                                     "lisp/*.el"
                                                                     (:exclude
                                                                      ".dir-locals.el"
                                                                      "test.el"
                                                                      "tests.el"
                                                                      "*-test.el"
                                                                      "*-tests.el"
                                                                      "LICENSE"
                                                                      "README*"
                                                                      "*-pkg.el"))
                                                                    :source
                                                                    "MELPA"
                                                                    :protocol
                                                                    https
                                                                    :inherit t
                                                                    :depth 1
                                                                    :ref
                                                                    "19e166f0a8c539b4144cfbc614309d47a9b2a9b7"))
 (typo :source "lockfile" :date (26009 6537 612683 339000) :recipe
                                                           (:package "typo"
                                                            :fetcher github
                                                            :repo
                                                            "jorgenschaefer/typoel"
                                                            :files
                                                            ("*.el" "*.el.in"
                                                             "dir" "*.info"
                                                             "*.texi"
                                                             "*.texinfo"
                                                             "doc/dir"
                                                             "doc/*.info"
                                                             "doc/*.texi"
                                                             "doc/*.texinfo"
                                                             "lisp/*.el"
                                                             (:exclude
                                                              ".dir-locals.el"
                                                              "test.el"
                                                              "tests.el"
                                                              "*-test.el"
                                                              "*-tests.el"
                                                              "LICENSE"
                                                              "README*"
                                                              "*-pkg.el"))
                                                            :source "MELPA"
                                                            :protocol https
                                                            :inherit t :depth 1
                                                            :ref
                                                            "173ebe4fc7ac38f344b16e6eaf41f79e38f20d57"))
 (org-contrib :source "lockfile" :date (26009 6537 611765 344000) :recipe
                                                                  (:package
                                                                   "org-contrib"
                                                                   :local-repo
                                                                   "org-contrib"
                                                                   :repo
                                                                   "https://git.sr.ht/~bzg/org-contrib"
                                                                   :files
                                                                   (:defaults)
                                                                   :source "Org"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "c6dc355e132176cc493524740c6b4b48b59ad99e"))
 (ox-pandoc :source "lockfile" :date (26009 6537 610848 661000) :recipe
                                                                (:package
                                                                 "ox-pandoc"
                                                                 :repo
                                                                 "emacsorphanage/ox-pandoc"
                                                                 :fetcher github
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "399d787b6e2124bd782615338b845c3724a47718"))
 (citar :source "lockfile" :date (26009 6537 609289 0) :recipe
                                                       (:package "citar" :repo
                                                                         "emacs-citar/citar"
                                                        :fetcher github :files
                                                                        (:defaults
                                                                         (:exclude
                                                                          "citar-embark.el"))
                                                        :old-names
                                                        (bibtex-actions) :source
                                                                         "MELPA"
                                                        :protocol https :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "885b86f6733fd70f42c32dd7791d3447f93db990"))
 (citar-embark :source "lockfile" :date (26009 6537 608275 897000) :recipe
                                                                   (:package
                                                                    "citar-embark"
                                                                    :repo
                                                                    "emacs-citar/citar"
                                                                    :fetcher
                                                                    github
                                                                    :files
                                                                    ("citar-embark.el")
                                                                    :source
                                                                    "MELPA"
                                                                    :protocol
                                                                    https
                                                                    :inherit t
                                                                    :depth 1
                                                                    :ref
                                                                    "885b86f6733fd70f42c32dd7791d3447f93db990"))
 (ox-hugo :source "lockfile" :date (26009 6537 606511 962000) :recipe
                                                              (:package
                                                               "ox-hugo"
                                                               :fetcher github
                                                               :repo
                                                               "kaushalmodi/ox-hugo"
                                                               :files
                                                               ("*.el" "*.el.in"
                                                                "dir" "*.info"
                                                                "*.texi"
                                                                "*.texinfo"
                                                                "doc/dir"
                                                                "doc/*.info"
                                                                "doc/*.texi"
                                                                "doc/*.texinfo"
                                                                "lisp/*.el"
                                                                (:exclude
                                                                 ".dir-locals.el"
                                                                 "test.el"
                                                                 "tests.el"
                                                                 "*-test.el"
                                                                 "*-tests.el"
                                                                 "LICENSE"
                                                                 "README*"
                                                                 "*-pkg.el"))
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "cb1b6cfd7b080e889352150416c1725f11ba937a"))
 (denote :source "lockfile" :date (26009 6537 605317 658000) :recipe
                                                             (:package "denote"
                                                              :repo
                                                              "emacs-straight/denote"
                                                              :local-repo
                                                              "denote" :files
                                                                       ("*"
                                                                        (:exclude
                                                                         ".git"))
                                                              :source
                                                              "GNU-devel ELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         nil
                                                              :type git :host
                                                                        github
                                                              :ref
                                                              "ebca57bd56b508f57656bbb549d9985cf6587feb"))
 (denote-menu :source "lockfile" :date (26009 6537 604307 900000) :recipe
                                                                  (:package
                                                                   "denote-menu"
                                                                   :repo
                                                                   "namilus/denote-menu"
                                                                   :local-repo
                                                                   "denote-menu"
                                                                   :files
                                                                   ("*"
                                                                    (:exclude
                                                                     ".git"
                                                                     "COPYING"))
                                                                   :source
                                                                   "GNU-devel ELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1
                                                                   :type git
                                                                   :host github
                                                                   :ref
                                                                   "6d97b6be0511420dca27b294844bdaa5fa72f753"))
 (pdf-tools :source "lockfile" :date (26009 6537 603110 240000) :recipe
                                                                (:package
                                                                 "pdf-tools"
                                                                 :fetcher github
                                                                 :repo
                                                                 "vedang/pdf-tools"
                                                                 :files
                                                                 (:defaults
                                                                  "README"
                                                                  ("build"
                                                                   "Makefile")
                                                                  ("build"
                                                                   "server"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "c69e7656a4678fe25afbd29f3503dd19ee7f9896"))
 (djvu :source "lockfile" :date (26009 6537 601949 408000) :recipe
                                                           (:package "djvu"
                                                            :repo
                                                            "git://git.sv.gnu.org/emacs/elpa"
                                                            :local-repo "djvu"
                                                            :branch
                                                            "externals/djvu"
                                                            :files
                                                            ("*"
                                                             (:exclude ".git"))
                                                            :source
                                                            "GNU-devel ELPA"
                                                            :protocol https
                                                            :inherit t :depth 1
                                                            :ref
                                                            "1251c94f85329de9f957408d405742023f6c50e2"))
 (org-noter :source "lockfile" :date (26009 6537 600050 690000) :recipe
                                                                (:package
                                                                 "org-noter"
                                                                 :fetcher github
                                                                 :repo
                                                                 "org-noter/org-noter"
                                                                 :files
                                                                 ("*.el"
                                                                  "modules"
                                                                  (:exclude
                                                                   "*-test-utils.el"
                                                                   "*-devel.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "8be376384772c1f053cb2ce907ddf4d484b390dd"))
 (org-remark :source "lockfile" :date (26009 6537 598712 65000) :recipe
                                                                (:package
                                                                 "org-remark"
                                                                 :repo
                                                                 "https://github.com/nobiot/org-remark"
                                                                 :local-repo
                                                                 "org-remark"
                                                                 :files
                                                                 ("*"
                                                                  (:exclude
                                                                   ".git"))
                                                                 :source
                                                                 "GNU-devel ELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "eaa9dc77da0b281ec568324019140d27ca190dd3"))
 (org-transclusion :source "lockfile" :date (26009 6537 597136 715000) :recipe
                                                                       (:package
                                                                        "org-transclusion"
                                                                        :repo
                                                                        "https://github.com/nobiot/org-transclusion"
                                                                        :local-repo
                                                                        "org-transclusion"
                                                                        :files
                                                                        ("*"
                                                                         (:exclude
                                                                          ".git"))
                                                                        :source
                                                                        "GNU-devel ELPA"
                                                                        :protocol
                                                                        https
                                                                        :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "b10d4de93c6c0523bd4e7e72c11ef3a9a5630370"))
 (websocket :source "lockfile" :date (26009 6537 596206 878000) :recipe
                                                                (:package
                                                                 "websocket"
                                                                 :repo
                                                                 "ahyatt/emacs-websocket"
                                                                 :fetcher github
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "40c208eaab99999d7c1e4bea883648da24c03be3"))
 (simple-httpd :source "lockfile" :date (26009 6537 595250 911000) :recipe
                                                                   (:package
                                                                    "simple-httpd"
                                                                    :repo
                                                                    "skeeto/emacs-web-server"
                                                                    :fetcher
                                                                    github
                                                                    :files
                                                                    ("*.el"
                                                                     "*.el.in"
                                                                     "dir"
                                                                     "*.info"
                                                                     "*.texi"
                                                                     "*.texinfo"
                                                                     "doc/dir"
                                                                     "doc/*.info"
                                                                     "doc/*.texi"
                                                                     "doc/*.texinfo"
                                                                     "lisp/*.el"
                                                                     (:exclude
                                                                      ".dir-locals.el"
                                                                      "test.el"
                                                                      "tests.el"
                                                                      "*-test.el"
                                                                      "*-tests.el"
                                                                      "LICENSE"
                                                                      "README*"
                                                                      "*-pkg.el"))
                                                                    :source
                                                                    "MELPA"
                                                                    :protocol
                                                                    https
                                                                    :inherit t
                                                                    :depth 1
                                                                    :ref
                                                                    "3982c55e9061475038a3ccd61aecb2de3d407cec"))
 (f :source "lockfile" :date (26009 6537 594037 581000) :recipe
                                                        (:package "f" :fetcher
                                                                      github
                                                         :repo "rejeep/f.el"
                                                         :files
                                                         ("*.el" "*.el.in" "dir"
                                                          "*.info" "*.texi"
                                                          "*.texinfo" "doc/dir"
                                                          "doc/*.info"
                                                          "doc/*.texi"
                                                          "doc/*.texinfo"
                                                          "lisp/*.el"
                                                          (:exclude
                                                           ".dir-locals.el"
                                                           "test.el" "tests.el"
                                                           "*-test.el"
                                                           "*-tests.el"
                                                           "LICENSE" "README*"
                                                           "*-pkg.el"))
                                                         :source "MELPA"
                                                         :protocol https
                                                         :inherit t :depth 1
                                                         :ref
                                                         "4f03d7bb724a9049b0ab9ef86127694756f99656"))
 (org-roam-ui :source "lockfile" :date (26009 6537 592157 599000) :recipe
                                                                  (:package
                                                                   "org-roam-ui"
                                                                   :fetcher
                                                                   github :repo
                                                                          "org-roam/org-roam-ui"
                                                                   :files
                                                                   ("*.el" "out")
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1
                                                                   :host github
                                                                   :branch
                                                                   "main" :ref
                                                                          "5ac74960231db0bf7783c2ba7a19a60f582e91ab"))
 (lister :source "lockfile" :date (26009 6537 590471 761000) :recipe
                                                             (:package "lister"
                                                              :repo
                                                              "publicimageltd/lister"
                                                              :fetcher github
                                                              :files
                                                              ("*.el" "*.el.in"
                                                               "dir" "*.info"
                                                               "*.texi"
                                                               "*.texinfo"
                                                               "doc/dir"
                                                               "doc/*.info"
                                                               "doc/*.texi"
                                                               "doc/*.texinfo"
                                                               "lisp/*.el"
                                                               (:exclude
                                                                ".dir-locals.el"
                                                                "test.el"
                                                                "tests.el"
                                                                "*-test.el"
                                                                "*-tests.el"
                                                                "LICENSE"
                                                                "README*"
                                                                "*-pkg.el"))
                                                              :source "MELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "84fbba7450ac02cbb844727a28b6f245f553df7b"))
 (org-super-agenda :source "lockfile" :date (26009 6537 589000 907000) :recipe
                                                                       (:package
                                                                        "org-super-agenda"
                                                                        :fetcher
                                                                        github
                                                                        :repo
                                                                        "alphapapa/org-super-agenda"
                                                                        :files
                                                                        ("*.el"
                                                                         "*.el.in"
                                                                         "dir"
                                                                         "*.info"
                                                                         "*.texi"
                                                                         "*.texinfo"
                                                                         "doc/dir"
                                                                         "doc/*.info"
                                                                         "doc/*.texi"
                                                                         "doc/*.texinfo"
                                                                         "lisp/*.el"
                                                                         (:exclude
                                                                          ".dir-locals.el"
                                                                          "test.el"
                                                                          "tests.el"
                                                                          "*-test.el"
                                                                          "*-tests.el"
                                                                          "LICENSE"
                                                                          "README*"
                                                                          "*-pkg.el"))
                                                                        :source
                                                                        "MELPA"
                                                                        :protocol
                                                                        https
                                                                        :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "ee3379ae92b90c084717fb2a7614060ce12283cb"))
 (org-pomodoro :source "lockfile" :date (26009 6537 587977 644000) :recipe
                                                                   (:package
                                                                    "org-pomodoro"
                                                                    :fetcher
                                                                    github :repo
                                                                           "marcinkoziej/org-pomodoro"
                                                                    :files
                                                                    (:defaults
                                                                     "resources")
                                                                    :source
                                                                    "MELPA"
                                                                    :protocol
                                                                    https
                                                                    :inherit t
                                                                    :depth 1
                                                                    :ref
                                                                    "3f5bcfb80d61556d35fc29e5ddb09750df962cc6"))
 (work-timer :source "lockfile" :date (26009 6537 586380 593000) :recipe
                                                                 (:source nil
                                                                  :protocol ssh
                                                                  :inherit t
                                                                  :depth nil
                                                                  :type git
                                                                  :host github
                                                                  :repo
                                                                  "krisbalintona/work-timer"
                                                                  :files
                                                                  (:defaults
                                                                   "*.mp3")
                                                                  :package
                                                                  "work-timer"
                                                                  :ref
                                                                  "3260d6f993dd58db3a3b847f2e7d6878edbf2785"))
 (org-edna :source "lockfile" :date (26009 6537 584502 444000) :recipe
                                                               (:package
                                                                "org-edna" :repo
                                                                           "git://git.sv.gnu.org/emacs/elpa"
                                                                :local-repo
                                                                "org-edna"
                                                                :branch
                                                                "externals/org-edna"
                                                                :files
                                                                ("*"
                                                                 (:exclude
                                                                  ".git"))
                                                                :source
                                                                "GNU-devel ELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "8258a4dfa00aa522249cdf9aeea5be4de97bd7c1"))
 (org-gcal :source "lockfile" :date (26009 6537 583161 394000) :recipe
                                                               (:package
                                                                "org-gcal"
                                                                :fetcher github
                                                                :repo
                                                                "kidd/org-gcal.el"
                                                                :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "a2d16b372e5a5972d8cc343cf999ee5f0ba1eea7"))
 (org-timeblock :source "lockfile" :date (26009 6537 581756 43000) :recipe
                                                                   (:package
                                                                    "org-timeblock"
                                                                    :fetcher
                                                                    github :repo
                                                                           "ichernyshovvv/org-timeblock"
                                                                    :files
                                                                    ("*.el"
                                                                     "*.el.in"
                                                                     "dir"
                                                                     "*.info"
                                                                     "*.texi"
                                                                     "*.texinfo"
                                                                     "doc/dir"
                                                                     "doc/*.info"
                                                                     "doc/*.texi"
                                                                     "doc/*.texinfo"
                                                                     "lisp/*.el"
                                                                     (:exclude
                                                                      ".dir-locals.el"
                                                                      "test.el"
                                                                      "tests.el"
                                                                      "*-test.el"
                                                                      "*-tests.el"
                                                                      "LICENSE"
                                                                      "README*"
                                                                      "*-pkg.el"))
                                                                    :source
                                                                    "MELPA"
                                                                    :protocol
                                                                    https
                                                                    :inherit t
                                                                    :depth 1
                                                                    :type git
                                                                    :host github
                                                                    :ref
                                                                    "9d8486857066a30408c2139a48ba77ea532081ba"))
 (auctex :source "lockfile" :date (26009 6537 579971 670000) :recipe
                                                             (:package "auctex"
                                                              :repo
                                                              "emacs-straight/auctex"
                                                              :local-repo
                                                              "auctex" :files
                                                                       ("*.el"
                                                                        "*.info"
                                                                        "dir"
                                                                        "doc"
                                                                        "etc"
                                                                        "images"
                                                                        "latex"
                                                                        "style")
                                                              :source
                                                              "GNU-devel ELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :type
                                                                           git
                                                              :host github
                                                              :pre-build
                                                              (("chmod" "775"
                                                                "autogen.sh")
                                                               ("./autogen.sh"))
                                                              :ref
                                                              "e878a0b23aa7bdc87c25e85be09809ea63663484"))
 (cdlatex :source "lockfile" :date (26009 6537 578357 777000) :recipe
                                                              (:package
                                                               "cdlatex"
                                                               :fetcher github
                                                               :repo
                                                               "cdominik/cdlatex"
                                                               :files
                                                               ("*.el" "*.el.in"
                                                                "dir" "*.info"
                                                                "*.texi"
                                                                "*.texinfo"
                                                                "doc/dir"
                                                                "doc/*.info"
                                                                "doc/*.texi"
                                                                "doc/*.texinfo"
                                                                "lisp/*.el"
                                                                (:exclude
                                                                 ".dir-locals.el"
                                                                 "test.el"
                                                                 "tests.el"
                                                                 "*-test.el"
                                                                 "*-tests.el"
                                                                 "LICENSE"
                                                                 "README*"
                                                                 "*-pkg.el"))
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "33770dec73138909714711b05a63e79da5a19ccd"))
 (auctex-latexmk :source "lockfile" :date (26009 6537 576661 941000) :recipe
                                                                     (:package
                                                                      "auctex-latexmk"
                                                                      :fetcher
                                                                      github
                                                                      :repo
                                                                      "emacsmirror/auctex-latexmk"
                                                                      :files
                                                                      ("*.el"
                                                                       "*.el.in"
                                                                       "dir"
                                                                       "*.info"
                                                                       "*.texi"
                                                                       "*.texinfo"
                                                                       "doc/dir"
                                                                       "doc/*.info"
                                                                       "doc/*.texi"
                                                                       "doc/*.texinfo"
                                                                       "lisp/*.el"
                                                                       (:exclude
                                                                        ".dir-locals.el"
                                                                        "test.el"
                                                                        "tests.el"
                                                                        "*-test.el"
                                                                        "*-tests.el"
                                                                        "LICENSE"
                                                                        "README*"
                                                                        "*-pkg.el"))
                                                                      :source
                                                                      "MELPA"
                                                                      :protocol
                                                                      https
                                                                      :inherit t
                                                                      :depth 1
                                                                      :ref
                                                                      "b00a95e6b34c94987fda5a57c20cfe2f064b1c7a"))
 (popweb :source "lockfile" :date (26009 6537 574804 851000) :recipe
                                                             (:source nil
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :type
                                                                           git
                                                              :host github :repo
                                                                           "manateelazycat/popweb"
                                                              :files
                                                              (:defaults "*.py"
                                                               "*.js"
                                                               "extension/*/*")
                                                              :package "popweb"
                                                              :ref
                                                              "ca6262b0f0a44076526457e57056ffb92340e984"))
 (markdown-mode :source "lockfile" :date (26009 6537 573207 8000) :recipe
                                                                  (:package
                                                                   "markdown-mode"
                                                                   :fetcher
                                                                   github :repo
                                                                          "jrblevin/markdown-mode"
                                                                   :files
                                                                   ("*.el"
                                                                    "*.el.in"
                                                                    "dir"
                                                                    "*.info"
                                                                    "*.texi"
                                                                    "*.texinfo"
                                                                    "doc/dir"
                                                                    "doc/*.info"
                                                                    "doc/*.texi"
                                                                    "doc/*.texinfo"
                                                                    "lisp/*.el"
                                                                    (:exclude
                                                                     ".dir-locals.el"
                                                                     "test.el"
                                                                     "tests.el"
                                                                     "*-test.el"
                                                                     "*-tests.el"
                                                                     "LICENSE"
                                                                     "README*"
                                                                     "*-pkg.el"))
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "141f9a05d121f60fe5e411c0ad114e3d3216c9ad"))
 (markdown-xwidget :source "lockfile" :date (26009 6537 571521 321000) :recipe
                                                                       (:source
                                                                        nil
                                                                        :protocol
                                                                        https
                                                                        :inherit
                                                                        t :depth
                                                                          1
                                                                        :type
                                                                        git
                                                                        :host
                                                                        github
                                                                        :repo
                                                                        "cfclrk/markdown-xwidget"
                                                                        :files
                                                                        (:defaults
                                                                         "resources")
                                                                        :package
                                                                        "markdown-xwidget"
                                                                        :ref
                                                                        "f7f6f74729c9604a8a845ca73929822e0066c0ea"))
 (esup :source "lockfile" :date (26009 6537 570167 948000) :recipe
                                                           (:package "esup"
                                                            :fetcher github
                                                            :repo "jschaf/esup"
                                                            :files
                                                            ("*.el" "*.el.in"
                                                             "dir" "*.info"
                                                             "*.texi"
                                                             "*.texinfo"
                                                             "doc/dir"
                                                             "doc/*.info"
                                                             "doc/*.texi"
                                                             "doc/*.texinfo"
                                                             "lisp/*.el"
                                                             (:exclude
                                                              ".dir-locals.el"
                                                              "test.el"
                                                              "tests.el"
                                                              "*-test.el"
                                                              "*-tests.el"
                                                              "LICENSE"
                                                              "README*"
                                                              "*-pkg.el"))
                                                            :source "MELPA"
                                                            :protocol https
                                                            :inherit t :depth 1
                                                            :ref
                                                            "4b49c8d599d4cc0fbf994e9e54a9c78e5ab62a5f"))
 (explain-pause-mode :source "lockfile" :date (26009 6537 568660 475000) :recipe
                                                                         (:source
                                                                          nil
                                                                          :protocol
                                                                          https
                                                                          :inherit
                                                                          t
                                                                          :depth
                                                                          1
                                                                          :type
                                                                          git
                                                                          :host
                                                                          github
                                                                          :repo
                                                                          "lastquestion/explain-pause-mode"
                                                                          :package
                                                                          "explain-pause-mode"
                                                                          :ref
                                                                          "2356c8c3639cbeeb9751744dbe737267849b4b51"))
 (bug-hunter :source "lockfile" :date (26009 6537 567035 542000) :recipe
                                                                 (:package
                                                                  "bug-hunter"
                                                                  :repo
                                                                  "https://github.com/Malabarba/elisp-bug-hunter"
                                                                  :local-repo
                                                                  "bug-hunter"
                                                                  :files
                                                                  ("*"
                                                                   (:exclude
                                                                    ".git"))
                                                                  :source
                                                                  "GNU-devel ELPA"
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "31a2da8fd5825f0938a1cce976baf39805b13e9f"))
 (hl-todo :source "lockfile" :date (26009 6537 565607 889000) :recipe
                                                              (:package
                                                               "hl-todo" :repo
                                                                         "tarsius/hl-todo"
                                                               :fetcher github
                                                               :files
                                                               ("*.el" "*.el.in"
                                                                "dir" "*.info"
                                                                "*.texi"
                                                                "*.texinfo"
                                                                "doc/dir"
                                                                "doc/*.info"
                                                                "doc/*.texi"
                                                                "doc/*.texinfo"
                                                                "lisp/*.el"
                                                                (:exclude
                                                                 ".dir-locals.el"
                                                                 "test.el"
                                                                 "tests.el"
                                                                 "*-test.el"
                                                                 "*-tests.el"
                                                                 "LICENSE"
                                                                 "README*"
                                                                 "*-pkg.el"))
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "7146bbcab5248f3fb9d09acb981b8e63f0c73413"))
 (rainbow-mode :source "lockfile" :date (26009 6537 564646 432000) :recipe
                                                                   (:package
                                                                    "rainbow-mode"
                                                                    :repo
                                                                    "git://git.sv.gnu.org/emacs/elpa"
                                                                    :local-repo
                                                                    "rainbow-mode"
                                                                    :branch
                                                                    "externals/rainbow-mode"
                                                                    :files
                                                                    ("*"
                                                                     (:exclude
                                                                      ".git"))
                                                                    :source
                                                                    "GNU-devel ELPA"
                                                                    :protocol
                                                                    https
                                                                    :inherit t
                                                                    :depth 1
                                                                    :ref
                                                                    "f7db3b5919f70420a91eb199f8663468de3033f3"))
 (highlight-defined :source "lockfile" :date (26009 6537 562890 883000) :recipe
                                                                        (:package
                                                                         "highlight-defined"
                                                                         :fetcher
                                                                         github
                                                                         :repo
                                                                         "Fanael/highlight-defined"
                                                                         :files
                                                                         ("*.el"
                                                                          "*.el.in"
                                                                          "dir"
                                                                          "*.info"
                                                                          "*.texi"
                                                                          "*.texinfo"
                                                                          "doc/dir"
                                                                          "doc/*.info"
                                                                          "doc/*.texi"
                                                                          "doc/*.texinfo"
                                                                          "lisp/*.el"
                                                                          (:exclude
                                                                           ".dir-locals.el"
                                                                           "test.el"
                                                                           "tests.el"
                                                                           "*-test.el"
                                                                           "*-tests.el"
                                                                           "LICENSE"
                                                                           "README*"
                                                                           "*-pkg.el"))
                                                                         :source
                                                                         "MELPA"
                                                                         :protocol
                                                                         https
                                                                         :inherit
                                                                         t
                                                                         :depth
                                                                         1 :ref
                                                                           "4420bdda419875dacb065468aafe273b2022580e"))
 (highlight-quoted :source "lockfile" :date (26009 6537 561761 491000) :recipe
                                                                       (:package
                                                                        "highlight-quoted"
                                                                        :fetcher
                                                                        github
                                                                        :repo
                                                                        "Fanael/highlight-quoted"
                                                                        :files
                                                                        ("*.el"
                                                                         "*.el.in"
                                                                         "dir"
                                                                         "*.info"
                                                                         "*.texi"
                                                                         "*.texinfo"
                                                                         "doc/dir"
                                                                         "doc/*.info"
                                                                         "doc/*.texi"
                                                                         "doc/*.texinfo"
                                                                         "lisp/*.el"
                                                                         (:exclude
                                                                          ".dir-locals.el"
                                                                          "test.el"
                                                                          "tests.el"
                                                                          "*-test.el"
                                                                          "*-tests.el"
                                                                          "LICENSE"
                                                                          "README*"
                                                                          "*-pkg.el"))
                                                                        :source
                                                                        "MELPA"
                                                                        :protocol
                                                                        https
                                                                        :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "24103478158cd19fbcfb4339a3f1fa1f054f1469"))
 (adaptive-wrap :source "lockfile" :date (26009 6537 560389 933000) :recipe
                                                                    (:package
                                                                     "adaptive-wrap"
                                                                     :repo
                                                                     "git://git.sv.gnu.org/emacs/elpa"
                                                                     :local-repo
                                                                     "adaptive-wrap"
                                                                     :branch
                                                                     "externals/adaptive-wrap"
                                                                     :files
                                                                     ("*"
                                                                      (:exclude
                                                                       ".git"))
                                                                     :source
                                                                     "GNU-devel ELPA"
                                                                     :protocol
                                                                     https
                                                                     :inherit t
                                                                     :depth 1
                                                                     :ref
                                                                     "1a3cdaf967dfe1f611b15d177a3f6da7e07624fb"))
 (consult :source "lockfile" :date (26009 6537 558882 351000) :recipe
                                                              (:package
                                                               "consult" :repo
                                                                         "minad/consult"
                                                               :fetcher github
                                                               :files
                                                               ("*.el" "*.el.in"
                                                                "dir" "*.info"
                                                                "*.texi"
                                                                "*.texinfo"
                                                                "doc/dir"
                                                                "doc/*.info"
                                                                "doc/*.texi"
                                                                "doc/*.texinfo"
                                                                "lisp/*.el"
                                                                (:exclude
                                                                 ".dir-locals.el"
                                                                 "test.el"
                                                                 "tests.el"
                                                                 "*-test.el"
                                                                 "*-tests.el"
                                                                 "LICENSE"
                                                                 "README*"
                                                                 "*-pkg.el"))
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "e903d06e270a42f874846af34f3a05b9ff548085"))
 (embark :source "lockfile" :date (26009 6537 557975 166000) :recipe
                                                             (:package "embark"
                                                              :repo
                                                              "oantolin/embark"
                                                              :fetcher github
                                                              :files
                                                              ("embark.el"
                                                               "embark-org.el"
                                                               "embark.texi")
                                                              :source "MELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "b9f2b3b9a5b9c72cf1416097b9941c4f275dae94"))
 (embark-consult :source "lockfile" :date (26009 6537 556488 412000) :recipe
                                                                     (:package
                                                                      "embark-consult"
                                                                      :repo
                                                                      "oantolin/embark"
                                                                      :fetcher
                                                                      github
                                                                      :files
                                                                      ("embark-consult.el")
                                                                      :source
                                                                      "MELPA"
                                                                      :protocol
                                                                      https
                                                                      :inherit t
                                                                      :depth 1
                                                                      :ref
                                                                      "b9f2b3b9a5b9c72cf1416097b9941c4f275dae94"))
 (sudo-edit :source "lockfile" :date (26009 6537 554812 223000) :recipe
                                                                (:package
                                                                 "sudo-edit"
                                                                 :repo
                                                                 "nflath/sudo-edit"
                                                                 :fetcher github
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "74eb1e6986461baed9a9269566ff838530b4379b"))
 (vimrc-mode :source "lockfile" :date (26009 6537 553122 407000) :recipe
                                                                 (:package
                                                                  "vimrc-mode"
                                                                  :fetcher
                                                                  github :repo
                                                                         "mcandre/vimrc-mode"
                                                                  :files
                                                                  ("*.el"
                                                                   "*.el.in"
                                                                   "dir"
                                                                   "*.info"
                                                                   "*.texi"
                                                                   "*.texinfo"
                                                                   "doc/dir"
                                                                   "doc/*.info"
                                                                   "doc/*.texi"
                                                                   "doc/*.texinfo"
                                                                   "lisp/*.el"
                                                                   (:exclude
                                                                    ".dir-locals.el"
                                                                    "test.el"
                                                                    "tests.el"
                                                                    "*-test.el"
                                                                    "*-tests.el"
                                                                    "LICENSE"
                                                                    "README*"
                                                                    "*-pkg.el"))
                                                                  :source
                                                                  "MELPA"
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "13bc150a870d5d4a95f1111e4740e2b22813c30e"))
 (outshine :source "lockfile" :date (26009 6537 552156 843000) :recipe
                                                               (:package
                                                                "outshine"
                                                                :fetcher github
                                                                :repo
                                                                "alphapapa/outshine"
                                                                :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "bf1eed10dd7a89b63d0fc014944033db397c1e23"))
 (all-the-icons-dired :source "lockfile" :date (26009 6537 550815 773000)
                      :recipe
                      (:package "all-the-icons-dired" :repo
                                                      "wyuenho/all-the-icons-dired"
                       :fetcher github :files
                                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                        "*.texinfo" "doc/dir" "doc/*.info"
                                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                        (:exclude ".dir-locals.el" "test.el"
                                         "tests.el" "*-test.el" "*-tests.el"
                                         "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref
                                                                           "e157f0668f22ed586aebe0a2c0186ab07702986c"))
 (dired-open :source "lockfile" :date (26009 6537 549340 320000) :recipe
                                                                 (:package
                                                                  "dired-open"
                                                                  :fetcher
                                                                  github :repo
                                                                         "Fuco1/dired-hacks"
                                                                  :files
                                                                  ("dired-open.el")
                                                                  :source
                                                                  "MELPA"
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "874449d6fc98aee565e1715ec18acec3c1c2cafb"))
 (dired-hide-dotfiles :source "lockfile" :date (26009 6537 547703 495000)
                      :recipe
                      (:package "dired-hide-dotfiles" :fetcher github :repo
                                                                      "mattiasb/dired-hide-dotfiles"
                       :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref
                                                                           "6a379f23f64045f5950d229254ce6f32dbbf5364"))
 (consult-dir :source "lockfile" :date (26009 6537 545956 221000) :recipe
                                                                  (:package
                                                                   "consult-dir"
                                                                   :fetcher
                                                                   github :repo
                                                                          "karthink/consult-dir"
                                                                   :files
                                                                   ("*.el"
                                                                    "*.el.in"
                                                                    "dir"
                                                                    "*.info"
                                                                    "*.texi"
                                                                    "*.texinfo"
                                                                    "doc/dir"
                                                                    "doc/*.info"
                                                                    "doc/*.texi"
                                                                    "doc/*.texinfo"
                                                                    "lisp/*.el"
                                                                    (:exclude
                                                                     ".dir-locals.el"
                                                                     "test.el"
                                                                     "tests.el"
                                                                     "*-test.el"
                                                                     "*-tests.el"
                                                                     "LICENSE"
                                                                     "README*"
                                                                     "*-pkg.el"))
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "3f5f4b71ebe819392cb090cda71bd39a93bd830a"))
 (affe :source "lockfile" :date (26009 6537 544954 849000) :recipe
                                                           (:package "affe"
                                                            :repo "minad/affe"
                                                            :fetcher github
                                                            :files
                                                            ("*.el" "*.el.in"
                                                             "dir" "*.info"
                                                             "*.texi"
                                                             "*.texinfo"
                                                             "doc/dir"
                                                             "doc/*.info"
                                                             "doc/*.texi"
                                                             "doc/*.texinfo"
                                                             "lisp/*.el"
                                                             (:exclude
                                                              ".dir-locals.el"
                                                              "test.el"
                                                              "tests.el"
                                                              "*-test.el"
                                                              "*-tests.el"
                                                              "LICENSE"
                                                              "README*"
                                                              "*-pkg.el"))
                                                            :source "MELPA"
                                                            :protocol https
                                                            :inherit t :depth 1
                                                            :ref
                                                            "1fd5732afa5d68b120fd2e949702b1abde0466d7"))
 (dwim-shell-command :source "lockfile" :date (26009 6537 543652 272000) :recipe
                                                                         (:package
                                                                          "dwim-shell-command"
                                                                          :fetcher
                                                                          github
                                                                          :repo
                                                                          "xenodium/dwim-shell-command"
                                                                          :files
                                                                          ("*.el"
                                                                           "*.el.in"
                                                                           "dir"
                                                                           "*.info"
                                                                           "*.texi"
                                                                           "*.texinfo"
                                                                           "doc/dir"
                                                                           "doc/*.info"
                                                                           "doc/*.texi"
                                                                           "doc/*.texinfo"
                                                                           "lisp/*.el"
                                                                           (:exclude
                                                                            ".dir-locals.el"
                                                                            "test.el"
                                                                            "tests.el"
                                                                            "*-test.el"
                                                                            "*-tests.el"
                                                                            "LICENSE"
                                                                            "README*"
                                                                            "*-pkg.el"))
                                                                          :source
                                                                          "MELPA"
                                                                          :protocol
                                                                          https
                                                                          :inherit
                                                                          t
                                                                          :depth
                                                                          1 :ref
                                                                            "81c64e2015e70e3c2118dcb772d1aacf74416678"))
 (counsel-projectile :source "lockfile" :date (26009 6537 542289 721000) :recipe
                                                                         (:package
                                                                          "counsel-projectile"
                                                                          :fetcher
                                                                          github
                                                                          :repo
                                                                          "ericdanan/counsel-projectile"
                                                                          :files
                                                                          ("*.el"
                                                                           "*.el.in"
                                                                           "dir"
                                                                           "*.info"
                                                                           "*.texi"
                                                                           "*.texinfo"
                                                                           "doc/dir"
                                                                           "doc/*.info"
                                                                           "doc/*.texi"
                                                                           "doc/*.texinfo"
                                                                           "lisp/*.el"
                                                                           (:exclude
                                                                            ".dir-locals.el"
                                                                            "test.el"
                                                                            "tests.el"
                                                                            "*-test.el"
                                                                            "*-tests.el"
                                                                            "LICENSE"
                                                                            "README*"
                                                                            "*-pkg.el"))
                                                                          :source
                                                                          "MELPA"
                                                                          :protocol
                                                                          https
                                                                          :inherit
                                                                          t
                                                                          :depth
                                                                          1 :ref
                                                                            "40d1e1d4bb70acb00fddd6f4df9778bf2c52734b"))
 (project :source "lockfile" :date (26009 6537 540811 283000) :recipe
                                                              (:package
                                                               "project" :repo
                                                                         "https://github.com/emacs-mirror/emacs"
                                                               :local-repo
                                                               "project" :branch
                                                                         "master"
                                                               :files
                                                               ("lisp/progmodes/project.el"
                                                                (:exclude ".git"))
                                                               :source
                                                               "GNU-devel ELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "657275529e31226bbc6c92eb7f7af887474a0bb8"))
 (xref :source "lockfile" :date (26009 6537 539765 678000) :recipe
                                                           (:package "xref"
                                                            :repo
                                                            "https://github.com/emacs-mirror/emacs"
                                                            :local-repo "xref"
                                                            :branch "master"
                                                            :files
                                                            ("lisp/progmodes/xref.el"
                                                             (:exclude ".git"))
                                                            :source
                                                            "GNU-devel ELPA"
                                                            :protocol https
                                                            :inherit t :depth 1
                                                            :ref
                                                            "657275529e31226bbc6c92eb7f7af887474a0bb8"))
 (dumb-jump :source "lockfile" :date (26009 6537 538734 290000) :recipe
                                                                (:package
                                                                 "dumb-jump"
                                                                 :repo
                                                                 "jacktasia/dumb-jump"
                                                                 :fetcher github
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "d9503c157ab88f0ed2fa1301aeb57e95ac564760"))
 (seq :source "lockfile" :date (26009 6537 537094 108000) :recipe
                                                          (:package "seq" :repo
                                                                          "https://git.savannah.gnu.org/git/emacs/elpa.git"
                                                           :local-repo "seq"
                                                           :branch
                                                           "externals/seq"
                                                           :files
                                                           ("*"
                                                            (:exclude ".git"))
                                                           :source
                                                           "GNU-devel ELPA"
                                                           :protocol https
                                                           :inherit t :depth 1
                                                           :type git :host nil
                                                           :ref
                                                           "9d9f51b0e3ca59e0a488801064512f4878ac910b"))
 (magit :source "lockfile" :date (26009 6537 535225 567000) :recipe
                                                            (:package "magit"
                                                             :fetcher github
                                                             :repo "magit/magit"
                                                             :files
                                                             ("lisp/magit*.el"
                                                              "lisp/git-rebase.el"
                                                              "docs/magit.texi"
                                                              "docs/AUTHORS.md"
                                                              "LICENSE"
                                                              "Documentation/magit.texi"
                                                              "Documentation/AUTHORS.md"
                                                              (:exclude
                                                               "lisp/magit-libgit.el"
                                                               "lisp/magit-libgit-pkg.el"
                                                               "lisp/magit-section.el"
                                                               "lisp/magit-section-pkg.el"))
                                                             :source "MELPA"
                                                             :protocol https
                                                             :inherit t :depth 1
                                                             :ref
                                                             "5c652a086e289e5a257745baa9eb7d98ee734516"))
 (magit-lfs :source "lockfile" :date (26009 6537 533451 604000) :recipe
                                                                (:package
                                                                 "magit-lfs"
                                                                 :fetcher github
                                                                 :repo
                                                                 "Ailrun/magit-lfs"
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "cd9f46e1840270be27e2c2d9dcf036ff0781f66d"))
 (forge :source "lockfile" :date (26009 6537 532518 120000) :recipe
                                                            (:package "forge"
                                                             :fetcher github
                                                             :repo "magit/forge"
                                                             :files
                                                             ("*.el" "*.el.in"
                                                              "dir" "*.info"
                                                              "*.texi"
                                                              "*.texinfo"
                                                              "doc/dir"
                                                              "doc/*.info"
                                                              "doc/*.texi"
                                                              "doc/*.texinfo"
                                                              "lisp/*.el"
                                                              (:exclude
                                                               ".dir-locals.el"
                                                               "test.el"
                                                               "tests.el"
                                                               "*-test.el"
                                                               "*-tests.el"
                                                               "LICENSE"
                                                               "README*"
                                                               "*-pkg.el"))
                                                             :source "MELPA"
                                                             :protocol https
                                                             :inherit t :depth 1
                                                             :ref
                                                             "d8974735f010887f79c5fb848606c77290a353e0"))
 (abridge-diff :source "lockfile" :date (26009 6537 530665 358000) :recipe
                                                                   (:package
                                                                    "abridge-diff"
                                                                    :repo
                                                                    "jdtsmith/abridge-diff"
                                                                    :fetcher
                                                                    github
                                                                    :files
                                                                    ("*.el"
                                                                     "*.el.in"
                                                                     "dir"
                                                                     "*.info"
                                                                     "*.texi"
                                                                     "*.texinfo"
                                                                     "doc/dir"
                                                                     "doc/*.info"
                                                                     "doc/*.texi"
                                                                     "doc/*.texinfo"
                                                                     "lisp/*.el"
                                                                     (:exclude
                                                                      ".dir-locals.el"
                                                                      "test.el"
                                                                      "tests.el"
                                                                      "*-test.el"
                                                                      "*-tests.el"
                                                                      "LICENSE"
                                                                      "README*"
                                                                      "*-pkg.el"))
                                                                    :source
                                                                    "MELPA"
                                                                    :protocol
                                                                    https
                                                                    :inherit t
                                                                    :depth 1
                                                                    :ref
                                                                    "31e0ccaa9d0bd4ad257f5de25cc3c0b3395fafa1"))
 (keychain-environment :source "lockfile" :date (26009 6537 529498 796000)
                       :recipe
                       (:package "keychain-environment" :repo
                                                        "tarsius/keychain-environment"
                        :fetcher github :files
                                        ("*.el" "*.el.in" "dir" "*.info"
                                         "*.texi" "*.texinfo" "doc/dir"
                                         "doc/*.info" "doc/*.texi"
                                         "doc/*.texinfo" "lisp/*.el"
                                         (:exclude ".dir-locals.el" "test.el"
                                          "tests.el" "*-test.el" "*-tests.el"
                                          "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref
                                                                            "d3643196de6dc79ea77f9f4805028350fd76100b"))
 (git-gutter :source "lockfile" :date (26009 6537 528287 159000) :recipe
                                                                 (:package
                                                                  "git-gutter"
                                                                  :repo
                                                                  "emacsorphanage/git-gutter"
                                                                  :fetcher
                                                                  github :files
                                                                         ("*.el"
                                                                          "*.el.in"
                                                                          "dir"
                                                                          "*.info"
                                                                          "*.texi"
                                                                          "*.texinfo"
                                                                          "doc/dir"
                                                                          "doc/*.info"
                                                                          "doc/*.texi"
                                                                          "doc/*.texinfo"
                                                                          "lisp/*.el"
                                                                          (:exclude
                                                                           ".dir-locals.el"
                                                                           "test.el"
                                                                           "tests.el"
                                                                           "*-test.el"
                                                                           "*-tests.el"
                                                                           "LICENSE"
                                                                           "README*"
                                                                           "*-pkg.el"))
                                                                  :source
                                                                  "MELPA"
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "4c9e2f22b35918ce1cd9e64bce06a3ceda54095f"))
 (git-gutter-fringe :source "lockfile" :date (26009 6537 526833 768000) :recipe
                                                                        (:package
                                                                         "git-gutter-fringe"
                                                                         :repo
                                                                         "emacsorphanage/git-gutter-fringe"
                                                                         :fetcher
                                                                         github
                                                                         :files
                                                                         ("*.el"
                                                                          "*.el.in"
                                                                          "dir"
                                                                          "*.info"
                                                                          "*.texi"
                                                                          "*.texinfo"
                                                                          "doc/dir"
                                                                          "doc/*.info"
                                                                          "doc/*.texi"
                                                                          "doc/*.texinfo"
                                                                          "lisp/*.el"
                                                                          (:exclude
                                                                           ".dir-locals.el"
                                                                           "test.el"
                                                                           "tests.el"
                                                                           "*-test.el"
                                                                           "*-tests.el"
                                                                           "LICENSE"
                                                                           "README*"
                                                                           "*-pkg.el"))
                                                                         :source
                                                                         "MELPA"
                                                                         :protocol
                                                                         https
                                                                         :inherit
                                                                         t
                                                                         :depth
                                                                         1 :ref
                                                                           "648cb5b57faec55711803cdc9434e55a733c3eba"))
 (diff-hl :source "lockfile" :date (26009 6537 525476 708000) :recipe
                                                              (:package
                                                               "diff-hl"
                                                               :fetcher github
                                                               :repo
                                                               "dgutov/diff-hl"
                                                               :files
                                                               ("*.el" "*.el.in"
                                                                "dir" "*.info"
                                                                "*.texi"
                                                                "*.texinfo"
                                                                "doc/dir"
                                                                "doc/*.info"
                                                                "doc/*.texi"
                                                                "doc/*.texinfo"
                                                                "lisp/*.el"
                                                                (:exclude
                                                                 ".dir-locals.el"
                                                                 "test.el"
                                                                 "tests.el"
                                                                 "*-test.el"
                                                                 "*-tests.el"
                                                                 "LICENSE"
                                                                 "README*"
                                                                 "*-pkg.el"))
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "007c06b094de8e650f52b8ff9cfddf8f81f331d6"))
 (git-timemachine :source "lockfile" :date (26009 6537 523900 767000) :recipe
                                                                      (:package
                                                                       "git-timemachine"
                                                                       :fetcher
                                                                       codeberg
                                                                       :repo
                                                                       "pidu/git-timemachine"
                                                                       :files
                                                                       ("*.el"
                                                                        "*.el.in"
                                                                        "dir"
                                                                        "*.info"
                                                                        "*.texi"
                                                                        "*.texinfo"
                                                                        "doc/dir"
                                                                        "doc/*.info"
                                                                        "doc/*.texi"
                                                                        "doc/*.texinfo"
                                                                        "lisp/*.el"
                                                                        (:exclude
                                                                         ".dir-locals.el"
                                                                         "test.el"
                                                                         "tests.el"
                                                                         "*-test.el"
                                                                         "*-tests.el"
                                                                         "LICENSE"
                                                                         "README*"
                                                                         "*-pkg.el"))
                                                                       :source
                                                                       "MELPA"
                                                                       :protocol
                                                                       https
                                                                       :inherit
                                                                       t :depth
                                                                         1 :ref
                                                                           "ac933e5cd29583c131401f3bd991d98129c316df"))
 (deadgrep :source "lockfile" :date (26009 6537 522029 430000) :recipe
                                                               (:package
                                                                "deadgrep" :repo
                                                                           "Wilfred/deadgrep"
                                                                :fetcher github
                                                                :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "38abe362997d2f18633a75d04c09da751bf8085e"))
 (flycheck :source "lockfile" :date (26009 6537 520553 677000) :recipe
                                                               (:package
                                                                "flycheck" :repo
                                                                           "flycheck/flycheck"
                                                                :fetcher github
                                                                :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "e56e30d8c66ffc9776d07740658d3b542c1a8e21"))
 (consult-flycheck :source "lockfile" :date (26009 6537 519013 132000) :recipe
                                                                       (:package
                                                                        "consult-flycheck"
                                                                        :fetcher
                                                                        github
                                                                        :repo
                                                                        "minad/consult-flycheck"
                                                                        :files
                                                                        ("*.el"
                                                                         "*.el.in"
                                                                         "dir"
                                                                         "*.info"
                                                                         "*.texi"
                                                                         "*.texinfo"
                                                                         "doc/dir"
                                                                         "doc/*.info"
                                                                         "doc/*.texi"
                                                                         "doc/*.texinfo"
                                                                         "lisp/*.el"
                                                                         (:exclude
                                                                          ".dir-locals.el"
                                                                          "test.el"
                                                                          "tests.el"
                                                                          "*-test.el"
                                                                          "*-tests.el"
                                                                          "LICENSE"
                                                                          "README*"
                                                                          "*-pkg.el"))
                                                                        :source
                                                                        "MELPA"
                                                                        :protocol
                                                                        https
                                                                        :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "d83f87581af74f7a2739d8b1b90c37da5ae3d310"))
 (flymake :source "lockfile" :date (26009 6537 517284 353000) :recipe
                                                              (:package
                                                               "flymake" :repo
                                                                         "https://github.com/emacs-mirror/emacs"
                                                               :local-repo
                                                               "flymake" :branch
                                                                         "master"
                                                               :files
                                                               ("lisp/progmodes/flymake.el"
                                                                (:exclude ".git"))
                                                               :source
                                                               "GNU-devel ELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "657275529e31226bbc6c92eb7f7af887474a0bb8"))
 (flymake-collection :source "lockfile" :date (26009 6537 515909 881000) :recipe
                                                                         (:package
                                                                          "flymake-collection"
                                                                          :fetcher
                                                                          github
                                                                          :repo
                                                                          "mohkale/flymake-collection"
                                                                          :files
                                                                          (:defaults
                                                                           "src/*.el"
                                                                           "src/checkers/*.el")
                                                                          :old-names
                                                                          (flymake-rest)
                                                                          :source
                                                                          "MELPA"
                                                                          :protocol
                                                                          https
                                                                          :inherit
                                                                          t
                                                                          :depth
                                                                          1 :ref
                                                                            "852d47f7b4cac7345d40bd16067842e095aee13b"))
 (flymake-flycheck :source "lockfile" :date (26009 6537 514568 149000) :recipe
                                                                       (:package
                                                                        "flymake-flycheck"
                                                                        :fetcher
                                                                        github
                                                                        :repo
                                                                        "purcell/flymake-flycheck"
                                                                        :files
                                                                        ("*.el"
                                                                         "*.el.in"
                                                                         "dir"
                                                                         "*.info"
                                                                         "*.texi"
                                                                         "*.texinfo"
                                                                         "doc/dir"
                                                                         "doc/*.info"
                                                                         "doc/*.texi"
                                                                         "doc/*.texinfo"
                                                                         "lisp/*.el"
                                                                         (:exclude
                                                                          ".dir-locals.el"
                                                                          "test.el"
                                                                          "tests.el"
                                                                          "*-test.el"
                                                                          "*-tests.el"
                                                                          "LICENSE"
                                                                          "README*"
                                                                          "*-pkg.el"))
                                                                        :source
                                                                        "MELPA"
                                                                        :protocol
                                                                        https
                                                                        :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "6c13e1c6ff6790222facf37439cafcacba513322"))
 (package-lint-flymake :source "lockfile" :date (26009 6537 512931 935000)
                       :recipe
                       (:package "package-lint-flymake" :fetcher github :repo
                                                                        "purcell/package-lint"
                        :files ("package-lint-flymake.el") :source "MELPA"
                        :protocol https :inherit t :depth 1 :ref
                                                            "b82deb8d5b0d9515f8d026af183758a069ba6f69"))
 (quickrun :source "lockfile" :date (26009 6537 511980 838000) :recipe
                                                               (:package
                                                                "quickrun" :repo
                                                                           "emacsorphanage/quickrun"
                                                                :fetcher github
                                                                :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "131ce09918e2bfce8953a269bedc9a88fa0ee186"))
 (apheleia :source "lockfile" :date (26009 6537 511090 835000) :recipe
                                                               (:package
                                                                "apheleia"
                                                                :fetcher github
                                                                :repo
                                                                "radian-software/apheleia"
                                                                :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "923cd12108c7230c251bcaaf0699b7bc3c5c7fb4"))
 (devdocs-browser :source "lockfile" :date (26009 6537 510082 771000) :recipe
                                                                      (:package
                                                                       "devdocs-browser"
                                                                       :fetcher
                                                                       github
                                                                       :repo
                                                                       "blahgeek/emacs-devdocs-browser"
                                                                       :files
                                                                       ("*.el"
                                                                        "*.el.in"
                                                                        "dir"
                                                                        "*.info"
                                                                        "*.texi"
                                                                        "*.texinfo"
                                                                        "doc/dir"
                                                                        "doc/*.info"
                                                                        "doc/*.texi"
                                                                        "doc/*.texinfo"
                                                                        "lisp/*.el"
                                                                        (:exclude
                                                                         ".dir-locals.el"
                                                                         "test.el"
                                                                         "tests.el"
                                                                         "*-test.el"
                                                                         "*-tests.el"
                                                                         "LICENSE"
                                                                         "README*"
                                                                         "*-pkg.el"))
                                                                       :source
                                                                       "MELPA"
                                                                       :protocol
                                                                       https
                                                                       :inherit
                                                                       t :depth
                                                                         1 :ref
                                                                           "afc460e687bec4eb947ab85d207778fc3b9b3bbc"))
 (dash-docs :source "lockfile" :date (26009 6537 508656 671000) :recipe
                                                                (:package
                                                                 "dash-docs"
                                                                 :repo
                                                                 "dash-docs-el/dash-docs"
                                                                 :fetcher github
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :type
                                                                          git
                                                                 :host github
                                                                 :fork
                                                                 (:host github
                                                                  :repo
                                                                  "krisbalintona/dash-docs")
                                                                 :ref
                                                                 "29848b6b347ac520f7646c200ed2ec36cea3feda"))
 (treesit-auto :source "lockfile" :date (26009 6537 506880 143000) :recipe
                                                                   (:package
                                                                    "treesit-auto"
                                                                    :fetcher
                                                                    github :repo
                                                                           "renzmann/treesit-auto"
                                                                    :files
                                                                    ("*.el"
                                                                     "*.el.in"
                                                                     "dir"
                                                                     "*.info"
                                                                     "*.texi"
                                                                     "*.texinfo"
                                                                     "doc/dir"
                                                                     "doc/*.info"
                                                                     "doc/*.texi"
                                                                     "doc/*.texinfo"
                                                                     "lisp/*.el"
                                                                     (:exclude
                                                                      ".dir-locals.el"
                                                                      "test.el"
                                                                      "tests.el"
                                                                      "*-test.el"
                                                                      "*-tests.el"
                                                                      "LICENSE"
                                                                      "README*"
                                                                      "*-pkg.el"))
                                                                    :source
                                                                    "MELPA"
                                                                    :protocol
                                                                    https
                                                                    :inherit t
                                                                    :depth 1
                                                                    :ref
                                                                    "07a8f924cd4f020a2eb32b45d8543af9556f355d"))
 (eglot :source "lockfile" :date (26009 6537 505424 798000) :recipe
                                                            (:package "eglot"
                                                             :repo
                                                             "https://github.com/emacs-mirror/emacs"
                                                             :local-repo "eglot"
                                                             :branch "master"
                                                             :files
                                                             ("lisp/progmodes/eglot.el"
                                                              "doc/emacs/doclicense.texi"
                                                              "doc/emacs/docstyle.texi"
                                                              "doc/misc/eglot.texi"
                                                              "etc/EGLOT-NEWS"
                                                              (:exclude ".git"))
                                                             :source
                                                             "GNU-devel ELPA"
                                                             :protocol https
                                                             :inherit t :depth 1
                                                             :ref
                                                             "657275529e31226bbc6c92eb7f7af887474a0bb8"))
 (eglot-java :source "lockfile" :date (26009 6537 504206 98000) :recipe
                                                                (:package
                                                                 "eglot-java"
                                                                 :repo
                                                                 "yveszoundi/eglot-java"
                                                                 :fetcher github
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "dfd27558f8654725a23354f20d128af8616d3540"))
 (consult-eglot :source "lockfile" :date (26009 6537 502785 58000) :recipe
                                                                   (:package
                                                                    "consult-eglot"
                                                                    :fetcher
                                                                    github :repo
                                                                           "mohkale/consult-eglot"
                                                                    :files
                                                                    ("*.el"
                                                                     "*.el.in"
                                                                     "dir"
                                                                     "*.info"
                                                                     "*.texi"
                                                                     "*.texinfo"
                                                                     "doc/dir"
                                                                     "doc/*.info"
                                                                     "doc/*.texi"
                                                                     "doc/*.texinfo"
                                                                     "lisp/*.el"
                                                                     (:exclude
                                                                      ".dir-locals.el"
                                                                      "test.el"
                                                                      "tests.el"
                                                                      "*-test.el"
                                                                      "*-tests.el"
                                                                      "LICENSE"
                                                                      "README*"
                                                                      "*-pkg.el"))
                                                                    :source
                                                                    "MELPA"
                                                                    :protocol
                                                                    https
                                                                    :inherit t
                                                                    :depth 1
                                                                    :ref
                                                                    "049c6319b8a48ff66189d49592c7759f0b356596"))
 (lsp-mode :source "lockfile" :date (26009 6537 501731 819000) :recipe
                                                               (:package
                                                                "lsp-mode" :repo
                                                                           "emacs-lsp/lsp-mode"
                                                                :fetcher github
                                                                :files
                                                                (:defaults
                                                                 "clients/*.el")
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "57cd9bf153c5ac4ef49d8833aeac77ffa7ec8ea9"))
 (lsp-ui :source "lockfile" :date (26009 6537 500320 76000) :recipe
                                                            (:package "lsp-ui"
                                                             :repo
                                                             "emacs-lsp/lsp-ui"
                                                             :fetcher github
                                                             :files
                                                             (:defaults
                                                              "lsp-ui-doc.html"
                                                              "resources")
                                                             :source "MELPA"
                                                             :protocol https
                                                             :inherit t :depth 1
                                                             :ref
                                                             "0dd39900c8ed8145d207985cb2f65cedd1ffb410"))
 (consult-lsp :source "lockfile" :date (26009 6537 498546 994000) :recipe
                                                                  (:package
                                                                   "consult-lsp"
                                                                   :fetcher
                                                                   github :repo
                                                                          "gagbo/consult-lsp"
                                                                   :files
                                                                   ("*.el"
                                                                    "*.el.in"
                                                                    "dir"
                                                                    "*.info"
                                                                    "*.texi"
                                                                    "*.texinfo"
                                                                    "doc/dir"
                                                                    "doc/*.info"
                                                                    "doc/*.texi"
                                                                    "doc/*.texinfo"
                                                                    "lisp/*.el"
                                                                    (:exclude
                                                                     ".dir-locals.el"
                                                                     "test.el"
                                                                     "tests.el"
                                                                     "*-test.el"
                                                                     "*-tests.el"
                                                                     "LICENSE"
                                                                     "README*"
                                                                     "*-pkg.el"))
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "f8db3252c0daa41225ba4ed1c0d178b281cd3e90"))
 (lsp-treemacs :source "lockfile" :date (26009 6537 497222 876000) :recipe
                                                                   (:package
                                                                    "lsp-treemacs"
                                                                    :repo
                                                                    "emacs-lsp/lsp-treemacs"
                                                                    :fetcher
                                                                    github
                                                                    :files
                                                                    (:defaults
                                                                     "icons")
                                                                    :source
                                                                    "MELPA"
                                                                    :protocol
                                                                    https
                                                                    :inherit t
                                                                    :depth 1
                                                                    :ref
                                                                    "e66ae2196503d4e84334519e56b4388feffa5060"))
 (lsp-bridge :source "lockfile" :date (26009 6537 496246 481000) :recipe
                                                                 (:source nil
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1
                                                                  :type git
                                                                  :host github
                                                                  :repo
                                                                  "manateelazycat/lsp-bridge"
                                                                  :files
                                                                  (:defaults
                                                                   "*.py"
                                                                   "langserver"
                                                                   "acm")
                                                                  :package
                                                                  "lsp-bridge"
                                                                  :ref
                                                                  "dfc5c3210ec1d6c421ef38e51e30f568ad4a3a54"))
 (realgud :source "lockfile" :date (26009 6537 495285 576000) :recipe
                                                              (:package
                                                               "realgud"
                                                               :fetcher github
                                                               :repo
                                                               "realgud/realgud"
                                                               :files
                                                               ("realgud.el"
                                                                "realgud/.nosearch"
                                                                "realgud-recursive-autoloads.el"
                                                                ("realgud/common"
                                                                 "realgud/common/*.el")
                                                                ("realgud/common/buffer"
                                                                 "realgud/common/buffer/*.el")
                                                                ("realgud/debugger/bashdb"
                                                                 "realgud/debugger/bashdb/*.el")
                                                                ("realgud/debugger/gdb"
                                                                 "realgud/debugger/gdb/*.el")
                                                                ("realgud/debugger/gub"
                                                                 "realgud/debugger/gub/*.el")
                                                                ("realgud/debugger/ipdb"
                                                                 "realgud/debugger/ipdb/*.el")
                                                                ("realgud/debugger/jdb"
                                                                 "realgud/debugger/jdb/*.el")
                                                                ("realgud/debugger/kshdb"
                                                                 "realgud/debugger/kshdb/*.el")
                                                                ("realgud/debugger/nodejs"
                                                                 "realgud/debugger/nodejs/*.el")
                                                                ("realgud/debugger/pdb"
                                                                 "realgud/debugger/pdb/*.el")
                                                                ("realgud/debugger/perldb"
                                                                 "realgud/debugger/perldb/*.el")
                                                                ("realgud/debugger/rdebug"
                                                                 "realgud/debugger/rdebug/*.el")
                                                                ("realgud/debugger/remake"
                                                                 "realgud/debugger/remake/*.el")
                                                                ("realgud/debugger/trepan"
                                                                 "realgud/debugger/trepan/*.el")
                                                                ("realgud/debugger/trepan.pl"
                                                                 "realgud/debugger/trepan.pl/*.el")
                                                                ("realgud/debugger/trepan2"
                                                                 "realgud/debugger/trepan2/*.el")
                                                                ("realgud/debugger/trepan3k"
                                                                 "realgud/debugger/trepan3k/*.el")
                                                                ("realgud/debugger/trepanjs"
                                                                 "realgud/debugger/trepanjs/*.el")
                                                                ("realgud/debugger/zshdb"
                                                                 "realgud/debugger/zshdb/*.el")
                                                                ("realgud/lang"
                                                                 "realgud/lang/*.el"))
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "365063ea8ce8ec6a852cb388088d84147421c3c2"))
 (dap-mode :source "lockfile" :date (26009 6537 494255 650000) :recipe
                                                               (:package
                                                                "dap-mode" :repo
                                                                           "emacs-lsp/dap-mode"
                                                                :fetcher github
                                                                :files
                                                                (:defaults
                                                                 "icons")
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "038521a572e162fb5fc1744fe06816f93065c50a"))
 (lisp-keyword-indent :source "lockfile" :date (26009 6537 493183 506000)
                      :recipe
                      (:source nil :protocol https :inherit t :depth 1 :type git
                       :host github :repo "twlz0ne/lisp-keyword-indent.el"
                       :package "lisp-keyword-indent" :ref
                                                      "2b47b219c78a903bd60dd11c8e3642c1368322ff"))
 (eros :source "lockfile" :date (26009 6537 491543 654000) :recipe
                                                           (:package "eros"
                                                            :fetcher github
                                                            :repo "xiongtx/eros"
                                                            :files
                                                            ("*.el" "*.el.in"
                                                             "dir" "*.info"
                                                             "*.texi"
                                                             "*.texinfo"
                                                             "doc/dir"
                                                             "doc/*.info"
                                                             "doc/*.texi"
                                                             "doc/*.texinfo"
                                                             "lisp/*.el"
                                                             (:exclude
                                                              ".dir-locals.el"
                                                              "test.el"
                                                              "tests.el"
                                                              "*-test.el"
                                                              "*-tests.el"
                                                              "LICENSE"
                                                              "README*"
                                                              "*-pkg.el"))
                                                            :source "MELPA"
                                                            :protocol https
                                                            :inherit t :depth 1
                                                            :ref
                                                            "a9a92bdc6be0521a6a06eb464be55ed61946639c"))
 (rainbow-delimiters :source "lockfile" :date (26009 6537 489662 219000) :recipe
                                                                         (:package
                                                                          "rainbow-delimiters"
                                                                          :fetcher
                                                                          github
                                                                          :repo
                                                                          "Fanael/rainbow-delimiters"
                                                                          :files
                                                                          ("*.el"
                                                                           "*.el.in"
                                                                           "dir"
                                                                           "*.info"
                                                                           "*.texi"
                                                                           "*.texinfo"
                                                                           "doc/dir"
                                                                           "doc/*.info"
                                                                           "doc/*.texi"
                                                                           "doc/*.texinfo"
                                                                           "lisp/*.el"
                                                                           (:exclude
                                                                            ".dir-locals.el"
                                                                            "test.el"
                                                                            "tests.el"
                                                                            "*-test.el"
                                                                            "*-tests.el"
                                                                            "LICENSE"
                                                                            "README*"
                                                                            "*-pkg.el"))
                                                                          :source
                                                                          "MELPA"
                                                                          :protocol
                                                                          https
                                                                          :inherit
                                                                          t
                                                                          :depth
                                                                          1 :ref
                                                                            "f40ece58df8b2f0fb6c8576b527755a552a5e763"))
 (help-find :source "lockfile" :date (26009 6537 488216 181000) :recipe
                                                                (:package
                                                                 "help-find"
                                                                 :fetcher github
                                                                 :repo
                                                                 "duncanburke/help-find"
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "ef7266fc480367c12bff64817c875af940d0c9c0"))
 (elisp-demos :source "lockfile" :date (26009 6537 487176 528000) :recipe
                                                                  (:package
                                                                   "elisp-demos"
                                                                   :fetcher
                                                                   github :repo
                                                                          "xuchunyang/elisp-demos"
                                                                   :files
                                                                   (:defaults
                                                                    "*.org")
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "8d0cd806b109076e6c4383edf59dbab9435dc5dc"))
 (suggest :source "lockfile" :date (26009 6537 485846 388000) :recipe
                                                              (:package
                                                               "suggest" :repo
                                                                         "Wilfred/suggest.el"
                                                               :fetcher github
                                                               :files
                                                               ("*.el" "*.el.in"
                                                                "dir" "*.info"
                                                                "*.texi"
                                                                "*.texinfo"
                                                                "doc/dir"
                                                                "doc/*.info"
                                                                "doc/*.texi"
                                                                "doc/*.texinfo"
                                                                "lisp/*.el"
                                                                (:exclude
                                                                 ".dir-locals.el"
                                                                 "test.el"
                                                                 "tests.el"
                                                                 "*-test.el"
                                                                 "*-tests.el"
                                                                 "LICENSE"
                                                                 "README*"
                                                                 "*-pkg.el"))
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "999ab73a3e546bd8d30f0de2408c511abe43135a"))
 (ssh-config-mode :source "lockfile" :date (26009 6537 484149 930000) :recipe
                                                                      (:package
                                                                       "ssh-config-mode"
                                                                       :fetcher
                                                                       github
                                                                       :repo
                                                                       "jhgorrell/ssh-config-mode-el"
                                                                       :files
                                                                       (:defaults
                                                                        "*.txt")
                                                                       :source
                                                                       "MELPA"
                                                                       :protocol
                                                                       https
                                                                       :inherit
                                                                       t :depth
                                                                         1 :ref
                                                                           "d560a0876a93ad4130baf33dae1b9405ad37a405"))
 (lua-mode :source "lockfile" :date (26009 6537 482438 955000) :recipe
                                                               (:package
                                                                "lua-mode" :repo
                                                                           "immerrr/lua-mode"
                                                                :fetcher github
                                                                :files
                                                                (:defaults
                                                                 (:exclude
                                                                  "init-tryout.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "d074e4134b1beae9ed4c9b512af741ca0d852ba3"))
 (company-lua :source "lockfile" :date (26009 6537 481099 659000) :recipe
                                                                  (:package
                                                                   "company-lua"
                                                                   :fetcher
                                                                   github :repo
                                                                          "ptrv/company-lua"
                                                                   :files
                                                                   (:defaults
                                                                    "lua")
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "29f6819de4d691e5fd0b62893a9f4fbc1c6fcb52"))
 (web-mode :source "lockfile" :date (26009 6537 479514 710000) :recipe
                                                               (:package
                                                                "web-mode" :repo
                                                                           "fxbois/web-mode"
                                                                :fetcher github
                                                                :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "82847071ce93293bdb7945db08d970f13fd883cf"))
 (js2-mode :source "lockfile" :date (26009 6537 477694 350000) :recipe
                                                               (:package
                                                                "js2-mode" :repo
                                                                           "mooz/js2-mode"
                                                                :fetcher github
                                                                :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "ca0af32eea0281322a9ce268d98f01fbb88bbb7a"))
 (json-mode :source "lockfile" :date (26009 6537 476238 394000) :recipe
                                                                (:package
                                                                 "json-mode"
                                                                 :fetcher github
                                                                 :repo
                                                                 "json-emacs/json-mode"
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "a93a0c76784376fbb9105719f25c7489991056a1"))
 (yaml-mode :source "lockfile" :date (26009 6537 475163 905000) :recipe
                                                                (:package
                                                                 "yaml-mode"
                                                                 :repo
                                                                 "yoshiki/yaml-mode"
                                                                 :fetcher github
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "5b58248ab255dff6cfa4c4057a191bc4446ee5b6"))
 (python :source "lockfile" :date (26009 6537 474003 694000) :recipe
                                                             (:package "python"
                                                              :repo
                                                              "https://github.com/emacs-mirror/emacs"
                                                              :local-repo
                                                              "python" :branch
                                                                       "master"
                                                              :files
                                                              ("lisp/progmodes/python.el"
                                                               (:exclude ".git"))
                                                              :source
                                                              "GNU-devel ELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "657275529e31226bbc6c92eb7f7af887474a0bb8"))
 (lsp-pyright :source "lockfile" :date (26009 6537 472351 900000) :recipe
                                                                  (:package
                                                                   "lsp-pyright"
                                                                   :repo
                                                                   "emacs-lsp/lsp-pyright"
                                                                   :fetcher
                                                                   github :files
                                                                          ("*.el"
                                                                           "*.el.in"
                                                                           "dir"
                                                                           "*.info"
                                                                           "*.texi"
                                                                           "*.texinfo"
                                                                           "doc/dir"
                                                                           "doc/*.info"
                                                                           "doc/*.texi"
                                                                           "doc/*.texinfo"
                                                                           "lisp/*.el"
                                                                           (:exclude
                                                                            ".dir-locals.el"
                                                                            "test.el"
                                                                            "tests.el"
                                                                            "*-test.el"
                                                                            "*-tests.el"
                                                                            "LICENSE"
                                                                            "README*"
                                                                            "*-pkg.el"))
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "2f2631ae242d5770dbe6cb924e44c1ee5671789d"))
 (python-pytest :source "lockfile" :date (26009 6537 471361 119000) :recipe
                                                                    (:package
                                                                     "python-pytest"
                                                                     :fetcher
                                                                     github
                                                                     :repo
                                                                     "wbolster/emacs-python-pytest"
                                                                     :files
                                                                     ("*.el"
                                                                      "*.el.in"
                                                                      "dir"
                                                                      "*.info"
                                                                      "*.texi"
                                                                      "*.texinfo"
                                                                      "doc/dir"
                                                                      "doc/*.info"
                                                                      "doc/*.texi"
                                                                      "doc/*.texinfo"
                                                                      "lisp/*.el"
                                                                      (:exclude
                                                                       ".dir-locals.el"
                                                                       "test.el"
                                                                       "tests.el"
                                                                       "*-test.el"
                                                                       "*-tests.el"
                                                                       "LICENSE"
                                                                       "README*"
                                                                       "*-pkg.el"))
                                                                     :source
                                                                     "MELPA"
                                                                     :protocol
                                                                     https
                                                                     :inherit t
                                                                     :depth 1
                                                                     :ref
                                                                     "bdfb3e81eedc6b76ed0c5f77079e7cc8adff7b00"))
 (lsp-java :source "lockfile" :date (26009 6537 470137 489000) :recipe
                                                               (:package
                                                                "lsp-java" :repo
                                                                           "emacs-lsp/lsp-java"
                                                                :fetcher github
                                                                :files
                                                                (:defaults
                                                                 "icons")
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "449673da7221a30f1b1756cedcc48b9a2b52a51e"))
 (helm-lsp :source "lockfile" :date (26009 6537 468094 300000) :recipe
                                                               (:package
                                                                "helm-lsp" :repo
                                                                           "emacs-lsp/helm-lsp"
                                                                :fetcher github
                                                                :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "c2c6974dadfac459b1a69a1217441283874cea92"))
 (gdb-bp-session :source "lockfile" :date (26009 6537 466545 690000) :recipe
                                                                     (:source
                                                                      nil
                                                                      :protocol
                                                                      https
                                                                      :inherit t
                                                                      :depth 1
                                                                      :type git
                                                                      :host
                                                                      github
                                                                      :repo
                                                                      "emacsmirror/gdb-bp-session"
                                                                      :package
                                                                      "gdb-bp-session"
                                                                      :ref
                                                                      "5c566b00407dd527bb8f95634aec11e490ea5b75"))
 (rustic :source "lockfile" :date (26009 6537 465597 398000) :recipe
                                                             (:package "rustic"
                                                              :repo
                                                              "brotzeit/rustic"
                                                              :fetcher github
                                                              :files
                                                              ("*.el" "*.el.in"
                                                               "dir" "*.info"
                                                               "*.texi"
                                                               "*.texinfo"
                                                               "doc/dir"
                                                               "doc/*.info"
                                                               "doc/*.texi"
                                                               "doc/*.texinfo"
                                                               "lisp/*.el"
                                                               (:exclude
                                                                ".dir-locals.el"
                                                                "test.el"
                                                                "tests.el"
                                                                "*-test.el"
                                                                "*-tests.el"
                                                                "LICENSE"
                                                                "README*"
                                                                "*-pkg.el"))
                                                              :source "MELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "39423d1cf4fa054c36bf9577356451f4c06ee148"))
 (haskell-mode :source "lockfile" :date (26009 6537 464229 67000) :recipe
                                                                  (:package
                                                                   "haskell-mode"
                                                                   :repo
                                                                   "haskell/haskell-mode"
                                                                   :fetcher
                                                                   github :files
                                                                          (:defaults
                                                                           "NEWS"
                                                                           "logo.svg")
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "79eaf444a72109f93f552abb53f834cc63bbf9f2"))
 (lsp-haskell :source "lockfile" :date (26009 6537 463199 753000) :recipe
                                                                  (:package
                                                                   "lsp-haskell"
                                                                   :repo
                                                                   "emacs-lsp/lsp-haskell"
                                                                   :fetcher
                                                                   github :files
                                                                          ("*.el"
                                                                           "*.el.in"
                                                                           "dir"
                                                                           "*.info"
                                                                           "*.texi"
                                                                           "*.texinfo"
                                                                           "doc/dir"
                                                                           "doc/*.info"
                                                                           "doc/*.texi"
                                                                           "doc/*.texinfo"
                                                                           "lisp/*.el"
                                                                           (:exclude
                                                                            ".dir-locals.el"
                                                                            "test.el"
                                                                            "tests.el"
                                                                            "*-test.el"
                                                                            "*-tests.el"
                                                                            "LICENSE"
                                                                            "README*"
                                                                            "*-pkg.el"))
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "89d16370434e9a247e95b8b701f524f5abfc884b"))
 (topsy :source "lockfile" :date (26009 6537 461462 88000) :recipe
                                                           (:package "topsy"
                                                            :fetcher github
                                                            :repo
                                                            "alphapapa/topsy.el"
                                                            :files
                                                            ("*.el" "*.el.in"
                                                             "dir" "*.info"
                                                             "*.texi"
                                                             "*.texinfo"
                                                             "doc/dir"
                                                             "doc/*.info"
                                                             "doc/*.texi"
                                                             "doc/*.texinfo"
                                                             "lisp/*.el"
                                                             (:exclude
                                                              ".dir-locals.el"
                                                              "test.el"
                                                              "tests.el"
                                                              "*-test.el"
                                                              "*-tests.el"
                                                              "LICENSE"
                                                              "README*"
                                                              "*-pkg.el"))
                                                            :source "MELPA"
                                                            :protocol https
                                                            :inherit t :depth 1
                                                            :ref
                                                            "8b6c6d5026ac72b4c3704ed7bb8fafe1ea343699"))
 (fish-mode :source "lockfile" :date (26009 6537 460424 418000) :recipe
                                                                (:package
                                                                 "fish-mode"
                                                                 :fetcher github
                                                                 :repo
                                                                 "wwwjfy/emacs-fish"
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "6869efbc05cdb628209338bf57fe52742e53d956"))
 (shrink-path :source "lockfile" :date (26009 6537 459233 99000) :recipe
                                                                 (:package
                                                                  "shrink-path"
                                                                  :fetcher
                                                                  gitlab :repo
                                                                         "bennya/shrink-path.el"
                                                                  :files
                                                                  ("*.el"
                                                                   "*.el.in"
                                                                   "dir"
                                                                   "*.info"
                                                                   "*.texi"
                                                                   "*.texinfo"
                                                                   "doc/dir"
                                                                   "doc/*.info"
                                                                   "doc/*.texi"
                                                                   "doc/*.texinfo"
                                                                   "lisp/*.el"
                                                                   (:exclude
                                                                    ".dir-locals.el"
                                                                    "test.el"
                                                                    "tests.el"
                                                                    "*-test.el"
                                                                    "*-tests.el"
                                                                    "LICENSE"
                                                                    "README*"
                                                                    "*-pkg.el"))
                                                                  :source
                                                                  "MELPA"
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "c14882c8599aec79a6e8ef2d06454254bb3e1e41"))
 (eshell-syntax-highlighting :source "lockfile" :date (26009 6537 456988 401000)
                             :recipe
                             (:package "eshell-syntax-highlighting" :fetcher
                                                                    github :repo
                                                                           "akreisher/eshell-syntax-highlighting"
                              :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                               "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el"
                               (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                              :source "MELPA" :protocol https :inherit t :depth
                                                                         1 :ref
                                                                           "4ac27eec6595ba116a6151dfaf0b0e0440101e10"))
 (pcmpl-args :source "lockfile" :date (26009 6537 455260 233000) :recipe
                                                                 (:package
                                                                  "pcmpl-args"
                                                                  :fetcher
                                                                  github :repo
                                                                         "JonWaltman/pcmpl-args.el"
                                                                  :files
                                                                  ("*.el"
                                                                   "*.el.in"
                                                                   "dir"
                                                                   "*.info"
                                                                   "*.texi"
                                                                   "*.texinfo"
                                                                   "doc/dir"
                                                                   "doc/*.info"
                                                                   "doc/*.texi"
                                                                   "doc/*.texinfo"
                                                                   "lisp/*.el"
                                                                   (:exclude
                                                                    ".dir-locals.el"
                                                                    "test.el"
                                                                    "tests.el"
                                                                    "*-test.el"
                                                                    "*-tests.el"
                                                                    "LICENSE"
                                                                    "README*"
                                                                    "*-pkg.el"))
                                                                  :source
                                                                  "MELPA"
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "43229e1096f89c277190f09a3d794781f8fa0015"))
 (vterm :source "lockfile" :date (26009 6537 453691 606000) :recipe
                                                            (:package "vterm"
                                                             :fetcher github
                                                             :repo
                                                             "akermu/emacs-libvterm"
                                                             :files
                                                             ("CMakeLists.txt"
                                                              "elisp.c"
                                                              "elisp.h"
                                                              "emacs-module.h"
                                                              "etc" "utf8.c"
                                                              "utf8.h"
                                                              "vterm.el"
                                                              "vterm-module.c"
                                                              "vterm-module.h")
                                                             :source "MELPA"
                                                             :protocol https
                                                             :inherit t :depth 1
                                                             :ref
                                                             "c3a3a23a5eace137947524c93644204bf6b56cff"))
 (notmuch-indicator :source "lockfile" :date (26009 6537 452302 436000) :recipe
                                                                        (:package
                                                                         "notmuch-indicator"
                                                                         :repo
                                                                         "https://git.sr.ht/~protesilaos/notmuch-indicator"
                                                                         :local-repo
                                                                         "notmuch-indicator"
                                                                         :files
                                                                         ("*"
                                                                          (:exclude
                                                                           ".git"
                                                                           "COPYING"
                                                                           "doclicense.texi"))
                                                                         :source
                                                                         "GNU-devel ELPA"
                                                                         :protocol
                                                                         https
                                                                         :inherit
                                                                         t
                                                                         :depth
                                                                         1 :ref
                                                                           "72abc677a01008010c3d1ceafed824be253bd7e7"))
 (mu4e-column-faces :source "lockfile" :date (26009 6537 451214 852000) :recipe
                                                                        (:package
                                                                         "mu4e-column-faces"
                                                                         :repo
                                                                         "Alexander-Miller/mu4e-column-faces"
                                                                         :fetcher
                                                                         github
                                                                         :files
                                                                         ("*.el"
                                                                          "*.el.in"
                                                                          "dir"
                                                                          "*.info"
                                                                          "*.texi"
                                                                          "*.texinfo"
                                                                          "doc/dir"
                                                                          "doc/*.info"
                                                                          "doc/*.texi"
                                                                          "doc/*.texinfo"
                                                                          "lisp/*.el"
                                                                          (:exclude
                                                                           ".dir-locals.el"
                                                                           "test.el"
                                                                           "tests.el"
                                                                           "*-test.el"
                                                                           "*-tests.el"
                                                                           "LICENSE"
                                                                           "README*"
                                                                           "*-pkg.el"))
                                                                         :source
                                                                         "MELPA"
                                                                         :protocol
                                                                         https
                                                                         :inherit
                                                                         t
                                                                         :depth
                                                                         1 :ref
                                                                           "1bbb646ea07deb1bd2daa4c6eb36e0f65aac40b0"))
 (org-msg :source "lockfile" :date (26009 6537 449866 248000) :recipe
                                                              (:package
                                                               "org-msg" :repo
                                                                         "jeremy-compostella/org-msg"
                                                               :fetcher github
                                                               :files
                                                               ("*.el" "*.el.in"
                                                                "dir" "*.info"
                                                                "*.texi"
                                                                "*.texinfo"
                                                                "doc/dir"
                                                                "doc/*.info"
                                                                "doc/*.texi"
                                                                "doc/*.texinfo"
                                                                "lisp/*.el"
                                                                (:exclude
                                                                 ".dir-locals.el"
                                                                 "test.el"
                                                                 "tests.el"
                                                                 "*-test.el"
                                                                 "*-tests.el"
                                                                 "LICENSE"
                                                                 "README*"
                                                                 "*-pkg.el"))
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1
                                                               :type git :host
                                                                         github
                                                               :ref
                                                               "055de4abf611c5d5e12c770fe149c1861b402817"))
 (mu4e-send-delay :source "lockfile" :date (26009 6537 448267 303000) :recipe
                                                                      (:source
                                                                       nil
                                                                       :protocol
                                                                       ssh
                                                                       :inherit
                                                                       t :depth
                                                                         nil
                                                                       :type git
                                                                       :host
                                                                       github
                                                                       :repo
                                                                       "krisbalintona/mu4e-send-delay"
                                                                       :package
                                                                       "mu4e-send-delay"
                                                                       :ref
                                                                       "e09019480a946d617f452f13460229552c364300"))
 (elfeed :source "lockfile" :date (26009 6537 446634 856000) :recipe
                                                             (:package "elfeed"
                                                              :repo
                                                              "skeeto/elfeed"
                                                              :fetcher github
                                                              :files
                                                              (:defaults
                                                               "README.md")
                                                              :source "MELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "55fb162fa27e71b88effa59a83c57842e262b00f"))
 (elfeed-org :source "lockfile" :date (26009 6537 445713 94000) :recipe
                                                                (:package
                                                                 "elfeed-org"
                                                                 :repo
                                                                 "remyhonig/elfeed-org"
                                                                 :fetcher github
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "d62d23e25c5e3be3d70b7fbe1eaeb6e43f93a061"))
 (popwin :source "lockfile" :date (26009 6537 444606 514000) :recipe
                                                             (:package "popwin"
                                                              :fetcher github
                                                              :repo
                                                              "emacsorphanage/popwin"
                                                              :files
                                                              ("*.el" "*.el.in"
                                                               "dir" "*.info"
                                                               "*.texi"
                                                               "*.texinfo"
                                                               "doc/dir"
                                                               "doc/*.info"
                                                               "doc/*.texi"
                                                               "doc/*.texinfo"
                                                               "lisp/*.el"
                                                               (:exclude
                                                                ".dir-locals.el"
                                                                "test.el"
                                                                "tests.el"
                                                                "*-test.el"
                                                                "*-tests.el"
                                                                "LICENSE"
                                                                "README*"
                                                                "*-pkg.el"))
                                                              :source "MELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "b64499bb29663886b8f09f308eac609ce03fa601"))
 (elfeed-goodies :source "lockfile" :date (26009 6537 442772 768000) :recipe
                                                                     (:package
                                                                      "elfeed-goodies"
                                                                      :repo
                                                                      "jeetelongname/elfeed-goodies"
                                                                      :fetcher
                                                                      github
                                                                      :files
                                                                      ("*.el"
                                                                       "*.el.in"
                                                                       "dir"
                                                                       "*.info"
                                                                       "*.texi"
                                                                       "*.texinfo"
                                                                       "doc/dir"
                                                                       "doc/*.info"
                                                                       "doc/*.texi"
                                                                       "doc/*.texinfo"
                                                                       "lisp/*.el"
                                                                       (:exclude
                                                                        ".dir-locals.el"
                                                                        "test.el"
                                                                        "tests.el"
                                                                        "*-test.el"
                                                                        "*-tests.el"
                                                                        "LICENSE"
                                                                        "README*"
                                                                        "*-pkg.el"))
                                                                      :source
                                                                      "MELPA"
                                                                      :protocol
                                                                      https
                                                                      :inherit t
                                                                      :depth 1
                                                                      :ref
                                                                      "544ef42ead011d960a0ad1c1d34df5d222461a6b"))
 (language-detection :source "lockfile" :date (26009 6537 441691 496000) :recipe
                                                                         (:package
                                                                          "language-detection"
                                                                          :fetcher
                                                                          github
                                                                          :repo
                                                                          "andreasjansson/language-detection.el"
                                                                          :files
                                                                          ("*.el"
                                                                           "*.el.in"
                                                                           "dir"
                                                                           "*.info"
                                                                           "*.texi"
                                                                           "*.texinfo"
                                                                           "doc/dir"
                                                                           "doc/*.info"
                                                                           "doc/*.texi"
                                                                           "doc/*.texinfo"
                                                                           "lisp/*.el"
                                                                           (:exclude
                                                                            ".dir-locals.el"
                                                                            "test.el"
                                                                            "tests.el"
                                                                            "*-test.el"
                                                                            "*-tests.el"
                                                                            "LICENSE"
                                                                            "README*"
                                                                            "*-pkg.el"))
                                                                          :source
                                                                          "MELPA"
                                                                          :protocol
                                                                          https
                                                                          :inherit
                                                                          t
                                                                          :depth
                                                                          1 :ref
                                                                            "54a6ecf55304fba7d215ef38a4ec96daff2f35a4"))
 (nov :source "lockfile" :date (26009 6537 440183 182000) :recipe
                                                          (:package "nov"
                                                           :fetcher git :url
                                                                        "https://depp.brause.cc/nov.el.git"
                                                           :files
                                                           ("*.el" "*.el.in"
                                                            "dir" "*.info"
                                                            "*.texi" "*.texinfo"
                                                            "doc/dir"
                                                            "doc/*.info"
                                                            "doc/*.texi"
                                                            "doc/*.texinfo"
                                                            "lisp/*.el"
                                                            (:exclude
                                                             ".dir-locals.el"
                                                             "test.el"
                                                             "tests.el"
                                                             "*-test.el"
                                                             "*-tests.el"
                                                             "LICENSE" "README*"
                                                             "*-pkg.el"))
                                                           :source "MELPA"
                                                           :protocol https
                                                           :inherit t :depth 1
                                                           :ref
                                                           "cc31ce0356226c3a2128119b08de6107e38fdd17"))
 (justify-kp :source "lockfile" :date (26009 6537 438372 961000) :recipe
                                                                 (:source nil
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1
                                                                  :type git
                                                                  :host github
                                                                  :repo
                                                                  "Fuco1/justify-kp"
                                                                  :package
                                                                  "justify-kp"
                                                                  :ref
                                                                  "33a186e297c0359547820088669486afd7b5fddb"))
 (plz :source "lockfile" :date (26009 6537 436904 662000) :recipe
                                                          (:package "plz" :repo
                                                                          "alphapapa/plz.el"
                                                           :local-repo "plz"
                                                           :files
                                                           ("*"
                                                            (:exclude ".git"
                                                             "LICENSE"))
                                                           :source
                                                           "GNU-devel ELPA"
                                                           :protocol https
                                                           :inherit t :depth 1
                                                           :type git :host
                                                                     github :ref
                                                                            "e50ccf2c164335d8f03b922b605ea826942a7c1b"))
 (ement :source "lockfile" :date (26009 6537 435128 284000) :recipe
                                                            (:package "ement"
                                                             :repo
                                                             "alphapapa/ement.el"
                                                             :local-repo "ement"
                                                             :files
                                                             ("*"
                                                              (:exclude ".git"))
                                                             :source
                                                             "GNU-devel ELPA"
                                                             :protocol https
                                                             :inherit t :depth 1
                                                             :type git :host
                                                                       github
                                                             :ref
                                                             "1579f78cc368bfbc9ced0a1e33ee442085dfb33b"))
 (ledger-mode :source "lockfile" :date (26009 6537 433175 454000) :recipe
                                                                  (:package
                                                                   "ledger-mode"
                                                                   :fetcher
                                                                   github :repo
                                                                          "ledger/ledger-mode"
                                                                   :files
                                                                   ("ledger-*.el"
                                                                    "doc/*.texi")
                                                                   :old-names
                                                                   (ldg-mode)
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "34ccac03056c3d9ef5f64bd658044658e6893811"))
 (flycheck-ledger :source "lockfile" :date (26009 6537 431536 734000) :recipe
                                                                      (:package
                                                                       "flycheck-ledger"
                                                                       :fetcher
                                                                       github
                                                                       :repo
                                                                       "purcell/flycheck-ledger"
                                                                       :files
                                                                       ("*.el"
                                                                        "*.el.in"
                                                                        "dir"
                                                                        "*.info"
                                                                        "*.texi"
                                                                        "*.texinfo"
                                                                        "doc/dir"
                                                                        "doc/*.info"
                                                                        "doc/*.texi"
                                                                        "doc/*.texinfo"
                                                                        "lisp/*.el"
                                                                        (:exclude
                                                                         ".dir-locals.el"
                                                                         "test.el"
                                                                         "tests.el"
                                                                         "*-test.el"
                                                                         "*-tests.el"
                                                                         "LICENSE"
                                                                         "README*"
                                                                         "*-pkg.el"))
                                                                       :source
                                                                       "MELPA"
                                                                       :protocol
                                                                       https
                                                                       :inherit
                                                                       t :depth
                                                                         1 :ref
                                                                           "628e25ba66604946085571652a94a54f4d1ad96f"))
 (yasnippet :source "lockfile" :date (26009 6537 430463 788000) :recipe
                                                                (:package
                                                                 "yasnippet"
                                                                 :repo
                                                                 "joaotavora/yasnippet"
                                                                 :fetcher github
                                                                 :files
                                                                 ("yasnippet.el"
                                                                  "snippets")
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "ae36504a5f4f1db8240ad99df1ee9f5e3f570db4"))
 (consult-yasnippet :source "lockfile" :date (26009 6537 428522 811000) :recipe
                                                                        (:package
                                                                         "consult-yasnippet"
                                                                         :fetcher
                                                                         github
                                                                         :repo
                                                                         "mohkale/consult-yasnippet"
                                                                         :files
                                                                         ("*.el"
                                                                          "*.el.in"
                                                                          "dir"
                                                                          "*.info"
                                                                          "*.texi"
                                                                          "*.texinfo"
                                                                          "doc/dir"
                                                                          "doc/*.info"
                                                                          "doc/*.texi"
                                                                          "doc/*.texinfo"
                                                                          "lisp/*.el"
                                                                          (:exclude
                                                                           ".dir-locals.el"
                                                                           "test.el"
                                                                           "tests.el"
                                                                           "*-test.el"
                                                                           "*-tests.el"
                                                                           "LICENSE"
                                                                           "README*"
                                                                           "*-pkg.el"))
                                                                         :source
                                                                         "MELPA"
                                                                         :protocol
                                                                         https
                                                                         :inherit
                                                                         t
                                                                         :depth
                                                                         1 :ref
                                                                           "ae0450889484f23dc4ec37518852a2c61b89f184"))
 (scratch :source "lockfile" :date (26009 6537 427256 401000) :recipe
                                                              (:package
                                                               "scratch"
                                                               :fetcher codeberg
                                                               :repo
                                                               "emacs-weirdware/scratch"
                                                               :files
                                                               ("scratch.el")
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "f000648c9663833a76a8de9b1e78c99a9d698e48"))
 (tmr :source "lockfile" :date (26009 6537 425831 654000) :recipe
                                                          (:package "tmr" :repo
                                                                          "protesilaos/tmr.el"
                                                           :local-repo "tmr"
                                                           :files
                                                           ("*"
                                                            (:exclude ".git"
                                                             "COPYING"
                                                             "doclicense.texi"
                                                             "Makefile"))
                                                           :source
                                                           "GNU-devel ELPA"
                                                           :protocol https
                                                           :inherit t :depth 1
                                                           :type git :host
                                                                     gitlab :ref
                                                                            "8e6a0e7537ccddbbc443fd03dc65bf23c88c1e9e"))
 (all-the-icons-completion :source "lockfile" :date (26009 6537 424287 763000)
                           :recipe
                           (:package "all-the-icons-completion" :repo
                                                                "iyefrat/all-the-icons-completion"
                            :fetcher github :files
                                            ("*.el" "*.el.in" "dir" "*.info"
                                             "*.texi" "*.texinfo" "doc/dir"
                                             "doc/*.info" "doc/*.texi"
                                             "doc/*.texinfo" "lisp/*.el"
                                             (:exclude ".dir-locals.el"
                                              "test.el" "tests.el" "*-test.el"
                                              "*-tests.el" "LICENSE" "README*"
                                              "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t :depth 1
                            :ref "8eb3e410d63f5d0657b41829e7898793e81f31c0"))
 (vc-msg :source "lockfile" :date (26009 6537 422452 354000) :recipe
                                                             (:package "vc-msg"
                                                              :fetcher github
                                                              :repo
                                                              "redguardtoo/vc-msg"
                                                              :files
                                                              ("*.el" "*.el.in"
                                                               "dir" "*.info"
                                                               "*.texi"
                                                               "*.texinfo"
                                                               "doc/dir"
                                                               "doc/*.info"
                                                               "doc/*.texi"
                                                               "doc/*.texinfo"
                                                               "lisp/*.el"
                                                               (:exclude
                                                                ".dir-locals.el"
                                                                "test.el"
                                                                "tests.el"
                                                                "*-test.el"
                                                                "*-tests.el"
                                                                "LICENSE"
                                                                "README*"
                                                                "*-pkg.el"))
                                                              :source "MELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "5b0f27307402442b062448d548c29da02ba92295"))
 (alt-comment-dwim :source "lockfile" :date (26009 6537 421257 349000) :recipe
                                                                       (:source
                                                                        nil
                                                                        :protocol
                                                                        ssh
                                                                        :inherit
                                                                        t :depth
                                                                          nil
                                                                        :type
                                                                        git
                                                                        :host
                                                                        gitlab
                                                                        :repo
                                                                        "PreciousPudding/alt-comment-dwim"
                                                                        :package
                                                                        "alt-comment-dwim"
                                                                        :ref
                                                                        "0a1e1e298d3b2dd746c4da130cb500d9e66b7b9d"))
 (info-variable-pitch :source "lockfile" :date (26009 6537 419898 285000)
                      :recipe
                      (:source nil :protocol https :inherit t :depth 1 :type git
                       :host github :repo "kisaragi-hiu/info-variable-pitch"
                       :package "info-variable-pitch" :ref
                                                      "e18e8dfb5dbea304fcf2312eb6cc8a0736e6eda0"))
 (info-colors :source "lockfile" :date (26009 6537 418233 887000) :recipe
                                                                  (:package
                                                                   "info-colors"
                                                                   :fetcher
                                                                   github :repo
                                                                          "ubolonton/info-colors"
                                                                   :files
                                                                   ("*.el"
                                                                    "*.el.in"
                                                                    "dir"
                                                                    "*.info"
                                                                    "*.texi"
                                                                    "*.texinfo"
                                                                    "doc/dir"
                                                                    "doc/*.info"
                                                                    "doc/*.texi"
                                                                    "doc/*.texinfo"
                                                                    "lisp/*.el"
                                                                    (:exclude
                                                                     ".dir-locals.el"
                                                                     "test.el"
                                                                     "tests.el"
                                                                     "*-test.el"
                                                                     "*-tests.el"
                                                                     "LICENSE"
                                                                     "README*"
                                                                     "*-pkg.el"))
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "2e237c301ba62f0e0286a27c1abe48c4c8441143"))
 (inform :source "lockfile" :date (26009 6537 416382 67000) :recipe
                                                            (:package "inform"
                                                             :fetcher github
                                                             :repo
                                                             "dieter-wilhelm/inform"
                                                             :files
                                                             ("*.el" "*.el.in"
                                                              "dir" "*.info"
                                                              "*.texi"
                                                              "*.texinfo"
                                                              "doc/dir"
                                                              "doc/*.info"
                                                              "doc/*.texi"
                                                              "doc/*.texinfo"
                                                              "lisp/*.el"
                                                              (:exclude
                                                               ".dir-locals.el"
                                                               "test.el"
                                                               "tests.el"
                                                               "*-test.el"
                                                               "*-tests.el"
                                                               "LICENSE"
                                                               "README*"
                                                               "*-pkg.el"))
                                                             :source "MELPA"
                                                             :protocol https
                                                             :inherit t :depth 1
                                                             :ref
                                                             "8ff0a19a9f40cfa8283da8ed73de94c35a327423"))
 (eldoc :source "lockfile" :date (26009 6537 414919 409000) :recipe
                                                            (:package "eldoc"
                                                             :repo
                                                             "https://github.com/emacs-mirror/emacs"
                                                             :local-repo "eldoc"
                                                             :branch "master"
                                                             :files
                                                             ("lisp/emacs-lisp/eldoc.el"
                                                              (:exclude ".git"))
                                                             :source
                                                             "GNU-devel ELPA"
                                                             :protocol https
                                                             :inherit t :depth 1
                                                             :ref
                                                             "657275529e31226bbc6c92eb7f7af887474a0bb8"))
 (pulsar :source "lockfile" :date (26009 6537 413373 945000) :recipe
                                                             (:package "pulsar"
                                                              :repo
                                                              "https://git.sr.ht/~protesilaos/pulsar"
                                                              :local-repo
                                                              "pulsar" :files
                                                                       ("*"
                                                                        (:exclude
                                                                         ".git"
                                                                         "COPYING"
                                                                         "doclicense.texi"))
                                                              :source
                                                              "GNU-devel ELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "1ca4941f2cf68771c0fd9561e1631d2f701f6b31"))
 (pocket-reader :source "lockfile" :date (26009 6537 412462 612000) :recipe
                                                                    (:package
                                                                     "pocket-reader"
                                                                     :fetcher
                                                                     github
                                                                     :repo
                                                                     "alphapapa/pocket-reader.el"
                                                                     :files
                                                                     ("*.el"
                                                                      "*.el.in"
                                                                      "dir"
                                                                      "*.info"
                                                                      "*.texi"
                                                                      "*.texinfo"
                                                                      "doc/dir"
                                                                      "doc/*.info"
                                                                      "doc/*.texi"
                                                                      "doc/*.texinfo"
                                                                      "lisp/*.el"
                                                                      (:exclude
                                                                       ".dir-locals.el"
                                                                       "test.el"
                                                                       "tests.el"
                                                                       "*-test.el"
                                                                       "*-tests.el"
                                                                       "LICENSE"
                                                                       "README*"
                                                                       "*-pkg.el"))
                                                                     :source
                                                                     "MELPA"
                                                                     :protocol
                                                                     https
                                                                     :inherit t
                                                                     :depth 1
                                                                     :type git
                                                                     :host
                                                                     github :ref
                                                                            "ef6b6892ef13eff3479d79c7f6bc918dd0444e88"))
 (fancy-compilation :source "lockfile" :date (26009 6537 411492 800000) :recipe
                                                                        (:package
                                                                         "fancy-compilation"
                                                                         :fetcher
                                                                         codeberg
                                                                         :repo
                                                                         "ideasman42/emacs-fancy-compilation"
                                                                         :files
                                                                         ("*.el"
                                                                          "*.el.in"
                                                                          "dir"
                                                                          "*.info"
                                                                          "*.texi"
                                                                          "*.texinfo"
                                                                          "doc/dir"
                                                                          "doc/*.info"
                                                                          "doc/*.texi"
                                                                          "doc/*.texinfo"
                                                                          "lisp/*.el"
                                                                          (:exclude
                                                                           ".dir-locals.el"
                                                                           "test.el"
                                                                           "tests.el"
                                                                           "*-test.el"
                                                                           "*-tests.el"
                                                                           "LICENSE"
                                                                           "README*"
                                                                           "*-pkg.el"))
                                                                         :source
                                                                         "MELPA"
                                                                         :protocol
                                                                         https
                                                                         :inherit
                                                                         t
                                                                         :depth
                                                                         1 :ref
                                                                           "74833d618024cee47c24aabdc6e9daa4009d0690"))
 (image-popup :source "lockfile" :date (26009 6537 409915 75000) :recipe
                                                                 (:source nil
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1
                                                                  :type git
                                                                  :host gitlab
                                                                  :repo
                                                                  "OlMon/image-popup"
                                                                  :branch
                                                                  "master"
                                                                  :package
                                                                  "image-popup"
                                                                  :ref
                                                                  "8d8e86d1ac08738ad744af6283abd48db1d6858d"))
 (form-feed :source "lockfile" :date (26009 6537 408302 976000) :recipe
                                                                (:package
                                                                 "form-feed"
                                                                 :fetcher git
                                                                 :url
                                                                 "https://depp.brause.cc/form-feed.git"
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "ac1f0ef30a11979f5dfe12d8c05a666739e486ff"))
 (logos :source "lockfile" :date (26009 6537 407010 758000) :recipe
                                                            (:package "logos"
                                                             :repo
                                                             "https://git.sr.ht/~protesilaos/logos"
                                                             :local-repo "logos"
                                                             :files
                                                             ("*"
                                                              (:exclude ".git"
                                                               "COPYING"
                                                               "doclicense.texi"))
                                                             :source
                                                             "GNU-devel ELPA"
                                                             :protocol https
                                                             :inherit t :depth 1
                                                             :ref
                                                             "1cab0eaef796058f6ed73112db1009213ad9bbc6"))
 (engine-mode :source "lockfile" :date (26009 6537 405772 952000) :recipe
                                                                  (:package
                                                                   "engine-mode"
                                                                   :repo
                                                                   "hrs/engine-mode"
                                                                   :fetcher
                                                                   github :files
                                                                          ("*.el"
                                                                           "*.el.in"
                                                                           "dir"
                                                                           "*.info"
                                                                           "*.texi"
                                                                           "*.texinfo"
                                                                           "doc/dir"
                                                                           "doc/*.info"
                                                                           "doc/*.texi"
                                                                           "doc/*.texinfo"
                                                                           "lisp/*.el"
                                                                           (:exclude
                                                                            ".dir-locals.el"
                                                                            "test.el"
                                                                            "tests.el"
                                                                            "*-test.el"
                                                                            "*-tests.el"
                                                                            "LICENSE"
                                                                            "README*"
                                                                            "*-pkg.el"))
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "19fbf8e21df8f45083a904fdba2fac85b3b02dd0"))
 (pcre2el :source "lockfile" :date (26009 6537 404203 362000) :recipe
                                                              (:package
                                                               "pcre2el"
                                                               :fetcher github
                                                               :repo
                                                               "joddie/pcre2el"
                                                               :files
                                                               ("*.el" "*.el.in"
                                                                "dir" "*.info"
                                                                "*.texi"
                                                                "*.texinfo"
                                                                "doc/dir"
                                                                "doc/*.info"
                                                                "doc/*.texi"
                                                                "doc/*.texinfo"
                                                                "lisp/*.el"
                                                                (:exclude
                                                                 ".dir-locals.el"
                                                                 "test.el"
                                                                 "tests.el"
                                                                 "*-test.el"
                                                                 "*-tests.el"
                                                                 "LICENSE"
                                                                 "README*"
                                                                 "*-pkg.el"))
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "018531ba0cf8e2b28d1108136a0e031b6a45f1c1"))
 (sentex :source "lockfile" :date (26009 6537 402077 848000) :recipe
                                                             (:package "sentex"
                                                              :fetcher codeberg
                                                              :repo
                                                              "martianh/sentex"
                                                              :files ("*")
                                                              :source "MELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :type
                                                                           git
                                                              :host codeberg
                                                              :ref
                                                              "b08c48bfab391a259d93e182b9115b2aba275d82"))
 (recursion-indicator :source "lockfile" :date (26009 6537 400654 213000)
                      :recipe
                      (:package "recursion-indicator" :repo
                                                      "minad/recursion-indicator"
                       :fetcher github :files
                                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                        "*.texinfo" "doc/dir" "doc/*.info"
                                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                        (:exclude ".dir-locals.el" "test.el"
                                         "tests.el" "*-test.el" "*-tests.el"
                                         "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref
                                                                           "dd9195da974c330d00e63e14cafc855590154ff5"))
 (lorem-ipsum :source "lockfile" :date (26009 6537 399096 586000) :recipe
                                                                  (:package
                                                                   "lorem-ipsum"
                                                                   :fetcher
                                                                   github :repo
                                                                          "jschaf/emacs-lorem-ipsum"
                                                                   :files
                                                                   ("*.el"
                                                                    "*.el.in"
                                                                    "dir"
                                                                    "*.info"
                                                                    "*.texi"
                                                                    "*.texinfo"
                                                                    "doc/dir"
                                                                    "doc/*.info"
                                                                    "doc/*.texi"
                                                                    "doc/*.texinfo"
                                                                    "lisp/*.el"
                                                                    (:exclude
                                                                     ".dir-locals.el"
                                                                     "test.el"
                                                                     "tests.el"
                                                                     "*-test.el"
                                                                     "*-tests.el"
                                                                     "LICENSE"
                                                                     "README*"
                                                                     "*-pkg.el"))
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "4e87a899868e908a7a9e1812831d76c8d072f885"))
 (writeroom-mode :source "lockfile" :date (26009 6537 398163 893000) :recipe
                                                                     (:package
                                                                      "writeroom-mode"
                                                                      :fetcher
                                                                      github
                                                                      :repo
                                                                      "joostkremers/writeroom-mode"
                                                                      :files
                                                                      ("*.el"
                                                                       "*.el.in"
                                                                       "dir"
                                                                       "*.info"
                                                                       "*.texi"
                                                                       "*.texinfo"
                                                                       "doc/dir"
                                                                       "doc/*.info"
                                                                       "doc/*.texi"
                                                                       "doc/*.texinfo"
                                                                       "lisp/*.el"
                                                                       (:exclude
                                                                        ".dir-locals.el"
                                                                        "test.el"
                                                                        "tests.el"
                                                                        "*-test.el"
                                                                        "*-tests.el"
                                                                        "LICENSE"
                                                                        "README*"
                                                                        "*-pkg.el"))
                                                                      :source
                                                                      "MELPA"
                                                                      :protocol
                                                                      https
                                                                      :inherit t
                                                                      :depth 1
                                                                      :ref
                                                                      "f4d035e91d20bf1dd3f2857b9cc344f844979a78"))
 (clipetty :source "lockfile" :date (26009 6537 396527 558000) :recipe
                                                               (:package
                                                                "clipetty" :repo
                                                                           "spudlyo/clipetty"
                                                                :fetcher github
                                                                :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "01b39044b9b65fa4ea7d3166f8b1ffab6f740362"))
 (goto-chg :source "lockfile" :date (26009 6537 394782 319000) :recipe
                                                               (:package
                                                                "goto-chg" :repo
                                                                           "emacs-evil/goto-chg"
                                                                :fetcher github
                                                                :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "278cd3e6d5107693aa2bb33189ca503f22f227d0"))
 (fontify-patch :source "lockfile" :date (26009 6537 392881 127000) :recipe
                                                                    (:source nil
                                                                     :protocol
                                                                     https
                                                                     :inherit t
                                                                     :depth 1
                                                                     :type git
                                                                     :host
                                                                     github
                                                                     :repo
                                                                     "whame/fontify-patch"
                                                                     :package
                                                                     "fontify-patch"
                                                                     :ref
                                                                     "9c13c4109505c84bbab3ca6ee8ff0fb391ed6dec"))
 (frame-local :source "lockfile" :date (26009 6537 391225 536000) :recipe
                                                                  (:package
                                                                   "frame-local"
                                                                   :fetcher
                                                                   github :repo
                                                                          "sebastiencs/frame-local"
                                                                   :files
                                                                   ("*.el"
                                                                    "*.el.in"
                                                                    "dir"
                                                                    "*.info"
                                                                    "*.texi"
                                                                    "*.texinfo"
                                                                    "doc/dir"
                                                                    "doc/*.info"
                                                                    "doc/*.texi"
                                                                    "doc/*.texinfo"
                                                                    "lisp/*.el"
                                                                    (:exclude
                                                                     ".dir-locals.el"
                                                                     "test.el"
                                                                     "tests.el"
                                                                     "*-test.el"
                                                                     "*-tests.el"
                                                                     "LICENSE"
                                                                     "README*"
                                                                     "*-pkg.el"))
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "7ee1106c3bcd4022f48421f8cb1ef4f995da816e"))
 (prescient :source "lockfile" :date (26009 6537 389809 354000) :recipe
                                                                (:package
                                                                 "prescient"
                                                                 :fetcher github
                                                                 :repo
                                                                 "radian-software/prescient.el"
                                                                 :files
                                                                 ("prescient.el")
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "864b352e7ecc649cd13ff23172c9cc8039129cc9"))
 (swiper :source "lockfile" :date (26009 6537 388165 205000) :recipe
                                                             (:package "swiper"
                                                              :repo
                                                              "abo-abo/swiper"
                                                              :fetcher github
                                                              :files
                                                              ("swiper.el")
                                                              :source "MELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "8c30f4cab5948aa8d942a3b2bbf5fb6a94d9441d"))
 (helm-core :source "lockfile" :date (26009 6537 386739 475000) :recipe
                                                                (:package
                                                                 "helm-core"
                                                                 :repo
                                                                 "emacs-helm/helm"
                                                                 :fetcher github
                                                                 :files
                                                                 ("helm-core.el"
                                                                  "helm-lib.el"
                                                                  "helm-source.el"
                                                                  "helm-multi-match.el")
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "f7ca648c2c3b308b90739f1862b4f750585ee362"))
 (wfnames :source "lockfile" :date (26009 6537 385382 75000) :recipe
                                                             (:package "wfnames"
                                                              :fetcher github
                                                              :repo
                                                              "thierryvolpiatto/wfnames"
                                                              :files
                                                              ("*.el" "*.el.in"
                                                               "dir" "*.info"
                                                               "*.texi"
                                                               "*.texinfo"
                                                               "doc/dir"
                                                               "doc/*.info"
                                                               "doc/*.texi"
                                                               "doc/*.texinfo"
                                                               "lisp/*.el"
                                                               (:exclude
                                                                ".dir-locals.el"
                                                                "test.el"
                                                                "tests.el"
                                                                "*-test.el"
                                                                "*-tests.el"
                                                                "LICENSE"
                                                                "README*"
                                                                "*-pkg.el"))
                                                              :source "MELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "19b452fb698a5ba3b0f1d6e7d69a5e19af2c83e7"))
 (popup :source "lockfile" :date (26009 6537 384012 351000) :recipe
                                                            (:package "popup"
                                                             :fetcher github
                                                             :repo
                                                             "auto-complete/popup-el"
                                                             :files
                                                             ("*.el" "*.el.in"
                                                              "dir" "*.info"
                                                              "*.texi"
                                                              "*.texinfo"
                                                              "doc/dir"
                                                              "doc/*.info"
                                                              "doc/*.texi"
                                                              "doc/*.texinfo"
                                                              "lisp/*.el"
                                                              (:exclude
                                                               ".dir-locals.el"
                                                               "test.el"
                                                               "tests.el"
                                                               "*-test.el"
                                                               "*-tests.el"
                                                               "LICENSE"
                                                               "README*"
                                                               "*-pkg.el"))
                                                             :source "MELPA"
                                                             :protocol https
                                                             :inherit t :depth 1
                                                             :ref
                                                             "6fa7c440879ade009dd0ea37eccc771ced0ef86d"))
 (async :source "lockfile" :date (26009 6537 382462 288000) :recipe
                                                            (:package "async"
                                                             :repo
                                                             "jwiegley/emacs-async"
                                                             :fetcher github
                                                             :files
                                                             ("*.el" "*.el.in"
                                                              "dir" "*.info"
                                                              "*.texi"
                                                              "*.texinfo"
                                                              "doc/dir"
                                                              "doc/*.info"
                                                              "doc/*.texi"
                                                              "doc/*.texinfo"
                                                              "lisp/*.el"
                                                              (:exclude
                                                               ".dir-locals.el"
                                                               "test.el"
                                                               "tests.el"
                                                               "*-test.el"
                                                               "*-tests.el"
                                                               "LICENSE"
                                                               "README*"
                                                               "*-pkg.el"))
                                                             :source "MELPA"
                                                             :protocol https
                                                             :inherit t :depth 1
                                                             :ref
                                                             "d604187c3c8a3290e78c00b4532b83d8d908b5c3"))
 (pretty-hydra :source "lockfile" :date (26009 6537 380543 883000) :recipe
                                                                   (:package
                                                                    "pretty-hydra"
                                                                    :repo
                                                                    "jerrypnz/major-mode-hydra.el"
                                                                    :fetcher
                                                                    github
                                                                    :files
                                                                    ("pretty-hydra.el")
                                                                    :source
                                                                    "MELPA"
                                                                    :protocol
                                                                    https
                                                                    :inherit t
                                                                    :depth 1
                                                                    :ref
                                                                    "d0a5dadee97c3752fcdef113cf2ba1923972a480"))
 (magit-section :source "lockfile" :date (26009 6537 379292 201000) :recipe
                                                                    (:package
                                                                     "magit-section"
                                                                     :fetcher
                                                                     github
                                                                     :repo
                                                                     "magit/magit"
                                                                     :files
                                                                     ("lisp/magit-section.el"
                                                                      "lisp/magit-section-pkg.el"
                                                                      "docs/magit-section.texi"
                                                                      "Documentation/magit-section.texi")
                                                                     :source
                                                                     "MELPA"
                                                                     :protocol
                                                                     https
                                                                     :inherit t
                                                                     :depth 1
                                                                     :ref
                                                                     "5c652a086e289e5a257745baa9eb7d98ee734516"))
 (hydra :source "lockfile" :date (26009 6537 377829 844000) :recipe
                                                            (:package "hydra"
                                                             :repo
                                                             "abo-abo/hydra"
                                                             :fetcher github
                                                             :files
                                                             (:defaults
                                                              (:exclude "lv.el"))
                                                             :source "MELPA"
                                                             :protocol https
                                                             :inherit t :depth 1
                                                             :ref
                                                             "317e1de33086637579a7aeb60f77ed0405bf359b"))
 (s :source "lockfile" :date (26009 6537 376187 648000) :recipe
                                                        (:package "s" :fetcher
                                                                      github
                                                         :repo "magnars/s.el"
                                                         :files
                                                         ("*.el" "*.el.in" "dir"
                                                          "*.info" "*.texi"
                                                          "*.texinfo" "doc/dir"
                                                          "doc/*.info"
                                                          "doc/*.texi"
                                                          "doc/*.texinfo"
                                                          "lisp/*.el"
                                                          (:exclude
                                                           ".dir-locals.el"
                                                           "test.el" "tests.el"
                                                           "*-test.el"
                                                           "*-tests.el"
                                                           "LICENSE" "README*"
                                                           "*-pkg.el"))
                                                         :source "MELPA"
                                                         :protocol https
                                                         :inherit t :depth 1
                                                         :ref
                                                         "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (lv :source "lockfile" :date (26009 6537 374312 494000) :recipe
                                                         (:package "lv" :repo
                                                                        "abo-abo/hydra"
                                                          :fetcher github :files
                                                                          ("lv.el")
                                                          :source "MELPA"
                                                          :protocol https
                                                          :inherit t :depth 1
                                                          :ref
                                                          "317e1de33086637579a7aeb60f77ed0405bf359b"))
 (grammarly :source "lockfile" :date (26009 6537 372949 182000) :recipe
                                                                (:package
                                                                 "grammarly"
                                                                 :repo
                                                                 "emacs-grammarly/grammarly"
                                                                 :fetcher github
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "813944714a04ae2a3cdaca845c1c9d70ced462ca"))
 (request :source "lockfile"
          :date (26009 6537 371908 617000) :recipe
                                           (:package "request" :repo
                                                               "tkf/emacs-request"
                                            :fetcher github :files
                                                            ("request.el")
                                            :source "MELPA" :protocol https
                                            :inherit t :depth 1 :ref
                                                                "01e338c335c07e4407239619e57361944a82cb8a"))
 (spinner :source "lockfile" :date (26009 6537 370280 36000) :recipe
                                                             (:package "spinner"
                                                              :repo
                                                              "https://github.com/Malabarba/spinner.el"
                                                              :local-repo
                                                              "spinner" :files
                                                                        ("*"
                                                                         (:exclude
                                                                          ".git"))
                                                              :source
                                                              "GNU-devel ELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (jeison :source "lockfile" :date (26009 6537 369236 315000) :recipe
                                                             (:package "jeison"
                                                              :repo
                                                              "SavchenkoValeriy/jeison"
                                                              :fetcher github
                                                              :files
                                                              ("*.el" "*.el.in"
                                                               "dir" "*.info"
                                                               "*.texi"
                                                               "*.texinfo"
                                                               "doc/dir"
                                                               "doc/*.info"
                                                               "doc/*.texi"
                                                               "doc/*.texinfo"
                                                               "lisp/*.el"
                                                               (:exclude
                                                                ".dir-locals.el"
                                                                "test.el"
                                                                "tests.el"
                                                                "*-test.el"
                                                                "*-tests.el"
                                                                "LICENSE"
                                                                "README*"
                                                                "*-pkg.el"))
                                                              :source "MELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "19a51770f24eaa7b538c7be6a8a5c25d154b641f"))
 (esxml :source "lockfile" :date (26009 6537 367304 886000) :recipe
                                                            (:package "esxml"
                                                             :fetcher github
                                                             :repo
                                                             "tali713/esxml"
                                                             :files
                                                             ("esxml.el"
                                                              "esxml-query.el")
                                                             :source "MELPA"
                                                             :protocol https
                                                             :inherit t :depth 1
                                                             :ref
                                                             "225693096a587492d76bf696d1f0c25c61f7d531"))
 (kv :source "lockfile" :date (26009 6537 365878 85000) :recipe
                                                        (:package "kv" :fetcher
                                                                       github
                                                         :repo
                                                         "nicferrier/emacs-kv"
                                                         :files
                                                         ("*.el" "*.el.in" "dir"
                                                          "*.info" "*.texi"
                                                          "*.texinfo" "doc/dir"
                                                          "doc/*.info"
                                                          "doc/*.texi"
                                                          "doc/*.texinfo"
                                                          "lisp/*.el"
                                                          (:exclude
                                                           ".dir-locals.el"
                                                           "test.el" "tests.el"
                                                           "*-test.el"
                                                           "*-tests.el"
                                                           "LICENSE" "README*"
                                                           "*-pkg.el"))
                                                         :source "MELPA"
                                                         :protocol https
                                                         :inherit t :depth 1
                                                         :ref
                                                         "721148475bce38a70e0b678ba8aa923652e8900e"))
 (parsebib :source "lockfile" :date (26009 6537 364189 903000) :recipe
                                                               (:package
                                                                "parsebib"
                                                                :fetcher github
                                                                :repo
                                                                "joostkremers/parsebib"
                                                                :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "ace9df707108b17759c004c7387655277122d4c1"))
 (citeproc :source "lockfile" :date (26009 6537 362642 485000) :recipe
                                                               (:package
                                                                "citeproc"
                                                                :fetcher github
                                                                :repo
                                                                "andras-simonyi/citeproc-el"
                                                                :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "c61c98b9d230ea28b2ca49498134803e1f8ea526"))
 (queue :source "lockfile" :date (26009 6537 360990 371000) :recipe
                                                            (:package "queue"
                                                             :repo
                                                             "git://git.sv.gnu.org/emacs/elpa"
                                                             :local-repo "queue"
                                                             :branch
                                                             "externals/queue"
                                                             :files
                                                             ("*"
                                                              (:exclude ".git"))
                                                             :source
                                                             "GNU-devel ELPA"
                                                             :protocol https
                                                             :inherit t :depth 1
                                                             :ref
                                                             "f986fb68e75bdae951efb9e11a3012ab6bd408ee"))
 (string-inflection :source "lockfile" :date (26009 6537 358988 8000) :recipe
                                                                      (:package
                                                                       "string-inflection"
                                                                       :fetcher
                                                                       github
                                                                       :repo
                                                                       "akicho8/string-inflection"
                                                                       :files
                                                                       ("*.el"
                                                                        "*.el.in"
                                                                        "dir"
                                                                        "*.info"
                                                                        "*.texi"
                                                                        "*.texinfo"
                                                                        "doc/dir"
                                                                        "doc/*.info"
                                                                        "doc/*.texi"
                                                                        "doc/*.texinfo"
                                                                        "lisp/*.el"
                                                                        (:exclude
                                                                         ".dir-locals.el"
                                                                         "test.el"
                                                                         "tests.el"
                                                                         "*-test.el"
                                                                         "*-tests.el"
                                                                         "LICENSE"
                                                                         "README*"
                                                                         "*-pkg.el"))
                                                                       :source
                                                                       "MELPA"
                                                                       :protocol
                                                                       https
                                                                       :inherit
                                                                       t :depth
                                                                         1 :ref
                                                                           "50ad54970b3cc79b6b83979bde9889ad9a9e1a9c"))
 (tomelr :source "lockfile" :date (26009 6537 357285 949000) :recipe
                                                             (:package "tomelr"
                                                              :repo
                                                              "https://github.com/kaushalmodi/tomelr"
                                                              :local-repo
                                                              "tomelr" :files
                                                                       ("*"
                                                                        (:exclude
                                                                         ".git"
                                                                         "LICENSE"))
                                                              :source
                                                              "GNU-devel ELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "670e0a08f625175fd80137cf69e799619bf8a381"))
 (tablist :source "lockfile" :date (26009 6537 355206 231000) :recipe
                                                              (:package
                                                               "tablist"
                                                               :fetcher github
                                                               :repo
                                                               "emacsorphanage/tablist"
                                                               :files
                                                               ("*.el" "*.el.in"
                                                                "dir" "*.info"
                                                                "*.texi"
                                                                "*.texinfo"
                                                                "doc/dir"
                                                                "doc/*.info"
                                                                "doc/*.texi"
                                                                "doc/*.texinfo"
                                                                "lisp/*.el"
                                                                (:exclude
                                                                 ".dir-locals.el"
                                                                 "test.el"
                                                                 "tests.el"
                                                                 "*-test.el"
                                                                 "*-tests.el"
                                                                 "LICENSE"
                                                                 "README*"
                                                                 "*-pkg.el"))
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "fcd37147121fabdf003a70279cf86fbe08cfac6f"))
 (org-roam :source "lockfile" :date (26009 6537 353990 136000) :recipe
                                                               (:package
                                                                "org-roam"
                                                                :fetcher github
                                                                :repo
                                                                "org-roam/org-roam"
                                                                :files
                                                                (:defaults
                                                                 "extensions/*")
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "5c06471c3a11348342719fd9011486455adeb701"))
 (emacsql :source "lockfile" :date (26009 6537 352420 747000) :recipe
                                                              (:package
                                                               "emacsql"
                                                               :fetcher github
                                                               :repo
                                                               "magit/emacsql"
                                                               :files
                                                               (:defaults
                                                                "sqlite")
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "e2b2df570d213cbaa995dd500a70b1ec95758925"))
 (ts :source "lockfile" :date (26009 6537 351412 733000) :recipe
                                                         (:package "ts" :fetcher
                                                                        github
                                                          :repo
                                                          "alphapapa/ts.el"
                                                          :files
                                                          ("*.el" "*.el.in"
                                                           "dir" "*.info"
                                                           "*.texi" "*.texinfo"
                                                           "doc/dir"
                                                           "doc/*.info"
                                                           "doc/*.texi"
                                                           "doc/*.texinfo"
                                                           "lisp/*.el"
                                                           (:exclude
                                                            ".dir-locals.el"
                                                            "test.el" "tests.el"
                                                            "*-test.el"
                                                            "*-tests.el"
                                                            "LICENSE" "README*"
                                                            "*-pkg.el"))
                                                          :source "MELPA"
                                                          :protocol https
                                                          :inherit t :depth 1
                                                          :ref
                                                          "552936017cfdec89f7fc20c254ae6b37c3f22c5b"))
 (alert :source "lockfile" :date (26009 6537 349733 878000) :recipe
                                                            (:package "alert"
                                                             :fetcher github
                                                             :repo
                                                             "jwiegley/alert"
                                                             :files
                                                             ("*.el" "*.el.in"
                                                              "dir" "*.info"
                                                              "*.texi"
                                                              "*.texinfo"
                                                              "doc/dir"
                                                              "doc/*.info"
                                                              "doc/*.texi"
                                                              "doc/*.texinfo"
                                                              "lisp/*.el"
                                                              (:exclude
                                                               ".dir-locals.el"
                                                               "test.el"
                                                               "tests.el"
                                                               "*-test.el"
                                                               "*-tests.el"
                                                               "LICENSE"
                                                               "README*"
                                                               "*-pkg.el"))
                                                             :source "MELPA"
                                                             :protocol https
                                                             :inherit t :depth 1
                                                             :ref
                                                             "7774b5fd2feb98d4910ff06435d08c19fba93e26"))
 (gntp :source "lockfile" :date (26009 6537 348056 346000) :recipe
                                                           (:package "gntp"
                                                            :repo
                                                            "tekai/gntp.el"
                                                            :fetcher github
                                                            :files
                                                            ("*.el" "*.el.in"
                                                             "dir" "*.info"
                                                             "*.texi"
                                                             "*.texinfo"
                                                             "doc/dir"
                                                             "doc/*.info"
                                                             "doc/*.texi"
                                                             "doc/*.texinfo"
                                                             "lisp/*.el"
                                                             (:exclude
                                                              ".dir-locals.el"
                                                              "test.el"
                                                              "tests.el"
                                                              "*-test.el"
                                                              "*-tests.el"
                                                              "LICENSE"
                                                              "README*"
                                                              "*-pkg.el"))
                                                            :source "MELPA"
                                                            :protocol https
                                                            :inherit t :depth 1
                                                            :ref
                                                            "767571135e2c0985944017dc59b0be79af222ef5"))
 (log4e :source "lockfile" :date (26009 6537 346679 559000) :recipe
                                                            (:package "log4e"
                                                             :repo "aki2o/log4e"
                                                             :fetcher github
                                                             :files
                                                             ("*.el" "*.el.in"
                                                              "dir" "*.info"
                                                              "*.texi"
                                                              "*.texinfo"
                                                              "doc/dir"
                                                              "doc/*.info"
                                                              "doc/*.texi"
                                                              "doc/*.texinfo"
                                                              "lisp/*.el"
                                                              (:exclude
                                                               ".dir-locals.el"
                                                               "test.el"
                                                               "tests.el"
                                                               "*-test.el"
                                                               "*-tests.el"
                                                               "LICENSE"
                                                               "README*"
                                                               "*-pkg.el"))
                                                             :source "MELPA"
                                                             :protocol https
                                                             :inherit t :depth 1
                                                             :ref
                                                             "737d275eac28dbdfb0b26d28e99da148bfce9d16"))
 (aio :source "lockfile" :date (26009 6537 345640 65000) :recipe
                                                         (:package "aio"
                                                          :fetcher github :repo
                                                                          "skeeto/emacs-aio"
                                                          :files
                                                          ("aio.el" "README.md"
                                                           "UNLICENSE")
                                                          :source "MELPA"
                                                          :protocol https
                                                          :inherit t :depth 1
                                                          :ref
                                                          "da93523e235529fa97d6f251319d9e1d6fc24a41"))
 (elnode :source "lockfile" :date (26009 6537 344464 937000) :recipe
                                                             (:package "elnode"
                                                              :fetcher github
                                                              :repo
                                                              "jcaw/elnode"
                                                              :branch "melpa"
                                                              :files
                                                              ("default*"
                                                               "elnode*")
                                                              :source "MELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "29ef0f51a65a24fca7fdcdb4140d2e4556e4bb29"))
 (persist :source "lockfile" :date (26009 6537 342930 373000) :recipe
                                                              (:package
                                                               "persist" :repo
                                                                         "https://gitlab.com/phillord/persist"
                                                               :local-repo
                                                               "persist" :files
                                                                         ("*"
                                                                          (:exclude
                                                                           ".git"))
                                                               :source
                                                               "GNU-devel ELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "ec0b42318cc627e1b4a169bda0d335a67d74c320"))
 (request-deferred :source "lockfile" :date (26009 6537 341247 160000) :recipe
                                                                       (:package
                                                                        "request-deferred"
                                                                        :repo
                                                                        "tkf/emacs-request"
                                                                        :fetcher
                                                                        github
                                                                        :files
                                                                        ("request-deferred.el")
                                                                        :source
                                                                        "MELPA"
                                                                        :protocol
                                                                        https
                                                                        :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "01e338c335c07e4407239619e57361944a82cb8a"))
 (deferred :source "lockfile" :date (26009 6537 339529 132000) :recipe
                                                               (:package
                                                                "deferred" :repo
                                                                           "kiwanami/emacs-deferred"
                                                                :fetcher github
                                                                :files
                                                                ("deferred.el")
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "2239671d94b38d92e9b28d4e12fd79814cfb9c16"))
 (web :source "lockfile" :date (26009 6537 337975 433000) :recipe
                                                          (:package "web"
                                                           :fetcher github :repo
                                                                           "nicferrier/emacs-web"
                                                           :files ("web.el")
                                                           :source "MELPA"
                                                           :protocol https
                                                           :inherit t :depth 1
                                                           :ref
                                                           "483188dac4bc6b409b985c9dae45f3324a425efd"))
 (noflet :source "lockfile" :date (26009 6537 336979 181000) :recipe
                                                             (:package "noflet"
                                                              :fetcher github
                                                              :repo
                                                              "nicferrier/emacs-noflet"
                                                              :files
                                                              ("*.el" "*.el.in"
                                                               "dir" "*.info"
                                                               "*.texi"
                                                               "*.texinfo"
                                                               "doc/dir"
                                                               "doc/*.info"
                                                               "doc/*.texi"
                                                               "doc/*.texinfo"
                                                               "lisp/*.el"
                                                               (:exclude
                                                                ".dir-locals.el"
                                                                "test.el"
                                                                "tests.el"
                                                                "*-test.el"
                                                                "*-tests.el"
                                                                "LICENSE"
                                                                "README*"
                                                                "*-pkg.el"))
                                                              :source "MELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "7ae84dc3257637af7334101456dafe1759c6b68a"))
 (creole :source "lockfile" :date (26009 6537 335980 284000) :recipe
                                                             (:package "creole"
                                                              :fetcher github
                                                              :repo
                                                              "nicferrier/elwikicreole"
                                                              :files
                                                              ("*.el" "*.el.in"
                                                               "dir" "*.info"
                                                               "*.texi"
                                                               "*.texinfo"
                                                               "doc/dir"
                                                               "doc/*.info"
                                                               "doc/*.texi"
                                                               "doc/*.texinfo"
                                                               "lisp/*.el"
                                                               (:exclude
                                                                ".dir-locals.el"
                                                                "test.el"
                                                                "tests.el"
                                                                "*-test.el"
                                                                "*-tests.el"
                                                                "LICENSE"
                                                                "README*"
                                                                "*-pkg.el"))
                                                              :source "MELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "7d5cffe93857f6c75ca09ac79c0e47b8d4410e53"))
 (fakir :source "lockfile" :date (26009 6537 334309 504000) :recipe
                                                            (:package "fakir"
                                                             :fetcher github
                                                             :repo
                                                             "nicferrier/emacs-fakir"
                                                             :files
                                                             ("*.el" "*.el.in"
                                                              "dir" "*.info"
                                                              "*.texi"
                                                              "*.texinfo"
                                                              "doc/dir"
                                                              "doc/*.info"
                                                              "doc/*.texi"
                                                              "doc/*.texinfo"
                                                              "lisp/*.el"
                                                              (:exclude
                                                               ".dir-locals.el"
                                                               "test.el"
                                                               "tests.el"
                                                               "*-test.el"
                                                               "*-tests.el"
                                                               "LICENSE"
                                                               "README*"
                                                               "*-pkg.el"))
                                                             :source "MELPA"
                                                             :protocol https
                                                             :inherit t :depth 1
                                                             :ref
                                                             "1fca406ad7de80fece6319ff75d4230b648534b0"))
 (db :source "lockfile" :date (26009 6537 332859 650000) :recipe
                                                         (:package "db" :fetcher
                                                                        github
                                                          :repo
                                                          "nicferrier/emacs-db"
                                                          :files
                                                          ("*.el" "*.el.in"
                                                           "dir" "*.info"
                                                           "*.texi" "*.texinfo"
                                                           "doc/dir"
                                                           "doc/*.info"
                                                           "doc/*.texi"
                                                           "doc/*.texinfo"
                                                           "lisp/*.el"
                                                           (:exclude
                                                            ".dir-locals.el"
                                                            "test.el" "tests.el"
                                                            "*-test.el"
                                                            "*-tests.el"
                                                            "LICENSE" "README*"
                                                            "*-pkg.el"))
                                                          :source "MELPA"
                                                          :protocol https
                                                          :inherit t :depth 1
                                                          :ref
                                                          "b3a423fb8e72f9013009cbe033d654df2ce31438"))
 (mustache :source "lockfile" :date (26009 6537 331539 609000) :recipe
                                                               (:package
                                                                "mustache"
                                                                :fetcher github
                                                                :repo
                                                                "Wilfred/mustache.el"
                                                                :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "229e01f0f0a5684499bcc6a11a5bf8dbe14fd4e8"))
 (outorg :source "lockfile" :date (26009 6537 330428 130000) :recipe
                                                             (:package "outorg"
                                                              :fetcher github
                                                              :repo
                                                              "alphapapa/outorg"
                                                              :files
                                                              ("*.el" "*.el.in"
                                                               "dir" "*.info"
                                                               "*.texi"
                                                               "*.texinfo"
                                                               "doc/dir"
                                                               "doc/*.info"
                                                               "doc/*.texi"
                                                               "doc/*.texinfo"
                                                               "lisp/*.el"
                                                               (:exclude
                                                                ".dir-locals.el"
                                                                "test.el"
                                                                "tests.el"
                                                                "*-test.el"
                                                                "*-tests.el"
                                                                "LICENSE"
                                                                "README*"
                                                                "*-pkg.el"))
                                                              :source "MELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "ef0f86f4b893b30be8bcf8b43a5ec357a6c70f07"))
 (dired-hacks-utils :source "lockfile" :date (26009 6537 329170 938000) :recipe
                                                                        (:package
                                                                         "dired-hacks-utils"
                                                                         :fetcher
                                                                         github
                                                                         :repo
                                                                         "Fuco1/dired-hacks"
                                                                         :files
                                                                         ("dired-hacks-utils.el")
                                                                         :source
                                                                         "MELPA"
                                                                         :protocol
                                                                         https
                                                                         :inherit
                                                                         t
                                                                         :depth
                                                                         1 :ref
                                                                           "874449d6fc98aee565e1715ec18acec3c1c2cafb"))
 (projectile :source "lockfile" :date (26009 6537 327811 624000) :recipe
                                                                 (:package
                                                                  "projectile"
                                                                  :fetcher
                                                                  github :repo
                                                                         "bbatsov/projectile"
                                                                  :files
                                                                  ("*.el"
                                                                   "*.el.in"
                                                                   "dir"
                                                                   "*.info"
                                                                   "*.texi"
                                                                   "*.texinfo"
                                                                   "doc/dir"
                                                                   "doc/*.info"
                                                                   "doc/*.texi"
                                                                   "doc/*.texinfo"
                                                                   "lisp/*.el"
                                                                   (:exclude
                                                                    ".dir-locals.el"
                                                                    "test.el"
                                                                    "tests.el"
                                                                    "*-test.el"
                                                                    "*-tests.el"
                                                                    "LICENSE"
                                                                    "README*"
                                                                    "*-pkg.el"))
                                                                  :source
                                                                  "MELPA"
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "e0fc464d1e41c8d7267636ea2b7330491087c19d"))
 (git-commit :source "lockfile" :date (26009 6537 326132 268000) :recipe
                                                                 (:package
                                                                  "git-commit"
                                                                  :fetcher
                                                                  github :repo
                                                                         "magit/magit"
                                                                  :files
                                                                  ("lisp/git-commit.el"
                                                                   "lisp/git-commit-pkg.el")
                                                                  :old-names
                                                                  (git-commit-mode)
                                                                  :source
                                                                  "MELPA"
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "5c652a086e289e5a257745baa9eb7d98ee734516"))
 (transient :source "lockfile" :date (26009 6537 324397 779000) :recipe
                                                                (:package
                                                                 "transient"
                                                                 :fetcher github
                                                                 :repo
                                                                 "magit/transient"
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "8cc2415a543a1f59b7ee973979daf080aa04c55d"))
 (with-editor :source "lockfile"
              :date (26009 6537 323070 124000) :recipe
                                               (:package "with-editor" :fetcher
                                                                       github
                                                :repo "magit/with-editor" :files
                                                                          ("*.el"
                                                                           "*.el.in"
                                                                           "dir"
                                                                           "*.info"
                                                                           "*.texi"
                                                                           "*.texinfo"
                                                                           "doc/dir"
                                                                           "doc/*.info"
                                                                           "doc/*.texi"
                                                                           "doc/*.texinfo"
                                                                           "lisp/*.el"
                                                                           (:exclude
                                                                            ".dir-locals.el"
                                                                            "test.el"
                                                                            "tests.el"
                                                                            "*-test.el"
                                                                            "*-tests.el"
                                                                            "LICENSE"
                                                                            "README*"
                                                                            "*-pkg.el"))
                                                :source "MELPA" :protocol https
                                                :inherit t :depth 1 :ref
                                                                    "d43db3c58c34d4dbc3ce6f68ec24fecf3452b20e"))
 (closql :source "lockfile" :date (26009 6537 322058 463000) :recipe
                                                             (:package "closql"
                                                              :fetcher github
                                                              :repo
                                                              "magit/closql"
                                                              :files
                                                              ("*.el" "*.el.in"
                                                               "dir" "*.info"
                                                               "*.texi"
                                                               "*.texinfo"
                                                               "doc/dir"
                                                               "doc/*.info"
                                                               "doc/*.texi"
                                                               "doc/*.texinfo"
                                                               "lisp/*.el"
                                                               (:exclude
                                                                ".dir-locals.el"
                                                                "test.el"
                                                                "tests.el"
                                                                "*-test.el"
                                                                "*-tests.el"
                                                                "LICENSE"
                                                                "README*"
                                                                "*-pkg.el"))
                                                              :source "MELPA"
                                                              :protocol https
                                                              :inherit t :depth
                                                                         1 :ref
                                                                           "6bc90e8bef74778883bfa2092a6fedc3c4eb032c"))
 (ghub :source "lockfile" :date (26009 6537 321100 212000) :recipe
                                                           (:package "ghub"
                                                            :fetcher github
                                                            :repo "magit/ghub"
                                                            :files
                                                            ("*.el" "*.el.in"
                                                             "dir" "*.info"
                                                             "*.texi"
                                                             "*.texinfo"
                                                             "doc/dir"
                                                             "doc/*.info"
                                                             "doc/*.texi"
                                                             "doc/*.texinfo"
                                                             "lisp/*.el"
                                                             (:exclude
                                                              ".dir-locals.el"
                                                              "test.el"
                                                              "tests.el"
                                                              "*-test.el"
                                                              "*-tests.el"
                                                              "LICENSE"
                                                              "README*"
                                                              "*-pkg.el"))
                                                            :source "MELPA"
                                                            :protocol https
                                                            :inherit t :depth 1
                                                            :ref
                                                            "ba56fe223fbabab89fee577a1d3b0704d7c4a0df"))
 (yaml :source "lockfile" :date (26009 6537 320096 366000) :recipe
                                                           (:package "yaml"
                                                            :repo "zkry/yaml.el"
                                                            :fetcher github
                                                            :files
                                                            ("*.el" "*.el.in"
                                                             "dir" "*.info"
                                                             "*.texi"
                                                             "*.texinfo"
                                                             "doc/dir"
                                                             "doc/*.info"
                                                             "doc/*.texi"
                                                             "doc/*.texinfo"
                                                             "lisp/*.el"
                                                             (:exclude
                                                              ".dir-locals.el"
                                                              "test.el"
                                                              "tests.el"
                                                              "*-test.el"
                                                              "*-tests.el"
                                                              "LICENSE"
                                                              "README*"
                                                              "*-pkg.el"))
                                                            :source "MELPA"
                                                            :protocol https
                                                            :inherit t :depth 1
                                                            :ref
                                                            "70c4fcead97e9bd6594e418c922ae769818f4245"))
 (treepy :source "lockfile" :date (26009 6537 319109 51000) :recipe
                                                            (:package "treepy"
                                                             :repo
                                                             "volrath/treepy.el"
                                                             :fetcher github
                                                             :files
                                                             ("*.el" "*.el.in"
                                                              "dir" "*.info"
                                                              "*.texi"
                                                              "*.texinfo"
                                                              "doc/dir"
                                                              "doc/*.info"
                                                              "doc/*.texi"
                                                              "doc/*.texinfo"
                                                              "lisp/*.el"
                                                              (:exclude
                                                               ".dir-locals.el"
                                                               "test.el"
                                                               "tests.el"
                                                               "*-test.el"
                                                               "*-tests.el"
                                                               "LICENSE"
                                                               "README*"
                                                               "*-pkg.el"))
                                                             :source "MELPA"
                                                             :protocol https
                                                             :inherit t :depth 1
                                                             :ref
                                                             "75fe3ec37e6f9b2bdfd6d0584efd984d0c00a43e"))
 (fringe-helper :source "lockfile" :date (26009 6537 317459 521000) :recipe
                                                                    (:package
                                                                     "fringe-helper"
                                                                     :fetcher
                                                                     github
                                                                     :repo
                                                                     "nschum/fringe-helper.el"
                                                                     :files
                                                                     ("*.el"
                                                                      "*.el.in"
                                                                      "dir"
                                                                      "*.info"
                                                                      "*.texi"
                                                                      "*.texinfo"
                                                                      "doc/dir"
                                                                      "doc/*.info"
                                                                      "doc/*.texi"
                                                                      "doc/*.texinfo"
                                                                      "lisp/*.el"
                                                                      (:exclude
                                                                       ".dir-locals.el"
                                                                       "test.el"
                                                                       "tests.el"
                                                                       "*-test.el"
                                                                       "*-tests.el"
                                                                       "LICENSE"
                                                                       "README*"
                                                                       "*-pkg.el"))
                                                                     :source
                                                                     "MELPA"
                                                                     :protocol
                                                                     https
                                                                     :inherit t
                                                                     :depth 1
                                                                     :ref
                                                                     "ef4a9c023bae18ec1ddd7265f1f2d6d2e775efdd"))
 (pkg-info :source "lockfile" :date (26009 6537 315723 128000) :recipe
                                                               (:package
                                                                "pkg-info" :repo
                                                                           "emacsorphanage/pkg-info"
                                                                :fetcher github
                                                                :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "76ba7415480687d05a4353b27fea2ae02b8d9d61"))
 (epl :source "lockfile" :date (26009 6537 314361 539000) :recipe
                                                          (:package "epl" :repo
                                                                          "cask/epl"
                                                           :fetcher github
                                                           :files
                                                           ("*.el" "*.el.in"
                                                            "dir" "*.info"
                                                            "*.texi" "*.texinfo"
                                                            "doc/dir"
                                                            "doc/*.info"
                                                            "doc/*.texi"
                                                            "doc/*.texinfo"
                                                            "lisp/*.el"
                                                            (:exclude
                                                             ".dir-locals.el"
                                                             "test.el"
                                                             "tests.el"
                                                             "*-test.el"
                                                             "*-tests.el"
                                                             "LICENSE" "README*"
                                                             "*-pkg.el"))
                                                           :source "MELPA"
                                                           :protocol https
                                                           :inherit t :depth 1
                                                           :ref
                                                           "78ab7a85c08222cd15582a298a364774e3282ce6"))
 (package-lint :source "lockfile" :date (26009 6537 312956 479000) :recipe
                                                                   (:package
                                                                    "package-lint"
                                                                    :fetcher
                                                                    github :repo
                                                                           "purcell/package-lint"
                                                                    :files
                                                                    (:defaults
                                                                     "data"
                                                                     (:exclude
                                                                      "*flymake.el"))
                                                                    :source
                                                                    "MELPA"
                                                                    :protocol
                                                                    https
                                                                    :inherit t
                                                                    :depth 1
                                                                    :ref
                                                                    "b82deb8d5b0d9515f8d026af183758a069ba6f69"))
 (treemacs :source "lockfile" :date (26009 6537 311915 533000) :recipe
                                                               (:package
                                                                "treemacs"
                                                                :fetcher github
                                                                :repo
                                                                "Alexander-Miller/treemacs"
                                                                :files
                                                                (:defaults
                                                                 "Changelog.org"
                                                                 "icons"
                                                                 "src/elisp/treemacs*.el"
                                                                 "src/scripts/treemacs*.py"
                                                                 (:exclude
                                                                  "src/extra/*"))
                                                                :source "MELPA"
                                                                :protocol https
                                                                :inherit t
                                                                :depth 1 :ref
                                                                         "529876dcc0d2c30667f1697c4eb7a5f137da4c3e"))
 (pfuture :source "lockfile" :date (26009 6537 310234 625000) :recipe
                                                              (:package
                                                               "pfuture" :repo
                                                                         "Alexander-Miller/pfuture"
                                                               :fetcher github
                                                               :files
                                                               ("*.el" "*.el.in"
                                                                "dir" "*.info"
                                                                "*.texi"
                                                                "*.texinfo"
                                                                "doc/dir"
                                                                "doc/*.info"
                                                                "doc/*.texi"
                                                                "doc/*.texinfo"
                                                                "lisp/*.el"
                                                                (:exclude
                                                                 ".dir-locals.el"
                                                                 "test.el"
                                                                 "tests.el"
                                                                 "*-test.el"
                                                                 "*-tests.el"
                                                                 "LICENSE"
                                                                 "README*"
                                                                 "*-pkg.el"))
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "19b53aebbc0f2da31de6326c495038901bffb73c"))
 (cfrs :source "lockfile" :date (26009 6537 308383 316000) :recipe
                                                           (:package "cfrs"
                                                            :repo
                                                            "Alexander-Miller/cfrs"
                                                            :fetcher github
                                                            :files
                                                            ("*.el" "*.el.in"
                                                             "dir" "*.info"
                                                             "*.texi"
                                                             "*.texinfo"
                                                             "doc/dir"
                                                             "doc/*.info"
                                                             "doc/*.texi"
                                                             "doc/*.texinfo"
                                                             "lisp/*.el"
                                                             (:exclude
                                                              ".dir-locals.el"
                                                              "test.el"
                                                              "tests.el"
                                                              "*-test.el"
                                                              "*-tests.el"
                                                              "LICENSE"
                                                              "README*"
                                                              "*-pkg.el"))
                                                            :source "MELPA"
                                                            :protocol https
                                                            :inherit t :depth 1
                                                            :ref
                                                            "f3a21f237b2a54e6b9f8a420a9da42b4f0a63121"))
 (posframe :source "lockfile" :date (26009 6537 307216 43000) :recipe
                                                              (:package
                                                               "posframe"
                                                               :fetcher github
                                                               :repo
                                                               "tumashu/posframe"
                                                               :files
                                                               ("*.el" "*.el.in"
                                                                "dir" "*.info"
                                                                "*.texi"
                                                                "*.texinfo"
                                                                "doc/dir"
                                                                "doc/*.info"
                                                                "doc/*.texi"
                                                                "doc/*.texinfo"
                                                                "lisp/*.el"
                                                                (:exclude
                                                                 ".dir-locals.el"
                                                                 "test.el"
                                                                 "tests.el"
                                                                 "*-test.el"
                                                                 "*-tests.el"
                                                                 "LICENSE"
                                                                 "README*"
                                                                 "*-pkg.el"))
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "017deece88360c7297265680d78a0bb316470716"))
 (load-relative :source "lockfile" :date (26009 6537 305921 340000) :recipe
                                                                    (:package
                                                                     "load-relative"
                                                                     :fetcher
                                                                     github
                                                                     :repo
                                                                     "rocky/emacs-load-relative"
                                                                     :files
                                                                     ("*.el"
                                                                      "*.el.in"
                                                                      "dir"
                                                                      "*.info"
                                                                      "*.texi"
                                                                      "*.texinfo"
                                                                      "doc/dir"
                                                                      "doc/*.info"
                                                                      "doc/*.texi"
                                                                      "doc/*.texinfo"
                                                                      "lisp/*.el"
                                                                      (:exclude
                                                                       ".dir-locals.el"
                                                                       "test.el"
                                                                       "tests.el"
                                                                       "*-test.el"
                                                                       "*-tests.el"
                                                                       "LICENSE"
                                                                       "README*"
                                                                       "*-pkg.el"))
                                                                     :source
                                                                     "MELPA"
                                                                     :protocol
                                                                     https
                                                                     :inherit t
                                                                     :depth 1
                                                                     :ref
                                                                     "b7987c265a64435299d6b02f960ed2c894c4a145"))
 (loc-changes :source "lockfile" :date (26009 6537 304403 518000) :recipe
                                                                  (:package
                                                                   "loc-changes"
                                                                   :fetcher
                                                                   github :repo
                                                                          "rocky/emacs-loc-changes"
                                                                   :files
                                                                   ("loc-changes.el")
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "622371e432f50626aaac82f8ee2841f71685b0fb"))
 (test-simple :source "lockfile" :date (26009 6537 303408 608000) :recipe
                                                                  (:package
                                                                   "test-simple"
                                                                   :fetcher
                                                                   github :repo
                                                                          "rocky/emacs-test-simple"
                                                                   :files
                                                                   ("*.el"
                                                                    "*.el.in"
                                                                    "dir"
                                                                    "*.info"
                                                                    "*.texi"
                                                                    "*.texinfo"
                                                                    "doc/dir"
                                                                    "doc/*.info"
                                                                    "doc/*.texi"
                                                                    "doc/*.texinfo"
                                                                    "lisp/*.el"
                                                                    (:exclude
                                                                     ".dir-locals.el"
                                                                     "test.el"
                                                                     "tests.el"
                                                                     "*-test.el"
                                                                     "*-tests.el"
                                                                     "LICENSE"
                                                                     "README*"
                                                                     "*-pkg.el"))
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "8b191842318bb05da74052025192d32ebebb033a"))
 (bui :source "lockfile" :date (26009 6537 302452 121000) :recipe
                                                          (:package "bui" :repo
                                                                          "alezost/bui.el"
                                                           :fetcher github
                                                           :files
                                                           ("*.el" "*.el.in"
                                                            "dir" "*.info"
                                                            "*.texi" "*.texinfo"
                                                            "doc/dir"
                                                            "doc/*.info"
                                                            "doc/*.texi"
                                                            "doc/*.texinfo"
                                                            "lisp/*.el"
                                                            (:exclude
                                                             ".dir-locals.el"
                                                             "test.el"
                                                             "tests.el"
                                                             "*-test.el"
                                                             "*-tests.el"
                                                             "LICENSE" "README*"
                                                             "*-pkg.el"))
                                                           :source "MELPA"
                                                           :protocol https
                                                           :inherit t :depth 1
                                                           :ref
                                                           "f3a137628e112a91910fd33c0cff0948fa58d470"))
 (lsp-docker :source "lockfile" :date (26009 6537 301042 802000) :recipe
                                                                 (:package
                                                                  "lsp-docker"
                                                                  :repo
                                                                  "emacs-lsp/lsp-docker"
                                                                  :fetcher
                                                                  github :files
                                                                         ("*.el"
                                                                          "*.el.in"
                                                                          "dir"
                                                                          "*.info"
                                                                          "*.texi"
                                                                          "*.texinfo"
                                                                          "doc/dir"
                                                                          "doc/*.info"
                                                                          "doc/*.texi"
                                                                          "doc/*.texinfo"
                                                                          "lisp/*.el"
                                                                          (:exclude
                                                                           ".dir-locals.el"
                                                                           "test.el"
                                                                           "tests.el"
                                                                           "*-test.el"
                                                                           "*-tests.el"
                                                                           "LICENSE"
                                                                           "README*"
                                                                           "*-pkg.el"))
                                                                  :source
                                                                  "MELPA"
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "60e1103ac7c8e30d036ea65fad489210682d6259"))
 (loop :source "lockfile" :date (26009 6537 299473 824000) :recipe
       (:package "loop" :repo "Wilfred/loop.el" :fetcher github :files
                                                                ("*.el"
                                                                 "*.el.in" "dir"
                                                                 "*.info"
                                                                 "*.texi"
                                                                 "*.texinfo"
                                                                 "doc/dir"
                                                                 "doc/*.info"
                                                                 "doc/*.texi"
                                                                 "doc/*.texinfo"
                                                                 "lisp/*.el"
                                                                 (:exclude
                                                                  ".dir-locals.el"
                                                                  "test.el"
                                                                  "tests.el"
                                                                  "*-test.el"
                                                                  "*-tests.el"
                                                                  "LICENSE"
                                                                  "README*"
                                                                  "*-pkg.el"))
        :source "MELPA" :protocol https :inherit t :depth 1 :ref
                                                            "9db6372791bbd0cf3fa907ed0ae3e6b7bcf6cc57"))
 (json-snatcher :source "lockfile" :date (26009 6537 297642 583000) :recipe
                                                                    (:package
                                                                     "json-snatcher"
                                                                     :fetcher
                                                                     github
                                                                     :repo
                                                                     "Sterlingg/json-snatcher"
                                                                     :files
                                                                     ("*.el"
                                                                      "*.el.in"
                                                                      "dir"
                                                                      "*.info"
                                                                      "*.texi"
                                                                      "*.texinfo"
                                                                      "doc/dir"
                                                                      "doc/*.info"
                                                                      "doc/*.texi"
                                                                      "doc/*.texinfo"
                                                                      "lisp/*.el"
                                                                      (:exclude
                                                                       ".dir-locals.el"
                                                                       "test.el"
                                                                       "tests.el"
                                                                       "*-test.el"
                                                                       "*-tests.el"
                                                                       "LICENSE"
                                                                       "README*"
                                                                       "*-pkg.el"))
                                                                     :source
                                                                     "MELPA"
                                                                     :protocol
                                                                     https
                                                                     :inherit t
                                                                     :depth 1
                                                                     :ref
                                                                     "b28d1c0670636da6db508d03872d96ffddbc10f2"))
 (rust-mode :source "lockfile" :date (26009 6537 296504 444000) :recipe
                                                                (:package
                                                                 "rust-mode"
                                                                 :repo
                                                                 "rust-lang/rust-mode"
                                                                 :fetcher github
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "9c26dc1195ec05309ee15c014631fb9abd6cf5d2"))
 (xterm-color :source "lockfile" :date (26009 6537 295094 455000) :recipe
                                                                  (:package
                                                                   "xterm-color"
                                                                   :repo
                                                                   "atomontage/xterm-color"
                                                                   :fetcher
                                                                   github :files
                                                                          ("*.el"
                                                                           "*.el.in"
                                                                           "dir"
                                                                           "*.info"
                                                                           "*.texi"
                                                                           "*.texinfo"
                                                                           "doc/dir"
                                                                           "doc/*.info"
                                                                           "doc/*.texi"
                                                                           "doc/*.texinfo"
                                                                           "lisp/*.el"
                                                                           (:exclude
                                                                            ".dir-locals.el"
                                                                            "test.el"
                                                                            "tests.el"
                                                                            "*-test.el"
                                                                            "*-tests.el"
                                                                            "LICENSE"
                                                                            "README*"
                                                                            "*-pkg.el"))
                                                                   :source
                                                                   "MELPA"
                                                                   :protocol
                                                                   https
                                                                   :inherit t
                                                                   :depth 1 :ref
                                                                            "2ad407c651e90fff2ea85d17bf074cee2c022912"))
 (htmlize :source "lockfile" :date (26009 6537 294014 475000) :recipe
                                                              (:package
                                                               "htmlize"
                                                               :fetcher github
                                                               :repo
                                                               "hniksic/emacs-htmlize"
                                                               :version-regexp
                                                               "release/\\(.*\\)"
                                                               :files
                                                               ("*.el" "*.el.in"
                                                                "dir" "*.info"
                                                                "*.texi"
                                                                "*.texinfo"
                                                                "doc/dir"
                                                                "doc/*.info"
                                                                "doc/*.texi"
                                                                "doc/*.texinfo"
                                                                "lisp/*.el"
                                                                (:exclude
                                                                 ".dir-locals.el"
                                                                 "test.el"
                                                                 "tests.el"
                                                                 "*-test.el"
                                                                 "*-tests.el"
                                                                 "LICENSE"
                                                                 "README*"
                                                                 "*-pkg.el"))
                                                               :source "MELPA"
                                                               :protocol https
                                                               :inherit t :depth
                                                                          1 :ref
                                                                            "dd27bc3f26efd728f2b1f01f9e4ac4f61f2ffbf9"))
 (powerline :source "lockfile" :date (26009 6537 292215 846000) :recipe
                                                                (:package
                                                                 "powerline"
                                                                 :fetcher github
                                                                 :repo
                                                                 "milkypostman/powerline"
                                                                 :files
                                                                 ("*.el"
                                                                  "*.el.in"
                                                                  "dir" "*.info"
                                                                  "*.texi"
                                                                  "*.texinfo"
                                                                  "doc/dir"
                                                                  "doc/*.info"
                                                                  "doc/*.texi"
                                                                  "doc/*.texinfo"
                                                                  "lisp/*.el"
                                                                  (:exclude
                                                                   ".dir-locals.el"
                                                                   "test.el"
                                                                   "tests.el"
                                                                   "*-test.el"
                                                                   "*-tests.el"
                                                                   "LICENSE"
                                                                   "README*"
                                                                   "*-pkg.el"))
                                                                 :source "MELPA"
                                                                 :protocol https
                                                                 :inherit t
                                                                 :depth 1 :ref
                                                                          "c35c35bdf5ce2d992882c1f06f0f078058870d4a"))
 (taxy :source "lockfile" :date (26009 6537 291250 622000) :recipe
                                                           (:package "taxy"
                                                            :repo
                                                            "https://github.com/alphapapa/taxy.el.git"
                                                            :local-repo "taxy"
                                                            :files
                                                            ("*"
                                                             (:exclude ".git"
                                                              "images"))
                                                            :source
                                                            "GNU-devel ELPA"
                                                            :protocol https
                                                            :inherit t :depth 1
                                                            :ref
                                                            "b27fa67ecf3f8954ce0d5c2747d1de4dc94ff09f"))
 (taxy-magit-section :source "lockfile" :date (26009 6537 289604 188000) :recipe
                                                                         (:package
                                                                          "taxy-magit-section"
                                                                          :repo
                                                                          "https://github.com/alphapapa/taxy.el.git"
                                                                          :local-repo
                                                                          "taxy-magit-section"
                                                                          :branch
                                                                          "package/taxy-magit-section"
                                                                          :files
                                                                          ("*"
                                                                           (:exclude
                                                                            ".git"))
                                                                          :source
                                                                          "GNU-devel ELPA"
                                                                          :protocol
                                                                          https
                                                                          :inherit
                                                                          t
                                                                          :depth
                                                                          1 :ref
                                                                            "594531569c03206cbc83b74d679ffd0e93edb4d0"))
 (peg :source "lockfile" :date (26009 6537 288544 56000) :recipe
                                                         (:package "peg" :repo
                                                                         "git://git.sv.gnu.org/emacs/elpa"
                                                          :local-repo "peg"
                                                          :branch
                                                          "externals/peg" :files
                                                                          ("*"
                                                                           (:exclude
                                                                            ".git"
                                                                            "COPYING"))
                                                          :source
                                                          "GNU-devel ELPA"
                                                          :protocol https
                                                          :inherit t :depth 1
                                                          :ref
                                                          "f55ca24ba1d51d80e920d611217140e1013e4f3d"))
 (pocket-lib :source "lockfile" :date (26009 6537 286813 494000) :recipe
                                                                 (:package
                                                                  "pocket-lib"
                                                                  :fetcher
                                                                  github :repo
                                                                         "alphapapa/pocket-lib.el"
                                                                  :files
                                                                  ("*.el"
                                                                   "*.el.in"
                                                                   "dir"
                                                                   "*.info"
                                                                   "*.texi"
                                                                   "*.texinfo"
                                                                   "doc/dir"
                                                                   "doc/*.info"
                                                                   "doc/*.texi"
                                                                   "doc/*.texinfo"
                                                                   "lisp/*.el"
                                                                   (:exclude
                                                                    ".dir-locals.el"
                                                                    "test.el"
                                                                    "tests.el"
                                                                    "*-test.el"
                                                                    "*-tests.el"
                                                                    "LICENSE"
                                                                    "README*"
                                                                    "*-pkg.el"))
                                                                  :source
                                                                  "MELPA"
                                                                  :protocol
                                                                  https :inherit
                                                                        t :depth
                                                                          1 :ref
                                                                            "f794e3e619e1f6cad25bbfd5fe019a7e62820bf4"))
 (ov :source "lockfile" :date (26009 6537 285738 283000) :recipe
                                                         (:package "ov" :fetcher
                                                                        github
                                                          :repo
                                                          "emacsorphanage/ov"
                                                          :files
                                                          ("*.el" "*.el.in"
                                                           "dir" "*.info"
                                                           "*.texi" "*.texinfo"
                                                           "doc/dir"
                                                           "doc/*.info"
                                                           "doc/*.texi"
                                                           "doc/*.texinfo"
                                                           "lisp/*.el"
                                                           (:exclude
                                                            ".dir-locals.el"
                                                            "test.el" "tests.el"
                                                            "*-test.el"
                                                            "*-tests.el"
                                                            "LICENSE" "README*"
                                                            "*-pkg.el"))
                                                          :source "MELPA"
                                                          :protocol https
                                                          :inherit t :depth 1
                                                          :ref
                                                          "e2971ad986b6ac441e9849031d34c56c980cf40b"))
 (rainbow-identifiers :source "lockfile" :date (26009 6537 284253 663000)
                      :recipe
                      (:package "rainbow-identifiers" :fetcher github :repo
                                                                      "Fanael/rainbow-identifiers"
                       :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref
                                                                           "19fbfded1baa98d12335f26f6d7b20e5ae44ce2e"))
 (visual-fill-column :source "lockfile" :date (26009 6537 282087 473000) :recipe
                                                                         (:package
                                                                          "visual-fill-column"
                                                                          :fetcher
                                                                          codeberg
                                                                          :repo
                                                                          "joostkremers/visual-fill-column"
                                                                          :files
                                                                          ("*.el"
                                                                           "*.el.in"
                                                                           "dir"
                                                                           "*.info"
                                                                           "*.texi"
                                                                           "*.texinfo"
                                                                           "doc/dir"
                                                                           "doc/*.info"
                                                                           "doc/*.texi"
                                                                           "doc/*.texinfo"
                                                                           "lisp/*.el"
                                                                           (:exclude
                                                                            ".dir-locals.el"
                                                                            "test.el"
                                                                            "tests.el"
                                                                            "*-test.el"
                                                                            "*-tests.el"
                                                                            "LICENSE"
                                                                            "README*"
                                                                            "*-pkg.el"))
                                                                          :source
                                                                          "MELPA"
                                                                          :protocol
                                                                          https
                                                                          :inherit
                                                                          t
                                                                          :depth
                                                                          1 :ref
                                                                            "db7c7c236555c9c684e1294a277efefdc25fa5c4")))
