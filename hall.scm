(hall-description
  (name "zlib")
  (prefix "guile")
  (version "0.1")
  (author "Mathieu Othacehe")
  (copyright (2020))
  (synopsis "")
  (description "")
  (home-page "")
  (license gpl3+)
  (dependencies `())
  (files (libraries
           ((scheme-file "zlib")
            (directory "zlib" ((scheme-file "config")))))
         (tests ((directory "tests" ((scheme-file "zlib")))))
         (programs ())
         (documentation
           ((org-file "README")
            (symlink "README" "README.org")
            (text-file "HACKING")
            (text-file "COPYING")
            (text-file "NEWS")
            (text-file "AUTHORS")
            (text-file "ChangeLog")))
         (infrastructure
           ((scheme-file "guix")
            (scheme-file "hall")
            (directory
              "build-aux"
              ((scheme-file "test-driver")))
            (autoconf-file "configure")
            (automake-file "Makefile")
            (in-file "pre-inst-env")))))
