(use-modules
  (guix gexp)
  (guix utils)
  (guix packages)
  ((guix licenses) #:prefix license:)
  (guix git-download)
  (guix build-system gnu)
  (gnu packages)
  (gnu packages autotools)
  (gnu packages compression)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages pkg-config)
  (gnu packages texinfo))

(define vcs-file?
  ;; Return true if the given file is under version control.
  (or (git-predicate (current-source-directory))
      (const #t)))                                ;not in a Git checkout

(package
  (name "guile-zlib")
  (version "42.0")
  (source (local-file "." "guile-zlib-checkout"
                      #:recursive? #t
                      #:select? vcs-file?))
  (build-system gnu-build-system)
  (native-inputs (list autoconf automake pkg-config guile-3.0))
  (inputs (list guile-3.0 zlib))
  (synopsis "Guile bindings to zlib")
  (description
   "This package provides Guile bindings for zlib, a lossless
data-compression library.  The bindings are written in pure Scheme by using
Guile's foreign function interface.")
  (home-page "https://notabug.org/guile-zlib/guile-zlib")
  (license license:gpl3+))
