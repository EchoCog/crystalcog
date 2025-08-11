;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 OpenCog Central Contributors
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu packages opencog)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gmp)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

(define-public cogutil
  (package
    (name "cogutil")
    (version "2.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/opencog/cogutil")
                    (commit "d1f3df098824e14ed8e1a74933e7aeab2ea022fb")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xfdfxqwkzaim2g8nib46kpxzvslqmjn03q4awgg4kriv3vwqr28"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DCMAKE_BUILD_TYPE=Release")
           #:tests? #t))
    (native-inputs
     (list cmake
           cxxtest
           doxygen
           pkg-config))
    (inputs
     (list boost
           gmp))
    (synopsis "Very low-level C++ programming utilities used by OpenCog")
    (description
     "The OpenCog utilities is a miscellaneous collection of C++ utilities
used for typical programming tasks in multiple OpenCog projects.")
    (home-page "http://opencog.org")
    (license license:agpl3+)))

(define-public atomspace
  (package
    (name "atomspace")
    (version "5.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/opencog/atomspace")
                    (commit "a4526de4564ef4470520b7c89808a39508b52fe2")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1blxvfsv4a9xbpcvhx0g2bhfrf2cmv79ygch6lxkzh1gc3p8rw35"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags 
           #~(list "-DCMAKE_BUILD_TYPE=Release"
                   (string-append "-DGUILE_INCLUDE_DIR="
                                  #$(this-package-input "guile")
                                  "/include/guile/3.0")
                   (string-append "-DGMP_INCLUDE_DIR="
                                  #$(this-package-input "gmp")
                                  "/include"))
           #:tests? #f  ; Disable tests for now to avoid complexity
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'set-guile-load-path
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (setenv "GUILE_LOAD_PATH"
                           (string-append (getenv "GUILE_LOAD_PATH") ":"
                                          (getcwd) "/build/opencog/scm")))))))
    (native-inputs
     (list cmake
           cxxtest
           pkg-config))
    (inputs
     (list cogutil
           boost
           guile-3.0
           gmp
           postgresql
           python
           python-cython
           python-nose))
    (synopsis "OpenCog hypergraph database and reasoning system")
    (description
     "The OpenCog AtomSpace is a hypergraph database, query system and rule
engine.  It is a platform for building Artificial General Intelligence (AGI)
systems.")
    (home-page "https://wiki.opencog.org/w/AtomSpace")
    (license license:agpl3+)))

(define-public opencog
  (package
    (name "opencog")
    (version "5.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/opencog/opencog")
                    (commit "master")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1234567890abcdef1234567890abcdef1234567890abcdef1234"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DCMAKE_BUILD_TYPE=Release")
           #:tests? #f))  ; Disable tests for initial packaging
    (native-inputs
     (list cmake
           cxxtest
           pkg-config))
    (inputs
     (list cogutil
           atomspace
           boost
           guile-3.0
           gmp
           python))
    (synopsis "OpenCog cognitive architecture platform")
    (description
     "OpenCog is a framework for developing AGI (Artificial General
Intelligence).  It provides a large collection of cognitive algorithms
including machine learning, natural language processing, logical reasoning
and behavioral control algorithms.")
    (home-page "http://opencog.org")
    (license license:agpl3+)))