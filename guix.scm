;; Agent-Zero Genesis Guix Manifest
;; This file can be used with 'guix environment -m guix.scm'

(use-modules (gnu packages)
             (gnu packages guile)
             (gnu packages maths)
             (gnu packages pkg-config)
             (gnu packages boost)
             (gnu packages cmake)
             (gnu packages gcc)
             (gnu packages opencog))

(packages->manifest
  (list
    ;; Core Guile
    guile-3.0
    guile-lib
    
    ;; Build tools
    cmake
    gcc-toolchain
    pkg-config
    
    ;; OpenCog packages
    cogutil
    atomspace
    opencog
    
    ;; Math and scientific computing
    boost))
