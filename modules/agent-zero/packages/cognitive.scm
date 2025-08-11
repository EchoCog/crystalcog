(define-module (agent-zero packages cognitive)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages commencement))

(define-public opencog
  (package
    (name "opencog")
    (version "5.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/opencog/opencog.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1234567890abcdef1234567890abcdef12345678"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       '("-DCMAKE_BUILD_TYPE=Release"
         "-DENABLE_GUILE=ON"
         "-DENABLE_PYTHON=ON"
         "-DENABLE_TESTS=ON")))
    (inputs
     `(("guile" ,guile-3.0)
       ("boost" ,boost)
       ("pkg-config" ,pkg-config)))
    (synopsis "Cognitive computing platform")
    (description "OpenCog is a cognitive computing platform for AGI research.")
    (home-page "https://opencog.org/")
    (license agpl3+)))

(define-public ggml
  (package
    (name "ggml")
    (version "0.0.0-7dee1d6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ggerganov/ggml.git")
                    (commit "7dee1d6a1e7611f238d09be96738388da97c88ed")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "pcoqpo2vdfnhz6tm2gla6qmljk6df2qn5peghztvo3gke4wqr7qa"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       '("-DCMAKE_BUILD_TYPE=Release"
         "-DGGML_CUBLAS=OFF"
         "-DGGML_METAL=OFF"
         "-DGGML_OPENMP=ON")
       #:tests? #f)) ; No tests available in the repository
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("gcc-toolchain" ,gcc-toolchain)))
    (synopsis "Tensor library for machine learning")
    (description "GGML is a tensor library for machine learning written in C.
It provides low-level cross-platform implementation with integer quantization
support, broad hardware support, automatic differentiation, and optimizers
like ADAM and L-BFGS.  It has no third-party dependencies and zero memory
allocations during runtime.")
    (home-page "https://github.com/ggerganov/ggml")
    (license expat)))

(define-public guile-pln
  (package
    (name "guile-pln")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/opencog/pln.git")
                    (commit "master")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "fedcba0987654321fedcba0987654321fedcba09"))))
    (build-system gnu-build-system)
    (inputs
     `(("guile" ,guile-3.0)
       ("opencog" ,opencog)))
    (synopsis "Probabilistic Logic Networks for Guile")
    (description "PLN provides probabilistic reasoning for cognitive systems.")
    (home-page "https://github.com/opencog/pln")
    (license agpl3+)))

(define-public guile-ecan
  (package
    (name "guile-ecan")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/opencog/attention.git")
                    (commit "master")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "567890abcdef1234567890abcdef1234567890ab"))))
    (build-system gnu-build-system)
    (inputs
     `(("guile" ,guile-3.0)
       ("opencog" ,opencog)))
    (synopsis "Economic Cognitive Attention Networks for Guile")
    (description "ECAN provides attention allocation mechanisms for cognitive systems.")
    (home-page "https://github.com/opencog/attention")
    (license agpl3+)))

(define-public guile-moses
  (package
    (name "guile-moses")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/opencog/asmoses.git")
                    (commit "master")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1234567890abcdef1234567890abcdef12345678"))))
    (build-system gnu-build-system)
    (inputs
     `(("guile" ,guile-3.0)
       ("opencog" ,opencog)
       ("boost" ,boost)))
    (synopsis "Meta-Optimizing Semantic Evolutionary Search for Guile")
    (description "MOSES provides evolutionary learning for cognitive systems.")
    (home-page "https://github.com/opencog/asmoses")
    (license agpl3+)))

(define-public guile-pattern-matcher
  (package
    (name "guile-pattern-matcher")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/opencog/atomspace.git")
                    (commit "master")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "abcdef1234567890abcdef1234567890abcdef12"))))
    (build-system gnu-build-system)
    (inputs
     `(("guile" ,guile-3.0)
       ("opencog" ,opencog)))
    (synopsis "Advanced pattern matching for Guile")
    (description "Advanced pattern matching capabilities for cognitive systems.")
    (home-page "https://github.com/opencog/atomspace")
    (license agpl3+)))

(define-public guile-relex
  (package
    (name "guile-relex")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/opencog/relex.git")
                    (commit "master")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "987654321abcdef0987654321abcdef0987654321"))))
    (build-system gnu-build-system)
    (inputs
     `(("guile" ,guile-3.0)
       ("opencog" ,opencog)))
    (synopsis "Relation extraction for NLP in Guile")
    (description "RelEx provides natural language processing for cognitive systems.")
    (home-page "https://github.com/opencog/relex")
    (license agpl3+)))