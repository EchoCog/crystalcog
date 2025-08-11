;; Enhanced Tensor Field Encoding Tests
;; /tests/agent-zero/enhanced-tensor-tests.scm

(use-modules (srfi srfi-64)
             (agent-zero kernel)
             (agent-zero meta-cognition))

(test-begin "enhanced-tensor-field-encoding-tests")

(test-group "backward-compatibility"
  (test-assert "basic tensor field encoding unchanged"
    (let ((kernel (spawn-cognitive-kernel '(4 6) 0.5)))
      (let ((encoding (tensor-field-encoding kernel)))
        (and (= (length encoding) 2)
             (= (car encoding) 4.0)
             (= (cadr encoding) 9.0)))))

  (test-assert "encoding consistency maintained"
    (let ((kernel1 (spawn-cognitive-kernel '(4 4) 0.7))
          (kernel2 (spawn-cognitive-kernel '(4 4) 0.7)))
      (equal? (tensor-field-encoding kernel1)
              (tensor-field-encoding kernel2)))))

(test-group "encoding-types"
  (test-assert "fibonacci encoding"
    (let ((kernel (spawn-cognitive-kernel '(3 5) 0.6)))
      (let ((encoding (tensor-field-encoding kernel 'fibonacci)))
        (and (= (length encoding) 2)
             (> (car encoding) 0)
             (> (cadr encoding) 0)))))

  (test-assert "harmonic encoding"
    (let ((kernel (spawn-cognitive-kernel '(2 4) 0.5)))
      (let ((encoding (tensor-field-encoding kernel 'harmonic)))
        (and (= (length encoding) 2)
             (> (car encoding) 0)
             (> (cadr encoding) 0)))))

  (test-assert "factorial encoding"
    (let ((kernel (spawn-cognitive-kernel '(2 3) 0.8)))
      (let ((encoding (tensor-field-encoding kernel 'factorial)))
        (and (= (length encoding) 2)
             (< (abs (- (car encoding) 1.6)) 0.0001)   ; 2 * 1 * 0.8
             (< (abs (- (cadr encoding) 4.8)) 0.0001))))) ; 3 * 2 * 0.8

  (test-assert "power-of-two encoding"
    (let ((kernel (spawn-cognitive-kernel '(4 8) 0.5)))
      (let ((encoding (tensor-field-encoding kernel 'power-of-two)))
        (and (= (length encoding) 2)
             (= (car encoding) 2.0)   ; 4 * 1 * 0.5
             (= (cadr encoding) 8.0)))))) ; 8 * 2 * 0.5

(test-group "attention-weighting"
  (test-assert "attention affects encoding"
    (let ((kernel1 (spawn-cognitive-kernel '(4 4) 0.5))
          (kernel2 (spawn-cognitive-kernel '(4 4) 1.0)))
      (let ((encoding1 (tensor-field-encoding kernel1))
            (encoding2 (tensor-field-encoding kernel2)))
        (and (< (car encoding1) (car encoding2))
             (< (cadr encoding1) (cadr encoding2))))))

  (test-assert "attention can be disabled"
    (let ((kernel (spawn-cognitive-kernel '(2 3) 0.5)))
      (let ((with-attention (tensor-field-encoding kernel 'prime #t))
            (without-attention (tensor-field-encoding kernel 'prime #f)))
        (and (not (equal? with-attention without-attention))
             (> (car without-attention) (car with-attention)))))))

(test-group "meta-level-encoding"
  (test-assert "meta-level can be included"
    (let ((kernel (spawn-cognitive-kernel '(4 4) 0.7)))
      (let ((without-meta (tensor-field-encoding kernel 'prime #t #f))
            (with-meta (tensor-field-encoding kernel 'prime #t #t)))
        (and (= (length without-meta) 2)
             (= (length with-meta) 3)
             (= (caddr with-meta) 0))))) ; meta-level starts at 0

  (test-assert "meta-level reflects kernel state"
    (let ((kernel (spawn-cognitive-kernel '(2 2) 0.5)))
      (let ((encoding (tensor-field-encoding kernel 'prime #t #t)))
        (= (caddr encoding) (recursive-depth kernel))))))

(test-group "normalization"
  (test-assert "unit normalization"
    (let ((kernel (spawn-cognitive-kernel '(3 4) 0.8)))
      (let ((normalized (tensor-field-encoding kernel 'prime #t #f 'unit)))
        (let ((magnitude (sqrt (apply + (map (lambda (x) (* x x)) normalized)))))
          (< (abs (- magnitude 1.0)) 0.0001))))) ; Close to unit length

  (test-assert "standard normalization"
    (let ((kernel (spawn-cognitive-kernel '(5 7) 0.6)))
      (let ((standardized (tensor-field-encoding kernel 'prime #t #f 'standard)))
        (let ((mean (/ (apply + standardized) (length standardized))))
          (< (abs mean) 0.0001)))))) ; Close to zero mean

(test-group "hypergraph-tensor-encoding"
  (test-assert "hypergraph encoding includes connectivity"
    (let ((kernel (spawn-cognitive-kernel '(4 4) 0.7)))
      (let ((hypergraph-encoding (hypergraph-tensor-encoding kernel)))
        (and (> (length hypergraph-encoding) 2)
             (list? hypergraph-encoding)))))

  (test-assert "hypergraph encoding differs from basic"
    (let ((kernel (spawn-cognitive-kernel '(3 3) 0.8)))
      (let ((basic (tensor-field-encoding kernel))
            (hypergraph (hypergraph-tensor-encoding kernel)))
        (not (equal? basic hypergraph))))))

(test-group "cognitive-operation-encoding"
  (test-assert "reasoning operation encoding"
    (let ((kernel (spawn-cognitive-kernel '(4 4) 0.6)))
      (let ((reasoning-encoding (cognitive-tensor-field-encoding kernel 'reasoning)))
        (and (= (length reasoning-encoding) 2)
             (> (car reasoning-encoding) 0)
             (> (cadr reasoning-encoding) 0)))))

  (test-assert "different operations produce different encodings"
    (let ((kernel (spawn-cognitive-kernel '(3 3) 0.7)))
      (let ((reasoning (cognitive-tensor-field-encoding kernel 'reasoning))
            (learning (cognitive-tensor-field-encoding kernel 'learning))
            (attention (cognitive-tensor-field-encoding kernel 'attention)))
        (and (not (equal? reasoning learning))
             (not (equal? reasoning attention))
             (not (equal? learning attention))))))

  (test-assert "all cognitive operations supported"
    (let ((kernel (spawn-cognitive-kernel '(2 2) 0.5)))
      (let ((operations '(reasoning learning attention memory adaptation)))
        (= (length (filter (lambda (op)
                             (let ((encoding (cognitive-tensor-field-encoding kernel op)))
                               (and (list? encoding)
                                    (= (length encoding) 2)
                                    (number? (car encoding))
                                    (number? (cadr encoding)))))
                           operations))
           5)))))

(test-group "mathematical-sequence-generators"
  (test-assert "prime generation"
    (let ((primes (generate-primes 5)))
      (and (= (length primes) 5)
           (equal? primes '(2 3 5 7 11)))))

  (test-assert "fibonacci generation"
    (let ((fib (generate-fibonacci 5)))
      (and (= (length fib) 5)
           (equal? fib '(1 1 2 3 5)))))

  (test-assert "harmonic generation"
    (let ((harmonics (generate-harmonics 3)))
      (and (= (length harmonics) 3)
           (= (car harmonics) 1.0)
           (= (cadr harmonics) 0.5))))

  (test-assert "factorial generation"
    (let ((factorials (generate-factorials 4)))
      (and (= (length factorials) 4)
           (equal? factorials '(1 2 6 24)))))

  (test-assert "powers of two generation"
    (let ((powers (generate-powers-of-two 4)))
      (and (= (length powers) 4)
           (equal? powers '(1 2 4 8))))))

(test-end "enhanced-tensor-field-encoding-tests")

;; Test runner function
(define (run-enhanced-tensor-tests)
  "Run enhanced tensor field encoding tests."
  (display "Running enhanced tensor field encoding tests...")
  (newline))

;; Export test runner
(define-public run-enhanced-tensor-tests run-enhanced-tensor-tests)