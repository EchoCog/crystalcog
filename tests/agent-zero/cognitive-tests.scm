;; Agent-Zero Cognitive Function Tests
;; /tests/agent-zero/cognitive-tests.scm

(use-modules (srfi srfi-64)
             (agent-zero kernel)
             (agent-zero meta-cognition))

(test-begin "cognitive-kernel-tests")

(test-group "kernel-creation"
  (test-assert "basic kernel creation"
    (let ((kernel (spawn-cognitive-kernel '(32 32) 0.5)))
      (and kernel
           (= (length (kernel-tensor-shape kernel)) 2)
           (= (car (kernel-tensor-shape kernel)) 32)
           (= (cadr (kernel-tensor-shape kernel)) 32))))

  (test-assert "kernel with different shapes"
    (let ((kernel1 (spawn-cognitive-kernel '(64 64) 0.8))
          (kernel2 (spawn-cognitive-kernel '(128 32) 0.6)))
      (and kernel1 kernel2
           (not (equal? (kernel-tensor-shape kernel1)
                       (kernel-tensor-shape kernel2))))))

  (test-assert "kernel attention setting"
    (let ((kernel (spawn-cognitive-kernel '(16 16) 0.9)))
      (= (kernel-attention kernel) 0.9))))

(test-group "tensor-field-encoding"
  (test-assert "prime factorization encoding"
    (let ((kernel (spawn-cognitive-kernel '(2 3) 0.5)))
      (let ((encoding (tensor-field-encoding kernel)))
        (and (= (length encoding) 2)
             (> (car encoding) 0)
             (> (cadr encoding) 0)))))

  (test-assert "encoding consistency"
    (let ((kernel1 (spawn-cognitive-kernel '(4 4) 0.5))
          (kernel2 (spawn-cognitive-kernel '(4 4) 0.5)))
      (equal? (tensor-field-encoding kernel1)
              (tensor-field-encoding kernel2)))))

(test-group "hypergraph-state"
  (test-assert "hypergraph state representation"
    (let ((kernel (spawn-cognitive-kernel '(8 8) 0.7)))
      (let ((state (hypergraph-state kernel)))
        (and (list? state)
             (assoc 'atomspace state)
             (assoc 'tensor-shape state)
             (assoc 'attention state)
             (assoc 'meta-level state)))))

  (test-assert "state contains correct values"
    (let ((kernel (spawn-cognitive-kernel '(16 8) 0.6)))
      (let ((state (hypergraph-state kernel)))
        (and (equal? (assoc-ref state 'tensor-shape) '(16 8))
             (= (assoc-ref state 'attention) 0.6)
             (= (assoc-ref state 'meta-level) 0))))))

(test-end "cognitive-kernel-tests")

(test-begin "meta-cognition-tests")

(test-group "recursive-self-description"
  (test-assert "self-description generation"
    (let ((kernel (spawn-cognitive-kernel '(32 32) 0.8)))
      (let ((self-desc (recursive-self-description kernel)))
        (and (list? self-desc)
             (assoc 'tensor-shape self-desc)
             (assoc 'cognitive-function self-desc)
             (assoc 'attention-allocation self-desc)
             (assoc 'meta-level self-desc)))))

  (test-assert "meta-level increment"
    (let ((kernel (spawn-cognitive-kernel '(16 16) 0.5)))
      (let ((self-desc (recursive-self-description kernel)))
        (= (assoc-ref self-desc 'meta-level) 1)))))

(test-group "attention-allocation"
  (test-assert "ECAN network creation"
    (let ((network (make-ecan-network)))
      (ecan-network? network)))

  (test-assert "node addition to network"
    (let ((network (make-ecan-network))
          (kernel1 (spawn-cognitive-kernel '(64 64) 0.8))
          (kernel2 (spawn-cognitive-kernel '(32 32) 0.4)))
      (ecan-add-node! network kernel1)
      (ecan-add-node! network kernel2)
      (= (length (ecan-nodes network)) 2)))

  (test-assert "attention allocation"
    (let ((kernels (list (spawn-cognitive-kernel '(64 64) 0.8)
                         (spawn-cognitive-kernel '(32 32) 0.4))))
      (let ((allocations (adaptive-attention-allocation kernels '(goal-1 goal-2))))
        (and (list? allocations)
             (= (length allocations) 2)
             (every (lambda (alloc)
                      (and (assoc 'kernel alloc)
                           (assoc 'attention-score alloc)
                           (assoc 'activation-priority alloc)))
                    allocations))))))

(test-group "cognitive-reasoning"
  (test-assert "PLN backward chaining simulation"
    (let ((atomspace (make-atomspace)))
      (let ((result (pln-backward-chaining atomspace '(ConceptNode "intelligence"))))
        (and (list? result)
             (eq? (car result) 'reasoning-result)))))

  (test-assert "meta-cognitive reflection"
    (let ((kernel (spawn-cognitive-kernel '(64 32) 0.9)))
      (let ((reflection (meta-cognitive-reflection kernel)))
        (and (list? reflection)
             (assoc 'current-state reflection)
             (assoc 'self-assessment reflection)
             (assoc 'adaptation-suggestions reflection)
             (assoc 'meta-learning reflection))))))

(test-end "meta-cognition-tests")

(test-begin "integration-tests")

(test-group "kernel-meta-cognition-integration"
  (test-assert "kernel with meta-cognition"
    (let ((kernel (spawn-cognitive-kernel '(128 64) 0.85)))
      (let ((self-desc (recursive-self-description kernel))
            (state (hypergraph-state kernel)))
        (and self-desc state
             (equal? (assoc-ref self-desc 'tensor-shape)
                    (assoc-ref state 'tensor-shape))))))

  (test-assert "multiple kernels coordination"
    (let ((kernel1 (spawn-cognitive-kernel '(64 64) 0.9))
          (kernel2 (spawn-cognitive-kernel '(32 32) 0.7))
          (kernel3 (spawn-cognitive-kernel '(16 16) 0.5)))
      (let ((allocations (adaptive-attention-allocation 
                         (list kernel1 kernel2 kernel3)
                         '(reasoning learning adaptation))))
        (= (length allocations) 3)))))

(test-group "cognitive-processing-pipeline"
  (test-assert "end-to-end cognitive processing"
    (let ((kernel (spawn-cognitive-kernel '(64 64) 0.8)))
      ;; Generate self-description
      (let ((self-desc (recursive-self-description kernel)))
        ;; Perform attention allocation
        (let ((allocations (adaptive-attention-allocation 
                           (list kernel) 
                           '(reasoning))))
          ;; Get hypergraph state
          (let ((state (hypergraph-state kernel)))
            ;; Verify all components work together
            (and self-desc allocations state
                 (not (null? self-desc))
                 (not (null? allocations))
                 (not (null? state)))))))))

(test-end "integration-tests")

;; Test runner function
(define (run-all-tests)
  "Run all Agent-Zero cognitive tests."
  (display "Running Agent-Zero Cognitive Tests...")
  (newline)
  
  ;; The tests have already been defined and will run automatically
  ;; when this module is loaded by the test runner
  
  (display "All Agent-Zero tests completed!")
  (newline))

;; Export test runner for external use
(define-public run-agent-zero-tests run-all-tests)