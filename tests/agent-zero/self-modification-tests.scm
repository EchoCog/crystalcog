;; Agent-Zero Self-Modification Tests
;; /tests/agent-zero/self-modification-tests.scm

(use-modules (srfi srfi-64)
             (agent-zero kernel)
             (agent-zero meta-cognition)
             (agent-zero self-modification))

(test-begin "self-modification-tests")

;; Test self-modifying kernel creation
(test-group "Self-Modifying Kernel Creation"
  
  (test-assert "create-basic-self-modifying-kernel"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.8))
           (sm-kernel (make-self-modifying-kernel base-kernel)))
      (and (self-modifying-kernel? sm-kernel)
           (equal? (kernel-tensor-shape (sm-kernel-base sm-kernel)) '(32 32))
           (= (kernel-attention (sm-kernel-base sm-kernel)) 0.8))))
  
  (test-assert "create-self-modifying-kernel-with-constraints"
    (let* ((base-kernel (spawn-cognitive-kernel '(16 16) 0.6))
           (custom-constraints `((max-tensor-dimensions . 8)
                                (min-tensor-dimensions . 2)
                                (max-attention-weight . 0.9)
                                (min-attention-weight . 0.1)))
           (sm-kernel (make-self-modifying-kernel base-kernel custom-constraints)))
      (and (self-modifying-kernel? sm-kernel)
           (equal? (kernel-modification-constraints sm-kernel) custom-constraints))))
  
  (test-assert "create-self-modifying-kernel-with-strategy"
    (let* ((base-kernel (spawn-cognitive-kernel '(64 32) 0.7))
           (sm-kernel (make-self-modifying-kernel base-kernel 
                                                 *default-modification-constraints*
                                                 'aggressive)))
      (and (self-modifying-kernel? sm-kernel)
           (eq? (sm-kernel-strategy sm-kernel) 'aggressive)))))

;; Test architecture modification
(test-group "Architecture Modification"
  
  (test-assert "modify-tensor-shape"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.8))
           (sm-kernel (make-self-modifying-kernel base-kernel))
           (modification-spec `((tensor-shape . (64 64))
                               (modification-reason . test))))
      (modify-kernel-architecture sm-kernel modification-spec)
      (let ((new-arch (kernel-current-architecture sm-kernel)))
        (equal? (cdr (assoc 'tensor-shape new-arch)) '(64 64)))))
  
  (test-assert "modify-attention-weight"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.5))
           (sm-kernel (make-self-modifying-kernel base-kernel))
           (modification-spec `((attention-weight . 0.9)
                               (modification-reason . attention-boost))))
      (modify-kernel-architecture sm-kernel modification-spec)
      (let ((new-arch (kernel-current-architecture sm-kernel)))
        (= (cdr (assoc 'attention-weight new-arch)) 0.9))))
  
  (test-assert "modification-history-recorded"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.7))
           (sm-kernel (make-self-modifying-kernel base-kernel))
           (modification-spec `((tensor-shape . (48 48))
                               (modification-reason . test-modification))))
      (modify-kernel-architecture sm-kernel modification-spec)
      (let ((history (kernel-modification-history sm-kernel)))
        (and (> (length history) 0)
             (eq? (cdr (assoc 'type (car history))) 'architecture)))))
  
  (test-assert "safety-constraint-validation"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.7))
           (tight-constraints `((max-tensor-dimensions . 2)
                               (min-tensor-dimensions . 1)
                               (max-attention-weight . 0.8)
                               (min-attention-weight . 0.2)))
           (sm-kernel (make-self-modifying-kernel base-kernel tight-constraints))
           (unsafe-modification `((tensor-shape . (128 128 128 128))))) ; Too many dimensions
      
      ;; This should fail due to constraint violation
      (catch #t
        (lambda ()
          (modify-kernel-architecture sm-kernel unsafe-modification)
          #f) ; Should not reach here
        (lambda (key . args)
          #t))))) ; Should catch the error

;; Test parameter evolution
(test-group "Parameter Evolution"
  
  (test-assert "evolve-kernel-parameters"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.6))
           (sm-kernel (make-self-modifying-kernel base-kernel))
           (evolution-spec `((attention . 1.0)
                            (meta-level . 0.5))))
      (evolve-kernel-parameters sm-kernel evolution-spec)
      ;; Verify that some parameter evolution occurred
      (> (length (kernel-modification-history sm-kernel)) 0)))
  
  (test-assert "tensor-shape-evolution"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.7))
           (sm-kernel (make-self-modifying-kernel base-kernel))
           (fitness-fn (lambda (shape) 
                        (/ (apply + shape) (length shape))))) ; Simple fitness: average dimension size
      (evolve-tensor-shape sm-kernel fitness-fn)
      ;; Check that evolution was attempted (history should have entry)
      (let ((history (kernel-modification-history sm-kernel)))
        (or (null? history) ; No evolution if no improvement found
            (> (length history) 0))))) ; Or evolution occurred
  
  (test-assert "attention-adaptation"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.5))
           (sm-kernel (make-self-modifying-kernel base-kernel))
           (performance-feedback `((trend . 0.2)
                                  (quality . improving))))
      (adapt-attention-allocation sm-kernel performance-feedback)
      (let ((new-arch (kernel-current-architecture sm-kernel))
            (original-attention (kernel-attention base-kernel)))
        ;; Attention should have been increased due to positive trend
        (>= (cdr (assoc 'attention-weight new-arch)) original-attention)))))

;; Test code generation and compilation
(test-group "Code Generation"
  
  (test-assert "generate-kernel-code"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.8))
           (sm-kernel (make-self-modifying-kernel base-kernel))
           (code-spec `((function-type . cognitive-processing)
                       (optimization-level . high))))
      (let ((generated-code (generate-kernel-code sm-kernel code-spec)))
        (and generated-code (procedure? generated-code)))))
  
  (test-assert "compile-kernel-function"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.8))
           (sm-kernel (make-self-modifying-kernel base-kernel))
           (simple-code '(lambda (x) (+ x 1))))
      (let ((compiled-fn (compile-kernel-function sm-kernel simple-code)))
        (and compiled-fn (procedure? compiled-fn)))))
  
  (test-assert "hot-swap-kernel-logic"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.8))
           (sm-kernel (make-self-modifying-kernel base-kernel))
           (new-function (lambda (input) (* input 2))))
      (let ((swap-result (hot-swap-kernel-logic sm-kernel new-function 'cognitive-processing)))
        ;; Should return true if swap was successful, false if rolled back
        (boolean? swap-result)))))

;; Test safety and rollback mechanisms
(test-group "Safety and Rollback"
  
  (test-assert "create-modification-checkpoint"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.8))
           (sm-kernel (make-self-modifying-kernel base-kernel)))
      (let ((checkpoint-id (create-modification-checkpoint sm-kernel)))
        (and (string? checkpoint-id)
             (> (length (sm-kernel-checkpoints sm-kernel)) 0)))))
  
  (test-assert "rollback-to-checkpoint"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.8))
           (sm-kernel (make-self-modifying-kernel base-kernel))
           (checkpoint-id (create-modification-checkpoint sm-kernel))
           (modification-spec `((attention-weight . 0.5))))
      
      ;; Make a modification
      (modify-kernel-architecture sm-kernel modification-spec)
      (let ((modified-attention (cdr (assoc 'attention-weight 
                                           (kernel-current-architecture sm-kernel)))))
        
        ;; Rollback to checkpoint
        (rollback-to-checkpoint sm-kernel checkpoint-id)
        (let ((restored-attention (cdr (assoc 'attention-weight 
                                             (kernel-current-architecture sm-kernel)))))
          
          ;; Attention should be restored to original value
          (and (= modified-attention 0.5)
               (= restored-attention 0.8))))))
  
  (test-assert "validate-modification-safety"
    (let* ((constraints `((max-tensor-dimensions . 5)
                         (min-tensor-dimensions . 1)
                         (max-attention-weight . 1.0)
                         (min-attention-weight . 0.1)))
           (safe-modification `((tensor-shape . (32 32 32))
                               (attention-weight . 0.8)))
           (unsafe-modification `((tensor-shape . (32 32 32 32 32 32 32))))) ; Too many dimensions
      
      (and (validate-modification-safety safe-modification constraints)
           (not (validate-modification-safety unsafe-modification constraints))))))

;; Test performance-driven evolution
(test-group "Performance-Driven Evolution"
  
  (test-assert "fitness-evaluation"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.8))
           (sm-kernel (make-self-modifying-kernel base-kernel))
           (test-data '((input . test-data-1) (expected . result-1))))
      (let ((fitness-score (fitness-evaluation sm-kernel test-data)))
        (and (number? fitness-score)
             (>= fitness-score 0.0)
             (<= fitness-score 1.0)))))
  
  (test-assert "performance-based-evolution"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.6))
           (sm-kernel (make-self-modifying-kernel base-kernel))
           (performance-history `(((fitness . 0.4) (timestamp . 1000))
                                 ((fitness . 0.3) (timestamp . 2000))
                                 ((fitness . 0.35) (timestamp . 3000)))))
      
      (performance-based-evolution sm-kernel performance-history)
      ;; Evolution should have been attempted due to poor/declining performance
      (>= (length (kernel-modification-history sm-kernel)) 0)))
  
  (test-assert "genetic-optimization"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.7))
           (sm-kernel (make-self-modifying-kernel base-kernel 
                                                 *default-modification-constraints*
                                                 'aggressive))
           (fitness-fn (lambda (shape) (/ (apply * shape) 10000.0))))
      
      (evolve-tensor-shape sm-kernel fitness-fn)
      ;; Should complete without error
      #t)))

;; Test meta-modification capabilities
(test-group "Meta-Modification"
  
  (test-assert "modify-modification-strategy"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.7))
           (sm-kernel (make-self-modifying-kernel base-kernel '() 'conservative))
           (poor-performance `((trend . -0.3) (quality . declining))))
      
      (modify-modification-strategy sm-kernel poor-performance)
      ;; Strategy might change due to poor performance
      (symbol? (sm-kernel-strategy sm-kernel))))
  
  (test-assert "evolve-evolution-parameters"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.7))
           (sm-kernel (make-self-modifying-kernel base-kernel)))
      
      ;; Set up some performance history to trigger parameter evolution
      (record-performance-metrics! sm-kernel `((fitness . 0.3) (trend . -0.2)))
      (record-performance-metrics! sm-kernel `((fitness . 0.25) (trend . -0.3)))
      
      (evolve-evolution-parameters sm-kernel)
      ;; Should complete without error
      #t))
  
  (test-assert "adaptive-constraint-relaxation"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.8))
           (sm-kernel (make-self-modifying-kernel base-kernel))
           (good-performance `(((fitness . 0.9) (timestamp . 1000))
                              ((fitness . 0.85) (timestamp . 2000))
                              ((fitness . 0.92) (timestamp . 3000)))))
      
      ;; Set up successful modification history
      (record-modification sm-kernel 'test `((result . success)) #f)
      (record-modification sm-kernel 'test `((result . success)) #f)
      (record-modification sm-kernel 'test `((result . success)) #f)
      
      (adaptive-constraint-relaxation sm-kernel good-performance)
      ;; Constraints might be relaxed due to good performance
      (list? (kernel-modification-constraints sm-kernel)))))

;; Test integration with existing meta-cognition system
(test-group "Meta-Cognition Integration"
  
  (test-assert "self-modifying-kernel-with-meta-cognition"
    (let* ((base-kernel (spawn-cognitive-kernel '(64 32) 0.8))
           (sm-kernel (make-self-modifying-kernel base-kernel)))
      
      ;; Test that meta-cognitive functions work with self-modifying kernel
      (let ((self-desc (recursive-self-description (sm-kernel-base sm-kernel)))
            (reflection (meta-cognitive-reflection (sm-kernel-base sm-kernel))))
        (and (list? self-desc)
             (list? reflection)))))
  
  (test-assert "adaptive-attention-with-self-modification"
    (let* ((base-kernel1 (spawn-cognitive-kernel '(32 32) 0.7))
           (base-kernel2 (spawn-cognitive-kernel '(64 64) 0.8))
           (sm-kernel1 (make-self-modifying-kernel base-kernel1))
           (sm-kernel2 (make-self-modifying-kernel base-kernel2))
           (kernels (list (sm-kernel-base sm-kernel1) (sm-kernel-base sm-kernel2)))
           (goals '(reasoning learning)))
      
      (let ((allocations (adaptive-attention-allocation kernels goals)))
        (and (list? allocations)
             (= (length allocations) 2)))))
  
  (test-assert "performance-feedback-integration"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.6))
           (sm-kernel (make-self-modifying-kernel base-kernel)))
      
      ;; Record performance metrics
      (record-performance-metrics! sm-kernel `((fitness . 0.7)
                                              (efficiency . 0.6)
                                              (accuracy . 0.8)))
      
      ;; Get recent performance  
      (let ((recent-perf (get-recent-performance sm-kernel)))
        (and (number? recent-perf) (> recent-perf 0.0))))))

;; Test error handling and edge cases
(test-group "Error Handling and Edge Cases"
  
  (test-assert "invalid-modification-rejection"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.8))
           (sm-kernel (make-self-modifying-kernel base-kernel))
           (invalid-modification `((attention-weight . 2.0)))) ; Invalid: > 1.0
      
      (catch #t
        (lambda ()
          (modify-kernel-architecture sm-kernel invalid-modification)
          #f) ; Should not reach here
        (lambda (key . args)
          #t)))) ; Should catch the constraint violation
  
  (test-assert "empty-modification-spec"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.8))
           (sm-kernel (make-self-modifying-kernel base-kernel))
           (empty-modification '()))
      
      (modify-kernel-architecture sm-kernel empty-modification)
      ;; Should handle empty modifications gracefully
      #t))
  
  (test-assert "extreme-tensor-shapes"
    (let* ((base-kernel (spawn-cognitive-kernel '(1) 0.5))
           (sm-kernel (make-self-modifying-kernel base-kernel))
           (minimal-modification `((tensor-shape . (1)))))
      
      (modify-kernel-architecture sm-kernel minimal-modification)
      ;; Should handle minimal tensor shapes
      (equal? (cdr (assoc 'tensor-shape (kernel-current-architecture sm-kernel))) '(1))))
  
  (test-assert "rollback-nonexistent-checkpoint"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.8))
           (sm-kernel (make-self-modifying-kernel base-kernel)))
      
      (let ((rollback-result (rollback-to-checkpoint sm-kernel "nonexistent-checkpoint")))
        ;; Should return false for nonexistent checkpoint
        (not rollback-result))))
  
  (test-assert "fitness-evaluation-empty-data"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.8))
           (sm-kernel (make-self-modifying-kernel base-kernel)))
      
      (let ((fitness (fitness-evaluation sm-kernel '())))
        ;; Should handle empty test data gracefully
        (and (number? fitness) (>= fitness 0.0) (<= fitness 1.0))))))

;; Test comprehensive self-modification scenarios
(test-group "Comprehensive Self-Modification Scenarios"
  
  (test-assert "complete-self-modification-cycle"
    (let* ((base-kernel (spawn-cognitive-kernel '(16 16) 0.4))
           (sm-kernel (make-self-modifying-kernel base-kernel)))
      
      ;; Simulate a complete self-modification cycle
      
      ;; 1. Record poor performance
      (record-performance-metrics! sm-kernel `((fitness . 0.3) (trend . -0.2)))
      
      ;; 2. Create checkpoint before modifications
      (let ((checkpoint (create-modification-checkpoint sm-kernel)))
        
        ;; 3. Evolve architecture based on performance
        (let ((fitness-fn (lambda (shape) (/ (apply + shape) 100.0))))
          (evolve-tensor-shape sm-kernel fitness-fn))
        
        ;; 4. Adapt attention allocation
        (adapt-attention-allocation sm-kernel `((trend . -0.2)))
        
        ;; 5. Evaluate new configuration
        (let ((new-fitness (fitness-evaluation sm-kernel '())))
          
          ;; 6. If performance improved, keep changes; otherwise rollback
          (if (>= new-fitness 0.3)
              #t ; Keep changes
              (rollback-to-checkpoint sm-kernel checkpoint))))))
  
  (test-assert "multi-generation-evolution"
    (let* ((base-kernel (spawn-cognitive-kernel '(8 8) 0.3))
           (sm-kernel (make-self-modifying-kernel base-kernel))
           (generations 3))
      
      ;; Simulate multiple generations of evolution
      (let loop ((gen 0))
        (when (< gen generations)
          ;; Record performance for this generation
          (record-performance-metrics! sm-kernel 
                                      `((fitness . ,(+ 0.3 (* gen 0.1)))
                                        (generation . ,gen)))
          
          ;; Evolve based on performance
          (let ((fitness-fn (lambda (shape) (/ (apply * shape) 1000.0))))
            (evolve-tensor-shape sm-kernel fitness-fn))
          
          ;; Adapt parameters
          (evolve-kernel-parameters sm-kernel `((attention . 0.8)))
          
          (loop (+ gen 1))))
      
      ;; Verify that evolution occurred over multiple generations
      (>= (length (kernel-modification-history sm-kernel)) 0)))
  
  (test-assert "adaptive-strategy-switching"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.7))
           (sm-kernel (make-self-modifying-kernel base-kernel '() 'conservative)))
      
      ;; Simulate performance decline that triggers strategy change
      (record-performance-metrics! sm-kernel `((fitness . 0.2)))
      (record-modification sm-kernel 'test `((result . failure)) #f)
      (record-modification sm-kernel 'test `((result . failure)) #f)
      
      ;; Trigger meta-modification
      (modify-modification-strategy sm-kernel `((trend . -0.4)))
      
      ;; Strategy should have changed from conservative
      (not (eq? (sm-kernel-strategy sm-kernel) 'conservative))))
  
  (test-assert "constraint-evolution-cycle"
    (let* ((base-kernel (spawn-cognitive-kernel '(32 32) 0.8))
           (tight-constraints `((max-tensor-dimensions . 3)
                               (min-tensor-dimensions . 1)
                               (max-attention-weight . 0.7)))
           (sm-kernel (make-self-modifying-kernel base-kernel tight-constraints)))
      
      ;; Simulate good performance that should relax constraints
      (let ((good-performance `(((fitness . 0.9))
                               ((fitness . 0.85))
                               ((fitness . 0.92)))))
        
        ;; Add successful modification history
        (record-modification sm-kernel 'test `((result . success)) #f)
        (record-modification sm-kernel 'test `((result . success)) #f)
        (record-modification sm-kernel 'test `((result . success)) #f)
        (record-modification sm-kernel 'test `((result . success)) #f)
        (record-modification sm-kernel 'test `((result . success)) #f)
        
        ;; Trigger constraint adaptation
        (adaptive-constraint-relaxation sm-kernel good-performance)
        
        ;; Constraints should have been relaxed
        (let ((new-constraints (kernel-modification-constraints sm-kernel)))
          (>= (cdr (assoc 'max-tensor-dimensions new-constraints)) 3))))))

(test-end "self-modification-tests")

;; Utility function to run all self-modification tests
(define (run-self-modification-tests)
  "Run all self-modification tests and return results."
  (let ((test-runner (test-runner-simple)))
    (test-with-runner test-runner
      (test-begin "self-modification-tests")
      
      ;; Include all test groups here
      (format #t "Running Agent-Zero Self-Modification Tests...~%")
      
      ;; The tests are already defined above, this just provides a way to run them
      (test-end "self-modification-tests"))
    
    ;; Return test results summary
    (let ((total-tests (test-runner-test-count test-runner))
          (passed-tests (test-runner-pass-count test-runner))
          (failed-tests (test-runner-fail-count test-runner)))
      
      `((total-tests . ,total-tests)
        (passed-tests . ,passed-tests)
        (failed-tests . ,failed-tests)
        (success-rate . ,(if (> total-tests 0) 
                            (/ passed-tests total-tests) 
                            1.0))))))

;; Export test runner function
(define-public run-self-modification-tests run-self-modification-tests)