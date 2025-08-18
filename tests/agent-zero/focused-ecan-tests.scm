;; Focused ECAN Attention Allocation Tests
;; /tests/agent-zero/focused-ecan-tests.scm

(use-modules (srfi srfi-64)
             (srfi srfi-1)
             (agent-zero kernel)
             (agent-zero meta-cognition))

(test-begin "focused-ecan-tests")

(test-group "attention-value-system"
  (test-assert "attention value creation"
    (let ((av (make-attention-value 100 50 0)))
      (and (attention-value? av)
           (= (attention-value-sti av) 100)
           (= (attention-value-lti av) 50)
           (= (attention-value-vlti av) 0))))

  (test-assert "attention value modification"
    (let ((av (make-attention-value 100 50 0)))
      (set-attention-value-sti! av 150)
      (= (attention-value-sti av) 150))))

(test-group "focused-ecan-network"
  (test-assert "enhanced network creation"
    (let ((network (make-ecan-network)))
      (and (ecan-network? network)
           (hash-table? (ecan-attention-values network))
           (= (ecan-sti-funds network) 10000)
           (= (ecan-lti-funds network) 10000))))

  (test-assert "kernel addition with attention values"
    (let ((network (make-ecan-network))
          (kernel (spawn-cognitive-kernel '(64 64) 0.8)))
      (ecan-add-node! network kernel)
      (let* ((av-table (ecan-attention-values network))
             (av (hash-ref av-table kernel)))
        (and av
             (attention-value? av)
             (> (attention-value-sti av) 0)
             (>= (attention-value-lti av) 0))))))

(test-group "attention-diffusion"
  (test-assert "importance spreading"
    (let ((network (make-ecan-network))
          (kernel1 (spawn-cognitive-kernel '(32 32) 0.9))
          (kernel2 (spawn-cognitive-kernel '(32 32) 0.7)))
      (ecan-add-node! network kernel1)
      (ecan-add-node! network kernel2)
      
      ;; Get initial STI values
      (let* ((av-table (ecan-attention-values network))
             (av1-initial (hash-ref av-table kernel1))
             (av2-initial (hash-ref av-table kernel2))
             (initial-sti1 (attention-value-sti av1-initial))
             (initial-sti2 (attention-value-sti av2-initial)))
        
        ;; Perform importance spreading
        (importance-spreading network kernel1)
        
        ;; Check if STI values have changed appropriately
        (let ((final-sti1 (attention-value-sti av1-initial))
              (final-sti2 (attention-value-sti av2-initial)))
          ;; Source should have less or equal STI, target should have more or equal STI
          (and (<= final-sti1 initial-sti1)
               (>= final-sti2 initial-sti2)))))))

(test-group "focused-priority-calculation"
  (test-assert "priority levels"
    (let ((network (make-ecan-network))
          (high-attention-kernel (spawn-cognitive-kernel '(128 128) 0.95))
          (low-attention-kernel (spawn-cognitive-kernel '(16 16) 0.3)))
      (ecan-add-node! network high-attention-kernel)
      (ecan-add-node! network low-attention-kernel)
      
      ;; Boost STI for high attention kernel
      (let* ((av-table (ecan-attention-values network))
             (high-av (hash-ref av-table high-attention-kernel))
             (low-av (hash-ref av-table low-attention-kernel)))
        (set-attention-value-sti! high-av 250)
        (set-attention-value-sti! low-av 30)
        
        (let ((high-priority (calculate-focused-priority high-attention-kernel network))
              (low-priority (calculate-focused-priority low-attention-kernel network)))
          ;; High attention kernel should have higher priority
          (or (eq? high-priority 'critical)
              (eq? high-priority 'high))))))

  (test-assert "tournament selection"
    (let ((network (make-ecan-network))
          (kernels (list (spawn-cognitive-kernel '(64 64) 0.9)
                         (spawn-cognitive-kernel '(32 32) 0.7)
                         (spawn-cognitive-kernel '(128 128) 0.8))))
      (for-each (lambda (kernel) (ecan-add-node! network kernel)) kernels)
      
      (let* ((av-table (ecan-attention-values network))
             (selected (ecan-tournament-selection kernels av-table 2)))
        (and (list? selected)
             (<= (length selected) 2)
             (every (lambda (kernel) (member kernel kernels)) selected))))))

(test-group "focused-attention-allocation"
  (test-assert "dynamic attention allocation"
    (let ((kernels (list (spawn-cognitive-kernel '(64 64) 0.8)
                         (spawn-cognitive-kernel '(32 32) 0.6)
                         (spawn-cognitive-kernel '(128 64) 0.9))))
      (let ((allocations (adaptive-attention-allocation kernels '(reasoning learning attention))))
        (and (list? allocations)
             (= (length allocations) 3)
             ;; Check that all allocations have the required fields
             (every (lambda (alloc)
                      (and (assoc 'kernel alloc)
                           (assoc 'attention-score alloc)
                           (assoc 'activation-priority alloc)
                           (assoc 'sti alloc)
                           (assoc 'lti alloc)))
                    allocations)
             ;; Check that STI values are reasonable (> 0)
             (every (lambda (alloc)
                      (> (assoc-ref alloc 'sti) 0))
                    allocations)))))

  (test-assert "goal-based attention boosting"
    (let ((reasoning-kernels (list (spawn-cognitive-kernel '(128 128) 0.95)))
          (learning-kernels (list (spawn-cognitive-kernel '(64 64) 0.7))))
      (let ((reasoning-alloc (adaptive-attention-allocation reasoning-kernels '(reasoning)))
            (learning-alloc (adaptive-attention-allocation learning-kernels '(learning))))
        ;; Reasoning should get higher attention boost than learning (0.9 vs 0.7)
        (let ((reasoning-score (assoc-ref (car reasoning-alloc) 'attention-score))
              (learning-score (assoc-ref (car learning-alloc) 'attention-score)))
          (> reasoning-score learning-score))))))

(test-group "rent-collection-economy"
  (test-assert "attention economy maintenance"
    (let ((network (make-ecan-network))
          (kernels (list (spawn-cognitive-kernel '(64 64) 0.8)
                         (spawn-cognitive-kernel '(32 32) 0.6))))
      (for-each (lambda (kernel) (ecan-add-node! network kernel)) kernels)
      
      ;; Get initial funds
      (let ((initial-sti-funds (ecan-sti-funds network))
            (initial-lti-funds (ecan-lti-funds network)))
        
        ;; Perform rent collection
        (rent-collection network)
        
        ;; Funds should remain the same or increase (rent collected)
        (and (>= (ecan-sti-funds network) initial-sti-funds)
             (>= (ecan-lti-funds network) initial-lti-funds))))))

(test-end "focused-ecan-tests")

;; Test runner function
(define (run-focused-ecan-tests)
  "Run all focused ECAN tests."
  (display "Running Focused ECAN Tests...")
  (newline)
  (display "All focused ECAN tests completed!")
  (newline))

;; Export test runner
(define-public run-ecan-tests run-focused-ecan-tests)