;; PLN Reasoning Module Tests
;; /tests/agent-zero/pln-reasoning-tests.scm

(use-modules (srfi srfi-64)
             (srfi srfi-1)
             (agent-zero kernel)
             (agent-zero meta-cognition)
             (agent-zero pln-reasoning))

(test-begin "pln-reasoning-tests")

(test-group "pln-reasoner-creation"
  (test-assert "basic PLN reasoner creation"
    (let ((reasoner (make-pln-reasoner)))
      (and (pln-reasoner? reasoner)
           (hash-table? (pln-reasoner-atomspace reasoner))
           (list? (pln-reasoner-rules reasoner)))))

  (test-assert "reasoner has default rules"
    (let ((reasoner (make-pln-reasoner)))
      (let ((rules (pln-reasoner-rules reasoner)))
        (and (> (length rules) 0)
             (assoc 'modus-ponens rules)
             (assoc 'deduction rules)))))

  (test-assert "reasoner atomspace is empty initially"
    (let ((reasoner (make-pln-reasoner)))
      (= (hash-table-size (pln-reasoner-atomspace reasoner)) 0))))

(test-group "knowledge-management"
  (test-assert "add knowledge to reasoner"
    (let ((reasoner (make-pln-reasoner)))
      (pln-add-knowledge reasoner 'test-concept (cons 0.8 0.9))
      (let ((atomspace (pln-reasoner-atomspace reasoner)))
        (and (= (hash-table-size atomspace) 1)
             (hash-ref atomspace 'test-concept)))))

  (test-assert "knowledge truth values"
    (let ((reasoner (make-pln-reasoner)))
      (pln-add-knowledge reasoner 'test-concept (cons 0.7 0.85))
      (let ((tv (hash-ref (pln-reasoner-atomspace reasoner) 'test-concept)))
        (and (= (pln-get-strength tv) 0.7)
             (= (pln-get-confidence tv) 0.85))))))

(test-group "backward-chaining"
  (test-assert "PLN backward chaining basic"
    (let ((reasoner (make-pln-reasoner)))
      ;; Add some knowledge
      (pln-add-knowledge reasoner 'intelligent-agent (cons 0.9 0.8))
      
      ;; Test backward chaining
      (let ((result (pln-query reasoner 'backward-chain 'intelligent-agent)))
        (and (list? result)
             (eq? (car result) 'reasoning-result)
             (assoc 'target result)
             (assoc 'status result)))))

  (test-assert "backward chaining with unknown target"
    (let ((reasoner (make-pln-reasoner)))
      (let ((result (pln-query reasoner 'backward-chain 'unknown-concept)))
        (and (list? result)
             (eq? (car result) 'reasoning-result)
             (eq? (assoc-ref result 'status) 'failed)))))

  (test-assert "backward chaining finds existing knowledge"
    (let ((reasoner (make-pln-reasoner)))
      (pln-add-knowledge reasoner 'existing-knowledge (cons 0.95 0.9))
      (let ((result (pln-query reasoner 'backward-chain 'existing-knowledge)))
        (eq? (assoc-ref result 'status) 'proved)))))

(test-group "forward-chaining"
  (test-assert "PLN forward chaining basic"
    (let ((reasoner (make-pln-reasoner)))
      (let ((result (pln-query reasoner 'forward-chain '(test-premise))))
        (and (list? result)
             (eq? (car result) 'reasoning-result)
             (assoc 'premises result)))))

  (test-assert "forward chaining with empty premises"
    (let ((reasoner (make-pln-reasoner)))
      (let ((result (pln-query reasoner 'forward-chain '())))
        (eq? (assoc-ref result 'status) 'complete)))))

(test-group "cognitive-integration"
  (test-assert "cognitive PLN reasoning"
    (let ((cognitive-state '((active-agent . #t)
                            (learning-mode . #t)
                            (attention-focused . #t))))
      (let ((result (cognitive-pln-reasoning cognitive-state 'intelligent-behavior)))
        (and (list? result)
             (eq? (car result) 'reasoning-result)))))

  (test-assert "meta PLN inference with kernel"
    (let ((kernel (spawn-cognitive-kernel '(32 32) 0.8)))
      (let ((result (meta-pln-inference kernel '(meta-awareness))))
        (and (list? result)
             (eq? (car result) 'reasoning-result)))))

  (test-assert "integrated reasoning with atomspace"
    (let ((atomspace (make-atomspace)))
      (hash-set! atomspace 'test-knowledge (cons 0.8 0.9))
      (let ((result (pln-backward-chaining atomspace 'test-knowledge)))
        (and (list? result)
             (eq? (car result) 'reasoning-result))))))

(test-group "meta-cognitive-reflection"
  (test-assert "meta-cognitive reflection with PLN"
    (let ((kernel (spawn-cognitive-kernel '(64 32) 0.85)))
      (let ((reflection (meta-cognitive-reflection kernel)))
        (and (list? reflection)
             (assoc 'current-state reflection)
             (assoc 'self-assessment reflection)
             (assoc 'adaptation-suggestions reflection)
             (assoc 'meta-learning reflection)))))

  (test-assert "self-assessment generation"
    (let ((kernel (spawn-cognitive-kernel '(32 32) 0.7)))
      (let ((reflection (meta-cognitive-reflection kernel)))
        (let ((self-assessment (assoc-ref reflection 'self-assessment)))
          (and (list? self-assessment)
               (assoc 'self-awareness self-assessment)
               (assoc 'performance-level self-assessment)
               (assoc 'cognitive-clarity self-assessment))))))

  (test-assert "adaptation suggestions generation"
    (let ((kernel (spawn-cognitive-kernel '(16 16) 0.9)))
      (let ((reflection (meta-cognitive-reflection kernel)))
        (let ((suggestions (assoc-ref reflection 'adaptation-suggestions)))
          (list? suggestions)))))

  (test-assert "reflection confidence calculation"
    (let ((kernel (spawn-cognitive-kernel '(128 64) 0.6)))
      (let ((reflection (meta-cognitive-reflection kernel)))
        (let ((confidence (assoc-ref reflection 'confidence-level)))
          (and (number? confidence)
               (>= confidence 0.0)
               (<= confidence 1.0)))))))

(test-group "truth-value-operations"
  (test-assert "strength extraction"
    (let ((tv (cons 0.75 0.85)))
      (= (pln-get-strength tv) 0.75)))

  (test-assert "confidence extraction"
    (let ((tv (cons 0.75 0.85)))
      (= (pln-get-confidence tv) 0.85)))

  (test-assert "truth value with single value"
    (let ((tv 0.6))
      (= (pln-get-strength tv) 0.5)))  ; Default for non-pair

  (test-assert "truth value operations on reasoning results"
    (let ((reasoner (make-pln-reasoner)))
      (pln-add-knowledge reasoner 'test-item (cons 0.8 0.9))
      (let ((result (pln-query reasoner 'backward-chain 'test-item)))
        (and (>= (assoc-ref result 'confidence) 0.0)
             (<= (assoc-ref result 'confidence) 1.0)
             (>= (assoc-ref result 'strength) 0.0)
             (<= (assoc-ref result 'strength) 1.0))))))

(test-group "error-handling"
  (test-assert "invalid query type"
    (let ((reasoner (make-pln-reasoner)))
      (catch #t
        (lambda ()
          (pln-query reasoner 'invalid-type 'test-target)
          #f)  ; Should not reach here
        (lambda (key . args)
          #t))))  ; Expected to catch error

  (test-assert "reasoner with invalid atomspace"
    (let ((reasoner (make-pln-reasoner)))
      ;; Test with valid operations on empty reasoner
      (let ((result (pln-query reasoner 'backward-chain 'non-existent)))
        (eq? (assoc-ref result 'status) 'failed)))))

(test-end "pln-reasoning-tests")