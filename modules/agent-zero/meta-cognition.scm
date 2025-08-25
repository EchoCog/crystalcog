;; Agent-Zero Meta-Cognition Module with Focused ECAN Attention Allocation
;; /modules/agent-zero/meta-cognition.scm

(define-module (agent-zero meta-cognition)
  #:use-module (agent-zero kernel)
  #:use-module (agent-zero pln-reasoning)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (recursive-self-description
            adaptive-attention-allocation
            make-ecan-network
            ecan-network?
            ecan-nodes
            ecan-add-node!
            ecan-allocate-attention!
            pln-backward-chaining
            pln-forward-chaining
            meta-cognitive-reflection
            ;; PLN reasoning exports
            cognitive-pln-reasoning
            meta-pln-inference
            ;; Focused ECAN exports
            make-attention-value
            attention-value?
            attention-value-sti
            attention-value-lti
            attention-value-vlti
            set-attention-value-sti!
            set-attention-value-lti!
            set-attention-value-vlti!
            ecan-attention-values
            ecan-sti-funds
            ecan-lti-funds
            focused-attention-diffusion
            importance-spreading
            rent-collection
            calculate-focused-priority
            ecan-tournament-selection))

;; Focused ECAN Attention Value System
(define-record-type <attention-value>
  (make-attention-value-internal sti lti vlti)
  attention-value?
  (sti attention-value-sti set-attention-value-sti!)
  (lti attention-value-lti set-attention-value-lti!)
  (vlti attention-value-vlti set-attention-value-vlti!))

(define (make-attention-value sti lti vlti)
  "Create a new attention value with STI, LTI, and VLTI components."
  (make-attention-value-internal sti lti vlti))

;; ECAN Parameters (from OpenCog ECAN system)
(define *af-max-size* 1000)
(define *af-min-size* 500)
(define *max-spread-percentage* 0.4)
(define *diffusion-tournament-size* 5)
(define *rent-tournament-size* 5)
(define *target-sti-funds* 10000)
(define *target-lti-funds* 10000)
(define *starting-sti-rent* 1)
(define *starting-lti-rent* 1)
(define *hebbian-max-allocation* 0.05)

;; Helper functions
(define (dotimes n proc)
  "Execute proc n times with index."
  (let loop ((i 0))
    (when (< i n)
      (proc i)
      (loop (+ i 1)))))
;; Enhanced ECAN network with focused attention
(define-record-type <ecan-network>
  (make-ecan-network-internal nodes attention-values sti-funds lti-funds)
  ecan-network?
  (nodes ecan-nodes set-ecan-nodes!)
  (attention-values ecan-attention-values set-ecan-attention-values!)
  (sti-funds ecan-sti-funds set-ecan-sti-funds!)
  (lti-funds ecan-lti-funds set-ecan-lti-funds!))

(define (make-ecan-network)
  "Create a new focused ECAN network with attention economy."
  (make-ecan-network-internal '() (make-hash-table) *target-sti-funds* *target-lti-funds*))

(define (ecan-add-node! network kernel)
  "Add a cognitive kernel node to ECAN network with initial attention values."
  (let* ((initial-sti (+ 50 (random 100)))  ; Random initial STI 50-150
         (initial-lti (+ 0 (random 50)))     ; Random initial LTI 0-50  
         (initial-vlti 0)                    ; VLTI starts at 0
         (av (make-attention-value initial-sti initial-lti initial-vlti)))
    (set-ecan-nodes! network (cons kernel (ecan-nodes network)))
    (hash-set! (ecan-attention-values network) kernel av)))

(define (focused-attention-diffusion network)
  "Perform focused importance diffusion across ECAN network nodes."
  (let ((nodes (ecan-nodes network))
        (av-table (ecan-attention-values network)))
    ;; Select nodes in attentional focus (high STI nodes)
    (let* ((focus-nodes (filter (lambda (node)
                                  (let ((av (hash-ref av-table node)))
                                    (and av (> (attention-value-sti av) 100))))
                                nodes))
           ;; Perform tournament selection for diffusion sources
           (diffusion-sources (ecan-tournament-selection focus-nodes av-table *diffusion-tournament-size*)))
      ;; Diffuse importance from selected sources
      (for-each (lambda (source)
                  (importance-spreading network source))
                diffusion-sources))))

(define (importance-spreading network source)
  "Spread importance from source node to connected nodes using ECAN dynamics."
  (let* ((av-table (ecan-attention-values network))
         (source-av (hash-ref av-table source))
         (source-sti (attention-value-sti source-av))
         ;; Calculate diffusion amount (percentage of STI to spread)
         (diffusion-amount (* source-sti *max-spread-percentage*))
         ;; Get neighbors (simulated - in real implementation would use AtomSpace connectivity)
         (neighbors (get-kernel-neighbors network source)))
    
    (when (> diffusion-amount 1) ; Only diffuse if amount is meaningful
      ;; Spread importance to neighbors based on connection strength
      (let ((per-neighbor-amount (/ diffusion-amount (max 1 (length neighbors)))))
        (for-each (lambda (neighbor)
                    (transfer-importance network source neighbor per-neighbor-amount))
                  neighbors)))))

(define (get-kernel-neighbors network kernel)
  "Get neighboring kernels based on similarity and connectivity patterns."
  (let* ((kernel-shape (kernel-tensor-shape kernel))
         (kernel-attention-val (kernel-attention kernel))
         (all-nodes (ecan-nodes network)))
    ;; Select neighbors based on tensor shape similarity and attention compatibility  
    (filter (lambda (other-kernel)
              (and (not (eq? kernel other-kernel))
                   (let* ((other-shape (kernel-tensor-shape other-kernel))
                          (other-attention-val (kernel-attention other-kernel))
                          ;; Calculate similarity metrics
                          (shape-similarity (calculate-shape-similarity kernel-shape other-shape))
                          (attention-compatibility (calculate-attention-compatibility kernel-attention-val other-attention-val)))
                     ;; Include as neighbor if similarity/compatibility above threshold
                     (and (> shape-similarity 0.3)
                          (> attention-compatibility 0.2)))))
            all-nodes)))

(define (calculate-shape-similarity shape1 shape2)
  "Calculate similarity between tensor shapes."
  (if (= (length shape1) (length shape2))
      (let* ((differences (map (lambda (s1 s2) (abs (- s1 s2))) shape1 shape2))
             (max-diff (apply max differences))
             (normalized-similarity (- 1.0 (/ max-diff (apply max (append shape1 shape2))))))
        (max 0.0 normalized-similarity))
      0.0))

(define (calculate-attention-compatibility att1 att2)
  "Calculate attention compatibility between kernels."
  (let ((att-diff (abs (- att1 att2))))
    (max 0.0 (- 1.0 att-diff))))

(define (transfer-importance network source target amount)
  "Transfer importance from source to target kernel."
  (let* ((av-table (ecan-attention-values network))
         (source-av (hash-ref av-table source))
         (target-av (hash-ref av-table target))
         (new-source-sti (max 0 (- (attention-value-sti source-av) amount)))
         (new-target-sti (+ (attention-value-sti target-av) (* amount 0.8)))) ; 80% transfer efficiency
    
    ;; Update attention values
    (set-attention-value-sti! source-av new-source-sti)
    (set-attention-value-sti! target-av new-target-sti)))

(define (rent-collection network)
  "Perform rent collection to maintain attention economy."
  (let* ((av-table (ecan-attention-values network))
         (nodes (ecan-nodes network))
         ;; Select nodes for rent collection using tournament selection
         (rent-targets (ecan-tournament-selection nodes av-table *rent-tournament-size*)))
    
    ;; Collect rent from selected nodes
    (for-each (lambda (node)
                (let* ((av (hash-ref av-table node))
                       (current-sti (attention-value-sti av))
                       (current-lti (attention-value-lti av))
                       (sti-rent (min *starting-sti-rent* (* current-sti 0.01)))
                       (lti-rent (min *starting-lti-rent* (* current-lti 0.005))))
                  ;; Collect STI rent
                  (when (> current-sti sti-rent)
                    (set-attention-value-sti! av (- current-sti sti-rent))
                    (set-ecan-sti-funds! network (+ (ecan-sti-funds network) sti-rent)))
                  ;; Collect LTI rent  
                  (when (> current-lti lti-rent)
                    (set-attention-value-lti! av (- current-lti lti-rent))
                    (set-ecan-lti-funds! network (+ (ecan-lti-funds network) lti-rent)))))
              rent-targets)))

(define (ecan-tournament-selection nodes av-table tournament-size)
  "Select nodes using tournament selection based on attention values."
  (let ((selected '())
        (remaining-nodes nodes))
    (let loop ((i 0))
      (when (and (< i (min tournament-size (length remaining-nodes)))
                 (not (null? remaining-nodes)))
        ;; Random tournament among remaining nodes
        (let* ((candidates (list-head remaining-nodes (min 5 (length remaining-nodes))))
               (winner (fold (lambda (node best)
                              (let* ((node-av (hash-ref av-table node))
                                     (best-av (hash-ref av-table best))
                                     (node-score (+ (attention-value-sti node-av) 
                                                   (* 0.1 (attention-value-lti node-av))))
                                     (best-score (+ (attention-value-sti best-av)
                                                   (* 0.1 (attention-value-lti best-av)))))
                                (if (> node-score best-score) node best)))
                            (car candidates)
                            (cdr candidates))))
          (set! selected (cons winner selected))
          (set! remaining-nodes (delete winner remaining-nodes))
          (loop (+ i 1)))))
    selected))

(define (calculate-focused-priority kernel network)
  "Calculate focused activation priority based on ECAN dynamics."
  (let* ((av-table (ecan-attention-values network))
         (av (hash-ref av-table kernel))
         (sti (attention-value-sti av))
         (lti (attention-value-lti av))
         (vlti (attention-value-vlti av))
         ;; Calculate composite attention score
         (attention-score (+ sti (* 0.1 lti) (* 0.05 vlti)))
         ;; Factor in kernel characteristics
         (kernel-complexity (apply * (kernel-tensor-shape kernel)))
         (kernel-attention (kernel-attention kernel))
         ;; Compute focused priority score
         (priority-score (+ attention-score 
                           (* 0.2 kernel-complexity)
                           (* 100 kernel-attention))))
    
    ;; Convert to priority level
    (cond
      ((> priority-score 200) 'critical)
      ((> priority-score 150) 'high)
      ((> priority-score 100) 'medium)
      ((> priority-score 50) 'low)
      (else 'minimal))))

(define (ecan-allocate-attention! network goals)
  "Perform focused attention allocation using ECAN dynamics."
  ;; First perform attention diffusion
  (focused-attention-diffusion network)
  
  ;; Then perform rent collection to maintain economy
  (rent-collection network)
  
  ;; Calculate goal-based attention boosts
  (let* ((nodes (ecan-nodes network))
         (av-table (ecan-attention-values network))
         (goal-boosts (calculate-goal-boosts goals)))
    
    ;; Apply goal-based attention boosts
    (for-each (lambda (node goal)
                (when (< (length goal-boosts) (length goals))
                  (let* ((av (hash-ref av-table node))
                         (boost (attention-score-for-goal goal))
                         (current-sti (attention-value-sti av))
                         (boosted-sti (+ current-sti (* boost 20)))) ; Boost STI based on goal relevance
                    (set-attention-value-sti! av boosted-sti))))
              nodes goals)
    
    ;; Return allocation results with focused priorities
    (map (lambda (node)
           (let* ((av (hash-ref av-table node))
                  (sti (attention-value-sti av))
                  (lti (attention-value-lti av))
                  (attention-score (+ sti (* 0.1 lti)))
                  (priority (calculate-focused-priority node network)))
             `((kernel . ,node)
               (attention-score . ,(/ attention-score 100.0)) ; Normalize to 0-1 range for compatibility
               (activation-priority . ,priority)
               (sti . ,sti)
               (lti . ,lti))))
         nodes)))

(define (calculate-goal-boosts goals)
  "Calculate attention boosts for different goals."
  (map (lambda (goal)
         (case goal
           ((goal-1) 0.8)
           ((goal-2) 0.6)
           ((reasoning) 0.9)
           ((learning) 0.7)
           ((attention) 0.85)
           ((memory) 0.65)
           ((adaptation) 0.75)
           (else 0.5)))
       goals))

(define (attention-score-for-goal goal)
  "Calculate attention score for a given goal (enhanced version)."
  (case goal
    ((goal-1) 0.8)
    ((goal-2) 0.6)
    ((reasoning) 0.9)
    ((learning) 0.7)
    ((attention) 0.85)
    ((memory) 0.65)
    ((adaptation) 0.75)
    (else 0.5)))

;; PLN reasoning integration
(define *default-pln-reasoner* #f)

(define (get-default-pln-reasoner)
  "Get or create the default PLN reasoner instance."
  (unless *default-pln-reasoner*
    (set! *default-pln-reasoner* (make-pln-reasoner)))
  *default-pln-reasoner*)

(define (pln-backward-chaining atomspace query)
  "Perform PLN backward chaining reasoning using the integrated PLN system."
  (let ((reasoner (get-default-pln-reasoner)))
    ;; Add atomspace knowledge to reasoner if needed
    (when (hash-table? atomspace)
      (hash-for-each (lambda (key value)
                       (pln-add-knowledge reasoner key value))
                     atomspace))
    
    ;; Perform backward chaining
    (pln-query reasoner 'backward-chain query)))

(define (pln-forward-chaining atomspace premises)
  "Perform PLN forward chaining reasoning using the integrated PLN system."
  (let ((reasoner (get-default-pln-reasoner)))
    ;; Add atomspace knowledge to reasoner if needed
    (when (hash-table? atomspace)
      (hash-for-each (lambda (key value)
                       (pln-add-knowledge reasoner key value))
                     atomspace))
    
    ;; Perform forward chaining
    (pln-query reasoner 'forward-chain premises)))

(define (cognitive-pln-reasoning cognitive-state query)
  "Perform PLN reasoning on cognitive state for a specific query."
  (let ((reasoner (make-pln-reasoner))
        (knowledge-base (cognitive-state->knowledge-base cognitive-state)))
    (apply-pln-reasoning reasoner knowledge-base query)))

(define (meta-pln-inference kernel meta-goals)
  "Perform meta-level PLN inference about kernel state and goals."
  (let ((reasoner (get-default-pln-reasoner))
        (meta-state (kernel->meta-state kernel)))
    (meta-pln-reasoning reasoner meta-goals)))

(define (cognitive-state->knowledge-base cognitive-state)
  "Convert cognitive state to PLN knowledge base format."
  (map (lambda (state-item)
         (cons (car state-item) 
               (cons 0.8 0.85))) ; Default truth values
       cognitive-state))

(define (kernel->meta-state kernel)
  "Extract meta-state information from cognitive kernel."
  `((kernel-active . ,(> (kernel-attention kernel) 0.5))
    (tensor-shape . ,(kernel-tensor-shape kernel))
    (cognitive-function . ,(kernel-function kernel))
    (attention-level . ,(kernel-attention kernel))))

;; Compatibility function for existing code
(define (make-atomspace)
  "Create atomspace representation compatible with PLN reasoning."
  (make-hash-table))

;; Public API functions
(define (recursive-self-description kernel)
  "Generate recursive self-description of cognitive kernel."
  (let ((tensor-shape (kernel-tensor-shape kernel))
        (cognitive-function (kernel-function kernel))
        (attention-allocation (kernel-attention kernel)))
    `((tensor-shape . ,tensor-shape)
      (cognitive-function . ,cognitive-function)
      (attention-allocation . ,attention-allocation)
      (meta-level . ,(+ 1 (recursive-depth kernel)))
      (self-model . ,(generate-self-model kernel))
      (cognitive-state . ,(hypergraph-state kernel)))))

(define (generate-self-model kernel)
  "Generate self-model representation."
  `((architecture . agent-zero-genesis)
    (cognitive-capabilities . (reasoning attention learning adaptation))
    (tensor-encoding . ,(tensor-field-encoding kernel))
    (recursive-depth . ,(recursive-depth kernel))))

(define (adaptive-attention-allocation kernels goals)
  "Use focused ECAN to dynamically prioritize kernel activation with real attention dynamics."
  (let ((ecan-network (make-ecan-network)))
    ;; Add all kernels to the ECAN network
    (for-each (lambda (kernel)
                (ecan-add-node! ecan-network kernel))
              kernels)
    
    ;; Perform focused attention allocation using ECAN dynamics
    (let ((allocations (ecan-allocate-attention! ecan-network goals)))
      ;; Return results in expected format for backward compatibility
      allocations)))

(define (meta-cognitive-reflection kernel)
  "Perform meta-cognitive reflection using PLN reasoning."
  (let* ((kernel-state (kernel->meta-state kernel))
         (reasoner (get-default-pln-reasoner))
         ;; Add kernel state as knowledge
         (knowledge-base (map (lambda (state-item)
                               (cons (car state-item) 
                                     (cons 0.7 0.8)))
                             kernel-state))
         ;; Define reflection queries
         (reflection-queries '((self-awareness)
                              (performance-assessment)
                              (adaptation-needed)
                              (learning-progress))))
    
    ;; Perform PLN reasoning for each reflection query
    (let ((reflection-results
           (map (lambda (query)
                  (let ((result (apply-pln-reasoning reasoner knowledge-base query)))
                    (cons query result)))
                reflection-queries)))
      
      ;; Generate comprehensive reflection report
      `((current-state . ,kernel-state)
        (self-assessment . ,(generate-self-assessment reflection-results))
        (adaptation-suggestions . ,(generate-adaptation-suggestions reflection-results))
        (meta-learning . ,(generate-meta-learning-insights reflection-results))
        (confidence-level . ,(calculate-reflection-confidence reflection-results))))))

(define (generate-self-assessment reflection-results)
  "Generate self-assessment from reflection results."
  (let ((awareness-result (assoc 'self-awareness reflection-results))
        (performance-result (assoc 'performance-assessment reflection-results)))
    `((self-awareness . ,(if awareness-result 
                           (pln-get-confidence (cdr awareness-result))
                           0.5))
      (performance-level . ,(if performance-result
                              (pln-get-strength (cdr performance-result))
                              0.5))
      (cognitive-clarity . 0.75))))

(define (generate-adaptation-suggestions reflection-results)
  "Generate adaptation suggestions from reflection results."
  (let ((adaptation-result (assoc 'adaptation-needed reflection-results)))
    (if (and adaptation-result 
             (> (pln-get-confidence (cdr adaptation-result)) 0.6))
        '((increase-attention-focus . high-priority)
          (enhance-learning-rate . medium-priority)
          (optimize-tensor-shape . low-priority))
        '((maintain-current-state . low-priority)))))

(define (generate-meta-learning-insights reflection-results)
  "Generate meta-learning insights from reflection results."
  (let ((learning-result (assoc 'learning-progress reflection-results)))
    `((learning-effectiveness . ,(if learning-result
                                   (pln-get-strength (cdr learning-result))
                                   0.6))
      (knowledge-integration . 0.7)
      (reasoning-efficiency . 0.8))))

(define (calculate-reflection-confidence reflection-results)
  "Calculate overall confidence in reflection results."
  (let ((confidences (map (lambda (result)
                           (pln-get-confidence (cdr result)))
                         reflection-results)))
    (if (null? confidences)
        0.5
        (/ (apply + confidences) (length confidences)))))

(define (calculate-priority score)
  "Calculate activation priority from attention score."
  (cond
    ((> score 0.8) 'high)
    ((> score 0.6) 'medium)
    ((> score 0.4) 'low)
    (else 'minimal)))

;; Meta-cognitive reasoning functions
(define (meta-cognitive-reflection kernel)
  "Perform meta-cognitive reflection on kernel state."
  `((current-state . ,(hypergraph-state kernel))
    (self-assessment . ,(assess-cognitive-performance kernel))
    (adaptation-suggestions . ,(suggest-adaptations kernel))
    (meta-learning . ,(extract-meta-patterns kernel))))

(define (assess-cognitive-performance kernel)
  "Assess current cognitive performance of kernel."
  (let ((attention (kernel-attention kernel))
        (complexity (length (kernel-tensor-shape kernel))))
    `((attention-efficiency . ,(if (> attention 0.7) 'high 'moderate))
      (processing-complexity . ,(if (> complexity 2) 'high 'low))
      (overall-performance . ,(if (and (> attention 0.6) (> complexity 1)) 'good 'needs-improvement)))))

(define (suggest-adaptations kernel)
  "Suggest adaptations for kernel improvement."
  (let ((performance (assess-cognitive-performance kernel)))
    (match (assoc 'overall-performance performance)
      (('overall-performance . 'needs-improvement)
       '((increase-attention . 0.1)
         (expand-tensor-dimensions . 1)
         (enhance-meta-level . 1)))
      (else '((maintain-current-state . #t))))))

(define (extract-meta-patterns kernel)
  "Extract meta-learning patterns from kernel."
  `((pattern-recognition . ,(> (length (kernel-tensor-shape kernel)) 1))
    (recursive-processing . ,(> (recursive-depth kernel) 0))
    (attention-dynamics . ,(> (kernel-attention kernel) 0.5))
    (cognitive-emergence . #t)))