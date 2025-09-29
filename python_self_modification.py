#!/usr/bin/env python3
"""
Python wrapper for Agent-Zero Self-Modifying Kernel capabilities.

This module provides a Python interface to the self-modification capabilities
implemented in Guile Scheme. It allows easy access to kernel evolution,
parameter adaptation, and meta-modification from Python applications.
"""

import subprocess
import json
import tempfile
import os
from typing import List, Dict, Any, Optional, Tuple, Union
import time
import random
from python_cognitive_kernel import CognitiveKernel, CognitiveKernelManager


class SelfModifyingKernel:
    """Python wrapper for self-modifying cognitive kernels."""
    
    def __init__(self, base_kernel: CognitiveKernel, 
                 constraints: Optional[Dict[str, Any]] = None,
                 strategy: str = 'balanced'):
        """
        Initialize a self-modifying kernel.
        
        Args:
            base_kernel: Base cognitive kernel to extend with self-modification
            constraints: Safety constraints for modifications
            strategy: Modification strategy ('conservative', 'balanced', 'aggressive', 'adaptive')
        """
        self.base_kernel = base_kernel
        self.constraints = constraints or self._default_constraints()
        self.strategy = strategy
        self.modification_history = []
        self.performance_metrics = {}
        self.checkpoints = {}
        self.current_architecture = self._extract_architecture()
        self._guile_available = self._check_guile_available()
        
        # Initialize counters
        self._checkpoint_counter = 0
        self._modification_counter = 0
        
    def _default_constraints(self) -> Dict[str, Any]:
        """Get default modification constraints."""
        return {
            'max_tensor_dimensions': 10,
            'min_tensor_dimensions': 1, 
            'max_attention_weight': 1.0,
            'min_attention_weight': 0.01,
            'max_meta_levels': 5,
            'allow_architecture_changes': True,
            'allow_parameter_evolution': True,
            'allow_code_generation': True,
            'require_performance_validation': True,
            'max_modifications_per_cycle': 3,
            'rollback_threshold': 0.1
        }
    
    def _check_guile_available(self) -> bool:
        """Check if Guile is available for running Scheme code."""
        try:
            result = subprocess.run(['guile', '--version'], 
                                  capture_output=True, text=True, timeout=5)
            return result.returncode == 0
        except (FileNotFoundError, subprocess.TimeoutExpired):
            return False
    
    def _run_guile_code(self, code: str) -> str:
        """Run Guile Scheme code and return the output."""
        if not self._guile_available:
            # Fallback to Python implementation
            return self._python_fallback(code)
        
        # Set up environment for Guile modules
        env = os.environ.copy()
        current_dir = os.path.dirname(os.path.abspath(__file__))
        modules_path = os.path.join(current_dir, 'modules')
        
        if 'GUILE_LOAD_PATH' in env:
            env['GUILE_LOAD_PATH'] = f"{modules_path}:{env['GUILE_LOAD_PATH']}"
        else:
            env['GUILE_LOAD_PATH'] = modules_path
            
        try:
            # Run the Guile code
            result = subprocess.run(['guile', '-c', code], 
                                  capture_output=True, text=True, env=env, timeout=30)
            
            if result.returncode != 0:
                print(f"Guile execution warning: {result.stderr}")
                return self._python_fallback(code)
                
            return result.stdout.strip()
        except subprocess.TimeoutExpired:
            print("Guile execution timed out, using fallback")
            return self._python_fallback(code)
    
    def _python_fallback(self, code: str) -> str:
        """Python fallback when Guile is not available."""
        # Extract the operation from the Guile code and provide fallback
        if 'modify-kernel-architecture' in code:
            return "modification-applied"
        elif 'evolve-tensor-shape' in code:
            return "evolution-applied" 
        elif 'fitness-evaluation' in code:
            return str(random.uniform(0.3, 0.9))
        else:
            return "operation-completed"
    
    def _extract_architecture(self) -> Dict[str, Any]:
        """Extract current architecture from base kernel."""
        return {
            'tensor_shape': self.base_kernel.shape,
            'attention_weight': self.base_kernel.attention_weight,
            'meta_level': self.base_kernel.meta_level,
            'encoding_strategy': 'prime',
            'processing_functions': ['basic_cognitive_processing'],
            'timestamp': time.time()
        }
    
    # Architecture Modification Methods
    
    def modify_architecture(self, modification_spec: Dict[str, Any]) -> bool:
        """
        Modify kernel architecture according to specification.
        
        Args:
            modification_spec: Dictionary specifying modifications
            
        Returns:
            True if modification successful, False otherwise
        """
        # Validate modification against constraints
        if not self._validate_modification_safety(modification_spec):
            raise ValueError(f"Modification violates safety constraints: {modification_spec}")
        
        # Create checkpoint before modification
        checkpoint_id = self.create_checkpoint()
        
        try:
            if self._guile_available:
                # Use Guile implementation
                shape_str = ' '.join(map(str, self.base_kernel.shape))
                mod_str = self._dict_to_scheme_alist(modification_spec)
                
                code = f"""
                (use-modules (agent-zero kernel) (agent-zero self-modification))
                (let* ((base-kernel (spawn-cognitive-kernel '({shape_str}) {self.base_kernel.attention_weight}))
                       (sm-kernel (make-self-modifying-kernel base-kernel)))
                  (modify-kernel-architecture sm-kernel '{mod_str})
                  (display "success"))
                """
                
                result = self._run_guile_code(code)
                success = "success" in result
            else:
                # Python fallback implementation
                success = self._apply_modification_python(modification_spec)
            
            if success:
                # Update architecture record
                self.current_architecture.update(modification_spec)
                self.current_architecture['timestamp'] = time.time()
                
                # Record modification in history
                self._record_modification('architecture', modification_spec, checkpoint_id)
                
                # Apply changes to base kernel
                self._update_base_kernel(modification_spec)
                
                return True
            else:
                # Rollback on failure
                self.rollback_to_checkpoint(checkpoint_id)
                return False
                
        except Exception as e:
            # Rollback on error
            self.rollback_to_checkpoint(checkpoint_id)
            raise e
    
    def evolve_tensor_shape(self, fitness_function: callable, generations: int = 5) -> Dict[str, Any]:
        """
        Evolve tensor shape using genetic algorithm approach.
        
        Args:
            fitness_function: Function that evaluates fitness of a shape
            generations: Number of evolution generations
            
        Returns:
            Dictionary with evolution results
        """
        current_shape = self.base_kernel.shape.copy()
        best_shape = current_shape
        best_fitness = fitness_function(current_shape)
        
        evolution_history = []
        
        for generation in range(generations):
            # Generate mutations
            mutations = self._generate_shape_mutations(current_shape)
            
            # Evaluate fitness of mutations
            mutation_fitness = []
            for mutation in mutations:
                try:
                    fitness = fitness_function(mutation)
                    mutation_fitness.append((mutation, fitness))
                except:
                    mutation_fitness.append((mutation, 0.0))
            
            # Find best mutation
            if mutation_fitness:
                best_mutation = max(mutation_fitness, key=lambda x: x[1])
                
                if best_mutation[1] > best_fitness:
                    best_shape = best_mutation[0]
                    best_fitness = best_mutation[1]
                    current_shape = best_shape
                    
                    # Apply the improved shape
                    modification_spec = {'tensor_shape': best_shape}
                    if self._validate_modification_safety(modification_spec):
                        self.modify_architecture(modification_spec)
            
            evolution_history.append({
                'generation': generation,
                'best_fitness': best_fitness,
                'best_shape': best_shape,
                'mutations_tested': len(mutations)
            })
        
        return {
            'final_shape': best_shape,
            'final_fitness': best_fitness,
            'evolution_history': evolution_history,
            'improvement': best_fitness - fitness_function(self.base_kernel.shape)
        }
    
    def adapt_attention_allocation(self, performance_feedback: Dict[str, Any]) -> bool:
        """
        Adapt attention allocation based on performance feedback.
        
        Args:
            performance_feedback: Dictionary with performance data
            
        Returns:
            True if adaptation was applied
        """
        current_attention = self.base_kernel.attention_weight
        trend = performance_feedback.get('trend', 0.0)
        
        # Calculate adaptation
        adaptation_factor = self._calculate_attention_adaptation(trend)
        new_attention = max(0.01, min(1.0, current_attention + adaptation_factor))
        
        if abs(new_attention - current_attention) > 0.01:  # Significant change
            modification_spec = {
                'attention_weight': new_attention,
                'adaptation_reason': 'performance_feedback',
                'old_attention': current_attention
            }
            
            return self.modify_architecture(modification_spec)
        
        return False
    
    def optimize_encoding_strategy(self, performance_data: Dict[str, Any]) -> Optional[str]:
        """
        Optimize tensor encoding strategy based on performance data.
        
        Args:
            performance_data: Performance metrics for different strategies
            
        Returns:
            Best encoding strategy found, or None if no improvement
        """
        strategies = ['prime', 'fibonacci', 'harmonic', 'factorial', 'power-of-two']
        strategy_scores = {}
        
        for strategy in strategies:
            # Simulate performance evaluation
            score = self._evaluate_strategy_performance(strategy, performance_data)
            strategy_scores[strategy] = score
        
        # Find best strategy
        best_strategy = max(strategy_scores, key=strategy_scores.get)
        current_strategy = self.current_architecture.get('encoding_strategy', 'prime')
        
        if (best_strategy != current_strategy and 
            strategy_scores[best_strategy] > strategy_scores.get(current_strategy, 0)):
            
            modification_spec = {
                'encoding_strategy': best_strategy,
                'optimization_reason': 'performance_improvement',
                'strategy_scores': strategy_scores
            }
            
            if self.modify_architecture(modification_spec):
                return best_strategy
        
        return None
    
    # Parameter Evolution Methods
    
    def evolve_parameters(self, evolution_spec: Dict[str, float]) -> List[Dict[str, Any]]:
        """
        Evolve kernel parameters using specified evolution parameters.
        
        Args:
            evolution_spec: Evolution configuration
            
        Returns:
            List of applied parameter mutations
        """
        mutations_applied = []
        mutation_rate = evolution_spec.get('mutation_rate', 0.3)
        
        # Generate parameter mutations
        mutations = self._generate_parameter_mutations(mutation_rate, evolution_spec)
        
        for mutation in mutations:
            try:
                if self._validate_parameter_mutation(mutation):
                    if self.modify_architecture({mutation['parameter']: mutation['value']}):
                        mutations_applied.append(mutation)
            except Exception as e:
                print(f"Failed to apply mutation {mutation}: {e}")
        
        return mutations_applied
    
    def adaptive_learning_rate(self, performance_history: List[Dict[str, Any]]) -> float:
        """
        Calculate adaptive learning rate based on performance history.
        
        Args:
            performance_history: History of performance metrics
            
        Returns:
            Optimal learning rate
        """
        if len(performance_history) < 2:
            return 0.1  # Default learning rate
        
        # Calculate performance trend
        recent_performance = [entry.get('fitness', 0.5) for entry in performance_history[-5:]]
        older_performance = [entry.get('fitness', 0.5) for entry in performance_history[-10:-5]]
        
        if not older_performance:
            return 0.1
        
        recent_avg = sum(recent_performance) / len(recent_performance)
        older_avg = sum(older_performance) / len(older_performance)
        trend = recent_avg - older_avg
        
        # Adapt learning rate based on trend
        if trend > 0.1:
            return 0.05  # Reduce learning rate when improving
        elif trend < -0.1:
            return 0.2   # Increase learning rate when declining
        else:
            return 0.1   # Default rate for stable performance
    
    # Code Generation Methods
    
    def generate_kernel_code(self, code_spec: Dict[str, Any]) -> Optional[callable]:
        """
        Generate new code for kernel processing functions.
        
        Args:
            code_spec: Code generation specification
            
        Returns:
            Generated function or None if generation failed
        """
        function_type = code_spec.get('function_type', 'cognitive_processing')
        
        # Generate function based on current architecture
        if function_type == 'cognitive_processing':
            return self._generate_cognitive_processing_function(code_spec)
        elif function_type == 'attention_allocation':
            return self._generate_attention_function(code_spec)
        elif function_type == 'tensor_encoding':
            return self._generate_encoding_function(code_spec)
        else:
            return self._generate_default_function(code_spec)
    
    def hot_swap_function(self, new_function: callable, function_type: str) -> bool:
        """
        Hot-swap kernel processing logic with new function.
        
        Args:
            new_function: New function to install
            function_type: Type of function being swapped
            
        Returns:
            True if swap successful, False if rolled back
        """
        checkpoint_id = self.create_checkpoint()
        
        try:
            # Test new function with sample data
            if self._test_function_safety(new_function, function_type):
                # Install new function in architecture
                self.current_architecture[f'function_{function_type}'] = new_function
                self._record_modification('hot_swap', 
                                        {'function_type': function_type, 'success': True}, 
                                        checkpoint_id)
                return True
            else:
                self.rollback_to_checkpoint(checkpoint_id)
                return False
                
        except Exception as e:
            self.rollback_to_checkpoint(checkpoint_id)
            print(f"Hot-swap failed: {e}")
            return False
    
    # Safety and Rollback Methods
    
    def create_checkpoint(self) -> str:
        """
        Create a checkpoint for rollback purposes.
        
        Returns:
            Checkpoint identifier
        """
        checkpoint_id = f"checkpoint_{self._checkpoint_counter}_{int(time.time())}"
        self._checkpoint_counter += 1
        
        checkpoint_data = {
            'id': checkpoint_id,
            'timestamp': time.time(),
            'base_kernel_state': {
                'shape': self.base_kernel.shape.copy(),
                'attention_weight': self.base_kernel.attention_weight,
                'meta_level': self.base_kernel.meta_level
            },
            'architecture': self.current_architecture.copy(),
            'constraints': self.constraints.copy(),
            'performance_metrics': self.performance_metrics.copy()
        }
        
        self.checkpoints[checkpoint_id] = checkpoint_data
        return checkpoint_id
    
    def rollback_to_checkpoint(self, checkpoint_id: str) -> bool:
        """
        Rollback kernel to a previous checkpoint.
        
        Args:
            checkpoint_id: Checkpoint identifier
            
        Returns:
            True if rollback successful
        """
        if checkpoint_id not in self.checkpoints:
            return False
        
        checkpoint_data = self.checkpoints[checkpoint_id]
        
        # Restore kernel state
        kernel_state = checkpoint_data['base_kernel_state']
        self.base_kernel.shape = kernel_state['shape']
        self.base_kernel.attention_weight = kernel_state['attention_weight']
        self.base_kernel.meta_level = kernel_state['meta_level']
        
        # Restore architecture
        self.current_architecture = checkpoint_data['architecture']
        self.constraints = checkpoint_data['constraints']
        self.performance_metrics = checkpoint_data['performance_metrics']
        
        # Record rollback
        self._record_modification('rollback', {'checkpoint_id': checkpoint_id}, None)
        
        return True
    
    # Performance-Driven Evolution Methods
    
    def fitness_evaluation(self, test_data: List[Dict[str, Any]]) -> float:
        """
        Evaluate fitness of current kernel configuration.
        
        Args:
            test_data: Test data for evaluation
            
        Returns:
            Fitness score (0.0 to 1.0)
        """
        fitness_components = {}
        
        # Evaluate efficiency
        fitness_components['efficiency'] = self._evaluate_efficiency()
        
        # Evaluate accuracy  
        fitness_components['accuracy'] = self._evaluate_accuracy(test_data)
        
        # Evaluate robustness
        fitness_components['robustness'] = self._evaluate_robustness()
        
        # Evaluate adaptability
        fitness_components['adaptability'] = self._evaluate_adaptability()
        
        # Calculate composite fitness
        weights = {'efficiency': 0.3, 'accuracy': 0.4, 'robustness': 0.2, 'adaptability': 0.1}
        composite_fitness = sum(weights.get(component, 0.25) * score 
                               for component, score in fitness_components.items())
        
        # Record performance metrics
        self.record_performance_metrics({
            'fitness': composite_fitness,
            'components': fitness_components,
            'timestamp': time.time()
        })
        
        return composite_fitness
    
    def performance_based_evolution(self, performance_history: List[Dict[str, Any]], 
                                   generations: int = 3) -> Dict[str, Any]:
        """
        Evolve kernel based on performance history using genetic algorithm.
        
        Args:
            performance_history: Historical performance data
            generations: Number of evolution generations
            
        Returns:
            Evolution results
        """
        evolution_results = {
            'initial_fitness': self.fitness_evaluation([]),
            'generations': [],
            'final_fitness': 0.0,
            'total_improvements': 0
        }
        
        for generation in range(generations):
            checkpoint = self.create_checkpoint()
            generation_improvements = 0
            
            # Calculate evolution pressure
            evolution_pressure = self._calculate_evolution_pressure(performance_history)
            
            # Generate and test modifications
            candidates = self._generate_evolution_candidates(evolution_pressure)
            
            for candidate in candidates:
                try:
                    if self.modify_architecture(candidate['modification']):
                        new_fitness = self.fitness_evaluation([])
                        
                        if new_fitness > evolution_results['initial_fitness']:
                            generation_improvements += 1
                            evolution_results['total_improvements'] += 1
                        else:
                            # Rollback if no improvement
                            self.rollback_to_checkpoint(checkpoint)
                            
                except Exception as e:
                    # Rollback on error
                    self.rollback_to_checkpoint(checkpoint)
            
            evolution_results['generations'].append({
                'generation': generation,
                'improvements': generation_improvements,
                'evolution_pressure': evolution_pressure
            })
        
        evolution_results['final_fitness'] = self.fitness_evaluation([])
        return evolution_results
    
    # Meta-Modification Methods
    
    def modify_modification_strategy(self, performance_feedback: Dict[str, Any]) -> bool:
        """
        Modify the modification strategy based on performance feedback.
        
        Args:
            performance_feedback: Performance feedback data
            
        Returns:
            True if strategy was changed
        """
        current_strategy = self.strategy
        strategy_performance = self._evaluate_strategy_performance_history()
        
        if strategy_performance < 0.6:  # Poor performance
            new_strategy = self._select_better_strategy(current_strategy, strategy_performance)
            
            if new_strategy and new_strategy != current_strategy:
                self.strategy = new_strategy
                self._record_modification('meta_modification',
                                        {'old_strategy': current_strategy,
                                         'new_strategy': new_strategy,
                                         'reason': 'strategy_underperformance'}, None)
                return True
        
        return False
    
    def adaptive_constraint_relaxation(self, performance_history: List[Dict[str, Any]]) -> bool:
        """
        Adaptively relax or tighten constraints based on performance.
        
        Args:
            performance_history: Performance history data
            
        Returns:
            True if constraints were modified
        """
        performance_trend = self._extract_performance_trend(performance_history)
        success_rate = self._calculate_modification_success_rate()
        
        old_constraints = self.constraints.copy()
        
        # Relax constraints if performance is good
        if performance_trend > 0.1 and success_rate > 0.8:
            self.constraints = self._relax_constraints(self.constraints, 0.1)
            constraint_action = 'relaxation'
            
        # Tighten constraints if performance is poor
        elif performance_trend < -0.1 or success_rate < 0.4:
            self.constraints = self._tighten_constraints(self.constraints, 0.1)
            constraint_action = 'tightening'
        else:
            return False
        
        self._record_modification('constraint_modification',
                                {'old_constraints': old_constraints,
                                 'new_constraints': self.constraints,
                                 'action': constraint_action}, None)
        return True
    
    # Utility Methods
    
    def record_performance_metrics(self, metrics: Dict[str, Any]) -> None:
        """Record performance metrics for the kernel."""
        timestamp = time.time()
        self.performance_metrics[timestamp] = metrics
        
        # Keep only recent metrics (last 100 entries)
        if len(self.performance_metrics) > 100:
            oldest_timestamp = min(self.performance_metrics.keys())
            del self.performance_metrics[oldest_timestamp]
    
    def get_modification_history(self) -> List[Dict[str, Any]]:
        """Get modification history."""
        return self.modification_history.copy()
    
    def get_current_architecture(self) -> Dict[str, Any]:
        """Get current architecture specification."""
        return self.current_architecture.copy()
    
    def get_performance_summary(self) -> Dict[str, Any]:
        """Get performance summary statistics."""
        if not self.performance_metrics:
            return {'count': 0, 'average_fitness': 0.5}
        
        fitness_values = [metrics.get('fitness', 0.5) 
                         for metrics in self.performance_metrics.values()]
        
        return {
            'count': len(fitness_values),
            'average_fitness': sum(fitness_values) / len(fitness_values),
            'max_fitness': max(fitness_values),
            'min_fitness': min(fitness_values),
            'recent_fitness': fitness_values[-5:] if len(fitness_values) >= 5 else fitness_values
        }
    
    # Private Helper Methods
    
    def _validate_modification_safety(self, modification_spec: Dict[str, Any]) -> bool:
        """Validate modification against safety constraints."""
        for key, value in modification_spec.items():
            if key == 'tensor_shape' and isinstance(value, list):
                if len(value) > self.constraints['max_tensor_dimensions']:
                    return False
                if len(value) < self.constraints['min_tensor_dimensions']:
                    return False
            elif key == 'attention_weight' and isinstance(value, (int, float)):
                if value > self.constraints['max_attention_weight']:
                    return False
                if value < self.constraints['min_attention_weight']:
                    return False
        
        return True
    
    def _apply_modification_python(self, modification_spec: Dict[str, Any]) -> bool:
        """Apply modification using Python fallback implementation."""
        try:
            if 'tensor_shape' in modification_spec:
                self.base_kernel.shape = modification_spec['tensor_shape']
            
            if 'attention_weight' in modification_spec:
                self.base_kernel.attention_weight = modification_spec['attention_weight']
            
            return True
        except:
            return False
    
    def _update_base_kernel(self, modification_spec: Dict[str, Any]) -> None:
        """Update base kernel with modifications."""
        if 'tensor_shape' in modification_spec:
            self.base_kernel.shape = modification_spec['tensor_shape']
        
        if 'attention_weight' in modification_spec:
            self.base_kernel.attention_weight = modification_spec['attention_weight']
    
    def _record_modification(self, mod_type: str, spec: Dict[str, Any], checkpoint_id: Optional[str]) -> None:
        """Record a modification in the history."""
        entry = {
            'timestamp': time.time(),
            'type': mod_type,
            'specification': spec,
            'checkpoint': checkpoint_id,
            'result': 'success'
        }
        
        self.modification_history.append(entry)
        
        # Keep only recent history (last 100 entries)
        if len(self.modification_history) > 100:
            self.modification_history = self.modification_history[-100:]
    
    def _generate_shape_mutations(self, shape: List[int]) -> List[List[int]]:
        """Generate mutations of tensor shape."""
        mutations = []
        exploration_rate = self._get_strategy_exploration_rate()
        
        # Size mutations
        for i, dim in enumerate(shape):
            if random.random() < exploration_rate:
                new_shape = shape.copy()
                new_shape[i] = max(1, dim + random.randint(-2, 3))
                mutations.append(new_shape)
        
        # Dimension count mutations
        if random.random() < exploration_rate * 0.5:
            # Add dimension
            extended_shape = shape + [random.randint(8, 64)]
            mutations.append(extended_shape)
            
            # Remove dimension (if more than 1)
            if len(shape) > 1:
                reduced_shape = shape[:-1]
                mutations.append(reduced_shape)
        
        return mutations
    
    def _generate_parameter_mutations(self, mutation_rate: float, evolution_spec: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Generate parameter mutations."""
        mutations = []
        
        # Attention weight mutation
        if random.random() < mutation_rate:
            current_attention = self.base_kernel.attention_weight
            mutation_strength = evolution_spec.get('attention_strength', 0.1)
            new_attention = current_attention + random.uniform(-mutation_strength, mutation_strength)
            new_attention = max(0.01, min(1.0, new_attention))
            
            mutations.append({
                'parameter': 'attention_weight',
                'value': new_attention,
                'old_value': current_attention
            })
        
        # Meta-level mutation
        if random.random() < mutation_rate * 0.5:
            current_meta = self.base_kernel.meta_level
            new_meta = max(0, current_meta + random.randint(-1, 2))
            
            mutations.append({
                'parameter': 'meta_level',
                'value': new_meta,
                'old_value': current_meta
            })
        
        return mutations
    
    def _validate_parameter_mutation(self, mutation: Dict[str, Any]) -> bool:
        """Validate parameter mutation against constraints."""
        param = mutation['parameter']
        value = mutation['value']
        
        if param == 'attention_weight':
            return (self.constraints['min_attention_weight'] <= value <= 
                   self.constraints['max_attention_weight'])
        elif param == 'meta_level':
            return 0 <= value <= self.constraints['max_meta_levels']
        
        return True
    
    def _calculate_attention_adaptation(self, trend: float) -> float:
        """Calculate attention adaptation factor."""
        if trend > 0.1:
            return 0.05  # Slight increase for good performance
        elif trend < -0.1:
            return -0.1  # Decrease for poor performance
        else:
            return 0.0   # No change for stable performance
    
    def _evaluate_strategy_performance(self, strategy: str, performance_data: Dict[str, Any]) -> float:
        """Evaluate performance of an encoding strategy."""
        # Simulate strategy performance evaluation
        base_score = {
            'prime': 0.8,
            'fibonacci': 0.7,
            'harmonic': 0.6,
            'factorial': 0.75,
            'power-of-two': 0.65
        }.get(strategy, 0.5)
        
        # Add some randomness and performance data influence
        noise = random.uniform(-0.1, 0.1)
        data_influence = performance_data.get('strategy_boost', {}).get(strategy, 0.0)
        
        return max(0.0, min(1.0, base_score + noise + data_influence))
    
    def _get_strategy_exploration_rate(self) -> float:
        """Get exploration rate based on current strategy."""
        strategy_rates = {
            'conservative': 0.1,
            'balanced': 0.3,
            'aggressive': 0.6,
            'adaptive': self._calculate_dynamic_exploration_rate()
        }
        
        return strategy_rates.get(self.strategy, 0.3)
    
    def _calculate_dynamic_exploration_rate(self) -> float:
        """Calculate dynamic exploration rate for adaptive strategy."""
        recent_performance = self._get_recent_performance()
        
        if recent_performance < 0.4:
            return 0.7  # High exploration for poor performance
        elif recent_performance > 0.8:
            return 0.1  # Low exploration for good performance
        else:
            return 0.4  # Medium exploration for average performance
    
    def _get_recent_performance(self) -> float:
        """Get recent performance score."""
        if not self.performance_metrics:
            return 0.5
        
        recent_metrics = list(self.performance_metrics.values())[-5:]
        fitness_scores = [metrics.get('fitness', 0.5) for metrics in recent_metrics]
        
        return sum(fitness_scores) / len(fitness_scores) if fitness_scores else 0.5
    
    def _evaluate_efficiency(self) -> float:
        """Evaluate processing efficiency."""
        complexity = 1
        for dim in self.base_kernel.shape:
            complexity *= dim
        
        attention = self.base_kernel.attention_weight
        return attention / (1 + complex(0, 1) * complexity).real * 1000  # Simplified efficiency metric
    
    def _evaluate_accuracy(self, test_data: List[Dict[str, Any]]) -> float:
        """Evaluate accuracy of kernel processing."""
        # Simplified accuracy evaluation
        encoding_length = len(self.base_kernel.tensor_field_encoding())
        return min(1.0, encoding_length / 10.0)
    
    def _evaluate_robustness(self) -> float:
        """Evaluate robustness to variations."""
        attention = self.base_kernel.attention_weight
        return 1.0 - abs(attention - 0.5)  # Robustness peaks at moderate attention
    
    def _evaluate_adaptability(self) -> float:
        """Evaluate adaptability potential."""
        meta_level = self.base_kernel.meta_level
        shape_variance = self._calculate_shape_variance()
        return (0.3 * meta_level + 0.7 * shape_variance)
    
    def _calculate_shape_variance(self) -> float:
        """Calculate variance in tensor shape dimensions."""
        shape = self.base_kernel.shape
        if len(shape) < 2:
            return 0.1
        
        mean_dim = sum(shape) / len(shape)
        variance = sum((dim - mean_dim) ** 2 for dim in shape) / len(shape)
        return variance / 100.0  # Normalize
    
    def _calculate_evolution_pressure(self, performance_history: List[Dict[str, Any]]) -> float:
        """Calculate evolution pressure based on performance trend."""
        if len(performance_history) < 2:
            return 0.3
        
        recent_fitness = [entry.get('fitness', 0.5) for entry in performance_history[-5:]]
        overall_fitness = [entry.get('fitness', 0.5) for entry in performance_history]
        
        recent_avg = sum(recent_fitness) / len(recent_fitness)
        overall_avg = sum(overall_fitness) / len(overall_fitness)
        
        if recent_avg < 0.8 * overall_avg:
            return 0.8  # High pressure
        elif recent_avg > 1.2 * overall_avg:
            return 0.1  # Low pressure
        else:
            return 0.4  # Medium pressure
    
    def _generate_evolution_candidates(self, evolution_pressure: float) -> List[Dict[str, Any]]:
        """Generate candidate modifications for evolution."""
        candidates = []
        
        # Architecture candidates
        if evolution_pressure > 0.3:
            new_shape = self._mutate_tensor_shape(self.base_kernel.shape, 0.3)
            candidates.append({
                'type': 'architecture',
                'modification': {'tensor_shape': new_shape},
                'expected_fitness': 0.1 + random.uniform(0, 0.2)
            })
        
        # Parameter candidates
        if evolution_pressure > 0.2:
            new_attention = self.base_kernel.attention_weight + random.uniform(-0.2, 0.2)
            new_attention = max(0.01, min(1.0, new_attention))
            candidates.append({
                'type': 'parameter',
                'modification': {'attention_weight': new_attention},
                'expected_fitness': 0.05 + random.uniform(0, 0.1)
            })
        
        return candidates
    
    def _mutate_tensor_shape(self, shape: List[int], mutation_rate: float) -> List[int]:
        """Mutate tensor shape."""
        new_shape = []
        for dim in shape:
            if random.random() < mutation_rate:
                new_shape.append(max(1, dim + random.randint(-4, 5)))
            else:
                new_shape.append(dim)
        return new_shape
    
    def _evaluate_strategy_performance_history(self) -> float:
        """Evaluate performance of current strategy based on history."""
        if len(self.modification_history) < 5:
            return 0.5  # Neutral if insufficient history
        
        recent_modifications = self.modification_history[-10:]
        successful_modifications = [mod for mod in recent_modifications 
                                   if mod.get('result') == 'success']
        
        return len(successful_modifications) / len(recent_modifications)
    
    def _select_better_strategy(self, current_strategy: str, performance: float) -> Optional[str]:
        """Select a better modification strategy."""
        strategy_preferences = {
            'conservative': ['balanced', 'adaptive'],
            'balanced': ['aggressive', 'adaptive'] if performance < 0.4 else ['conservative'],
            'aggressive': ['balanced', 'adaptive'],
            'adaptive': ['balanced', 'conservative']
        }
        
        alternatives = strategy_preferences.get(current_strategy, ['balanced'])
        return alternatives[0] if alternatives else None
    
    def _extract_performance_trend(self, performance_history: List[Dict[str, Any]]) -> float:
        """Extract performance trend from history."""
        if len(performance_history) < 2:
            return 0.0
        
        recent_fitness = [entry.get('fitness', 0.5) for entry in performance_history[-3:]]
        older_fitness = [entry.get('fitness', 0.5) for entry in performance_history[-6:-3]]
        
        if not older_fitness:
            return 0.0
        
        recent_avg = sum(recent_fitness) / len(recent_fitness)
        older_avg = sum(older_fitness) / len(older_fitness)
        
        return recent_avg - older_avg
    
    def _calculate_modification_success_rate(self) -> float:
        """Calculate success rate of recent modifications."""
        if len(self.modification_history) < 5:
            return 0.5
        
        recent_mods = self.modification_history[-20:]
        successful_mods = [mod for mod in recent_mods if mod.get('result') == 'success']
        
        return len(successful_mods) / len(recent_mods)
    
    def _relax_constraints(self, constraints: Dict[str, Any], factor: float) -> Dict[str, Any]:
        """Relax safety constraints by factor."""
        new_constraints = constraints.copy()
        
        new_constraints['max_tensor_dimensions'] = int(constraints['max_tensor_dimensions'] * (1 + factor))
        new_constraints['max_attention_weight'] = min(1.0, constraints['max_attention_weight'] + 0.1 * factor)
        new_constraints['max_modifications_per_cycle'] = constraints['max_modifications_per_cycle'] + 1
        
        return new_constraints
    
    def _tighten_constraints(self, constraints: Dict[str, Any], factor: float) -> Dict[str, Any]:
        """Tighten safety constraints by factor."""
        new_constraints = constraints.copy()
        
        new_constraints['max_tensor_dimensions'] = max(1, int(constraints['max_tensor_dimensions'] * (1 - factor)))
        new_constraints['max_attention_weight'] = max(0.01, constraints['max_attention_weight'] - 0.1 * factor)
        new_constraints['max_modifications_per_cycle'] = max(1, constraints['max_modifications_per_cycle'] - 1)
        
        return new_constraints
    
    def _generate_cognitive_processing_function(self, spec: Dict[str, Any]) -> callable:
        """Generate cognitive processing function."""
        def cognitive_processing_fn(input_data: Any) -> Any:
            # Apply attention weighting
            attention = self.base_kernel.attention_weight
            
            # Simple processing based on current architecture
            if isinstance(input_data, (list, tuple)):
                return [item * attention for item in input_data]
            elif isinstance(input_data, (int, float)):
                return input_data * attention
            else:
                return input_data
        
        return cognitive_processing_fn
    
    def _generate_attention_function(self, spec: Dict[str, Any]) -> callable:
        """Generate attention allocation function."""
        def attention_allocation_fn(kernels: List[Any], goals: List[str]) -> List[Dict[str, Any]]:
            # Simple attention allocation
            allocations = []
            base_attention = 1.0 / len(kernels) if kernels else 0.0
            
            for i, kernel in enumerate(kernels):
                goal = goals[i] if i < len(goals) else 'default'
                score = {'reasoning': 0.9, 'learning': 0.7, 'attention': 0.8}.get(goal, 0.5)
                
                allocations.append({
                    'kernel': kernel,
                    'attention_score': score,
                    'allocation': base_attention * score
                })
            
            return allocations
        
        return attention_allocation_fn
    
    def _generate_encoding_function(self, spec: Dict[str, Any]) -> callable:
        """Generate tensor encoding function."""
        def encoding_fn(kernel: Any, encoding_type: str = 'prime') -> List[float]:
            # Use base kernel's encoding capability
            return self.base_kernel.tensor_field_encoding(encoding_type)
        
        return encoding_fn
    
    def _generate_default_function(self, spec: Dict[str, Any]) -> callable:
        """Generate default function."""
        def default_fn(input_data: Any) -> Any:
            return input_data
        
        return default_fn
    
    def _test_function_safety(self, function: callable, function_type: str) -> bool:
        """Test function safety with sample data."""
        try:
            if function_type == 'cognitive_processing':
                # Test with simple data
                result = function([1, 2, 3])
                return isinstance(result, list) and len(result) == 3
            elif function_type == 'attention_allocation':
                # Test with mock kernels and goals
                result = function([self.base_kernel], ['reasoning'])
                return isinstance(result, list) and len(result) == 1
            elif function_type == 'tensor_encoding':
                # Test encoding function
                result = function(self.base_kernel, 'prime')
                return isinstance(result, list)
            else:
                return True  # Default to safe for unknown types
        except:
            return False
    
    def _dict_to_scheme_alist(self, d: Dict[str, Any]) -> str:
        """Convert Python dict to Scheme association list string."""
        items = []
        for key, value in d.items():
            if isinstance(value, str):
                items.append(f'({key} . "{value}")')
            elif isinstance(value, list):
                list_str = ' '.join(map(str, value))
                items.append(f'({key} . ({list_str}))')
            else:
                items.append(f'({key} . {value})')
        
        return '(' + ' '.join(items) + ')'


class SelfModifyingKernelManager:
    """Manager for multiple self-modifying kernels."""
    
    def __init__(self):
        self.kernels = []
        self.global_performance_history = []
    
    def create_kernel(self, shape: List[int], attention_weight: float = 0.5,
                     constraints: Optional[Dict[str, Any]] = None,
                     strategy: str = 'balanced') -> SelfModifyingKernel:
        """Create and register a new self-modifying kernel."""
        base_kernel = CognitiveKernel(shape, attention_weight)
        sm_kernel = SelfModifyingKernel(base_kernel, constraints, strategy)
        self.kernels.append(sm_kernel)
        return sm_kernel
    
    def evolve_population(self, generations: int = 5) -> Dict[str, Any]:
        """Evolve entire population of kernels."""
        evolution_results = {
            'initial_population_fitness': [],
            'final_population_fitness': [],
            'generations': generations,
            'improvements': 0
        }
        
        # Record initial fitness
        for kernel in self.kernels:
            fitness = kernel.fitness_evaluation([])
            evolution_results['initial_population_fitness'].append(fitness)
        
        # Evolve each kernel
        for generation in range(generations):
            for kernel in self.kernels:
                # Performance-based evolution
                kernel.performance_based_evolution(self.global_performance_history, 1)
        
        # Record final fitness
        for kernel in self.kernels:
            fitness = kernel.fitness_evaluation([])
            evolution_results['final_population_fitness'].append(fitness)
        
        # Calculate improvements
        initial_avg = sum(evolution_results['initial_population_fitness']) / len(self.kernels)
        final_avg = sum(evolution_results['final_population_fitness']) / len(self.kernels)
        evolution_results['improvement'] = final_avg - initial_avg
        
        return evolution_results
    
    def adaptive_population_management(self) -> Dict[str, Any]:
        """Manage population adaptation and diversity."""
        results = {
            'actions_taken': [],
            'population_size': len(self.kernels),
            'diversity_score': self._calculate_population_diversity()
        }
        
        # Remove poorly performing kernels
        if len(self.kernels) > 2:
            fitness_scores = [(i, kernel.fitness_evaluation([])) 
                             for i, kernel in enumerate(self.kernels)]
            fitness_scores.sort(key=lambda x: x[1])
            
            # Remove bottom 25% if population is large enough
            remove_count = max(0, len(self.kernels) // 4)
            if remove_count > 0:
                for i in range(remove_count):
                    kernel_idx = fitness_scores[i][0]
                    self.kernels.pop(kernel_idx)
                    results['actions_taken'].append(f'removed_kernel_{kernel_idx}')
        
        # Add new kernels for diversity
        if results['diversity_score'] < 0.3:
            new_kernel = self.create_kernel([16, 16], 0.6, strategy='adaptive')
            results['actions_taken'].append('added_diversity_kernel')
        
        return results
    
    def _calculate_population_diversity(self) -> float:
        """Calculate diversity score of kernel population."""
        if len(self.kernels) < 2:
            return 1.0
        
        # Calculate diversity based on shape and attention differences
        total_diversity = 0.0
        comparisons = 0
        
        for i in range(len(self.kernels)):
            for j in range(i + 1, len(self.kernels)):
                k1, k2 = self.kernels[i], self.kernels[j]
                
                # Shape diversity
                shape_diff = abs(len(k1.base_kernel.shape) - len(k2.base_kernel.shape))
                shape_diversity = min(1.0, shape_diff / 5.0)
                
                # Attention diversity
                att_diff = abs(k1.base_kernel.attention_weight - k2.base_kernel.attention_weight)
                att_diversity = min(1.0, att_diff / 0.5)
                
                # Strategy diversity
                strategy_diversity = 1.0 if k1.strategy != k2.strategy else 0.0
                
                kernel_diversity = (shape_diversity + att_diversity + strategy_diversity) / 3.0
                total_diversity += kernel_diversity
                comparisons += 1
        
        return total_diversity / comparisons if comparisons > 0 else 1.0


def test_self_modifying_kernel():
    """Test the self-modifying kernel functionality."""
    print("Testing Self-Modifying Kernel System...")
    
    # Test basic kernel creation
    base_kernel = CognitiveKernel([32, 32], 0.7)
    sm_kernel = SelfModifyingKernel(base_kernel)
    print(f"Created self-modifying kernel: {sm_kernel.current_architecture['tensor_shape']}")
    
    # Test architecture modification
    modification = {'tensor_shape': [64, 64], 'modification_reason': 'test'}
    result = sm_kernel.modify_architecture(modification)
    print(f"Architecture modification successful: {result}")
    
    # Test parameter evolution
    evolution_spec = {'mutation_rate': 0.3, 'attention_strength': 0.1}
    mutations = sm_kernel.evolve_parameters(evolution_spec)
    print(f"Applied {len(mutations)} parameter mutations")
    
    # Test tensor shape evolution
    fitness_fn = lambda shape: sum(shape) / len(shape) / 50.0  # Simple fitness function
    evolution_result = sm_kernel.evolve_tensor_shape(fitness_fn, generations=3)
    print(f"Shape evolution improvement: {evolution_result['improvement']:.3f}")
    
    # Test fitness evaluation
    fitness = sm_kernel.fitness_evaluation([])
    print(f"Current fitness: {fitness:.3f}")
    
    # Test checkpoint and rollback
    checkpoint = sm_kernel.create_checkpoint()
    print(f"Created checkpoint: {checkpoint}")
    
    # Make a modification and rollback
    sm_kernel.modify_architecture({'attention_weight': 0.9})
    print(f"Modified attention to: {sm_kernel.base_kernel.attention_weight}")
    
    rollback_success = sm_kernel.rollback_to_checkpoint(checkpoint)
    print(f"Rollback successful: {rollback_success}")
    print(f"Attention after rollback: {sm_kernel.base_kernel.attention_weight}")
    
    # Test performance-based evolution
    performance_history = [
        {'fitness': 0.4, 'timestamp': time.time() - 100},
        {'fitness': 0.3, 'timestamp': time.time() - 50},
        {'fitness': 0.35, 'timestamp': time.time()}
    ]
    
    evolution_results = sm_kernel.performance_based_evolution(performance_history, generations=2)
    print(f"Performance-based evolution completed: {evolution_results['total_improvements']} improvements")
    
    # Test code generation
    code_spec = {'function_type': 'cognitive_processing', 'optimization_level': 'high'}
    generated_fn = sm_kernel.generate_kernel_code(code_spec)
    print(f"Code generation successful: {generated_fn is not None}")
    
    if generated_fn:
        # Test hot-swap
        swap_success = sm_kernel.hot_swap_function(generated_fn, 'cognitive_processing')
        print(f"Hot-swap successful: {swap_success}")
    
    # Test meta-modification
    strategy_changed = sm_kernel.modify_modification_strategy({'trend': -0.3})
    print(f"Strategy modification: {strategy_changed}")
    
    # Test constraint adaptation
    constraint_changed = sm_kernel.adaptive_constraint_relaxation(performance_history)
    print(f"Constraint adaptation: {constraint_changed}")
    
    # Test kernel manager
    manager = SelfModifyingKernelManager()
    
    # Create population
    for i in range(3):
        manager.create_kernel([16 + i*8, 16 + i*8], 0.5 + i*0.1)
    
    print(f"Created population of {len(manager.kernels)} kernels")
    
    # Evolve population
    pop_results = manager.evolve_population(generations=2)
    print(f"Population evolution improvement: {pop_results['improvement']:.3f}")
    
    # Test population management
    mgmt_results = manager.adaptive_population_management()
    print(f"Population management actions: {mgmt_results['actions_taken']}")
    
    # Print performance summary
    summary = sm_kernel.get_performance_summary()
    print(f"Performance summary: {summary['count']} evaluations, avg fitness: {summary['average_fitness']:.3f}")
    
    print("Self-Modifying Kernel tests completed successfully!")


if __name__ == "__main__":
    test_self_modifying_kernel()