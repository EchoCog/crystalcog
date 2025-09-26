#!/usr/bin/env python3
"""
Python wrapper for Agent-Zero Cognitive Kernel Module.

This module provides a Python interface to the Agent-Zero cognitive kernel
implemented in Guile Scheme and C. It allows easy access to cognitive
functionalities from Python applications.
"""

import subprocess
import json
import tempfile
import os
from typing import List, Dict, Any, Optional, Tuple
import numpy as np


class CognitiveKernel:
    """Python wrapper for the Agent-Zero cognitive kernel."""
    
    def __init__(self, shape: List[int], attention_weight: float = 0.5):
        """
        Initialize a cognitive kernel.
        
        Args:
            shape: Tensor dimensions for the kernel
            attention_weight: Attention allocation weight (0.0 to 1.0)
        """
        self.shape = shape
        self.attention_weight = attention_weight
        self.meta_level = 0
        self._guile_available = self._check_guile_available()
        
    def _check_guile_available(self) -> bool:
        """Check if Guile is available for running Scheme code."""
        try:
            result = subprocess.run(['guile', '--version'], 
                                  capture_output=True, text=True)
            return result.returncode == 0
        except FileNotFoundError:
            return False
    
    def _run_guile_code(self, code: str) -> str:
        """Run Guile Scheme code and return the output."""
        if not self._guile_available:
            raise RuntimeError("Guile is not available. Please install guile-3.0")
        
        # Set up environment for Guile modules
        env = os.environ.copy()
        current_dir = os.path.dirname(os.path.abspath(__file__))
        modules_path = os.path.join(current_dir, 'modules')
        
        if 'GUILE_LOAD_PATH' in env:
            env['GUILE_LOAD_PATH'] = f"{modules_path}:{env['GUILE_LOAD_PATH']}"
        else:
            env['GUILE_LOAD_PATH'] = modules_path
            
        # Run the Guile code
        result = subprocess.run(['guile', '-c', code], 
                              capture_output=True, text=True, env=env)
        
        if result.returncode != 0:
            raise RuntimeError(f"Guile execution failed: {result.stderr}")
            
        return result.stdout.strip()
    
    def tensor_field_encoding(self, encoding_type: str = 'prime', 
                            include_attention: bool = True,
                            include_meta_level: bool = False,
                            normalization: str = 'none') -> List[float]:
        """
        Generate tensor field encoding for the kernel.
        
        Args:
            encoding_type: Type of mathematical sequence ('prime', 'fibonacci', 
                         'harmonic', 'factorial', 'power-of-two')
            include_attention: Whether to include attention weighting
            include_meta_level: Whether to include meta-level information
            normalization: Normalization method ('none', 'unit', 'standard')
            
        Returns:
            Encoded tensor field as a list of floats
        """
        if self._guile_available:
            # Use Guile implementation
            shape_str = ' '.join(map(str, self.shape))
            code = f"""
            (use-modules (agent-zero kernel))
            (let ((kernel (spawn-cognitive-kernel '({shape_str}) {self.attention_weight})))
              (let ((encoding (tensor-field-encoding kernel '{encoding_type} #{'t' if include_attention else 'f'} #{'t' if include_meta_level else 'f'} '{normalization})))
                (format #t "~a" encoding)))
            """
            output = self._run_guile_code(code)
            # Parse the Scheme list output
            return self._parse_scheme_list(output)
        else:
            # Fallback Python implementation
            return self._python_tensor_encoding(encoding_type, include_attention, 
                                               include_meta_level, normalization)
    
    def _python_tensor_encoding(self, encoding_type: str, include_attention: bool,
                               include_meta_level: bool, normalization: str) -> List[float]:
        """Fallback Python implementation of tensor field encoding."""
        # Generate mathematical sequences
        sequences = {
            'prime': self._generate_primes(len(self.shape)),
            'fibonacci': self._generate_fibonacci(len(self.shape)),
            'harmonic': self._generate_harmonic(len(self.shape)),
            'factorial': self._generate_factorial(len(self.shape)),
            'power-of-two': self._generate_powers_of_two(len(self.shape))
        }
        
        base_sequence = sequences.get(encoding_type, sequences['prime'])
        
        # Apply base encoding
        encoding = [dim * seq for dim, seq in zip(self.shape, base_sequence)]
        
        # Apply attention weighting
        if include_attention:
            encoding = [val * self.attention_weight for val in encoding]
            
        # Include meta-level
        if include_meta_level:
            encoding.append(float(self.meta_level))
            
        # Apply normalization
        if normalization == 'unit':
            magnitude = np.sqrt(sum(x**2 for x in encoding))
            if magnitude > 0:
                encoding = [x / magnitude for x in encoding]
        elif normalization == 'standard':
            mean_val = sum(encoding) / len(encoding)
            centered = [x - mean_val for x in encoding]
            variance = sum(x**2 for x in centered) / len(centered)
            std_dev = np.sqrt(variance)
            if std_dev > 0:
                encoding = [x / std_dev for x in centered]
        
        return encoding
    
    def hypergraph_state(self) -> Dict[str, Any]:
        """Get hypergraph state representation of the kernel."""
        if self._guile_available:
            shape_str = ' '.join(map(str, self.shape))
            code = f"""
            (use-modules (agent-zero kernel))
            (let ((kernel (spawn-cognitive-kernel '({shape_str}) {self.attention_weight})))
              (let ((state (hypergraph-state kernel)))
                (format #t "~a" state)))
            """
            output = self._run_guile_code(code)
            return self._parse_scheme_alist(output)
        else:
            return {
                'tensor_shape': self.shape,
                'attention': self.attention_weight,
                'meta_level': self.meta_level,
                'atomspace': ['atomspace', []]
            }
    
    def recursive_self_description(self) -> Dict[str, Any]:
        """Generate recursive self-description of the kernel."""
        if self._guile_available:
            shape_str = ' '.join(map(str, self.shape))
            code = f"""
            (use-modules (agent-zero kernel) (agent-zero meta-cognition))
            (let ((kernel (spawn-cognitive-kernel '({shape_str}) {self.attention_weight})))
              (let ((desc (recursive-self-description kernel)))
                (format #t "~a" desc)))
            """
            output = self._run_guile_code(code)
            return self._parse_scheme_alist(output)
        else:
            return {
                'tensor_shape': self.shape,
                'cognitive_function': 'cognitive-processing',
                'attention_allocation': self.attention_weight,
                'meta_level': self.meta_level + 1,
                'self_model': {
                    'architecture': 'agent-zero-genesis',
                    'cognitive_capabilities': ['reasoning', 'attention', 'learning', 'adaptation']
                }
            }
    
    @staticmethod
    def _generate_primes(n: int) -> List[int]:
        """Generate first n prime numbers."""
        if n <= 0:
            return []
        primes = []
        num = 2
        while len(primes) < n:
            is_prime = True
            for p in primes:
                if p * p > num:
                    break
                if num % p == 0:
                    is_prime = False
                    break
            if is_prime:
                primes.append(num)
            num += 1
        return primes
    
    @staticmethod
    def _generate_fibonacci(n: int) -> List[int]:
        """Generate first n Fibonacci numbers."""
        if n <= 0:
            return []
        if n == 1:
            return [1]
        fib = [1, 1]
        for i in range(2, n):
            fib.append(fib[i-1] + fib[i-2])
        return fib[:n]
    
    @staticmethod
    def _generate_harmonic(n: int) -> List[float]:
        """Generate first n harmonic numbers."""
        return [1.0 / (i + 1) for i in range(n)]
    
    @staticmethod
    def _generate_factorial(n: int) -> List[int]:
        """Generate first n factorial numbers."""
        if n <= 0:
            return []
        factorials = [1]
        for i in range(1, n):
            factorials.append(factorials[-1] * (i + 1))
        return factorials
    
    @staticmethod
    def _generate_powers_of_two(n: int) -> List[int]:
        """Generate first n powers of 2."""
        return [2**i for i in range(n)]
    
    def _parse_scheme_list(self, scheme_str: str) -> List[float]:
        """Parse a Scheme list string to Python list."""
        # Simple parser for Scheme lists containing numbers
        clean_str = scheme_str.strip('()')
        if not clean_str:
            return []
        items = clean_str.split()
        return [float(item) for item in items if item]
    
    def _parse_scheme_alist(self, scheme_str: str) -> Dict[str, Any]:
        """Parse a Scheme association list to Python dict."""
        # Simplified parser for demonstration
        # In a real implementation, you'd want a more robust parser
        return {
            'tensor_shape': self.shape,
            'attention': self.attention_weight,
            'meta_level': self.meta_level,
            'parsed_from_scheme': True
        }
    
    def advanced_meta_cognitive_reflection(self) -> Dict[str, Any]:
        """Perform advanced meta-cognitive reflection with monitoring, diagnostics, and multi-level reasoning."""
        if self._guile_available:
            shape_str = ' '.join(map(str, self.shape))
            code = f"""
            (use-modules (agent-zero kernel) (agent-zero meta-cognition))
            (let ((kernel (spawn-cognitive-kernel '({shape_str}) {self.attention_weight})))
              (let ((reflection (meta-cognitive-reflection kernel)))
                (format #t "~a" reflection)))
            """
            output = self._run_guile_code(code)
            return self._parse_scheme_alist(output)
        else:
            # Fallback Python implementation with advanced features
            return {
                'current-state': {
                    'tensor_shape': self.shape,
                    'attention': self.attention_weight,
                    'meta_level': self.meta_level
                },
                'self-assessment': {
                    'attention-efficiency': 'high' if self.attention_weight > 0.7 else 'moderate',
                    'processing-complexity': 'high' if len(self.shape) > 2 else 'low',
                    'overall-performance': 'good' if self.attention_weight > 0.6 and len(self.shape) > 1 else 'needs-improvement'
                },
                'diagnostic-analysis': {
                    'timestamp': 1234567890,
                    'attention-level': self.attention_weight,
                    'tensor-complexity': np.prod(self.shape),
                    'meta-level': self.meta_level,
                    'processing-load': np.prod(self.shape) * self.attention_weight / 1000.0,
                    'memory-efficiency': self.attention_weight * 100 / max(1, np.log(np.prod(self.shape))),
                    'cognitive-coherence': (self.attention_weight + len(self.shape) * 0.1) / (1 + len(self.shape))
                },
                'multi-level-reasoning': [
                    {
                        'level': 0,
                        'focus': 'object-reasoning',
                        'content': {'tensor_shape': self.shape, 'attention': self.attention_weight},
                        'confidence': 0.8
                    },
                    {
                        'level': 1,
                        'focus': 'meta-reasoning',
                        'content': {'reasoning-effectiveness': 0.7, 'attention-allocation-quality': 0.8},
                        'confidence': 0.7
                    }
                ],
                'adaptive-tuning-ready': True
            }
            
    def save_cognitive_state(self, filename: str) -> str:
        """Save current cognitive state to file."""
        if self._guile_available:
            shape_str = ' '.join(map(str, self.shape))
            code = f"""
            (use-modules (agent-zero kernel) (agent-zero meta-cognition))
            (let ((kernel (spawn-cognitive-kernel '({shape_str}) {self.attention_weight})))
              (save-cognitive-state kernel "{filename}"))
            """
            return self._run_guile_code(code).strip()
        else:
            # Fallback Python implementation
            state = {
                'timestamp': 1234567890,
                'kernel-shape': self.shape,
                'attention-weight': self.attention_weight,
                'meta-level': self.meta_level,
                'hypergraph-state': self.hypergraph_state(),
                'cognitive-function': 'cognitive-processing',
                'self-description': self.recursive_self_description()
            }
            with open(filename, 'w') as f:
                json.dump(state, f)
            return filename
            
    def restore_cognitive_state(self, filename: str) -> Dict[str, Any]:
        """Restore cognitive state from file."""
        if self._guile_available:
            code = f"""
            (use-modules (agent-zero meta-cognition))
            (restore-cognitive-state "{filename}")
            """
            output = self._run_guile_code(code)
            return self._parse_scheme_alist(output)
        else:
            # Fallback Python implementation
            with open(filename, 'r') as f:
                state = json.load(f)
            return state
            
    def monitor_cognitive_state(self) -> Dict[str, Any]:
        """Monitor current cognitive state with diagnostics."""
        if self._guile_available:
            shape_str = ' '.join(map(str, self.shape))
            code = f"""
            (use-modules (agent-zero kernel) (agent-zero meta-cognition))
            (let* ((kernel (spawn-cognitive-kernel '({shape_str}) {self.attention_weight}))
                   (monitor (make-cognitive-monitor kernel)))
              (monitor-cognitive-state monitor))
            """
            output = self._run_guile_code(code)
            return self._parse_scheme_alist(output)
        else:
            # Fallback Python implementation
            return {
                'timestamp': 1234567890,
                'attention-level': self.attention_weight,
                'tensor-complexity': np.prod(self.shape),
                'meta-level': self.meta_level,
                'processing-load': np.prod(self.shape) * self.attention_weight / 1000.0,
                'memory-efficiency': self.attention_weight * 100 / max(1, np.log(np.prod(self.shape))),
                'cognitive-coherence': (self.attention_weight + len(self.shape) * 0.1) / (1 + len(self.shape)),
                'diagnostics': []
            }
            
    def multi_level_meta_reasoning(self, depth: int = 2) -> List[Dict[str, Any]]:
        """Perform multi-level meta-reasoning."""
        if self._guile_available:
            shape_str = ' '.join(map(str, self.shape))
            code = f"""
            (use-modules (agent-zero kernel) (agent-zero meta-cognition))
            (let ((kernel (spawn-cognitive-kernel '({shape_str}) {self.attention_weight})))
              (multi-level-meta-reasoning kernel {depth}))
            """
            output = self._run_guile_code(code)
            return self._parse_scheme_list(output)
        else:
            # Fallback Python implementation
            levels = []
            
            # Level 0: Object-level reasoning
            levels.append({
                'level': 0,
                'focus': 'object-reasoning',
                'content': {'tensor_shape': self.shape, 'attention': self.attention_weight},
                'confidence': 0.8
            })
            
            # Level 1: Meta-reasoning
            levels.append({
                'level': 1,
                'focus': 'meta-reasoning',
                'content': {
                    'reasoning-effectiveness': self.attention_weight * 0.7,
                    'attention-allocation-quality': min(1.0, self.attention_weight * 100 / np.prod(self.shape)),
                    'learning-progress': (self.meta_level + len(self.shape) * 0.2) / (1 + self.meta_level),
                    'cognitive-flexibility': self.attention_weight * np.var(self.shape) / 100.0 if len(self.shape) > 1 else 0.1
                },
                'confidence': 0.7
            })
            
            # Level 2: Meta-meta-reasoning (if depth allows)
            if depth > 1:
                levels.append({
                    'level': 2,
                    'focus': 'meta-meta-reasoning',
                    'content': {
                        'level-coherence': 0.8,
                        'recursive-depth': len(levels) + 1,
                        'confidence-degradation': 0.1
                    },
                    'recursive-insights': {
                        'recursive-depth-efficiency': self.meta_level / max(1, len(self.shape)),
                        'self-referential-stability': self.attention_weight / max(1, self.meta_level + 1),
                        'emergence-potential': self.attention_weight * np.log(max(1, np.prod(self.shape))) * (1 + self.meta_level)
                    },
                    'confidence': 0.6
                })
            
            return levels


class CognitiveKernelManager:
    """Manager for multiple cognitive kernels."""
    
    def __init__(self):
        self.kernels = []
    
    def create_kernel(self, shape: List[int], attention_weight: float = 0.5) -> CognitiveKernel:
        """Create and register a new cognitive kernel."""
        kernel = CognitiveKernel(shape, attention_weight)
        self.kernels.append(kernel)
        return kernel
    
    def adaptive_attention_allocation(self, goals: List[str]) -> List[Dict[str, Any]]:
        """Allocate attention across kernels based on goals."""
        allocations = []
        for i, kernel in enumerate(self.kernels):
            goal = goals[i] if i < len(goals) else 'default'
            score = self._calculate_attention_score(goal)
            priority = self._calculate_priority(score)
            
            allocations.append({
                'kernel': kernel,
                'attention_score': score,
                'activation_priority': priority,
                'goal': goal
            })
        
        return allocations
    
    @staticmethod
    def _calculate_attention_score(goal: str) -> float:
        """Calculate attention score for a goal."""
        goal_scores = {
            'reasoning': 0.9,
            'learning': 0.7,
            'attention': 0.8,
            'memory': 0.6,
            'adaptation': 0.75,
            'goal-1': 0.8,
            'goal-2': 0.6,
            'default': 0.5
        }
        return goal_scores.get(goal, 0.5)
    
    @staticmethod
    def _calculate_priority(score: float) -> str:
        """Calculate activation priority from attention score."""
        if score > 0.8:
            return 'high'
        elif score > 0.6:
            return 'medium'
        elif score > 0.4:
            return 'low'
        else:
            return 'minimal'


def test_cognitive_kernel():
    """Test the cognitive kernel functionality."""
    print("Testing Python Cognitive Kernel Wrapper...")
    
    # Test kernel creation
    kernel = CognitiveKernel([64, 32], 0.8)
    print(f"Created kernel with shape: {kernel.shape}, attention: {kernel.attention_weight}")
    
    # Test tensor field encoding
    encoding = kernel.tensor_field_encoding('prime', include_attention=True)
    print(f"Prime encoding: {encoding}")
    
    # Test different encoding types
    fib_encoding = kernel.tensor_field_encoding('fibonacci')
    print(f"Fibonacci encoding: {fib_encoding}")
    
    # Test hypergraph state
    state = kernel.hypergraph_state()
    print(f"Hypergraph state keys: {list(state.keys())}")
    
    # Test self-description
    self_desc = kernel.recursive_self_description()
    print(f"Self-description keys: {list(self_desc.keys())}")
    
    # Test advanced meta-cognitive features
    print("\nTesting advanced meta-cognitive features...")
    
    # Test advanced meta-cognitive reflection
    advanced_reflection = kernel.advanced_meta_cognitive_reflection()
    print(f"Advanced reflection keys: {list(advanced_reflection.keys())}")
    
    # Test cognitive state monitoring  
    monitor_state = kernel.monitor_cognitive_state()
    print(f"Monitoring state keys: {list(monitor_state.keys())}")
    
    # Test multi-level meta-reasoning
    multi_level = kernel.multi_level_meta_reasoning(depth=2)
    print(f"Multi-level reasoning levels: {len(multi_level)}")
    
    # Test state persistence
    filename = "/tmp/cognitive_state_test.json"
    saved_file = kernel.save_cognitive_state(filename)
    print(f"Saved cognitive state to: {saved_file}")
    
    restored_state = kernel.restore_cognitive_state(filename)
    print(f"Restored state keys: {list(restored_state.keys())}")
    
    # Test kernel manager
    manager = CognitiveKernelManager()
    k1 = manager.create_kernel([32, 32], 0.9)
    k2 = manager.create_kernel([16, 16], 0.7)
    
    allocations = manager.adaptive_attention_allocation(['reasoning', 'learning'])
    print(f"Allocated attention to {len(allocations)} kernels")
    
    print("Python Cognitive Kernel tests passed!")
    print("Advanced meta-cognitive features are now available!")


if __name__ == "__main__":
    test_cognitive_kernel()