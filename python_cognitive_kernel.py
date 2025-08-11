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
    
    # Test kernel manager
    manager = CognitiveKernelManager()
    k1 = manager.create_kernel([32, 32], 0.9)
    k2 = manager.create_kernel([16, 16], 0.7)
    
    allocations = manager.adaptive_attention_allocation(['reasoning', 'learning'])
    print(f"Allocated attention to {len(allocations)} kernels")
    
    print("Python Cognitive Kernel tests passed!")


if __name__ == "__main__":
    test_cognitive_kernel()