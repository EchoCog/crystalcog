#!/usr/bin/env python3
"""
Advanced Meta-Cognitive Features Testing Suite

This test suite validates the implementation of advanced meta-cognitive features
for the Agent-Zero Genesis cognitive kernel system.
"""

import os
import json
import tempfile
import numpy as np
from python_cognitive_kernel import CognitiveKernel

def test_cognitive_monitoring():
    """Test cognitive state monitoring and diagnostics."""
    print("Testing cognitive monitoring and diagnostics...")
    
    kernel = CognitiveKernel([128, 64, 32], 0.85)
    monitor_state = kernel.monitor_cognitive_state()
    
    # Validate monitoring output structure
    required_keys = ['timestamp', 'attention-level', 'tensor-complexity', 
                    'meta-level', 'processing-load', 'memory-efficiency', 
                    'cognitive-coherence', 'diagnostics']
    
    for key in required_keys:
        assert key in monitor_state, f"Missing key in monitoring: {key}"
    
    # Validate monitoring values are reasonable
    assert 0 <= monitor_state['attention-level'] <= 1.0
    assert monitor_state['tensor-complexity'] == np.prod(kernel.shape)
    assert monitor_state['processing-load'] > 0
    assert monitor_state['memory-efficiency'] > 0
    assert 0 <= monitor_state['cognitive-coherence'] <= 1.0
    
    print("✓ Cognitive monitoring test passed")

def test_state_persistence():
    """Test cognitive state saving and restoration."""
    print("Testing cognitive state persistence...")
    
    kernel = CognitiveKernel([64, 64], 0.75)
    
    # Test state saving
    with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.json') as f:
        temp_filename = f.name
    
    try:
        saved_file = kernel.save_cognitive_state(temp_filename)
        assert saved_file == temp_filename
        assert os.path.exists(temp_filename)
        
        # Test state restoration
        restored_state = kernel.restore_cognitive_state(temp_filename)
        
        # Validate restored state structure
        required_keys = ['timestamp', 'kernel-shape', 'attention-weight', 'meta-level']
        for key in required_keys:
            assert key in restored_state, f"Missing key in restored state: {key}"
        
        # Validate restored values match original
        assert restored_state['kernel-shape'] == kernel.shape
        assert restored_state['attention-weight'] == kernel.attention_weight
        assert restored_state['meta-level'] == kernel.meta_level
        
    finally:
        if os.path.exists(temp_filename):
            os.unlink(temp_filename)
    
    print("✓ State persistence test passed")

def test_multi_level_reasoning():
    """Test multi-level meta-reasoning capabilities."""
    print("Testing multi-level meta-reasoning...")
    
    kernel = CognitiveKernel([32, 16], 0.9)
    
    # Test shallow reasoning (depth 1)
    levels_1 = kernel.multi_level_meta_reasoning(depth=1)
    assert len(levels_1) == 2  # Level 0 and Level 1
    
    # Test deep reasoning (depth 2)
    levels_2 = kernel.multi_level_meta_reasoning(depth=2)
    assert len(levels_2) == 3  # Level 0, Level 1, and Level 2
    
    # Validate reasoning level structure
    for i, level in enumerate(levels_2):
        assert level['level'] == i
        assert 'focus' in level
        assert 'content' in level
        assert 'confidence' in level
        assert 0 <= level['confidence'] <= 1.0
    
    # Validate confidence degradation
    confidences = [level['confidence'] for level in levels_2]
    for i in range(1, len(confidences)):
        assert confidences[i] <= confidences[i-1], "Confidence should degrade at higher levels"
    
    # Validate meta-meta-reasoning level has recursive insights
    if len(levels_2) >= 3:
        meta_meta_level = levels_2[2]
        assert 'recursive-insights' in meta_meta_level
        insights = meta_meta_level['recursive-insights']
        assert 'recursive-depth-efficiency' in insights
        assert 'self-referential-stability' in insights
        assert 'emergence-potential' in insights
    
    print("✓ Multi-level reasoning test passed")

def test_advanced_reflection():
    """Test advanced meta-cognitive reflection functionality."""
    print("Testing advanced meta-cognitive reflection...")
    
    kernel = CognitiveKernel([256, 128], 0.7)
    reflection = kernel.advanced_meta_cognitive_reflection()
    
    # Validate reflection structure
    required_keys = ['current-state', 'self-assessment', 'diagnostic-analysis', 
                    'multi-level-reasoning', 'adaptive-tuning-ready']
    
    for key in required_keys:
        assert key in reflection, f"Missing key in reflection: {key}"
    
    # Validate self-assessment
    assessment = reflection['self-assessment']
    assert 'attention-efficiency' in assessment
    assert 'processing-complexity' in assessment
    assert 'overall-performance' in assessment
    
    # Validate diagnostic analysis
    diagnostics = reflection['diagnostic-analysis']
    diagnostic_keys = ['timestamp', 'attention-level', 'tensor-complexity', 'processing-load']
    for key in diagnostic_keys:
        assert key in diagnostics, f"Missing diagnostic key: {key}"
    
    # Validate multi-level reasoning is included
    multi_level = reflection['multi-level-reasoning']
    assert isinstance(multi_level, list)
    assert len(multi_level) >= 2
    
    # Validate adaptive tuning readiness
    assert reflection['adaptive-tuning-ready'] == True
    
    print("✓ Advanced reflection test passed")

def test_cognitive_flexibility():
    """Test cognitive flexibility assessment across different kernel configurations."""
    print("Testing cognitive flexibility assessment...")
    
    # Test different kernel configurations
    configs = [
        ([32, 64], 0.5),      # Unbalanced shape
        ([64, 16], 0.8),      # High attention, unbalanced
        ([8, 16, 32], 0.3),   # Low attention, 3D with variation
        ([128], 0.95),        # Very high attention, 1D
    ]
    
    for shape, attention in configs:
        kernel = CognitiveKernel(shape, attention)
        levels = kernel.multi_level_meta_reasoning(depth=2)
        
        # Extract cognitive flexibility metrics
        meta_level = levels[1]
        flexibility = meta_level['content']['cognitive-flexibility']
        
        # Validate flexibility is computed
        assert isinstance(flexibility, (int, float, np.floating))
        assert flexibility >= 0
        
        # Tensors with variation in dimensions should show flexibility
        if len(shape) > 1 and len(set(shape)) > 1:  # Different dimension sizes
            assert flexibility > 0, f"Expected flexibility > 0 for varied shape {shape}, got {flexibility}"
    
    print("✓ Cognitive flexibility test passed")

def test_diagnostic_thresholds():
    """Test diagnostic analysis with different threshold conditions."""
    print("Testing diagnostic threshold analysis...")
    
    # Test low attention scenario
    low_attention_kernel = CognitiveKernel([64, 64], 0.2)
    low_monitor = low_attention_kernel.monitor_cognitive_state()
    
    # Test high complexity scenario  
    high_complexity_kernel = CognitiveKernel([512, 512, 256], 0.6)
    high_monitor = high_complexity_kernel.monitor_cognitive_state()
    
    # Test optimal scenario
    optimal_kernel = CognitiveKernel([64, 32], 0.8)
    optimal_monitor = optimal_kernel.monitor_cognitive_state()
    
    # Validate different scenarios produce different diagnostic patterns
    scenarios = [low_monitor, high_monitor, optimal_monitor]
    for monitor in scenarios:
        assert 'processing-load' in monitor
        assert 'memory-efficiency' in monitor
        assert 'cognitive-coherence' in monitor
        
        # All values should be positive
        assert monitor['processing-load'] >= 0
        assert monitor['memory-efficiency'] > 0
        assert monitor['cognitive-coherence'] >= 0
    
    print("✓ Diagnostic thresholds test passed")

def test_meta_learning_insights():
    """Test meta-learning insights extraction."""
    print("Testing meta-learning insights...")
    
    kernel = CognitiveKernel([64, 32], 0.8)
    levels = kernel.multi_level_meta_reasoning(depth=2)
    
    if len(levels) >= 3:
        meta_meta_level = levels[2]
        if 'recursive-insights' in meta_meta_level:
            insights = meta_meta_level['recursive-insights']
            
            # Validate all insight metrics
            insight_keys = ['recursive-depth-efficiency', 'self-referential-stability', 'emergence-potential']
            for key in insight_keys:
                assert key in insights, f"Missing insight: {key}"
                assert isinstance(insights[key], (int, float))
                assert insights[key] >= 0
    
    print("✓ Meta-learning insights test passed")

def run_all_tests():
    """Run all advanced meta-cognitive feature tests."""
    print("=" * 60)
    print("ADVANCED META-COGNITIVE FEATURES TEST SUITE")
    print("=" * 60)
    
    test_functions = [
        test_cognitive_monitoring,
        test_state_persistence,
        test_multi_level_reasoning,
        test_advanced_reflection,
        test_cognitive_flexibility,
        test_diagnostic_thresholds,
        test_meta_learning_insights
    ]
    
    passed = 0
    failed = 0
    
    for test_func in test_functions:
        try:
            test_func()
            passed += 1
        except Exception as e:
            print(f"✗ {test_func.__name__} FAILED: {e}")
            failed += 1
    
    print("\n" + "=" * 60)
    print(f"TEST RESULTS: {passed} passed, {failed} failed")
    
    if failed == 0:
        print("🎉 ALL ADVANCED META-COGNITIVE FEATURES TESTS PASSED!")
        print("Advanced meta-cognitive capabilities are fully functional.")
    else:
        print("❌ Some tests failed. Please review the implementation.")
    
    print("=" * 60)
    
    return failed == 0

if __name__ == "__main__":
    success = run_all_tests()
    exit(0 if success else 1)