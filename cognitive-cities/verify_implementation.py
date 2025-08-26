#!/usr/bin/env python3
"""
Verification script for Cognitive Cities Architecture implementation.
"""

import sys
import os

sys.path.insert(0, os.path.dirname(__file__))

def test_basic_imports():
    """Test basic imports of core components."""
    print("Testing basic imports...")
    
    try:
        from cognitive_core.shared_libraries.atomspace_manager import CognitiveAtomSpaceManager
        print("✓ AtomSpace manager import successful")
    except Exception as e:
        print(f"✗ AtomSpace manager import failed: {e}")
        return False

    try:
        from cognitive_core.shared_libraries.membrane_controller import MembraneController
        print("✓ Membrane controller import successful")
    except Exception as e:
        print(f"✗ Membrane controller import failed: {e}")
        return False

    try:
        from integration_hub.event_bus.event_bus import CognitiveEventBus
        print("✓ Event bus import successful")
    except Exception as e:
        print(f"✗ Event bus import failed: {e}")
        return False

    return True

def test_system5_mapping():
    """Test System-5 CNS component mapping."""
    print("\nTesting System-5 CNS mapping...")
    
    try:
        from cognitive_core.shared_libraries.atomspace_manager import CognitiveAtomSpaceManager
        manager = CognitiveAtomSpaceManager()
        
        mapping = manager.store_system5_mapping(7, "T", "thought_service", "cerebral_triad")
        print("✓ System-5 component mapping successful")
        return True
    except Exception as e:
        print(f"✗ System-5 component mapping failed: {e}")
        return False

def test_triad_services():
    """Test triad service instantiation."""
    print("\nTesting triad services...")
    
    try:
        sys.path.insert(0, os.path.dirname(__file__))
        
        from cerebral_triad.thought_service.thought_service import ThoughtService
        thought_service = ThoughtService()
        print("✓ Thought service instantiation successful")
        
        from somatic_triad.motor_control_service.motor_control import MotorControlService
        motor_service = MotorControlService()
        print("✓ Motor control service instantiation successful")
        
        from autonomic_triad.monitoring_service.monitoring_service import MonitoringService
        monitoring_service = MonitoringService()
        print("✓ Monitoring service instantiation successful")
        
        return True
    except Exception as e:
        print(f"✗ Triad service instantiation failed: {e}")
        return False

def main():
    """Run all verification tests."""
    print("Cognitive Cities Architecture Verification")
    print("=" * 50)
    
    tests = [
        test_basic_imports,
        test_system5_mapping,
        test_triad_services
    ]
    
    passed = 0
    total = len(tests)
    
    for test in tests:
        if test():
            passed += 1
    
    print(f"\nVerification Results: {passed}/{total} tests passed")
    
    if passed == total:
        print("✓ All verification tests passed!")
        return 0
    else:
        print("✗ Some verification tests failed")
        return 1

if __name__ == "__main__":
    sys.exit(main())
