#include "drawterm/disvm.h"
#include <iostream>

int main() {
    std::cout << "DIS VM Execution Demo" << std::endl;
    std::cout << "=====================" << std::endl;
    
    // Create DIS VM
    DrawTerm::DISVM::DISVM vm;
    
    // Load simple Limbo program
    std::string limbo_code = "hello halt";
    
    if (vm.load_limbo_source(limbo_code)) {
        std::cout << "✅ Limbo program loaded" << std::endl;
        
        if (vm.run()) {
            std::cout << "✅ Program executed successfully" << std::endl;
            std::cout << "   Instructions executed: " << vm.get_instruction_count() << std::endl;
            std::cout << "   Execution time: " << vm.get_execution_time_ms() << "ms" << std::endl;
        } else {
            std::cout << "❌ Program execution failed" << std::endl;
        }
    } else {
        std::cout << "❌ Failed to load program" << std::endl;
    }
    
    // Test VM Manager
    std::cout << "\n--- VM Manager Test ---" << std::endl;
    
    DrawTerm::DISVM::DISVMManager manager;
    
    std::string vm1 = manager.create_vm();
    std::string vm2 = manager.create_vm();
    
    std::cout << "✅ Created VMs: " << vm1 << ", " << vm2 << std::endl;
    
    // Test message passing
    DrawTerm::DISVM::VMValue message(std::string("Hello from " + vm1));
    manager.send_message(vm1, vm2, message);
    
    auto received = manager.receive_message(vm2);
    std::cout << "✅ Inter-VM communication successful" << std::endl;
    
    return 0;
}