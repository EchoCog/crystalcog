#include "drawterm/disvm.h"
#include <random>

namespace DrawTerm {
namespace DISVM {

// DISVMManager implementation
DISVMManager::DISVMManager() {
}

DISVMManager::~DISVMManager() {
    stop_all();
}

std::string DISVMManager::create_vm() {
    std::lock_guard<std::mutex> lock(manager_mutex_);
    
    std::string vm_id = generate_vm_id();
    auto vm = std::make_unique<DISVM>();
    vms_[vm_id] = std::move(vm);
    message_queues_[vm_id] = std::vector<VMValue>();
    
    return vm_id;
}

bool DISVMManager::destroy_vm(const std::string& vm_id) {
    std::lock_guard<std::mutex> lock(manager_mutex_);
    
    auto it = vms_.find(vm_id);
    if (it != vms_.end()) {
        it->second->reset();
        vms_.erase(it);
        message_queues_.erase(vm_id);
        return true;
    }
    
    return false;
}

DISVM* DISVMManager::get_vm(const std::string& vm_id) {
    std::lock_guard<std::mutex> lock(manager_mutex_);
    
    auto it = vms_.find(vm_id);
    return it != vms_.end() ? it->second.get() : nullptr;
}

bool DISVMManager::load_program(const std::string& vm_id, const Program& program) {
    DISVM* vm = get_vm(vm_id);
    return vm ? vm->load_program(program) : false;
}

bool DISVMManager::start_vm(const std::string& vm_id) {
    DISVM* vm = get_vm(vm_id);
    return vm ? vm->run() : false;
}

bool DISVMManager::stop_vm(const std::string& vm_id) {
    DISVM* vm = get_vm(vm_id);
    if (vm) {
        vm->pause();
        return true;
    }
    return false;
}

bool DISVMManager::send_message(const std::string& from_vm, const std::string& to_vm, 
                               const VMValue& message) {
    std::lock_guard<std::mutex> lock(manager_mutex_);
    
    auto it = message_queues_.find(to_vm);
    if (it != message_queues_.end()) {
        it->second.push_back(message);
        return true;
    }
    
    return false;
}

VMValue DISVMManager::receive_message(const std::string& vm_id) {
    std::lock_guard<std::mutex> lock(manager_mutex_);
    
    auto it = message_queues_.find(vm_id);
    if (it != message_queues_.end() && !it->second.empty()) {
        VMValue message = it->second.front();
        it->second.erase(it->second.begin());
        return message;
    }
    
    return VMValue(int64_t(0));
}

void DISVMManager::start_all() {
    std::lock_guard<std::mutex> lock(manager_mutex_);
    
    for (auto& [vm_id, vm] : vms_) {
        // Start each VM in a separate thread
        std::thread([vm = vm.get()]() {
            vm->run();
        }).detach();
    }
}

void DISVMManager::stop_all() {
    std::lock_guard<std::mutex> lock(manager_mutex_);
    
    for (auto& [vm_id, vm] : vms_) {
        vm->pause();
    }
}

void DISVMManager::reset_all() {
    std::lock_guard<std::mutex> lock(manager_mutex_);
    
    for (auto& [vm_id, vm] : vms_) {
        vm->reset();
    }
}

std::vector<std::string> DISVMManager::list_vms() const {
    std::lock_guard<std::mutex> lock(manager_mutex_);
    
    std::vector<std::string> vm_ids;
    for (const auto& [vm_id, vm] : vms_) {
        vm_ids.push_back(vm_id);
    }
    
    return vm_ids;
}

std::string DISVMManager::generate_vm_id() {
    return "vm_" + std::to_string(++vm_counter_);
}

} // namespace DISVM
} // namespace DrawTerm