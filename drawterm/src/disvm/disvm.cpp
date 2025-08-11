#include "drawterm/disvm.h"
#include <iostream>
#include <stdexcept>
#include <chrono>

namespace DrawTerm {
namespace DISVM {

// DISVM implementation
DISVM::DISVM() {
    start_time_ = std::chrono::steady_clock::now();
}

DISVM::~DISVM() {
    reset();
}

bool DISVM::load_program(const Program& program) {
    std::lock_guard<std::mutex> lock(state_mutex_);
    program_ = program;
    program_counter_ = 0;
    halted_ = false;
    return true;
}

bool DISVM::load_limbo_source(const std::string& source_code) {
    LimboCompiler compiler;
    Program program = compiler.compile(source_code);
    
    if (compiler.has_errors()) {
        std::cerr << "Compilation errors:" << std::endl;
        for (const auto& error : compiler.get_errors()) {
            std::cerr << "  " << error << std::endl;
        }
        return false;
    }
    
    return load_program(program);
}

bool DISVM::load_limbo_file(const std::string& filename) {
    LimboCompiler compiler;
    Program program = compiler.compile_file(filename);
    
    if (compiler.has_errors()) {
        std::cerr << "Compilation errors:" << std::endl;
        for (const auto& error : compiler.get_errors()) {
            std::cerr << "  " << error << std::endl;
        }
        return false;
    }
    
    return load_program(program);
}

bool DISVM::run() {
    std::lock_guard<std::mutex> lock(state_mutex_);
    
    if (program_.get_instructions().empty()) {
        std::cerr << "No program loaded" << std::endl;
        return false;
    }
    
    running_ = true;
    halted_ = false;
    start_time_ = std::chrono::steady_clock::now();
    
    const auto& instructions = program_.get_instructions();
    
    while (running_ && !halted_ && program_counter_ < instructions.size()) {
        if (!execute_instruction(instructions[program_counter_])) {
            std::cerr << "Execution error at instruction " << program_counter_ << std::endl;
            break;
        }
        
        if (instructions[program_counter_].opcode != Opcode::JMP &&
            instructions[program_counter_].opcode != Opcode::JEQ &&
            instructions[program_counter_].opcode != Opcode::JNE &&
            instructions[program_counter_].opcode != Opcode::JLT &&
            instructions[program_counter_].opcode != Opcode::JLE &&
            instructions[program_counter_].opcode != Opcode::JGT &&
            instructions[program_counter_].opcode != Opcode::JGE &&
            instructions[program_counter_].opcode != Opcode::CALL &&
            instructions[program_counter_].opcode != Opcode::RET) {
            program_counter_++;
        }
        
        instruction_count_++;
    }
    
    running_ = false;
    return !halted_ || program_counter_ >= instructions.size();
}

bool DISVM::step() {
    std::lock_guard<std::mutex> lock(state_mutex_);
    
    if (program_.get_instructions().empty() || halted_ || 
        program_counter_ >= program_.get_instructions().size()) {
        return false;
    }
    
    const auto& instructions = program_.get_instructions();
    bool result = execute_instruction(instructions[program_counter_]);
    
    if (instructions[program_counter_].opcode != Opcode::JMP &&
        instructions[program_counter_].opcode != Opcode::JEQ &&
        instructions[program_counter_].opcode != Opcode::JNE &&
        instructions[program_counter_].opcode != Opcode::JLT &&
        instructions[program_counter_].opcode != Opcode::JLE &&
        instructions[program_counter_].opcode != Opcode::JGT &&
        instructions[program_counter_].opcode != Opcode::JGE &&
        instructions[program_counter_].opcode != Opcode::CALL &&
        instructions[program_counter_].opcode != Opcode::RET) {
        program_counter_++;
    }
    
    instruction_count_++;
    return result;
}

void DISVM::pause() {
    std::lock_guard<std::mutex> lock(state_mutex_);
    running_ = false;
}

void DISVM::reset() {
    std::lock_guard<std::mutex> lock(state_mutex_);
    running_ = false;
    halted_ = false;
    program_counter_ = 0;
    instruction_count_ = 0;
    registers_.clear();
    stack_.clear();
    memory_.clear();
    channels_.clear();
    start_time_ = std::chrono::steady_clock::now();
}

void DISVM::set_register(const std::string& name, const VMValue& value) {
    std::lock_guard<std::mutex> lock(state_mutex_);
    registers_[name] = value;
}

VMValue DISVM::get_register(const std::string& name) const {
    std::lock_guard<std::mutex> lock(state_mutex_);
    auto it = registers_.find(name);
    return it != registers_.end() ? it->second : VMValue(int64_t(0));
}

void DISVM::push_stack(const VMValue& value) {
    std::lock_guard<std::mutex> lock(state_mutex_);
    stack_.push_back(value);
}

VMValue DISVM::pop_stack() {
    std::lock_guard<std::mutex> lock(state_mutex_);
    if (stack_.empty()) {
        return VMValue(int64_t(0));
    }
    VMValue value = stack_.back();
    stack_.pop_back();
    return value;
}

void DISVM::set_memory(uint64_t address, const VMValue& value) {
    std::lock_guard<std::mutex> lock(state_mutex_);
    memory_[address] = value;
}

VMValue DISVM::get_memory(uint64_t address) const {
    std::lock_guard<std::mutex> lock(state_mutex_);
    auto it = memory_.find(address);
    return it != memory_.end() ? it->second : VMValue(int64_t(0));
}

void DISVM::create_channel(const std::string& name) {
    std::lock_guard<std::mutex> lock(state_mutex_);
    channels_[name] = std::vector<VMValue>();
}

void DISVM::send_channel(const std::string& name, const VMValue& value) {
    std::lock_guard<std::mutex> lock(state_mutex_);
    auto it = channels_.find(name);
    if (it != channels_.end()) {
        it->second.push_back(value);
    }
}

VMValue DISVM::receive_channel(const std::string& name) {
    std::lock_guard<std::mutex> lock(state_mutex_);
    auto it = channels_.find(name);
    if (it != channels_.end() && !it->second.empty()) {
        VMValue value = it->second.front();
        it->second.erase(it->second.begin());
        return value;
    }
    return VMValue(int64_t(0));
}

void DISVM::set_ai_model(const std::string& model_name) {
    std::lock_guard<std::mutex> lock(state_mutex_);
    ai_model_name_ = model_name;
}

VMValue DISVM::ai_inference(const std::string& input) {
    std::lock_guard<std::mutex> lock(state_mutex_);
    // Placeholder for AI inference - would integrate with AI manager
    return VMValue(std::string("AI response to: " + input));
}

uint64_t DISVM::get_execution_time_ms() const {
    auto now = std::chrono::steady_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(now - start_time_);
    return duration.count();
}

bool DISVM::execute_instruction(const Instruction& instruction) {
    switch (instruction.opcode) {
        case Opcode::ADDI:
        case Opcode::SUBI:
        case Opcode::MULI:
        case Opcode::DIVI:
        case Opcode::MODI:
        case Opcode::ADDF:
        case Opcode::SUBF:
        case Opcode::MULF:
        case Opcode::DIVF:
            return execute_arithmetic(instruction.opcode, instruction.operands);
            
        case Opcode::JMP:
        case Opcode::JEQ:
        case Opcode::JNE:
        case Opcode::JLT:
        case Opcode::JLE:
        case Opcode::JGT:
        case Opcode::JGE:
        case Opcode::CALL:
        case Opcode::RET:
            return execute_control_flow(instruction.opcode, instruction.operands);
            
        case Opcode::LOAD:
        case Opcode::STORE:
        case Opcode::PUSH:
        case Opcode::POP:
            return execute_memory(instruction.opcode, instruction.operands);
            
        case Opcode::CATS:
        case Opcode::LENS:
        case Opcode::SUBS:
        case Opcode::CMPS:
            return execute_string(instruction.opcode, instruction.operands);
            
        case Opcode::READ:
        case Opcode::WRITE:
        case Opcode::SPAWN:
            return execute_io(instruction.opcode, instruction.operands);
            
        case Opcode::RENDER_GLYPH:
        case Opcode::SPAWN_VM:
        case Opcode::MOUNT_NS:
        case Opcode::AI_INFER:
        case Opcode::AI_TRAIN:
            return execute_drawkern(instruction.opcode, instruction.operands);
            
        case Opcode::HALT:
            halted_ = true;
            return true;
            
        default:
            std::cerr << "Unknown opcode: " << static_cast<int>(instruction.opcode) << std::endl;
            return false;
    }
}

bool DISVM::execute_arithmetic(Opcode opcode, const std::vector<VMValue>& operands) {
    if (operands.size() < 2) return false;
    
    // Simplified arithmetic operations
    switch (opcode) {
        case Opcode::ADDI: {
            int64_t a = to_int(operands[0]);
            int64_t b = to_int(operands[1]);
            push_stack(VMValue(a + b));
            break;
        }
        case Opcode::SUBI: {
            int64_t a = to_int(operands[0]);
            int64_t b = to_int(operands[1]);
            push_stack(VMValue(a - b));
            break;
        }
        // Add more arithmetic operations as needed
        default:
            return false;
    }
    
    return true;
}

bool DISVM::execute_control_flow(Opcode opcode, const std::vector<VMValue>& operands) {
    switch (opcode) {
        case Opcode::JMP:
            if (!operands.empty()) {
                program_counter_ = to_int(operands[0]);
            }
            break;
        case Opcode::HALT:
            halted_ = true;
            break;
        default:
            return false;
    }
    return true;
}

bool DISVM::execute_memory(Opcode opcode, const std::vector<VMValue>& operands) {
    switch (opcode) {
        case Opcode::PUSH:
            if (!operands.empty()) {
                push_stack(operands[0]);
            }
            break;
        case Opcode::POP:
            pop_stack();
            break;
        default:
            return false;
    }
    return true;
}

bool DISVM::execute_string(Opcode opcode, const std::vector<VMValue>& operands) {
    // String operations implementation
    return true;
}

bool DISVM::execute_io(Opcode opcode, const std::vector<VMValue>& operands) {
    // I/O operations implementation
    return true;
}

bool DISVM::execute_drawkern(Opcode opcode, const std::vector<VMValue>& operands) {
    switch (opcode) {
        case Opcode::AI_INFER:
            if (!operands.empty()) {
                std::string input = to_string(operands[0]);
                VMValue result = ai_inference(input);
                push_stack(result);
            }
            break;
        default:
            std::cout << "DrawKern operation: " << static_cast<int>(opcode) << std::endl;
            break;
    }
    return true;
}

int64_t DISVM::to_int(const VMValue& value) const {
    if (std::holds_alternative<int64_t>(value)) {
        return std::get<int64_t>(value);
    } else if (std::holds_alternative<double>(value)) {
        return static_cast<int64_t>(std::get<double>(value));
    } else if (std::holds_alternative<std::string>(value)) {
        try {
            return std::stoll(std::get<std::string>(value));
        } catch (...) {
            return 0;
        }
    }
    return 0;
}

double DISVM::to_float(const VMValue& value) const {
    if (std::holds_alternative<double>(value)) {
        return std::get<double>(value);
    } else if (std::holds_alternative<int64_t>(value)) {
        return static_cast<double>(std::get<int64_t>(value));
    } else if (std::holds_alternative<std::string>(value)) {
        try {
            return std::stod(std::get<std::string>(value));
        } catch (...) {
            return 0.0;
        }
    }
    return 0.0;
}

std::string DISVM::to_string(const VMValue& value) const {
    if (std::holds_alternative<std::string>(value)) {
        return std::get<std::string>(value);
    } else if (std::holds_alternative<int64_t>(value)) {
        return std::to_string(std::get<int64_t>(value));
    } else if (std::holds_alternative<double>(value)) {
        return std::to_string(std::get<double>(value));
    }
    return "";
}

} // namespace DISVM
} // namespace DrawTerm