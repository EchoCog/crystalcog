#pragma once

#include <string>
#include <vector>
#include <map>
#include <memory>
#include <functional>
#include <variant>
#include <thread>
#include <mutex>
#include <chrono>

namespace DrawTerm {
namespace DISVM {

/**
 * @brief DIS Virtual Machine opcodes for Limbo execution
 */
enum class Opcode : uint8_t {
    // Arithmetic operations
    ADDI = 0x01,    // Add integer
    SUBI = 0x02,    // Subtract integer
    MULI = 0x03,    // Multiply integer  
    DIVI = 0x04,    // Divide integer
    MODI = 0x05,    // Modulo integer
    ADDF = 0x06,    // Add float
    SUBF = 0x07,    // Subtract float
    MULF = 0x08,    // Multiply float
    DIVF = 0x09,    // Divide float
    
    // Control flow
    JMP = 0x10,     // Unconditional jump
    JEQ = 0x11,     // Jump if equal
    JNE = 0x12,     // Jump if not equal
    JLT = 0x13,     // Jump if less than
    JLE = 0x14,     // Jump if less than or equal
    JGT = 0x15,     // Jump if greater than
    JGE = 0x16,     // Jump if greater than or equal
    CALL = 0x17,    // Function call
    RET = 0x18,     // Return from function
    
    // Memory operations
    LOAD = 0x20,    // Load from memory
    STORE = 0x21,   // Store to memory
    PUSH = 0x22,    // Push to stack
    POP = 0x23,     // Pop from stack
    
    // String operations
    CATS = 0x30,    // Concatenate strings
    LENS = 0x31,    // String length
    SUBS = 0x32,    // Substring
    CMPS = 0x33,    // Compare strings
    
    // I/O operations
    READ = 0x40,    // Read from channel
    WRITE = 0x41,   // Write to channel
    SPAWN = 0x42,   // Spawn new thread
    
    // DrawKern specific operations
    RENDER_GLYPH = 0x50,  // Render a glyph description
    SPAWN_VM = 0x51,      // Spawn new VM instance
    MOUNT_NS = 0x52,      // Mount namespace
    AI_INFER = 0x53,      // AI model inference
    AI_TRAIN = 0x54,      // AI model training
    
    // System operations
    HALT = 0xFF     // Halt execution
};

/**
 * @brief DIS VM value types
 */
using VMValue = std::variant<
    int64_t,        // Integer
    double,         // Float
    std::string,    // String
    std::vector<uint8_t>  // Binary data
>;

/**
 * @brief DIS VM instruction
 */
struct Instruction {
    Opcode opcode;
    std::vector<VMValue> operands;
    
    // Serialization
    std::vector<uint8_t> serialize() const;
    static Instruction deserialize(const std::vector<uint8_t>& data, size_t& offset);
};

/**
 * @brief DIS VM bytecode program
 */
class Program {
public:
    Program() = default;
    Program(const std::vector<Instruction>& instructions);
    
    void add_instruction(const Instruction& instruction);
    const std::vector<Instruction>& get_instructions() const { return instructions_; }
    
    // Serialization
    std::vector<uint8_t> serialize() const;
    static Program deserialize(const std::vector<uint8_t>& data);

private:
    std::vector<Instruction> instructions_;
};

/**
 * @brief Limbo source code compiler to DIS bytecode
 */
class LimboCompiler {
public:
    LimboCompiler();
    
    // Compilation
    Program compile(const std::string& source_code);
    Program compile_file(const std::string& filename);
    
    // Error handling
    bool has_errors() const { return !errors_.empty(); }
    const std::vector<std::string>& get_errors() const { return errors_; }
    
private:
    std::vector<std::string> errors_;
    
    // Compilation phases
    std::vector<std::string> tokenize(const std::string& source);
    Program parse_and_generate(const std::vector<std::string>& tokens);
    
    void add_error(const std::string& error) { errors_.push_back(error); }
};

/**
 * @brief DIS Virtual Machine execution engine
 */
class DISVM {
public:
    DISVM();
    ~DISVM();
    
    // Program loading
    bool load_program(const Program& program);
    bool load_limbo_source(const std::string& source_code);
    bool load_limbo_file(const std::string& filename);
    
    // Execution control
    bool run();
    bool step();
    void pause();
    void reset();
    bool is_running() const { return running_; }
    bool is_halted() const { return halted_; }
    
    // State management
    void set_register(const std::string& name, const VMValue& value);
    VMValue get_register(const std::string& name) const;
    
    void push_stack(const VMValue& value);
    VMValue pop_stack();
    
    // Memory management
    void set_memory(uint64_t address, const VMValue& value);
    VMValue get_memory(uint64_t address) const;
    
    // Channel operations (for concurrent programming)
    void create_channel(const std::string& name);
    void send_channel(const std::string& name, const VMValue& value);
    VMValue receive_channel(const std::string& name);
    
    // AI integration
    void set_ai_model(const std::string& model_name);
    VMValue ai_inference(const std::string& input);
    
    // Performance monitoring
    uint64_t get_instruction_count() const { return instruction_count_; }
    uint64_t get_execution_time_ms() const;

private:
    Program program_;
    size_t program_counter_ = 0;
    bool running_ = false;
    bool halted_ = false;
    
    // VM state
    std::map<std::string, VMValue> registers_;
    std::vector<VMValue> stack_;
    std::map<uint64_t, VMValue> memory_;
    std::map<std::string, std::vector<VMValue>> channels_;
    
    // AI integration
    std::string ai_model_name_;
    
    // Performance counters
    uint64_t instruction_count_ = 0;
    std::chrono::steady_clock::time_point start_time_;
    
    // Execution methods
    bool execute_instruction(const Instruction& instruction);
    bool execute_arithmetic(Opcode opcode, const std::vector<VMValue>& operands);
    bool execute_control_flow(Opcode opcode, const std::vector<VMValue>& operands);
    bool execute_memory(Opcode opcode, const std::vector<VMValue>& operands);
    bool execute_string(Opcode opcode, const std::vector<VMValue>& operands);
    bool execute_io(Opcode opcode, const std::vector<VMValue>& operands);
    bool execute_drawkern(Opcode opcode, const std::vector<VMValue>& operands);
    
    // Helper methods
    int64_t to_int(const VMValue& value) const;
    double to_float(const VMValue& value) const;
    std::string to_string(const VMValue& value) const;
    
    mutable std::mutex state_mutex_;
};

/**
 * @brief Manager for coordinating multiple DIS VM instances
 */
class DISVMManager {
public:
    DISVMManager();
    ~DISVMManager();
    
    // VM lifecycle
    std::string create_vm();
    bool destroy_vm(const std::string& vm_id);
    DISVM* get_vm(const std::string& vm_id);
    
    // VM operations
    bool load_program(const std::string& vm_id, const Program& program);
    bool start_vm(const std::string& vm_id);
    bool stop_vm(const std::string& vm_id);
    
    // Inter-VM communication
    bool send_message(const std::string& from_vm, const std::string& to_vm, 
                     const VMValue& message);
    VMValue receive_message(const std::string& vm_id);
    
    // Global operations
    void start_all();
    void stop_all();
    void reset_all();
    
    // Statistics
    std::vector<std::string> list_vms() const;
    size_t vm_count() const { return vms_.size(); }

private:
    std::map<std::string, std::unique_ptr<DISVM>> vms_;
    std::map<std::string, std::vector<VMValue>> message_queues_;
    mutable std::mutex manager_mutex_;
    
    std::string generate_vm_id();
    uint64_t vm_counter_ = 0;
};

} // namespace DISVM
} // namespace DrawTerm