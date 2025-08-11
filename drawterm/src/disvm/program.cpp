#include "drawterm/disvm.h"

namespace DrawTerm {
namespace DISVM {

// Program implementation
Program::Program(const std::vector<Instruction>& instructions) 
    : instructions_(instructions) {
}

void Program::add_instruction(const Instruction& instruction) {
    instructions_.push_back(instruction);
}

std::vector<uint8_t> Program::serialize() const {
    std::vector<uint8_t> data;
    
    // Add instruction count
    uint32_t count = instructions_.size();
    data.push_back((count >> 0) & 0xFF);
    data.push_back((count >> 8) & 0xFF);
    data.push_back((count >> 16) & 0xFF);
    data.push_back((count >> 24) & 0xFF);
    
    // Add instructions
    for (const auto& instr : instructions_) {
        auto instr_data = instr.serialize();
        data.insert(data.end(), instr_data.begin(), instr_data.end());
    }
    
    return data;
}

Program Program::deserialize(const std::vector<uint8_t>& data) {
    Program program;
    
    if (data.size() < 4) return program;
    
    // Read instruction count
    uint32_t count = (static_cast<uint32_t>(data[0]) << 0) |
                     (static_cast<uint32_t>(data[1]) << 8) |
                     (static_cast<uint32_t>(data[2]) << 16) |
                     (static_cast<uint32_t>(data[3]) << 24);
    
    size_t offset = 4;
    for (uint32_t i = 0; i < count && offset < data.size(); ++i) {
        Instruction instr = Instruction::deserialize(data, offset);
        program.add_instruction(instr);
    }
    
    return program;
}

} // namespace DISVM
} // namespace DrawTerm