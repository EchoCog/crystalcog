// Placeholder implementations for remaining DISVM files
#include "drawterm/disvm.h"

namespace DrawTerm {
namespace DISVM {

// Instruction serialization/deserialization
std::vector<uint8_t> Instruction::serialize() const {
    std::vector<uint8_t> data;
    data.push_back(static_cast<uint8_t>(opcode));
    // Add operand serialization logic here
    return data;
}

Instruction Instruction::deserialize(const std::vector<uint8_t>& data, size_t& offset) {
    Instruction instr;
    if (offset < data.size()) {
        instr.opcode = static_cast<Opcode>(data[offset++]);
    }
    return instr;
}

} // namespace DISVM
} // namespace DrawTerm