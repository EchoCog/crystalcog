#include "drawterm/disvm.h"
#include <sstream>

namespace DrawTerm {
namespace DISVM {

// LimboCompiler implementation
LimboCompiler::LimboCompiler() {
}

Program LimboCompiler::compile(const std::string& source_code) {
    errors_.clear();
    
    // Simplified compilation - tokenize and parse
    auto tokens = tokenize(source_code);
    return parse_and_generate(tokens);
}

Program LimboCompiler::compile_file(const std::string& filename) {
    errors_.clear();
    
    // For now, just create a simple test program
    Program program;
    
    // Add a simple hello world program
    Instruction hello;
    hello.opcode = Opcode::PUSH;
    hello.operands.push_back(VMValue(std::string("Hello, DrawTerm!")));
    program.add_instruction(hello);
    
    Instruction halt;
    halt.opcode = Opcode::HALT;
    program.add_instruction(halt);
    
    return program;
}

std::vector<std::string> LimboCompiler::tokenize(const std::string& source) {
    std::vector<std::string> tokens;
    std::istringstream iss(source);
    std::string token;
    
    while (iss >> token) {
        tokens.push_back(token);
    }
    
    return tokens;
}

Program LimboCompiler::parse_and_generate(const std::vector<std::string>& tokens) {
    Program program;
    
    // Simplified parsing - just create basic instructions
    for (const auto& token : tokens) {
        if (token == "hello") {
            Instruction instr;
            instr.opcode = Opcode::PUSH;
            instr.operands.push_back(VMValue(std::string("Hello from Limbo!")));
            program.add_instruction(instr);
        } else if (token == "halt") {
            Instruction instr;
            instr.opcode = Opcode::HALT;
            program.add_instruction(instr);
        }
    }
    
    // If no instructions were added, add a default halt
    if (program.get_instructions().empty()) {
        Instruction halt;
        halt.opcode = Opcode::HALT;
        program.add_instruction(halt);
    }
    
    return program;
}

} // namespace DISVM
} // namespace DrawTerm