#include "drawterm/yacc_grammar.h"
#include "drawterm/drawterm_integration.h"
#include <iostream>

int main() {
    std::cout << "Yacc Grammar Parsing Demo" << std::endl;
    std::cout << "=========================" << std::endl;
    
    DrawTerm::Yacc::YaccGrammarSystem grammar;
    
    // Test AI workbench parsing
    std::string glyph = DrawTerm::Examples::get_ai_workbench_example();
    
    std::cout << "Parsing glyph description:" << std::endl;
    std::cout << glyph << std::endl;
    
    auto ast = grammar.parse(glyph);
    if (ast) {
        std::cout << "✅ Glyph parsed successfully" << std::endl;
        
        // Test code generation
        std::string yaml_output = grammar.generate_code(ast, DrawTerm::Yacc::CodeGenerator::OutputFormat::YAML);
        std::cout << "✅ YAML generation successful" << std::endl;
        
        std::string json_output = grammar.generate_code(ast, DrawTerm::Yacc::CodeGenerator::OutputFormat::JSON);
        std::cout << "✅ JSON generation successful" << std::endl;
        
    } else {
        std::cout << "❌ Failed to parse glyph" << std::endl;
    }
    
    // Test validation
    if (grammar.validate_glyph(glyph)) {
        std::cout << "✅ Glyph validation passed" << std::endl;
    } else {
        std::cout << "❌ Glyph validation failed" << std::endl;
    }
    
    // Test templates
    std::cout << "\n--- Template System Test ---" << std::endl;
    
    std::map<std::string, std::string> params;
    params["model_name"] = "test-model";
    params["port"] = "8080";
    
    std::string template_result = grammar.create_from_template("ai_workbench", params);
    if (!template_result.empty()) {
        std::cout << "✅ Template instantiation successful" << std::endl;
    } else {
        std::cout << "❌ Template instantiation failed" << std::endl;
    }
    
    return 0;
}