#pragma once

#include <string>
#include <vector>
#include <map>
#include <memory>
#include <variant>
#include <iostream>

namespace DrawTerm {
namespace Yacc {

/**
 * @brief Token types for the VM glyph description language
 */
enum class TokenType {
    // Literals
    IDENTIFIER,
    INTEGER,
    FLOAT,
    STRING,
    
    // Keywords
    VM,
    GLYPH,
    WORKBENCH,
    AI_MODEL,
    NAMESPACE,
    MOUNT,
    RENDER,
    
    // Operators
    ASSIGN,      // =
    ARROW,       // ->
    DOT,         // .
    COLON,       // :
    SEMICOLON,   // ;
    COMMA,       // ,
    
    // Delimiters
    LBRACE,      // {
    RBRACE,      // }
    LPAREN,      // (
    RPAREN,      // )
    LBRACKET,    // [
    RBRACKET,    // ]
    
    // Special
    NEWLINE,
    END_OF_FILE,
    INVALID
};

/**
 * @brief Lexical token
 */
struct Token {
    TokenType type;
    std::string value;
    size_t line;
    size_t column;
    
    Token(TokenType t, const std::string& v, size_t l = 0, size_t c = 0)
        : type(t), value(v), line(l), column(c) {}
};

/**
 * @brief AST node types
 */
enum class ASTNodeType {
    PROGRAM,
    VM_DECLARATION,
    GLYPH_DEFINITION,
    WORKBENCH_SPEC,
    AI_MODEL_SPEC,
    NAMESPACE_SPEC,
    MOUNT_DIRECTIVE,
    RENDER_DIRECTIVE,
    IDENTIFIER_NODE,
    LITERAL_NODE,
    ASSIGNMENT_NODE,
    BLOCK_NODE
};

/**
 * @brief Abstract Syntax Tree node
 */
class ASTNode {
public:
    ASTNodeType type;
    std::string value;
    std::vector<std::shared_ptr<ASTNode>> children;
    std::map<std::string, std::string> attributes;
    
    ASTNode(ASTNodeType t, const std::string& v = "") : type(t), value(v) {}
    
    void add_child(std::shared_ptr<ASTNode> child) { children.push_back(child); }
    void set_attribute(const std::string& key, const std::string& value) { 
        attributes[key] = value; 
    }
    
    std::string get_attribute(const std::string& key) const {
        auto it = attributes.find(key);
        return it != attributes.end() ? it->second : "";
    }
};

/**
 * @brief VM topology description structures
 */
struct VMDeclaration {
    std::string name;
    std::string type;
    std::map<std::string, std::string> properties;
    std::vector<std::string> dependencies;
};

struct GlyphDefinition {
    std::string name;
    std::string description;
    std::vector<VMDeclaration> vms;
    std::map<std::string, std::string> configuration;
};

struct WorkbenchSpecification {
    std::string name;
    std::string ai_model;
    std::vector<std::string> tools;
    std::map<std::string, std::string> environment;
};

/**
 * @brief Lexical analyzer for VM glyph description language
 */
class Lexer {
public:
    Lexer(const std::string& input);
    
    std::vector<Token> tokenize();
    bool has_errors() const { return !errors_.empty(); }
    const std::vector<std::string>& get_errors() const { return errors_; }

private:
    std::string input_;
    size_t position_ = 0;
    size_t line_ = 1;
    size_t column_ = 1;
    std::vector<std::string> errors_;
    
    static std::map<std::string, TokenType> keywords_;
    
    char current_char();
    char peek_char(size_t offset = 1);
    void advance();
    void skip_whitespace();
    void skip_comment();
    
    Token read_identifier();
    Token read_number();
    Token read_string();
    TokenType get_keyword_type(const std::string& identifier);
    
    void add_error(const std::string& message);
};

/**
 * @brief Parser for VM glyph description language
 */
class Parser {
public:
    Parser(const std::vector<Token>& tokens);
    
    std::shared_ptr<ASTNode> parse();
    bool has_errors() const { return !errors_.empty(); }
    const std::vector<std::string>& get_errors() const { return errors_; }

private:
    std::vector<Token> tokens_;
    size_t position_ = 0;
    std::vector<std::string> errors_;
    
    Token current_token();
    Token peek_token(size_t offset = 1);
    void advance();
    bool expect(TokenType type);
    bool match(TokenType type);
    
    // Parsing methods
    std::shared_ptr<ASTNode> parse_program();
    std::shared_ptr<ASTNode> parse_declaration();
    std::shared_ptr<ASTNode> parse_vm_declaration();
    std::shared_ptr<ASTNode> parse_glyph_definition();
    std::shared_ptr<ASTNode> parse_workbench_spec();
    std::shared_ptr<ASTNode> parse_ai_model_spec();
    std::shared_ptr<ASTNode> parse_namespace_spec();
    std::shared_ptr<ASTNode> parse_mount_directive();
    std::shared_ptr<ASTNode> parse_render_directive();
    std::shared_ptr<ASTNode> parse_block();
    std::shared_ptr<ASTNode> parse_assignment();
    std::shared_ptr<ASTNode> parse_expression();
    std::shared_ptr<ASTNode> parse_identifier();
    std::shared_ptr<ASTNode> parse_literal();
    
    void add_error(const std::string& message);
};

/**
 * @brief Code generator for different output formats
 */
class CodeGenerator {
public:
    enum class OutputFormat {
        CPP,
        JSON,
        YAML,
        DOCKER,
        KUBERNETES
    };
    
    CodeGenerator(OutputFormat format);
    
    std::string generate(const std::shared_ptr<ASTNode>& ast);
    std::string generate_cpp(const std::shared_ptr<ASTNode>& ast);
    std::string generate_json(const std::shared_ptr<ASTNode>& ast);
    std::string generate_yaml(const std::shared_ptr<ASTNode>& ast);
    std::string generate_docker(const std::shared_ptr<ASTNode>& ast);
    std::string generate_kubernetes(const std::shared_ptr<ASTNode>& ast);

private:
    OutputFormat format_;
    
    // Helper methods
    std::string indent(size_t level) const;
    std::string escape_string(const std::string& str) const;
    void generate_node(const std::shared_ptr<ASTNode>& node, std::ostream& output, size_t indent_level = 0);
};

/**
 * @brief Template system for common patterns
 */
class TemplateSystem {
public:
    TemplateSystem();
    
    // Built-in templates
    std::string get_ai_workbench_template(const std::string& model_name);
    std::string get_file_server_template(const std::string& root_path);
    std::string get_echo_server_template(uint16_t port);
    std::string get_distributed_compute_template(size_t worker_count);
    
    // Custom templates
    void register_template(const std::string& name, const std::string& template_code);
    std::string get_template(const std::string& name);
    std::vector<std::string> list_templates() const;
    
    // Template instantiation
    std::string instantiate_template(const std::string& template_name, 
                                   const std::map<std::string, std::string>& parameters);

private:
    std::map<std::string, std::string> templates_;
    
    void initialize_builtin_templates();
    std::string substitute_parameters(const std::string& template_code,
                                    const std::map<std::string, std::string>& parameters);
};

/**
 * @brief Validation engine for glyph descriptions
 */
class Validator {
public:
    Validator();
    
    bool validate(const std::shared_ptr<ASTNode>& ast);
    bool has_errors() const { return !errors_.empty(); }
    bool has_warnings() const { return !warnings_.empty(); }
    
    const std::vector<std::string>& get_errors() const { return errors_; }
    const std::vector<std::string>& get_warnings() const { return warnings_; }

private:
    std::vector<std::string> errors_;
    std::vector<std::string> warnings_;
    
    // Validation rules
    void validate_node(const std::shared_ptr<ASTNode>& node);
    void validate_vm_declaration(const std::shared_ptr<ASTNode>& node);
    void validate_glyph_definition(const std::shared_ptr<ASTNode>& node);
    void validate_workbench_spec(const std::shared_ptr<ASTNode>& node);
    void validate_dependencies(const std::shared_ptr<ASTNode>& node);
    void validate_naming_conventions(const std::shared_ptr<ASTNode>& node);
    
    void add_error(const std::string& message) { errors_.push_back(message); }
    void add_warning(const std::string& message) { warnings_.push_back(message); }
};

/**
 * @brief Main grammar system combining all components
 */
class YaccGrammarSystem {
public:
    YaccGrammarSystem();
    
    // Parsing workflow
    std::shared_ptr<ASTNode> parse(const std::string& source_code);
    std::shared_ptr<ASTNode> parse_file(const std::string& filename);
    
    // High-level parsing
    GlyphDefinition parse_glyph(const std::string& source_code);
    WorkbenchSpecification parse_ai_workbench(const std::string& source_code);
    std::vector<VMDeclaration> parse_vm_topology(const std::string& source_code);
    
    // Code generation
    std::string generate_code(const std::shared_ptr<ASTNode>& ast, 
                             CodeGenerator::OutputFormat format);
    
    // Template operations
    std::string create_from_template(const std::string& template_name,
                                   const std::map<std::string, std::string>& parameters);
    
    // Validation
    bool validate_glyph(const std::string& source_code);
    
    // Error handling
    bool has_errors() const;
    bool has_warnings() const;
    std::vector<std::string> get_all_errors() const;
    std::vector<std::string> get_all_warnings() const;

private:
    std::unique_ptr<Lexer> lexer_;
    std::unique_ptr<Parser> parser_;
    std::unique_ptr<CodeGenerator> code_generator_;
    std::unique_ptr<TemplateSystem> template_system_;
    std::unique_ptr<Validator> validator_;
    
    void clear_errors();
};

} // namespace Yacc
} // namespace DrawTerm