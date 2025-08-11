#pragma once

#include "styx_protocol.h"
#include "disvm.h"
#include "yacc_grammar.h"
#include "ai_models.h"

#include <memory>
#include <string>
#include <map>
#include <functional>

namespace DrawTerm {

/**
 * @brief Main DrawTerm integration class - "Infrastructure as Glyphs"
 */
class DrawTermSystem {
public:
    DrawTermSystem();
    ~DrawTermSystem();
    
    // System initialization
    bool initialize();
    void shutdown();
    bool is_initialized() const { return initialized_; }
    
    // Glyph rendering - the core of "Infrastructure as Glyphs"
    bool render_glyph(const std::string& glyph_description);
    bool render_glyph_file(const std::string& glyph_file);
    
    // AI Workbench deployment
    bool deploy_ai_workbench(const std::string& workbench_spec);
    
    // Network transparency
    bool start_styx_server(const std::string& bind_address, uint16_t port);
    bool stop_styx_server();
    
    // VM management
    std::string create_vm();
    bool destroy_vm(const std::string& vm_id);
    bool load_limbo_program(const std::string& vm_id, const std::string& source_code);
    bool start_vm(const std::string& vm_id);
    bool stop_vm(const std::string& vm_id);
    
    // AI model management
    bool load_ai_model(const std::string& model_name, const AI::ModelConfig& config);
    AI::InferenceResponse ai_infer(const std::string& model_name, 
                                  const std::string& prompt,
                                  const std::string& session_id = "");
    
    // Code generation
    std::string generate_deployment_code(const std::string& glyph_description,
                                       Yacc::CodeGenerator::OutputFormat format);
    
    // Template operations
    std::string create_from_template(const std::string& template_name,
                                   const std::map<std::string, std::string>& parameters);
    
    // System state
    struct SystemStatus {
        bool styx_server_running;
        size_t active_vms;
        size_t loaded_models;
        size_t active_sessions;
        std::vector<std::string> recent_errors;
    };
    
    SystemStatus get_system_status() const;

private:
    bool initialized_ = false;
    
    // Core components
    std::unique_ptr<Styx::StyxServer> styx_server_;
    std::unique_ptr<DISVM::DISVMManager> vm_manager_;
    std::unique_ptr<Yacc::YaccGrammarSystem> grammar_system_;
    std::unique_ptr<AI::DrawKernAIManager> ai_manager_;
    
    // Integration state
    std::map<std::string, std::string> rendered_glyphs_;
    std::vector<std::string> system_errors_;
    mutable std::mutex system_mutex_;
    
    // Helper methods
    bool validate_glyph(const std::string& glyph_description);
    void integrate_vm_with_ai(const std::string& vm_id);
    void add_system_error(const std::string& error);
};

/**
 * @brief Glyph representation for "Infrastructure as Glyphs"
 */
struct Glyph {
    std::string name;
    std::string description;
    std::vector<std::string> vms;
    std::map<std::string, std::string> configuration;
    std::vector<std::string> dependencies;
    
    // Serialization
    std::string to_yaml() const;
    std::string to_json() const;
    static Glyph from_yaml(const std::string& yaml_content);
    static Glyph from_json(const std::string& json_content);
};

/**
 * @brief AI Workbench glyph - specialized glyph for AI deployment
 */
struct AIWorkbenchGlyph : public Glyph {
    std::string ai_model;
    std::vector<std::string> tools;
    std::map<std::string, std::string> environment;
    size_t max_sessions = 100;
    
    // AI Workbench specific methods
    bool deploy();
    bool undeploy();
    bool is_deployed() const;
    
    static AIWorkbenchGlyph create_standard(const std::string& model_name);
    static AIWorkbenchGlyph create_research(const std::string& model_name);
    static AIWorkbenchGlyph create_production(const std::string& model_name);
};

/**
 * @brief Utility functions for DrawTerm operations
 */
namespace Utils {

/**
 * @brief Parse and validate glyph description
 */
bool validate_glyph_description(const std::string& description, 
                               std::vector<std::string>& errors);

/**
 * @brief Generate unique identifiers
 */
std::string generate_vm_id();
std::string generate_session_id();
std::string generate_glyph_id();

/**
 * @brief Configuration helpers
 */
std::map<std::string, std::string> parse_configuration(const std::string& config_string);
std::string serialize_configuration(const std::map<std::string, std::string>& config);

/**
 * @brief Network utilities
 */
bool is_port_available(uint16_t port);
std::string get_local_ip_address();

/**
 * @brief File system utilities
 */
bool create_glyph_workspace(const std::string& glyph_name);
bool cleanup_glyph_workspace(const std::string& glyph_name);
std::string get_glyph_workspace_path(const std::string& glyph_name);

} // namespace Utils

/**
 * @brief Demo and example configurations
 */
namespace Examples {

/**
 * @brief Get example glyph descriptions
 */
std::string get_ai_workbench_example();
std::string get_file_server_example();
std::string get_echo_server_example();
std::string get_distributed_compute_example();

/**
 * @brief Get example AI model configurations
 */
AI::ModelConfig get_ggml_example_config();
AI::ModelConfig get_rwkv_example_config();

/**
 * @brief Get example Limbo programs
 */
std::string get_hello_world_limbo();
std::string get_ai_inference_limbo();
std::string get_file_operations_limbo();

} // namespace Examples

} // namespace DrawTerm