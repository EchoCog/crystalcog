#include "drawterm/ai_models.h"
#include "drawterm/drawterm_integration.h"
#include <iostream>
#include <sstream>
#include <stdexcept>

namespace DrawTerm {
namespace AI {
namespace Integration {

void setup_dis_vm_ai_integration(DrawTerm::DISVM::DISVM& vm, 
                                DrawKernAIManager& ai_manager,
                                const std::string& default_model) {
    std::cout << "Setting up DIS VM AI integration with default model: " << default_model << std::endl;
    
    if (!ai_manager.is_model_loaded(default_model)) {
        throw std::runtime_error("Default model '" + default_model + "' is not loaded");
    }
    
    ai_manager.set_default_model(default_model);
    
    // Register AI callback opcodes with the DIS VM
    // This would typically register AI-specific opcodes that the VM can execute
    std::cout << "AI integration setup complete for DIS VM" << std::endl;
}

std::variant<int64_t, double, std::string, std::vector<uint8_t>>
handle_ai_inference(const std::string& model_name,
                   const std::string& prompt,
                   const std::string& session_id,
                   DrawKernAIManager& ai_manager) {
    
    try {
        // Validate inputs
        if (model_name.empty()) {
            throw std::invalid_argument("Model name cannot be empty");
        }
        if (prompt.empty()) {
            throw std::invalid_argument("Prompt cannot be empty");
        }
        
        // Check if model is loaded
        if (!ai_manager.is_model_loaded(model_name)) {
            throw std::runtime_error("Model '" + model_name + "' is not loaded");
        }
        
        // Create or get session
        if (ai_manager.get_session(session_id) == nullptr) {
            ai_manager.create_session(session_id);
        }
        
        // Perform inference
        InferenceResponse response = ai_manager.infer_simple(model_name, prompt, session_id);
        
        if (!response.success) {
            throw std::runtime_error("Inference failed: " + response.error_message);
        }
        
        // Return the text response as the primary result
        return response.text;
        
    } catch (const std::exception& e) {
        std::cerr << "AI inference error: " << e.what() << std::endl;
        return std::string("ERROR: ") + e.what();
    }
}

AIWorkbenchConfig create_ai_workbench_config(const std::string& config_file) {
    AIWorkbenchConfig config;
    
    try {
        // For demo purposes, create a default configuration
        // In a real implementation, this would parse the config file
        if (config_file.empty()) {
            // Default configuration
            config.name = "default_workbench";
            config.models = {"demo_model", "test_model"};
            config.default_model = "demo_model";
            config.max_sessions = 100;
            
            // Create default model configs
            ModelConfig demo_config;
            demo_config.model_path = "/models/demo_model";
            demo_config.type = ModelType::CUSTOM;
            demo_config.context_length = 2048;
            demo_config.temperature = 0.7f;
            demo_config.max_tokens = 512;
            
            ModelConfig test_config;
            test_config.model_path = "/models/test_model";
            test_config.type = ModelType::CUSTOM;
            test_config.context_length = 1024;
            test_config.temperature = 0.5f;
            test_config.max_tokens = 256;
            
            config.model_configs["demo_model"] = demo_config;
            config.model_configs["test_model"] = test_config;
            
        } else {
            // TODO: Parse actual config file
            throw std::runtime_error("Config file parsing not implemented yet");
        }
        
        std::cout << "Created AI workbench config: " << config.name 
                  << " with " << config.models.size() << " models" << std::endl;
                  
    } catch (const std::exception& e) {
        std::cerr << "Error creating workbench config: " << e.what() << std::endl;
        throw;
    }
    
    return config;
}

// Helper function to validate AI workbench configuration
bool validate_workbench_config(const AIWorkbenchConfig& config) {
    if (config.name.empty()) {
        std::cerr << "Workbench name cannot be empty" << std::endl;
        return false;
    }
    
    if (config.models.empty()) {
        std::cerr << "At least one model must be specified" << std::endl;
        return false;
    }
    
    if (config.default_model.empty()) {
        std::cerr << "Default model must be specified" << std::endl;
        return false;
    }
    
    // Check if default model is in the models list
    bool found_default = false;
    for (const auto& model : config.models) {
        if (model == config.default_model) {
            found_default = true;
            break;
        }
    }
    
    if (!found_default) {
        std::cerr << "Default model '" << config.default_model 
                  << "' not found in models list" << std::endl;
        return false;
    }
    
    // Check that all models have configurations
    for (const auto& model : config.models) {
        if (config.model_configs.find(model) == config.model_configs.end()) {
            std::cerr << "No configuration found for model '" << model << "'" << std::endl;
            return false;
        }
    }
    
    return true;
}

// Helper function to initialize AI workbench
bool initialize_ai_workbench(DrawKernAIManager& ai_manager, 
                            const AIWorkbenchConfig& config) {
    
    if (!validate_workbench_config(config)) {
        return false;
    }
    
    std::cout << "Initializing AI workbench: " << config.name << std::endl;
    
    try {
        // Load all configured models
        for (const auto& model_name : config.models) {
            auto config_it = config.model_configs.find(model_name);
            if (config_it != config.model_configs.end()) {
                bool loaded = ai_manager.load_model(model_name, config_it->second);
                if (!loaded) {
                    std::cerr << "Failed to load model: " << model_name << std::endl;
                    return false;
                }
                std::cout << "Loaded model: " << model_name << std::endl;
            }
        }
        
        // Set default model
        ai_manager.set_default_model(config.default_model);
        
        std::cout << "AI workbench initialized successfully" << std::endl;
        return true;
        
    } catch (const std::exception& e) {
        std::cerr << "Error initializing AI workbench: " << e.what() << std::endl;
        return false;
    }
}

} // namespace Integration
} // namespace AI
} // namespace DrawTerm