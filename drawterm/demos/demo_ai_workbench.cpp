#include "drawterm/ai_models.h"
#include "drawterm/drawterm_integration.h"
#include <iostream>

int main() {
    std::cout << "AI Workbench Demo" << std::endl;
    std::cout << "=================" << std::endl;
    
    DrawTerm::AI::DrawKernAIManager ai_manager;
    
    // Load example AI models
    auto ggml_config = DrawTerm::Examples::get_ggml_example_config();
    auto rwkv_config = DrawTerm::Examples::get_rwkv_example_config();
    
    std::cout << "Loading AI models..." << std::endl;
    
    // Note: These would load actual models in a real implementation
    if (ai_manager.load_model("ggml-test", ggml_config)) {
        std::cout << "✅ GGML model configuration loaded" << std::endl;
    } else {
        std::cout << "⚠️  GGML model loading skipped (no model file)" << std::endl;
    }
    
    if (ai_manager.load_model("rwkv-test", rwkv_config)) {
        std::cout << "✅ RWKV model configuration loaded" << std::endl;
    } else {
        std::cout << "⚠️  RWKV model loading skipped (no model file)" << std::endl;
    }
    
    // Create conversation session
    ai_manager.create_session("demo-session");
    std::cout << "✅ Conversation session created" << std::endl;
    
    // Test inference framework
    DrawTerm::AI::InferenceRequest request;
    request.prompt = "What is artificial intelligence?";
    request.session_id = "demo-session";
    request.max_tokens = 100;
    request.temperature = 0.7f;
    
    std::cout << "✅ AI inference framework ready" << std::endl;
    
    // Display statistics
    auto stats = ai_manager.get_global_stats();
    std::cout << "\n--- AI Manager Statistics ---" << std::endl;
    std::cout << "Models loaded: " << stats.total_models_loaded << std::endl;
    std::cout << "Active sessions: " << stats.total_sessions << std::endl;
    std::cout << "Total inferences: " << stats.total_inferences << std::endl;
    
    // List available models
    auto models = ai_manager.list_models();
    std::cout << "\nAvailable models: ";
    for (const auto& model : models) {
        std::cout << model << " ";
    }
    std::cout << std::endl;
    
    // List active sessions
    auto sessions = ai_manager.list_sessions();
    std::cout << "Active sessions: ";
    for (const auto& session : sessions) {
        std::cout << session << " ";
    }
    std::cout << std::endl;
    
    std::cout << "✅ AI workbench demo completed" << std::endl;
    
    return 0;
}