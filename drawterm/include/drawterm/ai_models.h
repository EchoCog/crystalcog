#pragma once

#include <string>
#include <vector>
#include <map>
#include <memory>
#include <functional>
#include <chrono>
#include <variant>
#include <mutex>

// Forward declarations
namespace DrawTerm {
namespace DISVM {
class DISVM;
}
}

namespace DrawTerm {
namespace AI {

/**
 * @brief AI model types supported by DrawKern
 */
enum class ModelType {
    GGML,
    RWKV,
    TRANSFORMER,
    CUSTOM
};

/**
 * @brief AI model configuration
 */
struct ModelConfig {
    std::string model_path;
    ModelType type;
    size_t context_length = 2048;
    float temperature = 0.7f;
    size_t max_tokens = 512;
    std::map<std::string, std::string> custom_params;
};

/**
 * @brief AI inference request
 */
struct InferenceRequest {
    std::string prompt;
    std::string session_id;
    size_t max_tokens = 512;
    float temperature = 0.7f;
    std::vector<std::string> stop_sequences;
    std::map<std::string, std::variant<int, float, std::string>> parameters;
};

/**
 * @brief AI inference response
 */
struct InferenceResponse {
    std::string text;
    std::string session_id;
    size_t tokens_generated;
    float confidence_score;
    std::chrono::milliseconds inference_time;
    bool success;
    std::string error_message;
    std::map<std::string, float> performance_metrics;
};

/**
 * @brief Conversation session for maintaining context
 */
class ConversationSession {
public:
    ConversationSession(const std::string& session_id);
    
    void add_message(const std::string& role, const std::string& content);
    void clear_history();
    std::string get_context(size_t max_tokens = 2048) const;
    
    const std::string& get_session_id() const { return session_id_; }
    size_t get_message_count() const { return messages_.size(); }
    
    // Conversation history
    struct Message {
        std::string role;     // "user", "assistant", "system"
        std::string content;
        std::chrono::system_clock::time_point timestamp;
    };
    
    const std::vector<Message>& get_messages() const { return messages_; }

private:
    std::string session_id_;
    std::vector<Message> messages_;
    mutable std::mutex session_mutex_;
};

/**
 * @brief Abstract base class for AI models
 */
class AIModel {
public:
    AIModel(const std::string& model_name, const ModelConfig& config);
    virtual ~AIModel() = default;
    
    // Model lifecycle
    virtual bool load() = 0;
    virtual void unload() = 0;
    virtual bool is_loaded() const = 0;
    
    // Inference
    virtual InferenceResponse infer(const InferenceRequest& request) = 0;
    virtual InferenceResponse infer_simple(const std::string& prompt, 
                                         const std::string& session_id = "") = 0;
    
    // Configuration
    const std::string& get_model_name() const { return model_name_; }
    const ModelConfig& get_config() const { return config_; }
    ModelType get_type() const { return config_.type; }
    
    // Performance monitoring
    struct PerformanceStats {
        size_t total_inferences = 0;
        std::chrono::milliseconds total_inference_time{0};
        size_t total_tokens_generated = 0;
        float average_tokens_per_second = 0.0f;
    };
    
    const PerformanceStats& get_performance_stats() const { return stats_; }
    void reset_performance_stats() { stats_ = {}; }

protected:
    std::string model_name_;
    ModelConfig config_;
    PerformanceStats stats_;
    mutable std::mutex model_mutex_;
    
    void update_performance_stats(const InferenceResponse& response);
};

/**
 * @brief GGML model implementation
 */
class GGMLModel : public AIModel {
public:
    GGMLModel(const std::string& model_name, const ModelConfig& config);
    ~GGMLModel() override;
    
    // AIModel interface
    bool load() override;
    void unload() override;
    bool is_loaded() const override { return loaded_; }
    
    InferenceResponse infer(const InferenceRequest& request) override;
    InferenceResponse infer_simple(const std::string& prompt, 
                                 const std::string& session_id = "") override;
    
    // GGML specific methods
    void set_gpu_layers(int layers) { gpu_layers_ = layers; }
    void set_context_size(size_t size) { context_size_ = size; }

private:
    bool loaded_ = false;
    void* model_handle_ = nullptr;  // Opaque handle to GGML model
    int gpu_layers_ = 0;
    size_t context_size_ = 2048;
    
    // GGML specific implementation
    bool initialize_ggml();
    void cleanup_ggml();
    std::string generate_text(const std::string& prompt, const InferenceRequest& request);
};

/**
 * @brief RWKV model implementation
 */
class RWKVModel : public AIModel {
public:
    RWKVModel(const std::string& model_name, const ModelConfig& config);
    ~RWKVModel() override;
    
    // AIModel interface
    bool load() override;
    void unload() override;
    bool is_loaded() const override { return loaded_; }
    
    InferenceResponse infer(const InferenceRequest& request) override;
    InferenceResponse infer_simple(const std::string& prompt, 
                                 const std::string& session_id = "") override;
    
    // RWKV specific methods
    void set_state_size(size_t size) { state_size_ = size; }
    void reset_state() { state_initialized_ = false; }

private:
    bool loaded_ = false;
    bool state_initialized_ = false;
    void* model_handle_ = nullptr;  // Opaque handle to RWKV model
    void* model_state_ = nullptr;   // RWKV state
    size_t state_size_ = 1024;
    
    // RWKV specific implementation
    bool initialize_rwkv();
    void cleanup_rwkv();
    std::string generate_text(const std::string& prompt, const InferenceRequest& request);
    void update_state(const std::string& text);
};

/**
 * @brief DrawKern AI Manager for coordinating multiple AI models
 */
class DrawKernAIManager {
public:
    DrawKernAIManager();
    ~DrawKernAIManager();
    
    // Model management
    bool load_model(const std::string& model_name, const ModelConfig& config);
    bool unload_model(const std::string& model_name);
    bool is_model_loaded(const std::string& model_name) const;
    
    std::vector<std::string> list_models() const;
    AIModel* get_model(const std::string& model_name);
    
    // Inference operations
    InferenceResponse infer(const std::string& model_name, 
                          const InferenceRequest& request);
    InferenceResponse infer_simple(const std::string& model_name,
                                 const std::string& prompt,
                                 const std::string& session_id = "");
    
    // Session management
    void create_session(const std::string& session_id);
    void destroy_session(const std::string& session_id);
    ConversationSession* get_session(const std::string& session_id);
    std::vector<std::string> list_sessions() const;
    
    // Batch operations
    std::vector<InferenceResponse> batch_infer(const std::string& model_name,
                                             const std::vector<InferenceRequest>& requests);
    
    // Performance monitoring
    struct GlobalStats {
        size_t total_models_loaded = 0;
        size_t total_sessions = 0;
        size_t total_inferences = 0;
        std::chrono::milliseconds total_inference_time{0};
    };
    
    GlobalStats get_global_stats() const;
    void reset_all_stats();
    
    // Configuration
    void set_default_model(const std::string& model_name);
    const std::string& get_default_model() const;

private:
    std::map<std::string, std::unique_ptr<AIModel>> models_;
    std::map<std::string, std::unique_ptr<ConversationSession>> sessions_;
    std::string default_model_;
    mutable std::mutex manager_mutex_;
    
    // Factory methods
    std::unique_ptr<AIModel> create_model(const std::string& model_name, 
                                        const ModelConfig& config);
};

/**
 * @brief Integration utilities for connecting AI models with DIS VMs
 */
namespace Integration {

/**
 * @brief Setup AI integration for a DIS VM
 */
void setup_dis_vm_ai_integration(DrawTerm::DISVM::DISVM& vm, 
                                DrawKernAIManager& ai_manager,
                                const std::string& default_model);

/**
 * @brief AI inference callback for DIS VM AI opcodes
 */
std::variant<int64_t, double, std::string, std::vector<uint8_t>>
handle_ai_inference(const std::string& model_name,
                   const std::string& prompt,
                   const std::string& session_id,
                   DrawKernAIManager& ai_manager);

/**
 * @brief Create AI workbench configuration
 */
struct AIWorkbenchConfig {
    std::string name;
    std::vector<std::string> models;
    std::map<std::string, ModelConfig> model_configs;
    std::string default_model;
    size_t max_sessions = 100;
};

AIWorkbenchConfig create_ai_workbench_config(const std::string& config_file);

} // namespace Integration

} // namespace AI
} // namespace DrawTerm