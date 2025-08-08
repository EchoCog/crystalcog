#ifndef COGNITIVE_H
#define COGNITIVE_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// Forward declarations
struct ggml_context;
struct ggml_tensor;

// Cognitive tensor operations
struct ggml_tensor* cognitive_attention_matrix(
    struct ggml_context* ctx,
    struct ggml_tensor* input,
    float attention_weight);

struct ggml_tensor* hypergraph_encoding(
    struct ggml_context* ctx,
    struct ggml_tensor* nodes,
    struct ggml_tensor* links);

struct ggml_tensor* cognitive_pattern_match(
    struct ggml_context* ctx,
    struct ggml_tensor* pattern,
    struct ggml_tensor* data);

struct ggml_tensor* meta_cognitive_transform(
    struct ggml_context* ctx,
    struct ggml_tensor* input,
    int meta_level);

// Cognitive kernel structure
typedef struct {
    struct ggml_tensor* tensor_field;
    float attention_weight;
    int meta_level;
    size_t kernel_id;
} cognitive_kernel_t;

// Kernel management functions
cognitive_kernel_t* create_cognitive_kernel(
    struct ggml_context* ctx,
    const int* shape,
    size_t shape_dims,
    float attention_weight);

void destroy_cognitive_kernel(cognitive_kernel_t* kernel);

int update_kernel_attention(cognitive_kernel_t* kernel, float new_weight);

// Hypergraph encoding functions
typedef struct {
    size_t node_count;
    size_t link_count;
    float* node_weights;
    float* link_weights;
    int* adjacency_matrix;
} hypergraph_t;

hypergraph_t* create_hypergraph(size_t node_count, size_t link_count);
void destroy_hypergraph(hypergraph_t* hg);

struct ggml_tensor* encode_hypergraph_to_tensor(
    struct ggml_context* ctx,
    const hypergraph_t* hg);

int decode_tensor_to_hypergraph(
    const struct ggml_tensor* tensor,
    hypergraph_t* hg);

#ifdef __cplusplus
}
#endif

#endif // COGNITIVE_H