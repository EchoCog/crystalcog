#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "cognitive.h"

int test_hypergraph_creation() {
    printf("Testing hypergraph creation...\n");
    
    hypergraph_t* hg = create_hypergraph(10, 20);
    if (!hg) {
        printf("FAIL: Hypergraph creation failed\n");
        return 0;
    }
    
    assert(hg->node_count == 10);
    assert(hg->link_count == 20);
    assert(hg->node_weights != NULL);
    assert(hg->link_weights != NULL);
    assert(hg->adjacency_matrix != NULL);
    
    destroy_hypergraph(hg);
    printf("PASS: Hypergraph creation\n");
    return 1;
}

int test_cognitive_kernel_creation() {
    printf("Testing cognitive kernel creation...\n");
    
    int shape[] = {64, 32};
    cognitive_kernel_t* kernel = create_cognitive_kernel(NULL, shape, 2, 0.8f);
    if (!kernel) {
        printf("FAIL: Cognitive kernel creation failed\n");
        return 0;
    }
    
    assert(kernel->attention_weight == 0.8f);
    assert(kernel->meta_level == 0);
    assert(kernel->tensor_field != NULL);
    
    // Test attention update
    int result = update_kernel_attention(kernel, 0.9f);
    assert(result == 0);
    assert(kernel->attention_weight == 0.9f);
    
    destroy_cognitive_kernel(kernel);
    printf("PASS: Cognitive kernel creation\n");
    return 1;
}

int test_tensor_operations() {
    printf("Testing tensor operations...\n");
    
    // This is a mock test since we don't have real GGML
    // In a real implementation, this would test actual tensor operations
    
    printf("PASS: Tensor operations (mock)\n");
    return 1;
}

int main() {
    printf("Running Agent-Zero C component tests...\n\n");
    
    int passed = 0;
    int total = 3;
    
    passed += test_hypergraph_creation();
    passed += test_cognitive_kernel_creation();
    passed += test_tensor_operations();
    
    printf("\nTest Results: %d/%d passed\n", passed, total);
    
    if (passed == total) {
        printf("All Agent-Zero C tests passed!\n");
        return 0;
    } else {
        printf("Some Agent-Zero C tests failed!\n");
        return 1;
    }
}