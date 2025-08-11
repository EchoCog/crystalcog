/*
 * Simple ImGui test application for OpenCog integration
 * Tests that ImGui can be compiled and run in the OpenCog environment
 */

#include <iostream>
#include <GLFW/glfw3.h>
#include "imgui.h"
#include "imgui_impl_glfw.h"
#include "imgui_impl_opengl3.h"

static void glfw_error_callback(int error, const char* description)
{
    fprintf(stderr, "GLFW Error %d: %s\n", error, description);
}

int main()
{
    std::cout << "OpenCog ImGui Integration Test" << std::endl;
    
    // Setup GLFW
    glfwSetErrorCallback(glfw_error_callback);
    if (!glfwInit())
        return -1;

    // GL 3.0 + GLSL 130
    const char* glsl_version = "#version 130";
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 0);

    // Create window
    GLFWwindow* window = glfwCreateWindow(800, 600, "OpenCog ImGui Test", nullptr, nullptr);
    if (window == nullptr)
        return -1;

    glfwMakeContextCurrent(window);
    glfwSwapInterval(1); // Enable vsync

    // Setup Dear ImGui context
    IMGUI_CHECKVERSION();
    ImGui::CreateContext();
    ImGuiIO& io = ImGui::GetIO(); (void)io;

    // Setup Dear ImGui style
    ImGui::StyleColorsDark();

    // Setup Platform/Renderer backends
    ImGui_ImplGlfw_InitForOpenGL(window, true);
    ImGui_ImplOpenGL3_Init(glsl_version);

    // Application state
    bool show_demo_window = true;
    bool show_opencog_info = true;
    ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);

    std::cout << "Starting main loop..." << std::endl;

    // Main loop
    while (!glfwWindowShouldClose(window))
    {
        glfwPollEvents();

        // Start the Dear ImGui frame
        ImGui_ImplOpenGL3_NewFrame();
        ImGui_ImplGlfw_NewFrame();
        ImGui::NewFrame();

        // Show the demo window
        if (show_demo_window)
            ImGui::ShowDemoWindow(&show_demo_window);

        // Show OpenCog info window
        if (show_opencog_info)
        {
            ImGui::Begin("OpenCog Integration Test", &show_opencog_info);
            ImGui::Text("OpenCog ImGui Integration Test");
            ImGui::Separator();
            ImGui::Text("This demonstrates that ImGui can be successfully");
            ImGui::Text("integrated into the OpenCog visualization system.");
            ImGui::Spacing();
            ImGui::Text("Key features to implement:");
            ImGui::BulletText("AtomSpace data visualization");
            ImGui::BulletText("Interactive atom graph display");
            ImGui::BulletText("Real-time CogServer connection");
            ImGui::BulletText("Atom filtering and search");
            ImGui::BulletText("Truth value visualization");
            ImGui::Spacing();
            if (ImGui::Button("Test Successful!"))
            {
                std::cout << "ImGui integration test passed!" << std::endl;
            }
            ImGui::End();
        }

        // Rendering
        ImGui::Render();
        int display_w, display_h;
        glfwGetFramebufferSize(window, &display_w, &display_h);
        glViewport(0, 0, display_w, display_h);
        glClearColor(clear_color.x * clear_color.w, 
                     clear_color.y * clear_color.w, 
                     clear_color.z * clear_color.w, 
                     clear_color.w);
        glClear(GL_COLOR_BUFFER_BIT);
        ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());

        glfwSwapBuffers(window);
    }

    // Cleanup
    ImGui_ImplOpenGL3_Shutdown();
    ImGui_ImplGlfw_Shutdown();
    ImGui::DestroyContext();

    glfwDestroyWindow(window);
    glfwTerminate();

    std::cout << "ImGui test completed successfully!" << std::endl;
    return 0;
}