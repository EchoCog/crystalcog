/*
 * opencog/visualization/imgui-visualizer/AtomSpaceImGuiInterface.cpp
 *
 * Copyright (C) 2024 OpenCog Foundation
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include "AtomSpaceImGuiInterface.h"
#include <iostream>
#include <algorithm>
#include <cstring>
#include <GLFW/glfw3.h>

AtomSpaceImGuiInterface::AtomSpaceImGuiInterface()
    : is_connected_(false)
    , server_address_("localhost:17001")
    , show_search_panel_(true)
    , show_atom_details_(true)
    , show_graph_view_(true)
    , auto_refresh_(false)
    , refresh_interval_(DEFAULT_REFRESH_INTERVAL)
    , last_refresh_time_(0.0f)
    , selected_atom_type_(0)
    , include_subtypes_(true)
    , sort_order_(0)
    , max_results_(100)
    , selected_vertex_(nullptr)
{
    // Initialize buffers
    std::memset(search_name_buffer_, 0, sizeof(search_name_buffer_));
    std::memset(search_uuid_buffer_, 0, sizeof(search_uuid_buffer_));
    
    // Initialize AtomTypes
    atom_types_ = std::make_unique<AtomTypes>();
    
    // Initialize AtomSpace interface
    try {
        atomspace_interface_ = std::make_unique<AtomSpaceInterface>(atom_types_.get());
        atomspace_interface_->server = server_address_;
        is_connected_ = TestConnection();
    } catch (const std::exception& e) {
        std::cerr << "Failed to initialize AtomSpace interface: " << e.what() << std::endl;
    }
}

AtomSpaceImGuiInterface::~AtomSpaceImGuiInterface()
{
    ClearResults();
}

void AtomSpaceImGuiInterface::SetServerAddress(const std::string& server)
{
    server_address_ = server;
    if (atomspace_interface_) {
        atomspace_interface_->server = server_address_;
    }
}

bool AtomSpaceImGuiInterface::TestConnection()
{
    if (!atomspace_interface_) return false;
    
    try {
        // Try a simple search to test connection
        AtomFilter filter;
        filter.name = "";
        filter.type = 0;
        filter.includeSubtypes = true;
        filter.sortOrder = 0;
        
        std::vector<Vertex*> test_results;
        bool is_finished = false;
        atomspace_interface_->SearchAtom(&filter, test_results, is_finished, 0);
        
        // Clean up test results
        for (auto* vertex : test_results) {
            delete vertex;
        }
        
        return true;
    } catch (const std::exception& e) {
        std::cerr << "Connection test failed: " << e.what() << std::endl;
        return false;
    }
}

void AtomSpaceImGuiInterface::RenderAtomSpaceWindow(bool* p_open)
{
    if (!ImGui::Begin("AtomSpace Viewer", p_open, ImGuiWindowFlags_MenuBar))
    {
        ImGui::End();
        return;
    }
    
    // Menu bar
    if (ImGui::BeginMenuBar())
    {
        if (ImGui::BeginMenu("View"))
        {
            ImGui::MenuItem("Search Panel", nullptr, &show_search_panel_);
            ImGui::MenuItem("Atom Details", nullptr, &show_atom_details_);
            ImGui::MenuItem("Graph View", nullptr, &show_graph_view_);
            ImGui::EndMenu();
        }
        
        if (ImGui::BeginMenu("Data"))
        {
            if (ImGui::MenuItem("Refresh"))
                RefreshData();
            ImGui::MenuItem("Auto Refresh", nullptr, &auto_refresh_);
            if (ImGui::SliderFloat("Refresh Interval", &refresh_interval_, 1.0f, 30.0f))
            {
                // Clamp refresh interval
                refresh_interval_ = std::max(1.0f, std::min(30.0f, refresh_interval_));
            }
            ImGui::EndMenu();
        }
        
        ImGui::EndMenuBar();
    }
    
    // Auto refresh logic
    if (auto_refresh_)
    {
        float current_time = static_cast<float>(glfwGetTime());
        if (current_time - last_refresh_time_ > refresh_interval_)
        {
            RefreshData();
            last_refresh_time_ = current_time;
        }
    }
    
    // Main content area
    RenderConnectionPanel();
    
    if (is_connected_)
    {
        ImGui::Separator();
        
        // Create layout with splitters
        if (show_search_panel_)
        {
            ImGui::BeginChild("SearchPanel", ImVec2(0, 200), true);
            RenderSearchPanel();
            ImGui::EndChild();
        }
        
        ImGui::BeginChild("MainContent", ImVec2(0, 0), false);
        
        // Split between results and details/graph
        float content_width = ImGui::GetContentRegionAvail().x;
        ImGui::BeginChild("ResultsPanel", ImVec2(content_width * 0.4f, 0), true);
        RenderResultsPanel();
        ImGui::EndChild();
        
        ImGui::SameLine();
        
        ImGui::BeginChild("DetailsAndGraph", ImVec2(0, 0), false);
        
        if (show_atom_details_)
        {
            ImGui::BeginChild("DetailsPanel", ImVec2(0, 300), true);
            RenderAtomDetailsPanel();
            ImGui::EndChild();
        }
        
        if (show_graph_view_)
        {
            if (show_atom_details_) ImGui::Separator();
            ImGui::BeginChild("GraphPanel", ImVec2(0, 0), true);
            RenderGraphView();
            ImGui::EndChild();
        }
        
        ImGui::EndChild(); // DetailsAndGraph
        ImGui::EndChild(); // MainContent
    }
    
    RenderStatusBar();
    
    ImGui::End();
}

void AtomSpaceImGuiInterface::RenderConnectionPanel()
{
    ImGui::Text("CogServer Connection");
    
    // Server address input
    ImGui::PushItemWidth(200.0f);
    if (ImGui::InputText("Server", &server_address_[0], server_address_.capacity() + 1))
    {
        server_address_.resize(strlen(server_address_.c_str()));
        SetServerAddress(server_address_);
    }
    ImGui::PopItemWidth();
    
    ImGui::SameLine();
    
    // Connect/Disconnect button
    if (is_connected_)
    {
        if (ImGui::Button("Disconnect"))
        {
            is_connected_ = false;
            ClearResults();
        }
        ImGui::SameLine();
        ImGui::TextColored(ImVec4(0.0f, 1.0f, 0.0f, 1.0f), "Connected");
    }
    else
    {
        if (ImGui::Button("Connect"))
        {
            is_connected_ = TestConnection();
        }
        ImGui::SameLine();
        ImGui::TextColored(ImVec4(1.0f, 0.0f, 0.0f, 1.0f), "Disconnected");
    }
    
    // Test connection button
    ImGui::SameLine();
    if (ImGui::Button("Test"))
    {
        is_connected_ = TestConnection();
    }
}

void AtomSpaceImGuiInterface::RenderSearchPanel()
{
    ImGui::Text("Search AtomSpace");
    
    // Search by name
    ImGui::InputText("Name", search_name_buffer_, sizeof(search_name_buffer_));
    
    // Search by UUID
    ImGui::InputText("UUID", search_uuid_buffer_, sizeof(search_uuid_buffer_));
    
    // Atom type selection
    ImGui::Text("Atom Type:");
    if (ImGui::BeginCombo("##AtomType", GetAtomTypeName(selected_atom_type_)))
    {
        for (int i = 0; i < 100; ++i) // Limit to reasonable number
        {
            const char* type_name = GetAtomTypeName(i);
            if (type_name && strlen(type_name) > 0)
            {
                bool is_selected = (selected_atom_type_ == i);
                if (ImGui::Selectable(type_name, is_selected))
                    selected_atom_type_ = i;
                if (is_selected)
                    ImGui::SetItemDefaultFocus();
            }
        }
        ImGui::EndCombo();
    }
    
    ImGui::Checkbox("Include Subtypes", &include_subtypes_);
    
    // Search options
    ImGui::SliderInt("Max Results", &max_results_, 10, MAX_SEARCH_RESULTS);
    
    // Search button
    if (ImGui::Button("Search"))
    {
        PerformSearch();
    }
    
    ImGui::SameLine();
    if (ImGui::Button("Clear"))
    {
        ClearResults();
    }
}

void AtomSpaceImGuiInterface::RenderResultsPanel()
{
    ImGui::Text("Search Results (%lu atoms)", search_results_.size());
    
    if (ImGui::BeginListBox("##Results", ImVec2(-1, -1)))
    {
        for (size_t i = 0; i < search_results_.size(); ++i)
        {
            Vertex* vertex = search_results_[i];
            if (!vertex) continue;
            
            // Create display text
            std::string display_text = vertex->name;
            if (display_text.empty())
                display_text = "UUID: " + vertex->uuid;
            
            // Add type information
            const char* type_name = GetAtomTypeName(vertex->type);
            if (type_name)
                display_text += " [" + std::string(type_name) + "]";
            
            bool is_selected = (selected_vertex_ == vertex);
            if (ImGui::Selectable(display_text.c_str(), is_selected))
            {
                selected_vertex_ = vertex;
                UpdateConnectedAtoms();
            }
            
            // Show additional info on hover
            if (ImGui::IsItemHovered())
            {
                ImGui::BeginTooltip();
                DisplayAtomInfo(vertex);
                ImGui::EndTooltip();
            }
        }
        ImGui::EndListBox();
    }
}

void AtomSpaceImGuiInterface::RenderAtomDetailsPanel()
{
    ImGui::Text("Atom Details");
    
    if (selected_vertex_)
    {
        DisplayAtomInfo(selected_vertex_);
        ImGui::Separator();
        DisplayTruthValue(selected_vertex_);
        ImGui::Separator();
        DisplayAtomConnections(selected_vertex_);
    }
    else
    {
        ImGui::Text("No atom selected");
    }
}

void AtomSpaceImGuiInterface::RenderGraphView()
{
    ImGui::Text("Graph Visualization");
    
    // Simple placeholder for graph visualization
    ImDrawList* draw_list = ImGui::GetWindowDrawList();
    ImVec2 canvas_pos = ImGui::GetCursorScreenPos();
    ImVec2 canvas_size = ImGui::GetContentRegionAvail();
    
    if (canvas_size.x < 50.0f) canvas_size.x = 50.0f;
    if (canvas_size.y < 50.0f) canvas_size.y = 50.0f;
    
    // Draw background
    draw_list->AddRectFilled(canvas_pos, 
                           ImVec2(canvas_pos.x + canvas_size.x, canvas_pos.y + canvas_size.y), 
                           IM_COL32(50, 50, 50, 255));
    
    // Draw border
    draw_list->AddRect(canvas_pos, 
                      ImVec2(canvas_pos.x + canvas_size.x, canvas_pos.y + canvas_size.y), 
                      IM_COL32(255, 255, 255, 255));
    
    // Simple visualization of selected atom and connections
    if (selected_vertex_ && !connected_atoms_.empty())
    {
        ImVec2 center(canvas_pos.x + canvas_size.x * 0.5f, canvas_pos.y + canvas_size.y * 0.5f);
        
        // Draw selected atom as central node
        ImVec4 color = GetAtomTypeColor(selected_vertex_->type);
        draw_list->AddCircleFilled(center, 20.0f, 
                                 IM_COL32(color.x * 255, color.y * 255, color.z * 255, 255));
        
        // Draw connected atoms around it
        float angle_step = 2.0f * 3.14159f / connected_atoms_.size();
        for (size_t i = 0; i < connected_atoms_.size(); ++i)
        {
            float angle = i * angle_step;
            ImVec2 pos(center.x + cos(angle) * 80.0f, center.y + sin(angle) * 80.0f);
            
            ImVec4 conn_color = GetAtomTypeColor(connected_atoms_[i]->type);
            draw_list->AddCircleFilled(pos, 15.0f, 
                                     IM_COL32(conn_color.x * 255, conn_color.y * 255, conn_color.z * 255, 255));
            
            // Draw connection line
            draw_list->AddLine(center, pos, IM_COL32(150, 150, 150, 255), 2.0f);
        }
    }
    
    // Make the canvas interactive
    ImGui::InvisibleButton("canvas", canvas_size);
}

void AtomSpaceImGuiInterface::RenderStatusBar()
{
    ImGui::Separator();
    ImGui::Text("Status: %s | Results: %lu | Selected: %s", 
                is_connected_ ? "Connected" : "Disconnected",
                search_results_.size(),
                selected_vertex_ ? selected_vertex_->name.c_str() : "None");
}

void AtomSpaceImGuiInterface::PerformSearch()
{
    if (!atomspace_interface_ || !is_connected_) return;
    
    ClearResults();
    
    try {
        AtomFilter filter;
        filter.name = std::string(search_name_buffer_);
        filter.type = selected_atom_type_;
        filter.includeSubtypes = include_subtypes_;
        filter.sortOrder = sort_order_;
        
        bool is_finished = false;
        atomspace_interface_->SearchAtom(&filter, search_results_, is_finished, 0);
        
        // Limit results
        if (search_results_.size() > static_cast<size_t>(max_results_))
        {
            // Clean up excess results
            for (size_t i = max_results_; i < search_results_.size(); ++i)
            {
                delete search_results_[i];
            }
            search_results_.resize(max_results_);
        }
    } catch (const std::exception& e) {
        std::cerr << "Search failed: " << e.what() << std::endl;
    }
}

void AtomSpaceImGuiInterface::UpdateConnectedAtoms()
{
    // Clear existing connected atoms
    for (auto* vertex : connected_atoms_) {
        delete vertex;
    }
    connected_atoms_.clear();
    
    if (!selected_vertex_ || !atomspace_interface_ || !is_connected_) return;
    
    try {
        NodeFilter node_filter;
        LinkFilter link_filter;
        atomspace_interface_->GetConnectedAtoms(selected_vertex_, &node_filter, &link_filter, connected_atoms_);
    } catch (const std::exception& e) {
        std::cerr << "Failed to get connected atoms: " << e.what() << std::endl;
    }
}

void AtomSpaceImGuiInterface::RefreshData()
{
    if (is_connected_) {
        PerformSearch();
    }
}

void AtomSpaceImGuiInterface::ClearResults()
{
    for (auto* vertex : search_results_) {
        delete vertex;
    }
    search_results_.clear();
    
    for (auto* vertex : connected_atoms_) {
        delete vertex;
    }
    connected_atoms_.clear();
    
    selected_vertex_ = nullptr;
}

void AtomSpaceImGuiInterface::DisplayAtomInfo(Vertex* vertex)
{
    if (!vertex) return;
    
    ImGui::Text("Name: %s", vertex->name.c_str());
    ImGui::Text("UUID: %s", vertex->uuid.c_str());
    ImGui::Text("Type: %s (%d)", GetAtomTypeName(vertex->type), vertex->type);
    ImGui::Text("STI: %d", vertex->sti);
    ImGui::Text("LTI: %d", vertex->lti);
}

void AtomSpaceImGuiInterface::DisplayTruthValue(Vertex* vertex)
{
    if (!vertex) return;
    
    ImGui::Text("Truth Value:");
    ImGui::Text("Strength: %.3f", vertex->truthValue.strength);
    ImGui::Text("Confidence: %.3f", vertex->truthValue.confidence);
    ImGui::Text("Count: %.1f", vertex->truthValue.count);
}

void AtomSpaceImGuiInterface::DisplayAtomConnections(Vertex* vertex)
{
    if (!vertex) return;
    
    ImGui::Text("Connected Atoms: %lu", connected_atoms_.size());
    
    for (size_t i = 0; i < connected_atoms_.size() && i < 10; ++i) // Show max 10
    {
        Vertex* conn = connected_atoms_[i];
        ImGui::Text("  %s [%s]", conn->name.c_str(), GetAtomTypeName(conn->type));
    }
    
    if (connected_atoms_.size() > 10)
    {
        ImGui::Text("  ... and %lu more", connected_atoms_.size() - 10);
    }
}

const char* AtomSpaceImGuiInterface::GetAtomTypeName(int type_id)
{
    if (atom_types_) {
        return atom_types_->GetAtomTypeName(type_id).c_str();
    }
    return "Unknown";
}

ImVec4 AtomSpaceImGuiInterface::GetAtomTypeColor(int type_id)
{
    // Simple color mapping based on type ID
    float hue = (type_id * 137.5f) / 360.0f; // Use golden angle for good distribution
    hue = hue - floor(hue); // Keep in [0,1] range
    
    // Convert HSV to RGB (simple version)
    float r, g, b;
    int i = static_cast<int>(hue * 6);
    float f = hue * 6 - i;
    
    switch (i % 6) {
        case 0: r = 1; g = f; b = 0; break;
        case 1: r = 1-f; g = 1; b = 0; break;
        case 2: r = 0; g = 1; b = f; break;
        case 3: r = 0; g = 1-f; b = 1; break;
        case 4: r = f; g = 0; b = 1; break;
        case 5: r = 1; g = 0; b = 1-f; break;
        default: r = g = b = 0; break;
    }
    
    return ImVec4(r * 0.8f + 0.2f, g * 0.8f + 0.2f, b * 0.8f + 0.2f, 1.0f);
}