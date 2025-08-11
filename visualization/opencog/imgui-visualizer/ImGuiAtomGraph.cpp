/*
 * opencog/visualization/imgui-visualizer/ImGuiAtomGraph.cpp
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

#include "ImGuiAtomGraph.h"
#include <cmath>
#include <algorithm>

ImGuiAtomGraph::ImGuiAtomGraph()
    : zoom_level_(1.0f)
    , pan_offset_(0.0f, 0.0f)
    , is_panning_(false)
    , last_mouse_pos_(0.0f, 0.0f)
    , node_radius_(20.0f)
    , link_thickness_(2.0f)
{
}

ImGuiAtomGraph::~ImGuiAtomGraph()
{
}

void ImGuiAtomGraph::Render(const std::vector<Vertex*>& vertices, Vertex* selected_vertex)
{
    ImDrawList* draw_list = ImGui::GetWindowDrawList();
    ImVec2 canvas_pos = ImGui::GetCursorScreenPos();
    ImVec2 canvas_size = ImGui::GetContentRegionAvail();
    
    if (canvas_size.x < 50.0f) canvas_size.x = 50.0f;
    if (canvas_size.y < 50.0f) canvas_size.y = 50.0f;
    
    // Store canvas bounds
    canvas_min_ = canvas_pos;
    canvas_max_ = ImVec2(canvas_pos.x + canvas_size.x, canvas_pos.y + canvas_size.y);
    
    // Draw background
    draw_list->AddRectFilled(canvas_min_, canvas_max_, IM_COL32(30, 30, 30, 255));
    draw_list->AddRect(canvas_min_, canvas_max_, IM_COL32(150, 150, 150, 255));
    
    // Handle input
    HandleInput();
    
    // Calculate layout if needed
    if (vertices.size() != node_positions_.size())
    {
        CalculateLayout(vertices);
    }
    
    // Render connections first (so they appear behind nodes)
    RenderConnections(vertices, draw_list);
    
    // Render nodes
    RenderNodes(vertices, selected_vertex, draw_list);
    
    // Handle selection
    HandleSelection(vertices);
    
    // Make canvas interactive
    ImGui::InvisibleButton("graph_canvas", canvas_size);
}

void ImGuiAtomGraph::HandleInput()
{
    ImGuiIO& io = ImGui::GetIO();
    ImVec2 mouse_pos = io.MousePos;
    
    // Check if mouse is over canvas
    bool is_hovered = ImGui::IsItemHovered();
    
    if (is_hovered)
    {
        // Handle zooming
        if (io.MouseWheel != 0.0f)
        {
            float zoom_factor = 1.0f + io.MouseWheel * 0.1f;
            zoom_level_ *= zoom_factor;
            zoom_level_ = std::max(0.1f, std::min(5.0f, zoom_level_));
        }
        
        // Handle panning
        if (ImGui::IsMouseDown(ImGuiMouseButton_Right))
        {
            if (!is_panning_)
            {
                is_panning_ = true;
                last_mouse_pos_ = mouse_pos;
            }
            else
            {
                ImVec2 delta = ImVec2(mouse_pos.x - last_mouse_pos_.x, mouse_pos.y - last_mouse_pos_.y);
                pan_offset_.x += delta.x;
                pan_offset_.y += delta.y;
                last_mouse_pos_ = mouse_pos;
            }
        }
        else
        {
            is_panning_ = false;
        }
    }
    else
    {
        is_panning_ = false;
    }
}

void ImGuiAtomGraph::HandleSelection(const std::vector<Vertex*>& vertices)
{
    ImGuiIO& io = ImGui::GetIO();
    
    if (ImGui::IsItemClicked(ImGuiMouseButton_Left))
    {
        ImVec2 mouse_pos = io.MousePos;
        
        // Find closest node to mouse
        float min_distance = node_radius_ * zoom_level_;
        Vertex* closest_vertex = nullptr;
        
        for (size_t i = 0; i < vertices.size() && i < node_positions_.size(); ++i)
        {
            ImVec2 node_screen_pos = WorldToScreen(node_positions_[i]);
            float distance = sqrtf(powf(mouse_pos.x - node_screen_pos.x, 2) + 
                                 powf(mouse_pos.y - node_screen_pos.y, 2));
            
            if (distance < min_distance)
            {
                min_distance = distance;
                closest_vertex = vertices[i];
            }
        }
        
        if (closest_vertex && on_node_selected_)
        {
            on_node_selected_(closest_vertex);
        }
    }
}

void ImGuiAtomGraph::RenderNodes(const std::vector<Vertex*>& vertices, Vertex* selected_vertex, ImDrawList* draw_list)
{
    for (size_t i = 0; i < vertices.size() && i < node_positions_.size(); ++i)
    {
        Vertex* vertex = vertices[i];
        if (!vertex) continue;
        
        ImVec2 screen_pos = WorldToScreen(node_positions_[i]);
        float screen_radius = node_radius_ * zoom_level_;
        
        // Skip if outside visible area
        if (screen_pos.x + screen_radius < canvas_min_.x || 
            screen_pos.x - screen_radius > canvas_max_.x ||
            screen_pos.y + screen_radius < canvas_min_.y || 
            screen_pos.y - screen_radius > canvas_max_.y)
        {
            continue;
        }
        
        // Get color based on atom type
        ImVec4 color = GetAtomTypeColor(vertex->type);
        ImU32 fill_color = IM_COL32(color.x * 255, color.y * 255, color.z * 255, 200);
        ImU32 border_color = IM_COL32(255, 255, 255, 255);
        
        // Highlight selected vertex
        if (vertex == selected_vertex)
        {
            border_color = IM_COL32(255, 255, 0, 255);
            draw_list->AddCircleFilled(screen_pos, screen_radius + 3.0f, IM_COL32(255, 255, 0, 100));
        }
        
        // Draw node
        draw_list->AddCircleFilled(screen_pos, screen_radius, fill_color);
        draw_list->AddCircle(screen_pos, screen_radius, border_color, 0, 2.0f);
        
        // Draw label if zoomed in enough
        if (zoom_level_ > 0.5f)
        {
            std::string label = vertex->name;
            if (label.empty()) label = "Node";
            if (label.length() > 10) label = label.substr(0, 10) + "...";
            
            ImVec2 text_size = ImGui::CalcTextSize(label.c_str());
            ImVec2 text_pos = ImVec2(screen_pos.x - text_size.x * 0.5f, 
                                   screen_pos.y + screen_radius + 5.0f);
            
            draw_list->AddText(text_pos, IM_COL32(255, 255, 255, 255), label.c_str());
        }
    }
}

void ImGuiAtomGraph::RenderConnections(const std::vector<Vertex*>& vertices, ImDrawList* draw_list)
{
    // Simple approach: draw connections between nodes that share similar properties
    // In a real implementation, this would use the actual atom relationships
    
    for (size_t i = 0; i < vertices.size() && i < node_positions_.size(); ++i)
    {
        for (size_t j = i + 1; j < vertices.size() && j < node_positions_.size(); ++j)
        {
            Vertex* v1 = vertices[i];
            Vertex* v2 = vertices[j];
            
            if (!v1 || !v2) continue;
            
            // Simple heuristic: connect nodes of similar types or with similar names
            bool should_connect = false;
            if (v1->type == v2->type && v1->type != 0)
            {
                should_connect = true;
            }
            else if (!v1->name.empty() && !v2->name.empty() && 
                     v1->name.find(v2->name.substr(0, 3)) != std::string::npos)
            {
                should_connect = true;
            }
            
            if (should_connect)
            {
                ImVec2 pos1 = WorldToScreen(node_positions_[i]);
                ImVec2 pos2 = WorldToScreen(node_positions_[j]);
                
                // Check if connection is visible
                if ((pos1.x < canvas_min_.x && pos2.x < canvas_min_.x) ||
                    (pos1.x > canvas_max_.x && pos2.x > canvas_max_.x) ||
                    (pos1.y < canvas_min_.y && pos2.y < canvas_min_.y) ||
                    (pos1.y > canvas_max_.y && pos2.y > canvas_max_.y))
                {
                    continue;
                }
                
                draw_list->AddLine(pos1, pos2, IM_COL32(100, 100, 100, 150), link_thickness_);
            }
        }
    }
}

void ImGuiAtomGraph::CalculateLayout(const std::vector<Vertex*>& vertices)
{
    node_positions_.clear();
    node_positions_.reserve(vertices.size());
    
    if (vertices.empty()) return;
    
    // Simple circular layout for now
    ImVec2 center(0.0f, 0.0f);
    float radius = 150.0f;
    
    if (vertices.size() == 1)
    {
        node_positions_.push_back(center);
    }
    else
    {
        float angle_step = 2.0f * 3.14159f / vertices.size();
        for (size_t i = 0; i < vertices.size(); ++i)
        {
            float angle = i * angle_step;
            ImVec2 pos(center.x + cos(angle) * radius, 
                      center.y + sin(angle) * radius);
            node_positions_.push_back(pos);
        }
    }
}

ImVec2 ImGuiAtomGraph::WorldToScreen(const ImVec2& world_pos)
{
    ImVec2 canvas_center = ImVec2((canvas_min_.x + canvas_max_.x) * 0.5f,
                                 (canvas_min_.y + canvas_max_.y) * 0.5f);
    
    ImVec2 scaled_pos = ImVec2(world_pos.x * zoom_level_, world_pos.y * zoom_level_);
    return ImVec2(canvas_center.x + scaled_pos.x + pan_offset_.x,
                  canvas_center.y + scaled_pos.y + pan_offset_.y);
}

ImVec2 ImGuiAtomGraph::ScreenToWorld(const ImVec2& screen_pos)
{
    ImVec2 canvas_center = ImVec2((canvas_min_.x + canvas_max_.x) * 0.5f,
                                 (canvas_min_.y + canvas_max_.y) * 0.5f);
    
    ImVec2 centered_pos = ImVec2(screen_pos.x - canvas_center.x - pan_offset_.x,
                                screen_pos.y - canvas_center.y - pan_offset_.y);
    return ImVec2(centered_pos.x / zoom_level_, centered_pos.y / zoom_level_);
}

ImVec4 ImGuiAtomGraph::GetAtomTypeColor(int type_id)
{
    // Color mapping based on type ID
    float hue = (type_id * 137.5f) / 360.0f;
    hue = hue - floor(hue);
    
    // Convert HSV to RGB
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
        default: r = g = b = 0.5f; break;
    }
    
    return ImVec4(r * 0.7f + 0.3f, g * 0.7f + 0.3f, b * 0.7f + 0.3f, 1.0f);
}