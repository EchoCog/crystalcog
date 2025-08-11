/*
 * opencog/visualization/imgui-visualizer/ImGuiAtomGraph.h
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

#ifndef IMGUIATOMGRAPH_H
#define IMGUIATOMGRAPH_H

#include <vector>
#include <functional>
#include "imgui.h"
#include "Vertex.h"

/**
 * Interactive graph visualization component for AtomSpace using ImGui
 * Provides zooming, panning, and node selection capabilities
 */
class ImGuiAtomGraph
{
public:
    ImGuiAtomGraph();
    virtual ~ImGuiAtomGraph();
    
    // Main rendering function
    void Render(const std::vector<Vertex*>& vertices, Vertex* selected_vertex = nullptr);
    
    // Callback for node selection
    std::function<void(Vertex*)> on_node_selected_;
    
    // Graph interaction settings
    void SetNodeRadius(float radius) { node_radius_ = radius; }
    void SetLinkThickness(float thickness) { link_thickness_ = thickness; }
    
    // View controls
    void ResetView();
    void FitToView(const std::vector<Vertex*>& vertices);
    
private:
    // View state
    float zoom_level_;
    ImVec2 pan_offset_;
    bool is_panning_;
    ImVec2 last_mouse_pos_;
    
    // Canvas bounds
    ImVec2 canvas_min_;
    ImVec2 canvas_max_;
    
    // Visual settings
    float node_radius_;
    float link_thickness_;
    
    // Layout data
    std::vector<ImVec2> node_positions_;
    
    // Input handling
    void HandleInput();
    void HandleSelection(const std::vector<Vertex*>& vertices);
    
    // Rendering methods
    void RenderNodes(const std::vector<Vertex*>& vertices, Vertex* selected_vertex, ImDrawList* draw_list);
    void RenderConnections(const std::vector<Vertex*>& vertices, ImDrawList* draw_list);
    
    // Layout calculation
    void CalculateLayout(const std::vector<Vertex*>& vertices);
    
    // Coordinate transformation
    ImVec2 WorldToScreen(const ImVec2& world_pos);
    ImVec2 ScreenToWorld(const ImVec2& screen_pos);
    
    // Visual helpers
    ImVec4 GetAtomTypeColor(int type_id);
};

#endif // IMGUIATOMGRAPH_H