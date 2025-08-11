/*
 * opencog/visualization/imgui-visualizer/AtomSpaceImGuiInterface.h
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

#ifndef ATOMSPACEIMGUIINTERFACE_H
#define ATOMSPACEIMGUIINTERFACE_H

#include <vector>
#include <string>
#include <memory>
#include "imgui.h"
#include "AtomSpaceInterface.h"
#include "AtomTypes.h"
#include "Vertex.h"

/**
 * ImGui interface wrapper for OpenCog AtomSpace visualization
 * Provides modern ImGui-based UI for exploring and manipulating AtomSpace data
 */
class AtomSpaceImGuiInterface
{
public:
    AtomSpaceImGuiInterface();
    virtual ~AtomSpaceImGuiInterface();
    
    // Main rendering function for the AtomSpace window
    void RenderAtomSpaceWindow(bool* p_open);
    
    // Connection management
    bool IsConnected() const { return is_connected_; }
    void SetServerAddress(const std::string& server);
    bool TestConnection();
    
private:
    // Core OpenCog components
    std::unique_ptr<AtomTypes> atom_types_;
    std::unique_ptr<AtomSpaceInterface> atomspace_interface_;
    
    // Connection state
    bool is_connected_;
    std::string server_address_;
    
    // UI state
    bool show_search_panel_;
    bool show_atom_details_;
    bool show_graph_view_;
    bool auto_refresh_;
    float refresh_interval_;
    float last_refresh_time_;
    
    // Search and filter state
    char search_name_buffer_[256];
    char search_uuid_buffer_[256];
    int selected_atom_type_;
    bool include_subtypes_;
    int sort_order_;
    int max_results_;
    
    // Current data
    std::vector<Vertex*> search_results_;
    std::vector<Vertex*> connected_atoms_;
    Vertex* selected_vertex_;
    
    // UI rendering methods
    void RenderConnectionPanel();
    void RenderSearchPanel();
    void RenderResultsPanel();
    void RenderAtomDetailsPanel();
    void RenderGraphView();
    void RenderStatusBar();
    
    // Data management
    void PerformSearch();
    void UpdateConnectedAtoms();
    void RefreshData();
    void ClearResults();
    
    // Helper methods
    void DisplayAtomInfo(Vertex* vertex);
    void DisplayTruthValue(Vertex* vertex);
    void DisplayAtomConnections(Vertex* vertex);
    const char* GetAtomTypeName(int type_id);
    ImVec4 GetAtomTypeColor(int type_id);
    
    // Graph visualization helpers
    void RenderAtomNode(Vertex* vertex, const ImVec2& pos);
    void RenderAtomLink(Vertex* from, Vertex* to);
    void CalculateGraphLayout();
    
    // Constants
    static const int MAX_SEARCH_RESULTS = 1000;
    static const float DEFAULT_REFRESH_INTERVAL = 5.0f;
};

#endif // ATOMSPACEIMGUIINTERFACE_H