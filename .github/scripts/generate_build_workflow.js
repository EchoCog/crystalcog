#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

// Adaptively discover components with build dependency analysis
// This will be populated by scanning the actual repository structure

// Template for build step
const BUILD_STEP_TEMPLATE = `      # Build and Install {{dir_name}}
      - name: Build and Install {{dir_name}}
        run: |
          # Clean existing directory
          rm -rf {{dir_name}}
          # Clone the repository
          git clone https://github.com/opencog/{{dir_name}}.git
          mkdir -p {{dir_name}}/build
          cd {{dir_name}}/build
          cmake -DCMAKE_BUILD_TYPE=Release ..
          make -j2
          sudo make install
          sudo ldconfig
          cd ../..`;

// Template for the complete workflow
const WORKFLOW_TEMPLATE = `# .github/workflows/ci-org-generalized.yml
# Auto-generated workflow for building and installing OpenCog components

name: CI Org Generalized

on:
  push:
    branches:
      - main
  pull_request:
    branches:
      - main

env:
  CCACHE_DIR: /ws/ccache
  MAKEFLAGS: -j2

jobs:
  build-and-test:
    name: Build and Test All Components
    runs-on: ubuntu-latest
    container:
      image: opencog/opencog-deps
      options: --user root
      env:
        CCACHE_DIR: /ws/ccache
        MAKEFLAGS: -j2
    services:
      opencog-postgres:
        image: opencog/postgres
        env:
          POSTGRES_USER: opencog_test
          POSTGRES_PASSWORD: cheese
          POSTGRES_DB: atomspace_db
        ports:
          - 5432:5432
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
    steps:
      # 1. Checkout the Repository
      - name: Checkout Repository
        uses: actions/checkout@v4

      # 2. Install Build Dependencies
      - name: Install Build Dependencies
        run: |
          sudo apt-get update
          sudo apt-get install -y ccache pkg-config cmake build-essential git

{{build_steps}}

{{test_steps}}

      # Upload Test Logs
      - name: Upload Test Logs
        if: always()
        uses: actions/upload-artifact@v4
        with:
          name: test-logs
          path: |
{{test_log_paths}}

      # (Optional) Package Components
      - name: Package Components
        if: github.ref == 'refs/heads/main'
        run: |
{{package_steps}}

      # Upload Build Artifacts
      - name: Upload Build Artifacts
        if: always()
        uses: actions/upload-artifact@v4
        with:
          name: build-artifacts
          path: |
{{artifact_paths}}`;

/**
 * Scan the repository to discover all components with CMakeLists.txt files
 */
function discoverComponents() {
  const rootDir = process.cwd();
  const components = [];
  
  try {
    // Get all directories that might contain components
    const entries = fs.readdirSync(rootDir, { withFileTypes: true });
    
    for (const entry of entries) {
      if (entry.isDirectory() && !entry.name.startsWith('.')) {
        const componentPath = path.join(rootDir, entry.name);
        const cmakeFile = path.join(componentPath, 'CMakeLists.txt');
        
        // Check if it has a CMakeLists.txt file (indicating it's buildable)
        if (fs.existsSync(cmakeFile)) {
          components.push(entry.name);
        }
      }
    }
    
    return components.sort();
  } catch (error) {
    console.error('Error discovering components:', error);
    return [];
  }
}

/**
 * Analyze dependency patterns from CircleCI configs and CMake files
 */
function analyzeDependencies(components) {
  const dependencyMap = new Map();
  
  // Initialize all components with empty dependencies
  components.forEach(component => {
    dependencyMap.set(component, new Set());
  });
  
  // Apply well-known dependency patterns based on OpenCog architecture
  // These are derived from CircleCI analysis but cleaned up to avoid circular deps
  const knownDependencies = {
    // Core foundation - no dependencies
    'cogutil': [],
    
    // AtomSpace layer - depends on cogutil
    'atomspace': ['cogutil'],
    
    // CogServer layer - depends on atomspace  
    'cogserver': ['cogutil', 'atomspace'],
    
    // Core reasoning components
    'unify': ['cogutil', 'atomspace'],
    'spacetime': ['cogutil', 'atomspace'],
    'ure': ['cogutil', 'atomspace', 'unify'],
    
    // Higher-level components
    'attention': ['cogutil', 'atomspace', 'cogserver'],
    'miner': ['cogutil', 'atomspace', 'unify', 'ure'],
    'pln': ['cogutil', 'atomspace', 'unify', 'ure', 'spacetime'],
    
    // Learning and MOSES
    'moses': ['cogutil'],
    'asmoses': ['cogutil', 'atomspace', 'unify', 'ure'],
    'learn': ['cogutil', 'atomspace'],
    
    // Main OpenCog - depends on most core components
    'opencog': ['cogutil', 'atomspace', 'cogserver', 'unify', 'ure', 'attention'],
    
    // Language and NLP
    'lg-atomese': ['cogutil', 'atomspace'],
    
    // AtomSpace extensions - depend on core atomspace
    'atomspace-bridge': ['cogutil', 'atomspace'],
    'atomspace-cog': ['cogutil', 'atomspace'],
    'atomspace-dht': ['cogutil', 'atomspace'],
    'atomspace-ipfs': ['cogutil', 'atomspace'],
    'atomspace-metta': ['cogutil', 'atomspace'],
    'atomspace-restful': ['cogutil', 'atomspace'],
    'atomspace-rocks': ['cogutil', 'atomspace'],
    'atomspace-rpc': ['cogutil', 'atomspace'],
    'atomspace-websockets': ['cogutil', 'atomspace'],
    'atomspace-agents': ['cogutil', 'atomspace'],
    
    // Vision and perception  
    'vision': ['cogutil', 'atomspace'],
    'perception': ['cogutil', 'atomspace'],
    
    // Specialized components
    'pattern-index': ['cogutil', 'atomspace'],
    'benchmark': ['cogutil', 'atomspace'],
    'cheminformatics': ['cogutil', 'atomspace'],
    'dimensional-embedding': ['cogutil', 'atomspace'],
    'ghost_bridge': ['cogutil', 'atomspace'],
    'sensory': ['cogutil', 'atomspace'],
    'visualization': ['cogutil', 'atomspace'],
    
    // Robot and embodiment
    'pau2motors': ['cogutil', 'atomspace'],
    'robots_config': ['cogutil', 'atomspace'],
    'ros-behavior-scripting': ['cogutil', 'atomspace'],
    'blender_api_msgs': ['cogutil', 'atomspace'],
    
    // Application specific
    'TinyCog': ['cogutil', 'atomspace'],
    'agi-bio': ['cogutil', 'atomspace'],
    'generate': ['cogutil', 'atomspace'],
    'python-attic': ['cogutil', 'atomspace']
  };
  
  // Apply known dependencies 
  components.forEach(component => {
    const dependencies = dependencyMap.get(component);
    const known = knownDependencies[component] || [];
    
    known.forEach(dep => {
      if (components.includes(dep) && dep !== component) {
        dependencies.add(dep);
      }
    });
  });
  
  return dependencyMap;
}

/**
 * Perform topological sort to determine correct build order
 */
function topologicalSort(components, dependencyMap) {
  const visited = new Set();
  const visiting = new Set();
  const result = [];
  
  function visit(component) {
    if (visiting.has(component)) {
      console.warn(`Circular dependency detected involving ${component}`);
      return;
    }
    
    if (visited.has(component)) {
      return;
    }
    
    visiting.add(component);
    
    const dependencies = dependencyMap.get(component) || new Set();
    for (const dep of dependencies) {
      if (components.includes(dep)) {
        visit(dep);
      }
    }
    
    visiting.delete(component);
    visited.add(component);
    result.push(component);
  }
  
  components.forEach(component => {
    if (!visited.has(component)) {
      visit(component);
    }
  });
  
  return result;
}

/**
 * Get the correct build sequence based on repository analysis
 */
function getBuildSequence() {
  console.log('Scanning repository for OpenCog components...');
  
  const discoveredComponents = discoverComponents();
  console.log(`Discovered ${discoveredComponents.length} components with CMakeLists.txt:`, discoveredComponents.join(', '));
  
  console.log('Analyzing dependency relationships...');
  const dependencyMap = analyzeDependencies(discoveredComponents);
  
  // Define core components that should always be included
  const coreComponents = [
    'cogutil', 'atomspace', 'cogserver', 'unify', 'ure', 'spacetime',
    'attention', 'miner', 'pln', 'asmoses', 'moses', 'opencog',
    'lg-atomese', 'learn'
  ];
  
  // Define optional components (atomspace extensions, specialized tools, etc.)
  const optionalComponents = discoveredComponents.filter(comp => 
    !coreComponents.includes(comp)
  );
  
  console.log(`Core components (${coreComponents.length}):`, coreComponents.join(', '));
  console.log(`Optional components (${optionalComponents.length}):`, optionalComponents.join(', '));
  
  // For now, build core components + some key optional ones
  const selectedComponents = [
    ...coreComponents.filter(comp => discoveredComponents.includes(comp)),
    // Add some important optional components
    'atomspace-rocks',    // RocksDB backend
    'atomspace-restful',  // REST API
    'pattern-index',      // Pattern indexing
    'vision',            // Vision processing
    'benchmark'          // Benchmarking tools
  ].filter(comp => discoveredComponents.includes(comp));
  
  // Show discovered dependencies for selected components
  console.log('Dependencies for selected components:');
  for (const component of selectedComponents) {
    const deps = dependencyMap.get(component);
    if (deps && deps.size > 0) {
      console.log(`  ${component}: [${Array.from(deps).join(', ')}]`);
    }
  }
  
  console.log('Computing optimal build sequence...');
  const buildSequence = topologicalSort(selectedComponents, dependencyMap);
  
  console.log(`Final build sequence (${buildSequence.length} components):`, buildSequence.join(' → '));
  
  return { 
    components: buildSequence, 
    dependencies: dependencyMap,
    coreComponents,
    optionalComponents,
    selectedComponents
  };
}

/**
 * Generate build steps for all components in dependency order
 */
function generateBuildSteps() {
  const { components: buildSequence } = getBuildSequence();
  
  // Generate build steps
  const buildSteps = buildSequence.map(component => {
    return BUILD_STEP_TEMPLATE.replace(/{{dir_name}}/g, component);
  }).join('\n\n');
  
  return { validComponents: buildSequence, buildSteps };
}

/**
 * Generate test steps for all valid components
 */
function generateTestSteps(validComponents) {
  const testCommands = validComponents.map(component => {
    return `          # ${component} Tests
          cd ${component}/build
          make tests
          make check ARGS="$MAKEFLAGS"
          cd ../..`;
  }).join('\n\n');
  
  return `      # Run Tests for Each Component
      - name: Run Tests
        run: |
${testCommands}`;
}

/**
 * Generate test log paths for artifact upload
 */
function generateTestLogPaths(validComponents) {
  return validComponents.map(component => 
    `            ${component}/build/Testing/Temporary/LastTest.log`
  ).join('\n');
}

/**
 * Generate package steps for components
 */
function generatePackageSteps(validComponents) {
  return validComponents.map(component => {
    return `          # ${component} Packaging
          cd ${component}/build
          make package || echo "${component} package target not defined."
          cd ../..`;
  }).join('\n\n');
}

/**
 * Generate artifact paths for build artifacts
 */
function generateArtifactPaths(validComponents) {
  return validComponents.map(component => 
    `            ${component}/build/`
  ).join('\n');
}

/**
 * Main function to generate the complete workflow
 */
function generateWorkflow() {
  console.log('Generating generalized build workflow...');
  
  const { validComponents, buildSteps } = generateBuildSteps();
  const testSteps = generateTestSteps(validComponents);
  const testLogPaths = generateTestLogPaths(validComponents);
  const packageSteps = generatePackageSteps(validComponents);
  const artifactPaths = generateArtifactPaths(validComponents);
  
  console.log(`Found ${validComponents.length} valid components: ${validComponents.join(', ')}`);
  
  const workflow = WORKFLOW_TEMPLATE
    .replace('{{build_steps}}', buildSteps)
    .replace('{{test_steps}}', testSteps)
    .replace('{{test_log_paths}}', testLogPaths)
    .replace('{{package_steps}}', packageSteps)
    .replace('{{artifact_paths}}', artifactPaths);
    
  return workflow;
}

/**
 * Save the generated workflow to file
 */
function saveWorkflow() {
  try {
    const workflow = generateWorkflow();
    const outputPath = path.join(process.cwd(), '.github', 'workflows', 'ci-org-generalized.yml');
    
    fs.writeFileSync(outputPath, workflow);
    console.log(`Generated workflow saved to: ${outputPath}`);
    console.log('Workflow generation completed successfully!');
    
    return true;
  } catch (error) {
    console.error('Error generating workflow:', error);
    return false;
  }
}

// Export functions for testing
module.exports = {
  generateWorkflow,
  generateBuildSteps,
  getBuildSequence,
  discoverComponents,
  analyzeDependencies,
  topologicalSort
};

// Run if called directly
if (require.main === module) {
  saveWorkflow();
}