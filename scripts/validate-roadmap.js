#!/usr/bin/env node

/**
 * Roadmap Validation Script for CrystalCog Development Roadmap
 * 
 * This script validates the structure and content of DEVELOPMENT-ROADMAP.md
 * to ensure it can be properly parsed by the GitHub workflow that generates
 * issues from roadmap items.
 * 
 * Usage: node scripts/validate-roadmap.js
 */

const fs = require('fs');
const path = require('path');

const ROADMAP_FILE = 'DEVELOPMENT-ROADMAP.md';

// Colors for console output
const colors = {
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  reset: '\x1b[0m',
  bold: '\x1b[1m'
};

function log(color, message) {
  console.log(`${color}${message}${colors.reset}`);
}

function validateRoadmapStructure(content) {
  const issues = [];
  const warnings = [];
  
  // Check for required sections
  if (!content.includes('## Next Steps')) {
    issues.push('Missing required "## Next Steps" section');
  }
  
  if (!content.includes('### Immediate Actions')) {
    issues.push('Missing required "### Immediate Actions" subsection');
  }
  
  if (!content.includes('### Success Metrics')) {
    warnings.push('No "### Success Metrics" section found (optional but recommended)');
  }
  
  // Check for proper section structure
  const nextStepsMatch = content.match(/## Next Steps\n\n([\s\S]*?)(?=\n## |$)/);
  if (nextStepsMatch) {
    const nextStepsContent = nextStepsMatch[1];
    const subsections = nextStepsContent.match(/### [^#\n]+/g);
    
    if (subsections && subsections.length > 0) {
      log(colors.green, `✓ Found ${subsections.length} subsections in Next Steps`);
    } else {
      warnings.push('No subsections found in Next Steps section');
    }
  }
  
  return { issues, warnings };
}

function parseAndValidateItems(content) {
  const parseResults = {
    totalSections: 0,
    totalItems: 0,
    totalTasks: 0,
    completedTasks: 0,
    incompleteTasks: 0,
    formatBreakdown: { checkbox: 0, checkmark: 0, plain: 0 },
    sections: []
  };
  
  // Find "Next Steps" section
  const nextStepsMatch = content.match(/## Next Steps\n\n([\s\S]*?)(?=\n## |$)/);
  if (!nextStepsMatch) {
    return parseResults;
  }
  
  const nextStepsContent = nextStepsMatch[1];
  
  // Parse subsections
  const subsectionRegex = /### ([^#\n]+)\n\n([\s\S]*?)(?=\n### |\n## |$)/g;
  let subsectionMatch;
  
  while ((subsectionMatch = subsectionRegex.exec(nextStepsContent)) !== null) {
    const [, sectionTitle, sectionContent] = subsectionMatch;
    
    // Skip non-actionable sections
    if (sectionTitle.includes('Summary') || sectionTitle.includes('Resources') || 
        sectionTitle.includes('Metrics') || sectionTitle.includes('Workflow')) {
      continue;
    }
    
    parseResults.totalSections++;
    const sectionData = {
      title: sectionTitle.trim(),
      items: []
    };
    
    // Parse numbered items
    const itemRegex = /(\d+)\.\s\*\*([^*]+)\*\*[^\n]*\n((?:\s*-\s[^\n]*\n?)*)/g;
    let itemMatch;
    
    while ((itemMatch = itemRegex.exec(sectionContent)) !== null) {
      const [, itemNumber, itemTitle, tasksText] = itemMatch;
      parseResults.totalItems++;
      
      const itemData = {
        number: parseInt(itemNumber),
        title: itemTitle.trim(),
        tasks: []
      };
      
      // Parse different task formats
      const tasks = [];
      
      // Standard markdown checkboxes
      const taskRegex = /\s*-\s\[([^\]]*)\]\s(.+)/g;
      let taskMatch;
      while ((taskMatch = taskRegex.exec(tasksText)) !== null) {
        const [, status, description] = taskMatch;
        tasks.push({
          completed: status.trim() === 'x',
          description: description.trim(),
          format: 'checkbox'
        });
        parseResults.formatBreakdown.checkbox++;
      }
      
      // Checkmark emojis
      const checkmarkRegex = /\s*-\s✅\s(.+)/g;
      let checkmarkMatch;
      while ((checkmarkMatch = checkmarkRegex.exec(tasksText)) !== null) {
        const [, description] = checkmarkMatch;
        tasks.push({
          completed: true,
          description: description.trim(),
          format: 'checkmark'
        });
        parseResults.formatBreakdown.checkmark++;
      }
      
      // Plain bullet points (assumed incomplete)
      const lines = tasksText.split('\n').filter(line => line.trim());
      for (const line of lines) {
        const trimmedLine = line.trim();
        // Skip if already processed
        if (trimmedLine.match(/^\s*-\s\[.*\]/) || trimmedLine.match(/^\s*-\s✅/)) {
          continue;
        }
        const plainMatch = trimmedLine.match(/^\s*-\s(.+)/);
        if (plainMatch) {
          const [, description] = plainMatch;
          if (!description.includes('✅') && !description.includes('[x]')) {
            tasks.push({
              completed: false,
              description: description.trim(),
              format: 'plain'
            });
            parseResults.formatBreakdown.plain++;
          }
        }
      }
      
      itemData.tasks = tasks;
      parseResults.totalTasks += tasks.length;
      parseResults.completedTasks += tasks.filter(t => t.completed).length;
      parseResults.incompleteTasks += tasks.filter(t => !t.completed).length;
      
      sectionData.items.push(itemData);
    }
    
    parseResults.sections.push(sectionData);
  }
  
  return parseResults;
}

function generateReport(parseResults) {
  log(colors.bold + colors.blue, '\n📊 ROADMAP ANALYSIS REPORT');
  log(colors.blue, '=' .repeat(50));
  
  log(colors.green, `✓ Total sections: ${parseResults.totalSections}`);
  log(colors.green, `✓ Total items: ${parseResults.totalItems}`);
  log(colors.green, `✓ Total tasks: ${parseResults.totalTasks}`);
  log(colors.green, `✓ Completed tasks: ${parseResults.completedTasks}`);
  log(colors.yellow, `⚠ Incomplete tasks: ${parseResults.incompleteTasks}`);
  
  console.log('\n📝 Task Format Breakdown:');
  log(colors.blue, `  • Checkbox format [x]/[ ]: ${parseResults.formatBreakdown.checkbox}`);
  log(colors.blue, `  • Checkmark format ✅: ${parseResults.formatBreakdown.checkmark}`);
  log(colors.blue, `  • Plain format: ${parseResults.formatBreakdown.plain}`);
  
  if (parseResults.incompleteTasks > 0) {
    log(colors.yellow, `\n⚠️  ${parseResults.incompleteTasks} incomplete tasks will generate GitHub issues`);
  } else {
    log(colors.green, '\n✅ All tasks are marked as completed');
  }
  
  // Detailed section breakdown
  console.log('\n📋 Section Details:');
  parseResults.sections.forEach((section, index) => {
    const incompleteTasks = section.items.reduce((count, item) => {
      return count + item.tasks.filter(t => !t.completed).length;
    }, 0);
    
    const totalTasks = section.items.reduce((count, item) => count + item.tasks.length, 0);
    
    console.log(`\n${index + 1}. ${section.title}`);
    console.log(`   Items: ${section.items.length}, Tasks: ${totalTasks}, Incomplete: ${incompleteTasks}`);
    
    if (incompleteTasks > 0) {
      section.items.forEach(item => {
        const incomplete = item.tasks.filter(t => !t.completed);
        if (incomplete.length > 0) {
          console.log(`   └─ ${item.title}: ${incomplete.length} incomplete task(s)`);
          incomplete.forEach(task => {
            console.log(`      • ${task.description} (${task.format})`);
          });
        }
      });
    }
  });
}

// Main validation function
function validateRoadmap() {
  log(colors.bold + colors.blue, '🔍 CrystalCog Roadmap Validator');
  log(colors.blue, '=' .repeat(40));
  
  // Check if file exists
  if (!fs.existsSync(ROADMAP_FILE)) {
    log(colors.red, `❌ File not found: ${ROADMAP_FILE}`);
    log(colors.yellow, '💡 Make sure you run this script from the repository root');
    process.exit(1);
  }
  
  log(colors.green, `✓ Found roadmap file: ${ROADMAP_FILE}`);
  
  // Read and validate content
  const content = fs.readFileSync(ROADMAP_FILE, 'utf8');
  log(colors.green, `✓ File size: ${content.length} characters`);
  
  // Validate structure
  const { issues, warnings } = validateRoadmapStructure(content);
  
  if (issues.length > 0) {
    log(colors.red, '\n❌ Structure Issues:');
    issues.forEach(issue => log(colors.red, `  • ${issue}`));
  }
  
  if (warnings.length > 0) {
    log(colors.yellow, '\n⚠️  Warnings:');
    warnings.forEach(warning => log(colors.yellow, `  • ${warning}`));
  }
  
  if (issues.length === 0) {
    log(colors.green, '\n✅ Roadmap structure is valid');
  }
  
  // Parse and analyze content
  const parseResults = parseAndValidateItems(content);
  generateReport(parseResults);
  
  // Summary
  console.log('\n' + '=' .repeat(50));
  if (issues.length === 0) {
    log(colors.green, '✅ Roadmap validation completed successfully');
    if (parseResults.incompleteTasks > 0) {
      log(colors.yellow, `💡 Ready to generate ${parseResults.incompleteTasks} GitHub issues`);
    } else {
      log(colors.blue, '💡 No incomplete tasks found - no issues will be generated');
    }
  } else {
    log(colors.red, '❌ Roadmap validation failed');
    log(colors.yellow, '💡 Fix the structure issues before running the issue generation workflow');
    process.exit(1);
  }
}

// Run validation
if (require.main === module) {
  validateRoadmap();
}

module.exports = { validateRoadmap, parseAndValidateItems, validateRoadmapStructure };