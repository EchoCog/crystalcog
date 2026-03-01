#!/usr/bin/env node

/**
 * Roadmap Validation Script for CrystalCog Development Roadmap
 * 
 * This script validates the structure and content of DEVELOPMENT-ROADMAP.md
 * to ensure it can be properly parsed by the GitHub workflow that generates
 * issues from roadmap items.
 * 
 * Usage: node scripts/validate-roadmap.js [ROADMAP_FILE]
 * 
 * Enhanced to use the unified roadmap parser module for consistency.
 */

const fs = require('fs');
const path = require('path');

// Try to load the unified parser module
let parser;
try {
  parser = require('../.github/scripts/roadmap-parser.js');
} catch (e) {
  // Fallback to relative path for different working directories
  try {
    parser = require('./.github/scripts/roadmap-parser.js');
  } catch (e2) {
    console.log('Note: Unified parser module not found, using built-in parsing');
    parser = null;
  }
}

const ROADMAP_FILE = process.argv[2] || process.env.ROADMAP_FILE || 'DEVELOPMENT-ROADMAP.md';

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
  
  // Check for required sections (support both formats)
  const hasNextSteps = content.includes('## Next Steps') || content.includes('## Next Development Steps');
  if (!hasNextSteps) {
    issues.push('Missing required "## Next Steps" or "## Next Development Steps" section');
  }
  
  if (!content.includes('### Immediate Actions') && !content.includes('**Immediate (Week 1-2)**')) {
    issues.push('Missing required "### Immediate Actions" subsection or immediate timeline section');
  }
  
  if (!content.includes('### Success Metrics')) {
    warnings.push('No "### Success Metrics" section found (optional but recommended)');
  }
  
  // Check for proper section structure (support both formats)
  const nextStepsMatch = content.match(/## Next (?:Development )?Steps\n\n([\s\S]*?)(?=\n## |$)/);
  if (nextStepsMatch) {
    const nextStepsContent = nextStepsMatch[1];
    const subsections = nextStepsContent.match(/### [^#\n]+/g) || nextStepsContent.match(/\d+\.\s\*\*[^*]+\*\*/g);
    
    if (subsections && subsections.length > 0) {
      log(colors.green, `✓ Found ${subsections.length} subsections in Next Steps`);
    } else {
      warnings.push('No subsections found in Next Steps section');
    }
  }
  
  return { issues, warnings };
}

function parseTasksFromText(tasksText, itemData, parseResults) {
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
}

function parseAndValidateItems(content) {
  // Use unified parser if available
  if (parser) {
    const parsed = parser.parseRoadmap(content);
    
    // Calculate total items by counting items in each section
    let totalItems = 0;
    for (const section of parsed.sections) {
      if (parsed.metadata.format === 'numbered-timeline') {
        // Timeline format: each section counts as 1 item
        totalItems += 1;
      } else if (section.items) {
        // Subsection format: count actual items array
        totalItems += section.items.length;
      }
    }
    
    // Convert parser result to expected format for generateReport
    const parseResults = {
      totalSections: parsed.stats.totalSections,
      totalItems: totalItems,
      totalTasks: parsed.stats.totalTasks,
      completedTasks: parsed.stats.completedTasks,
      incompleteTasks: parsed.stats.incompleteTasks,
      formatBreakdown: parsed.stats.formatBreakdown,
      sections: []
    };
    
    // Convert sections to expected format
    for (const section of parsed.sections) {
      if (parsed.metadata.format === 'numbered-timeline') {
        // Timeline format has tasks directly on section
        parseResults.sections.push({
          title: section.title,
          items: [{
            number: section.number,
            title: section.title,
            tasks: section.tasks
          }]
        });
      } else {
        // Subsection format has items array
        parseResults.sections.push({
          title: section.title,
          items: section.items || []
        });
      }
    }
    
    return parseResults;
  }
  
  // Fallback to built-in parsing if parser module is not available
  const parseResults = {
    totalSections: 0,
    totalItems: 0,
    totalTasks: 0,
    completedTasks: 0,
    incompleteTasks: 0,
    formatBreakdown: { checkbox: 0, checkmark: 0, plain: 0 },
    sections: []
  };
  
  // Find "Next Steps" or "Next Development Steps" section
  const nextStepsMatch = content.match(/## Next (?:Development )?Steps\n\n([\s\S]*?)(?=\n## |$)/);
  if (!nextStepsMatch) {
    return parseResults;
  }
  
  const nextStepsContent = nextStepsMatch[1];
  
  // Parse subsections - handle both ### format and numbered format
  let subsectionRegex = /### ([^#\n]+)\n\n([\s\S]*?)(?=\n### |\n## |$)/g;
  let subsectionMatch;
  let foundSubsections = false;
  
  while ((subsectionMatch = subsectionRegex.exec(nextStepsContent)) !== null) {
    foundSubsections = true;
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
      parseTasksFromText(tasksText, itemData, parseResults);
      sectionData.items.push(itemData);
    }
    
    parseResults.sections.push(sectionData);
  }
  
  // If no ### subsections found, try direct numbered format (for AGENT-ZERO-GENESIS.md)
  if (!foundSubsections) {
    const directItemRegex = /(\d+)\.\s\*\*([^*:]+)[^:]*\*\*:\s*\n((?:\s*-\s[^\n]*\n?)*)/g;
    let directItemMatch;
    
    while ((directItemMatch = directItemRegex.exec(nextStepsContent)) !== null) {
      parseResults.totalSections++;
      const [, itemNumber, sectionTitle, tasksText] = directItemMatch;
      
      const sectionData = {
        title: sectionTitle.trim(),
        items: [{
          number: parseInt(itemNumber),
          title: sectionTitle.trim(),
          tasks: []
        }]
      };
      
      // Parse different task formats
      parseTasksFromText(tasksText, sectionData.items[0], parseResults);
      parseResults.sections.push(sectionData);
      parseResults.totalItems++;
    }
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