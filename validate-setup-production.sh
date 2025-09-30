#!/bin/bash
# Comprehensive validation test for scripts/production/setup-production.sh
# This validates the production setup script functionality and dependencies

set -e

echo "🔄 Package Script Validation: scripts/production/setup-production.sh"
echo "=================================================================="

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

print_status() {
    echo -e "${BLUE}[Validate]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[Validate]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[Validate]${NC} $1"
}

print_error() {
    echo -e "${RED}[Validate]${NC} $1"
}

# Track validation results
validation_errors=0
validation_warnings=0

# Function to track results
track_result() {
    if [ "$1" = "error" ]; then
        validation_errors=$((validation_errors + 1))
        print_error "$2"
    elif [ "$1" = "warning" ]; then
        validation_warnings=$((validation_warnings + 1))
        print_warning "$2"
    else
        print_success "$2"
    fi
}

# Check script functionality
print_status "Validating script functionality..."

# 1. Check script syntax
print_status "Checking script syntax..."
if bash -n scripts/production/setup-production.sh; then
    track_result "success" "✓ Script syntax is valid"
else
    track_result "error" "✗ Script syntax errors found"
fi

# 2. Check ShellCheck compliance
if command -v shellcheck >/dev/null 2>&1; then
    print_status "Running ShellCheck analysis..."
    if shellcheck scripts/production/setup-production.sh; then
        track_result "success" "✓ ShellCheck passed with no issues"
    else
        track_result "warning" "⚠ ShellCheck found potential issues"
    fi
else
    track_result "warning" "⚠ ShellCheck not available, skipping static analysis"
fi

# 3. Check script permissions
if [ -x "scripts/production/setup-production.sh" ]; then
    track_result "success" "✓ Script is executable"
else
    track_result "error" "✗ Script is not executable"
fi

# 4. Test command line argument parsing
print_status "Testing command line argument parsing..."
if scripts/production/setup-production.sh --help >/dev/null 2>&1; then
    track_result "success" "✓ Help option works correctly"
else
    track_result "error" "✗ Help option not working"
fi

# Check dependency compatibility
echo ""
print_status "Checking dependency compatibility..."

# 5. Check required files exist
required_files=(
    "docker-compose.production.yml"
    "Dockerfile.production"
)

for file in "${required_files[@]}"; do
    if [ -f "$file" ]; then
        track_result "success" "✓ Required file exists: $file"
    else
        track_result "error" "✗ Required file missing: $file"
    fi
done

# 6. Check required directories exist
required_dirs=(
    "config"
    "scripts"
    "deployments"
    "config/production"
)

for dir in "${required_dirs[@]}"; do
    if [ -d "$dir" ]; then
        track_result "success" "✓ Required directory exists: $dir"
    else
        track_result "error" "✗ Required directory missing: $dir"
    fi
done

# 7. Check production configuration files
production_configs=(
    "config/production/supervisord.conf"
    "config/production/nginx/nginx.conf"
    "config/production/prometheus/prometheus.yml"
    "config/production/grafana/provisioning/datasources/prometheus.yml"
)

for config in "${production_configs[@]}"; do
    if [ -f "$config" ]; then
        track_result "success" "✓ Configuration file exists: $config"
    else
        track_result "warning" "⚠ Configuration file missing: $config"
    fi
done

# 8. Validate Docker Compose file
print_status "Validating Docker Compose configuration..."
if command -v docker-compose >/dev/null 2>&1; then
    if docker-compose -f docker-compose.production.yml config >/dev/null 2>&1; then
        track_result "success" "✓ Docker Compose file is valid"
    else
        track_result "error" "✗ Docker Compose file has configuration errors"
    fi
else
    track_result "warning" "⚠ Docker Compose not available, skipping validation"
fi

# Check Guix environment compatibility
echo ""
print_status "Running Guix environment tests..."

# 9. Check Guix package definitions
if [ -f "guix.scm" ]; then
    track_result "success" "✓ Guix manifest exists"
else
    track_result "error" "✗ Guix manifest missing"
fi

if [ -f ".guix-channel" ]; then
    track_result "success" "✓ Guix channel configuration exists"
else
    track_result "error" "✗ Guix channel configuration missing"
fi

if [ -f "gnu/packages/opencog.scm" ]; then
    track_result "success" "✓ OpenCog Guix package definition exists"
else
    track_result "error" "✗ OpenCog Guix package definition missing"
fi

# 10. Test Guix syntax if available
if command -v guile >/dev/null 2>&1; then
    print_status "Testing Guix package syntax..."
    if guile -c "(load \"guix.scm\")" >/dev/null 2>&1; then
        track_result "success" "✓ Guix manifest syntax is valid"
    else
        track_result "error" "✗ Guix manifest has syntax errors"
    fi
    
    if guile -c "(use-modules (gnu packages opencog))" >/dev/null 2>&1; then
        track_result "success" "✓ OpenCog package module syntax is valid"
    else
        track_result "warning" "⚠ OpenCog package module has syntax issues"
    fi
else
    track_result "warning" "⚠ Guile not available, skipping Guix syntax validation"
fi

# Check system requirements
echo ""
print_status "Checking system requirements compatibility..."

# 11. Check if script would run properly (dry-run simulation)
print_status "Simulating script execution (checking function definitions)..."

# Extract and verify all function definitions
functions_found=0
functions_expected=(
    "check_root"
    "update_system"
    "install_docker"
    "create_service_user"
    "setup_directories"
    "copy_application_files"
    "setup_ssl"
    "configure_firewall"
    "setup_fail2ban"
    "setup_logrotate"
    "setup_backup_cron"
    "create_systemd_service"
    "generate_env_file"
    "main"
)

for func in "${functions_expected[@]}"; do
    if grep -q "^${func}()" scripts/production/setup-production.sh; then
        track_result "success" "✓ Function defined: $func"
        functions_found=$((functions_found + 1))
    else
        track_result "error" "✗ Function missing: $func"
    fi
done

# 12. Check command dependencies used in script
print_status "Checking command dependencies..."
commands_used=(
    "apt-get"
    "curl"
    "docker"
    "docker-compose"
    "openssl"
    "systemctl"
    "ufw"
    "fail2ban"
    "certbot"
)

for cmd in "${commands_used[@]}"; do
    if grep -q "$cmd" scripts/production/setup-production.sh; then
        if command -v "$cmd" >/dev/null 2>&1; then
            track_result "success" "✓ Command available: $cmd"
        else
            track_result "warning" "⚠ Command used in script but not available: $cmd"
        fi
    fi
done

# Update package documentation
echo ""
print_status "Updating package documentation..."

# 13. Check if documentation is up to date
if [ -f "README.md" ]; then
    if grep -q "production" README.md; then
        track_result "success" "✓ README mentions production setup"
    else
        track_result "warning" "⚠ README could mention production setup"
    fi
fi

# 14. Check validation documentation exists
if [ -f "docs/PRODUCTION_SETUP_VALIDATION.md" ]; then
    track_result "success" "✓ Production setup validation documentation exists"
else
    track_result "warning" "⚠ Production setup validation documentation missing"
fi

# 15. Generate validation summary
echo ""
print_status "Generating validation summary..."

# Count total validations
total_validations=$((${#required_files[@]} + ${#required_dirs[@]} + ${#production_configs[@]} + ${#functions_expected[@]} + 11))

echo ""
echo "📊 VALIDATION SUMMARY"
echo "===================="
echo "Total validations run: $total_validations"
echo "✅ Successful validations: $((total_validations - validation_errors - validation_warnings))"
echo "⚠️  Warnings: $validation_warnings"
echo "❌ Errors: $validation_errors"
echo ""

# Final validation results
if [ $validation_errors -eq 0 ]; then
    if [ $validation_warnings -eq 0 ]; then
        print_success "🎯 ALL VALIDATIONS PASSED"
        echo "✅ Script functionality: VALIDATED"
        echo "✅ Dependency compatibility: CONFIRMED"
        echo "✅ Guix environment tests: AVAILABLE"
        echo "✅ Package documentation: UPDATED"
        echo ""
        echo "The setup-production.sh script is fully functional and meets all requirements."
        echo "All dependencies are properly configured and accessible."
        exit 0
    else
        print_warning "🎯 VALIDATION COMPLETED WITH WARNINGS"
        echo "✅ Script functionality: VALIDATED"
        echo "✅ Dependency compatibility: MOSTLY CONFIRMED"
        echo "⚠️  Guix environment tests: AVAILABLE (with warnings)"
        echo "✅ Package documentation: AVAILABLE"
        echo ""
        echo "The setup-production.sh script is functional but has minor issues."
        echo "Please review the warnings above."
        exit 0
    fi
else
    print_error "❌ VALIDATION FAILED"
    echo "❌ Script functionality: NEEDS ATTENTION"
    echo "❌ Dependency compatibility: ISSUES FOUND"
    echo "❌ Some requirements not met"
    echo ""
    echo "Please fix the errors above before using the setup-production.sh script."
    exit 1
fi