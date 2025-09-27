#!/bin/bash
# /scripts/generate-system-image.sh
# Agent-Zero Genesis System Image Generation Script

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
CONFIG_DIR="${PROJECT_ROOT}/config"
BUILD_DIR="${PROJECT_ROOT}/build/agent-zero"
SYSTEM_CONFIG="${CONFIG_DIR}/agent-zero-system.scm"
OUTPUT_DIR="${BUILD_DIR}/images"

print_status() {
    echo -e "${BLUE}[System Image]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[System Image]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[System Image]${NC} $1"
}

print_error() {
    echo -e "${RED}[System Image]${NC} $1"
}

# Check if Guix is available
check_guix() {
    if ! command -v guix >/dev/null 2>&1; then
        print_error "Guix package manager is required but not found"
        print_error "Please install Guix following the instructions in AGENT-ZERO-GENESIS.md"
        exit 1
    fi
    
    print_success "Guix package manager found"
}

# Validate system configuration
validate_system_config() {
    print_status "Validating system configuration..."
    
    if [[ ! -f "$SYSTEM_CONFIG" ]]; then
        print_error "System configuration not found: $SYSTEM_CONFIG"
        print_error "Please run 'make agent-zero' first to generate the configuration"
        exit 1
    fi
    
    # Basic syntax check of the Scheme configuration
    if command -v guile >/dev/null 2>&1; then
        if guile -c "(load \"$SYSTEM_CONFIG\")" 2>/dev/null; then
            print_success "System configuration syntax is valid"
        else
            print_warning "System configuration syntax check failed, but continuing..."
        fi
    else
        print_warning "Guile not available for configuration validation"
    fi
}

# Generate system image
generate_image() {
    local image_type="${1:-disk-image}"
    local output_name="${2:-agent-zero-system}"
    
    print_status "Generating Agent-Zero system image..."
    print_status "Image type: $image_type"
    print_status "Output name: $output_name"
    
    # Create output directory
    mkdir -p "$OUTPUT_DIR"
    
    # Create temporary system configuration if needed
    local temp_config="/tmp/agent-zero-system-$$.scm"
    
    if [[ "$USE_TEMP_CONFIG" == "true" ]]; then
        print_status "Creating temporary minimal system configuration..."
        cat > "$temp_config" << 'EOF'
(use-modules (gnu)
             (gnu system)
             (gnu services)
             (gnu packages))

(operating-system
  (host-name "agent-zero")
  (timezone "UTC")
  (locale "en_US.utf8")
  
  ;; Basic system configuration
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "/dev/sda")))
  
  (file-systems (cons (file-system
                        (device (file-system-label "root"))
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))
  
  ;; Basic services
  (services %base-services)
  
  ;; Core packages (minimal for bootstrapping)
  (packages (append %base-packages
                    (list))))
EOF
        config_file="$temp_config"
    else
        config_file="$SYSTEM_CONFIG"
    fi
    
    print_status "Using configuration: $config_file"
    
    # Generate the system image
    case "$image_type" in
        disk-image)
            print_status "Building disk image..."
            if guix system disk-image "$config_file" --file-system-type=ext4; then
                print_success "Disk image generation completed"
                # Find the generated image in the store
                local store_path=$(guix system disk-image "$config_file" --file-system-type=ext4 --dry-run 2>/dev/null | grep -o '/gnu/store/[^[:space:]]*')
                if [[ -n "$store_path" ]]; then
                    print_status "System image location: $store_path"
                    # Copy to our output directory with a meaningful name
                    if [[ -e "$store_path" ]]; then
                        cp "$store_path" "$OUTPUT_DIR/${output_name}.img"
                        print_success "System image copied to: $OUTPUT_DIR/${output_name}.img"
                    fi
                fi
            else
                print_error "Disk image generation failed"
                cleanup_temp_files "$temp_config"
                exit 1
            fi
            ;;
        vm-image)
            print_status "Building VM image..."
            if guix system vm-image "$config_file"; then
                print_success "VM image generation completed"
            else
                print_error "VM image generation failed"
                cleanup_temp_files "$temp_config"
                exit 1
            fi
            ;;
        iso-image)
            print_status "Building ISO image..."
            if guix system disk-image --file-system-type=iso9660 "$config_file"; then
                print_success "ISO image generation completed"
            else
                print_error "ISO image generation failed"
                cleanup_temp_files "$temp_config"
                exit 1
            fi
            ;;
        *)
            print_error "Unknown image type: $image_type"
            print_error "Supported types: disk-image, vm-image, iso-image"
            cleanup_temp_files "$temp_config"
            exit 1
            ;;
    esac
    
    cleanup_temp_files "$temp_config"
}

# Cleanup temporary files
cleanup_temp_files() {
    local temp_config="$1"
    if [[ -f "$temp_config" && "$temp_config" == "/tmp/agent-zero-system-"* ]]; then
        rm -f "$temp_config"
        print_status "Cleaned up temporary configuration"
    fi
}

# Show usage information
show_usage() {
    echo "Agent-Zero Genesis System Image Generation"
    echo ""
    echo "Usage: $0 [OPTIONS] [IMAGE_TYPE] [OUTPUT_NAME]"
    echo ""
    echo "IMAGE_TYPE:"
    echo "  disk-image    Generate a disk image (default)"
    echo "  vm-image      Generate a VM image"
    echo "  iso-image     Generate an ISO image"
    echo ""
    echo "OUTPUT_NAME:"
    echo "  Custom name for the output image (default: agent-zero-system)"
    echo ""
    echo "OPTIONS:"
    echo "  --minimal     Use minimal configuration for faster builds"
    echo "  --help        Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0                          # Generate default disk image"
    echo "  $0 vm-image agent-zero-vm   # Generate VM image with custom name"
    echo "  $0 --minimal disk-image     # Generate minimal disk image"
    echo ""
}

# Parse command line arguments
MINIMAL_CONFIG=false
IMAGE_TYPE="disk-image"
OUTPUT_NAME="agent-zero-system"

while [[ $# -gt 0 ]]; do
    case $1 in
        --minimal)
            MINIMAL_CONFIG=true
            USE_TEMP_CONFIG=true
            shift
            ;;
        --help)
            show_usage
            exit 0
            ;;
        disk-image|vm-image|iso-image)
            IMAGE_TYPE="$1"
            shift
            ;;
        -*)
            print_error "Unknown option: $1"
            show_usage
            exit 1
            ;;
        *)
            OUTPUT_NAME="$1"
            shift
            ;;
    esac
done

# Main execution
main() {
    print_status "Starting Agent-Zero system image generation..."
    
    check_guix
    validate_system_config
    generate_image "$IMAGE_TYPE" "$OUTPUT_NAME"
    
    print_success "System image generation completed successfully!"
    print_status "Output directory: $OUTPUT_DIR"
    
    # Show available images
    if [[ -d "$OUTPUT_DIR" ]]; then
        print_status "Generated images:"
        ls -la "$OUTPUT_DIR"
    fi
}

# Execute main function if script is run directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi