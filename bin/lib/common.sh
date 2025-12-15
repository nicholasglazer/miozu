#!/bin/bash
# Common utility functions for Miozu installer

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Print header with formatting
print_header() {
    echo -e "${YELLOW}\n=== $1 ===${NC}"
}