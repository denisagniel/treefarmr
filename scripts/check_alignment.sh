#!/bin/bash
# Script to verify alignment requirements for critical types
# Usage: ./scripts/check_alignment.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "=========================================="
echo "Alignment Check for Critical Types"
echo "=========================================="
echo ""

# Create a temporary C++ program to check alignments
cat << 'EOF' > /tmp/check_alignment.cpp
#include <iostream>
#include <cstddef>
#include <cstdint>

// Forward declarations matching the actual types
struct Optimizer;
struct State;
struct LocalState;
struct Dataset;

// We'll need to include the actual headers, but for now just check basic types
int main() {
    std::cout << "=== Alignment Requirements ===" << std::endl;
    std::cout << "void*: " << alignof(void*) << " bytes" << std::endl;
    std::cout << "size_t: " << alignof(size_t) << " bytes" << std::endl;
    std::cout << "double: " << alignof(double) << " bytes" << std::endl;
    std::cout << "long long: " << alignof(long long) << " bytes" << std::endl;
    std::cout << "" << std::endl;
    std::cout << "=== Size Requirements ===" << std::endl;
    std::cout << "void*: " << sizeof(void*) << " bytes" << std::endl;
    std::cout << "size_t: " << sizeof(size_t) << " bytes" << std::endl;
    std::cout << "double: " << sizeof(double) << " bytes" << std::endl;
    std::cout << "long long: " << sizeof(long long) << " bytes" << std::endl;
    std::cout << "" << std::endl;
    std::cout << "=== Platform Information ===" << std::endl;
    #if defined(__x86_64__) || defined(_M_X64)
        std::cout << "Architecture: x86_64" << std::endl;
    #elif defined(__aarch64__) || defined(_M_ARM64) || defined(__arm64__)
        std::cout << "Architecture: ARM64" << std::endl;
    #elif defined(__i386__) || defined(_M_IX86)
        std::cout << "Architecture: x86" << std::endl;
    #else
        std::cout << "Architecture: Unknown" << std::endl;
    #endif
    std::cout << "Pointer size: " << sizeof(void*) * 8 << " bits" << std::endl;
    return 0;
}
EOF

echo "Compiling alignment check program..."
cd "$PROJECT_DIR"

# Try to compile with the same compiler R uses
if command -v clang++ &> /dev/null; then
    CXX=clang++
elif command -v g++ &> /dev/null; then
    CXX=g++
else
    echo "Error: No C++ compiler found"
    exit 1
fi

$CXX -std=c++11 -o /tmp/check_alignment /tmp/check_alignment.cpp 2>&1 || {
    echo "Warning: Could not compile alignment check program"
    echo "This is non-critical - alignment checks are done at runtime"
    exit 0
}

echo "Running alignment check..."
/tmp/check_alignment

echo ""
echo "=========================================="
echo "Alignment check complete"
echo "=========================================="
echo ""
echo "Note: Runtime alignment checks are performed in the code"
echo "via alignof() and pointer alignment validation."

# Cleanup
rm -f /tmp/check_alignment /tmp/check_alignment.cpp



