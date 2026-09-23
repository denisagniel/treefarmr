#!/bin/bash
# Enhanced LLDB debugging script for SIGBUS crash in GOSDT::fit()
# Usage: ./scripts/debug_crash.sh [test_script.R]

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
TEST_SCRIPT="${1:-tests/testthat/test-memory-r-profiling.R}"

echo "=========================================="
echo "LLDB Debug Script for SIGBUS Crash"
echo "=========================================="
echo "Project directory: $PROJECT_DIR"
echo "Test script: $TEST_SCRIPT"
echo ""

# Create comprehensive LLDB command file
cat << 'EOF' > /tmp/lldb_commands.txt
# LLDB commands for debugging SIGBUS crash in GOSDT::fit()

# Set up signal handling
process handle SIGSEGV stop print
process handle SIGBUS stop print
process handle SIGABRT stop print
process handle SIGILL stop print

# Enable verbose output
settings set frame-format "frame #${frame.index}: ${frame.pc}{ ${module.file.basename}{${function.name-with-args}}}{ at ${line.file.basename}:${line.number}}\\n"
settings set thread-format "thread #${thread.index}: tid = ${thread.id}{, ${frame.pc}}{ ${module.file.basename}{${function.name-with-args}}}{ at ${line.file.basename}:${line.number}}\\n"

# Set breakpoints at critical locations
# Entry of GOSDT::fit(std::istream &, std::string &)
breakpoint set --file gosdt.cpp --line 93
breakpoint command add
  echo "=== BREAKPOINT: GOSDT::fit(std::istream &, std::string &) entry ===\n"
  frame variable
  print data_source
  continue
DONE

# Entry of GOSDT::fit(std::istream &, results_t &, std::unordered_set<Model> &)
breakpoint set --file gosdt.cpp --line 273
breakpoint command add
  echo "=== BREAKPOINT: GOSDT::fit(3-arg) entry ===\n"
  frame variable
  continue
DONE

# Optimizer::load() entry
breakpoint set --file optimizer.cpp --line 24
breakpoint command add
  echo "=== BREAKPOINT: Optimizer::load() entry ===\n"
  frame variable
  print data_source
  print Configuration::worker_limit
  continue
DONE

# State::initialize() entry
breakpoint set --file state.cpp --line 12
breakpoint command add
  echo "=== BREAKPOINT: State::initialize() entry ===\n"
  frame variable
  print workers
  continue
DONE

# Critical point where crash occurs (line 124 in gosdt.cpp)
breakpoint set --file gosdt.cpp --line 124
breakpoint command add
  echo "=== BREAKPOINT: About to call fit(data_source, results, models) ===\n"
  frame variable
  print data_source
  print pos
  print serialization_state
  print temp_optimizer
  continue
DONE

# When crash occurs, print comprehensive information
# This will be triggered automatically on SIGBUS
EOF

# Add crash handler commands
cat << 'EOF' >> /tmp/lldb_commands.txt

# Define command to run when crash occurs
command script add -f crash_handler crash_handler
def crash_handler(frame, bp_loc, dict):
    """Handler function for crashes"""
    print("\n" + "="*60)
    print("CRASH DETECTED - Collecting diagnostic information")
    print("="*60 + "\n")
    
    # Print full backtrace
    print("=== FULL BACKTRACE ===")
    lldb.debugger.HandleCommand("bt")
    
    # Print registers
    print("\n=== REGISTERS ===")
    lldb.debugger.HandleCommand("register read")
    
    # Print current frame variables
    print("\n=== CURRENT FRAME VARIABLES ===")
    lldb.debugger.HandleCommand("frame variable")
    
    # Print memory around crash point
    print("\n=== MEMORY AROUND CRASH POINT ===")
    lldb.debugger.HandleCommand("x/16x $pc")
    
    # Print disassembly
    print("\n=== DISASSEMBLY ===")
    lldb.debugger.HandleCommand("disassemble --frame")
    
    # Check alignment
    print("\n=== ALIGNMENT CHECK ===")
    lldb.debugger.HandleCommand("p (void*)$pc")
    lldb.debugger.HandleCommand("p ((unsigned long)$pc) % 8")
    
    print("\n" + "="*60)
    print("End of crash diagnostics")
    print("="*60 + "\n")

# Set breakpoint on SIGBUS
breakpoint set --name __abort
breakpoint command add
  crash_handler
  bt
  frame select 0
  frame variable
  register read
  x/16x $pc
  disassemble --frame
DONE

# Run the test
run -e "testthat::test_dir('tests/testthat', filter = 'memory-r-profiling')"

# If we get here without crashing, print success
echo "Test completed without crash"
bt
EOF

echo "LLDB commands written to /tmp/lldb_commands.txt"
echo ""
echo "To use:"
echo "  1. lldb R"
echo "  2. (lldb) command source /tmp/lldb_commands.txt"
echo "  3. The script will automatically set breakpoints and run the test"
echo ""
echo "Alternative: Run R with lldb directly:"
echo "  lldb -- Rscript -e \"testthat::test_dir('tests/testthat', filter = 'memory-r-profiling')\""
echo ""
echo "Manual breakpoint commands:"
echo "  (lldb) breakpoint set --file gosdt.cpp --line 93"
echo "  (lldb) breakpoint set --file gosdt.cpp --line 124"
echo "  (lldb) breakpoint set --file optimizer.cpp --line 24"
echo "  (lldb) breakpoint set --file state.cpp --line 12"
echo "  (lldb) run"
