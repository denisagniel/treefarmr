#!/usr/bin/env python3
"""
Post-processing script to fix RcppExports.cpp after Rcpp::compileAttributes() runs.
This prevents Rcpp global stream initialization from causing installation hangs.
"""

import sys
import os
import re

RCPP_EXPORTS_FILE = "src/RcppExports.cpp"

def fix_rcpp_exports():
    if not os.path.exists(RCPP_EXPORTS_FILE):
        print(f"Warning: {RCPP_EXPORTS_FILE} not found", file=sys.stderr)
        return 0
    
    with open(RCPP_EXPORTS_FILE, 'r') as f:
        content = f.read()
    
    # Check if fix is already applied
    if "RCPP_USE_GLOBAL_ROSTREAM defined - skipping global stream init" in content:
        print(f"Fix already applied to {RCPP_EXPORTS_FILE}")
        return 0
    
    # Check if problematic code exists
    if "Rcpp::Rcpp_cout_get()" not in content:
        print(f"No Rcpp global stream initialization found (may already be fixed)")
        return 0
    
    print(f"Applying fix to {RCPP_EXPORTS_FILE} to prevent installation hang...")
    
    # Pattern to match the problematic block
    pattern = r'#ifdef RCPP_USE_GLOBAL_ROSTREAM\s+Rcpp::Rostream<true>&\s+Rcpp::Rcout\s*=\s*Rcpp::Rcpp_cout_get\(\);\s+Rcpp::Rostream<false>&\s*Rcpp::Rcerr\s*=\s*Rcpp::Rcpp_cerr_get\(\);\s+#endif'
    
    # Replacement with fix
    replacement = '''#ifdef RCPP_USE_GLOBAL_ROSTREAM
// FIX: Prevent Rcpp global stream initialization hang
// We skip initializing global streams at static init time to prevent recursive initialization deadlocks
static void __attribute__((constructor)) rcpp_streams_skipped() {
    fprintf(stderr, "[TREEFARMR] RCPP_USE_GLOBAL_ROSTREAM defined - skipping global stream init\\n");
    fflush(stderr);
}
// NOTE: We do NOT initialize Rcpp::Rcout and Rcpp::Rcerr here to prevent static init hang
// Rcpp will create streams lazily when Rcpp::Rcout is first used
#endif'''
    
    # Try to replace using regex
    new_content = re.sub(
        r'#ifdef RCPP_USE_GLOBAL_ROSTREAM\s+Rcpp::Rostream<true>&\s+Rcpp::Rcout\s*=\s*Rcpp::Rcpp_cout_get\(\);\s+Rcpp::Rostream<false>&\s*Rcpp::Rcerr\s*=\s*Rcpp::Rcpp_cerr_get\(\);\s+#endif',
        replacement,
        content,
        flags=re.MULTILINE | re.DOTALL
    )
    
    # If regex didn't match, try line-by-line replacement
    if new_content == content:
        lines = content.split('\n')
        new_lines = []
        i = 0
        while i < len(lines):
            if lines[i].strip() == '#ifdef RCPP_USE_GLOBAL_ROSTREAM':
                # Found the start of the block
                new_lines.append('#ifdef RCPP_USE_GLOBAL_ROSTREAM')
                # Skip the next lines until #endif
                i += 1
                while i < len(lines) and lines[i].strip() != '#endif':
                    # Skip Rcpp stream initialization lines
                    if 'Rcpp::Rcpp_cout_get()' not in lines[i] and 'Rcpp::Rcpp_cerr_get()' not in lines[i]:
                        # Keep other lines if any
                        pass
                    i += 1
                # Add our fix before #endif
                new_lines.append('// FIX: Prevent Rcpp global stream initialization hang')
                new_lines.append('// We skip initializing global streams at static init time to prevent recursive initialization deadlocks')
                new_lines.append('static void __attribute__((constructor)) rcpp_streams_skipped() {')
                new_lines.append('    fprintf(stderr, "[TREEFARMR] RCPP_USE_GLOBAL_ROSTREAM defined - skipping global stream init\\n");')
                new_lines.append('    fflush(stderr);')
                new_lines.append('}')
                new_lines.append('// NOTE: We do NOT initialize Rcpp::Rcout and Rcpp::Rcerr here to prevent static init hang')
                new_lines.append('// Rcpp will create streams lazily when Rcpp::Rcout is first used')
                new_lines.append('#endif')
                i += 1
            else:
                new_lines.append(lines[i])
                i += 1
        new_content = '\n'.join(new_lines)
    
    # Add necessary includes if not present
    if '#include <cstdio>' not in new_content and '#include <Rcpp.h>' in new_content:
        new_content = new_content.replace(
            '#include <Rcpp.h>',
            '#include <Rcpp.h>\n#include <cstdio>'
        )
    
    # CRITICAL FIX: Ensure RCPP_USE_GLOBAL_ROSTREAM is undefined before Rcpp.h is included
    # This prevents Rcpp from generating static initializers that cause installation hangs
    if '#include <Rcpp.h>' in new_content and 'RCPP_USE_GLOBAL_ROSTREAM_DISABLED' not in new_content:
        # Insert the fix before the first #include <Rcpp.h>
        fix_block = '''// CRITICAL FIX: Prevent Rcpp global stream initialization during static init
// Undefine RCPP_USE_GLOBAL_ROSTREAM before including Rcpp.h to prevent
// Rcpp from generating static initializers that cause installation hangs
#ifdef RCPP_USE_GLOBAL_ROSTREAM
#undef RCPP_USE_GLOBAL_ROSTREAM
#define RCPP_USE_GLOBAL_ROSTREAM_DISABLED
#endif

'''
        new_content = new_content.replace('#include <Rcpp.h>', fix_block + '#include <Rcpp.h>', 1)
    
    # Write the fixed content
    with open(RCPP_EXPORTS_FILE, 'w') as f:
        f.write(new_content)
    
    print("Fix applied successfully")
    return 0

if __name__ == '__main__':
    sys.exit(fix_rcpp_exports())

