#!/bin/bash
# Script to remove deprecated RNGScope from auto-generated RcppExports.cpp
# This should be run after Rcpp::compileAttributes() regenerates the file

if [ ! -f "src/RcppExports.cpp" ]; then
    echo "Error: src/RcppExports.cpp not found"
    exit 1
fi

# Remove all RNGScope lines
sed -i.bak '/Rcpp::RNGScope rcpp_rngScope_gen;/d' src/RcppExports.cpp

# Remove backup file if sed created one
[ -f src/RcppExports.cpp.bak ] && rm src/RcppExports.cpp.bak

echo "Removed deprecated RNGScope from RcppExports.cpp"
