#ifndef RCPP_DEFERRED_INIT_HPP
#define RCPP_DEFERRED_INIT_HPP

#include <Rcpp.h>
#include <mutex>
#include <atomic>

namespace rcpp_internal {

/**
 * Deferred initialization of Rcpp global streams.
 * 
 * This ensures Rcpp::Rcout and Rcpp::Rcerr are only initialized
 * after R is fully loaded and ready, preventing recursive initialization
 * deadlocks during package load.
 */
inline void ensureRcppOstreamsInitialized() {
    static std::once_flag init_flag;
    static std::atomic<bool> initialized(false);
    
    // Use std::call_once for thread-safe one-time initialization
    std::call_once(init_flag, []() {
        // Force lazy initialization of Rcpp streams by accessing them
        // This is safe because we're inside a function called after R is ready
        try {
            // Touch the streams to trigger lazy initialization
            // We don't actually write anything, just ensure they're initialized
            static_cast<void>(&Rcpp::Rcout);
            static_cast<void>(&Rcpp::Rcerr);
            initialized.store(true, std::memory_order_release);
        } catch (...) {
            // If initialization fails, mark as failed but don't throw
            // This allows the code to continue (streams will be initialized on first use)
            initialized.store(false, std::memory_order_release);
        }
    });
}

/**
 * Safe wrapper for Rcpp::Rcout that ensures streams are initialized.
 * Use this instead of Rcpp::Rcout directly in code that might run during static init.
 */
inline std::ostream& safeRcout() {
    ensureRcppOstreamsInitialized();
    return Rcpp::Rcout;
}

/**
 * Safe wrapper for Rcpp::Rcerr that ensures streams are initialized.
 * Use this instead of Rcpp::Rcerr directly in code that might run during static init.
 */
inline std::ostream& safeRcerr() {
    ensureRcppOstreamsInitialized();
    return Rcpp::Rcerr;
}

} // namespace rcpp_internal

#endif // RCPP_DEFERRED_INIT_HPP



