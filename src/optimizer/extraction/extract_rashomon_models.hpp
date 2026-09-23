// Extract individual Model objects from Rashomon set for R serialization
// This complements rash_models.hpp which extracts compact ModelSet structures for file output
//
// Uses models_inner()'s scope parameter: when scope > 0, it filters by scope instead of
// task.upperbound(), allowing extraction of all trees within the Rashomon bound.

void Optimizer::extract_rashomon_models(std::unordered_set<Model> & results, float rashomon_bound) {
    results.clear();

    // Save and temporarily clear rashomon_flag so models() doesn't assert
    bool saved_flag = rashomon_flag;
    rashomon_flag = false;

    // Call models() with scope=rashomon_bound
    // When scope > 0, models_inner() uses scope for filtering instead of task.upperbound()
    std::unordered_set<std::shared_ptr<Model>> local_results;
    models(this->root, local_results, rashomon_bound);

    // Restore rashomon_flag
    rashomon_flag = saved_flag;

    if (model_limit_exceeded) {
        results.clear();
        return;
    }

    // Copy shared_ptr<Model> to Model objects
    for (auto it = local_results.begin(); it != local_results.end(); ++it) {
        results.insert(**it);
    }

    if (Configuration::verbose) {
        std::cout << "Extracted " << results.size() << " Model objects from Rashomon set" << std::endl;
    }
}
