#include "additive_metrics.hpp"
#include "state.hpp"
#include "configuration.hpp"

Objective::Objective(const int falses, const int regularization, State & state)
    : falses(falses), regularization(regularization),
      objective(falses * state.dataset.get_mismatch_cost() +
                regularization * Configuration::regularization){};