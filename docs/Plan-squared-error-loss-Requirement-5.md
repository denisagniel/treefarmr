# Plan: Squared-error (regression) loss for Requirement 5

This plan outlines how to implement **Requirement 5** from [Implementation-requirements-Rashomon-DML.md](Implementation-requirements-Rashomon-DML.md): support squared-error (regression) loss so that outcome nuisances \(m_0(X)\), \(m_1(X)\) can be fit with the same Rashomon/intersection/refit workflow for DML with continuous \(Y\).

---

## Goal

- Add a new loss: **squared error** (regression).
- For each leaf: loss = sum of \((y_i - \bar{y}_{\text{leaf}})^2\); penalized risk = loss + \(\lambda \cdot \#\text{leaves}\).
- Rashomon set = trees with penalized risk \(\le (1+\varepsilon)\times\) optimum (or \(+ \varepsilon\)), same as for classification.
- R API: `loss_function = "squared_error"` (or `"regression"`); prediction returns leaf mean (fitted value) instead of class probabilities.
- Ensure `refit_structure_on_data()`, `find_tree_intersection()`, and `predict.cf_rashomon()` work for regression (e.g. return a vector of fitted values when in regression mode).

---

## Phase 1: Design and theory

1. **Confirm formula** with the manuscript: penalized risk for regression = \(\sum_{\text{leaf}} \bigl( \sum_{i \in \text{leaf}} (y_i - \bar{y}_{\text{leaf}})^2 \bigr) + \lambda \cdot \#\text{leaves}\). Leaf value = \(\bar{y}_{\text{leaf}}\) (mean of \(y\) in leaf).
2. **Rashomon bound:** Same as classification: bound = optimum \(\times (1+\varepsilon)\) or optimum \(+ \varepsilon\); extraction logic unchanged except loss computation.
3. **Data:** \(y\) is continuous; features remain binary for the current backend. Decide if any encoding or validation changes are needed (e.g. allow numeric \(y\) in R and C++).

---

## Phase 2: C++ changes

**Files to touch:**

- **[src/configuration.hpp](src/configuration.hpp)** and **[src/configuration.cpp](src/configuration.cpp):** Add `SQUARED_ERROR` (or `REGRESSION`) to `LossFunction` enum; parse `"squared_error"` / `"regression"` from config.
- **[src/dataset.cpp](src/dataset.cpp):** For squared-error, compute per-leaf min loss (sum of squared residuals when leaf value = mean) and any bounds used by the optimizer. Dataset may need to store/accept continuous \(y\) (already numeric in C++).
- **[src/task.cpp](src/task.cpp)** (and **[src/optimizer/dispatch/dispatch.hpp](src/optimizer/dispatch/dispatch.hpp)** if used): Add branch for squared-error in lower-bound and priority logic (leaf optimal = mean; loss = sum of squared deviations).
- **[src/optimizer.cpp](src/optimizer.cpp):** Any loss-specific pruning or cancellation (see existing `Configuration::loss_function == LOG_LOSS` branches); add `SQUARED_ERROR` branches where needed.
- **[src/gosdt.cpp](src/gosdt.cpp):** Rashomon extraction uses the same bound; ensure serialization/output includes leaf mean (and optionally variance) for regression trees.
- **[src/model.cpp](src/model.cpp)** / model serialization: Regression tree nodes should output a single value (leaf mean) instead of prediction + probabilities.

**Order of work:** Configuration → dataset loss computation → task bounds → optimizer branches → Rashomon/output. Run existing tests after each step; add unit tests for regression loss on small data.

---

## Phase 3: R API and prediction

- **[R/treefarms.R](R/treefarms.R):** Add `loss_function = "squared_error"` (and possibly `"regression"` as alias). Allow continuous `y` when this loss is selected (relax `all(y %in% c(0,1))` for that path). Pass through to C++ config.
- **[R/fit_tree.R](R/fit_tree.R)** and **[R/fit_rashomon.R](R/fit_rashomon.R):** Accept and pass `loss_function = "squared_error"`.
- **Prediction:** Add a path in the predict logic (e.g. in [R/treefarms.R](R/treefarms.R) or [R/predict.R](R/predict.R)): when model is regression, traverse tree and return leaf mean for each row (no probabilities). New helper `get_fitted_from_tree(tree_json, X)` analogous to `get_probabilities_from_tree`.
- **Model class:** Consider a `treefarms_regression_model` or a flag on the existing model so `predict()` and `get_rashomon_trees()` know to treat as regression.

---

## Phase 4: DML integration (refit and cross-fit)

- **[R/rashomon.R](R/rashomon.R) `refit_structure_on_data()`:** For regression, leaf value = mean(y in leaf) instead of class probabilities. Return tree with same structure and fitted leaf means. Already supports this if we interpret “prediction” as the mean and “probabilities” as unused for regression.
- **`find_tree_intersection()`:** Structure-only comparison is unchanged (splits only); works for regression trees.
- **[R/cross_fitted_rashomon.R](R/cross_fitted_rashomon.R):** Allow `loss_function = "squared_error"` and continuous `y`; fold refits and `fold_refits` storage unchanged.
- **[R/predict.R](R/predict.R) `predict.cf_rashomon()`:** When using `fold_refits`, if the refit tree is regression (e.g. no probabilities), return fitted values (vector) instead of class/prob matrix. Caller can pass `type = "response"` or similar for regression.

---

## Phase 5: Documentation and status

- Update [Implementation-requirements-Rashomon-DML.md](Implementation-requirements-Rashomon-DML.md): Requirement 5 status → **Met** once squared-error is implemented and documented.
- Help files: document `loss_function = "squared_error"`, continuous \(y\), and that prediction returns fitted values for regression.
- Vignette or README: short example of fitting outcome nuisance with squared-error and using in DML (e.g. ATT with continuous outcome).

---

## Dependency order

1. Phase 1 (design).
2. Phase 2 (C++) – core regression loss and Rashomon extraction.
3. Phase 3 (R API and prediction) – so a single regression tree can be fit and predicted.
4. Phase 4 (refit + cross-fit) – so DML workflow uses regression nuisances.
5. Phase 5 (docs and status).

---

## Risks and alternatives

- **Scope creep:** C++ optimizer may have many loss-specific branches; limit to the minimal set of files that affect loss and bounds.
- **Alternative:** If adding regression to the C++ core is too large, consider an R-only “fixed structure, fit leaf means” path for continuous \(Y\) that uses the existing binary-tree structures and only refits leaf values in R (no new C++ loss). That would satisfy “use same Rashomon/intersection workflow” only if the Rashomon set were still built with a binary loss (e.g. binarized \(Y\)); theory would need to be checked. The plan above assumes a full C++ squared-error loss.
