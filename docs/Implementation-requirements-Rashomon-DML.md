# Implementation requirements for Rashomon DML

This document states what the theory in the manuscript requires of any implementation that realizes the Rashomon-based DML procedure (single tree per nuisance via intersection across cross-fitting folds). It is written in implementation-agnostic form. For each requirement, we give (a) what the theory requires, and (b) what an implementation must provide (API contract: purpose, arguments, return structure). An implementation that does not yet meet a requirement will need to be extended so that its behavior matches the theory.

---

## 1. Rashomon set per fold (API)

**Theory:** For each cross-fitting fold \(k\), the training set is \(\mathcal{D}^{(-k)}\) (all observations not in fold \(k\)). The Rashomon set for fold \(k\) is the set of trees with penalized risk \(\le (1+\varepsilon)\) times the optimum (or \(\le\) optimum \(+ \varepsilon\)) on that training data, with optional complexity cap (e.g., number of leaves \(\le s_n\)). The procedure needs access to *all* trees in this set (or their structures and predictions), not only the single best tree.

**Requirement:** The implementation must expose or document:
- (i) A way to return all trees (or their structures and predictions) in the Rashomon set for a given fold’s training data, not only the optimal tree.
- (ii) Support for both multiplicative tolerance \((1+\varepsilon)\) and additive tolerance (optimum \(+ \varepsilon\)), so that the bound can be specified in either form.
- (iii) Optional max_leaves or sieve parameter so that the Rashomon set can be restricted to trees with \(\#\text{leaves} \le s_n\) (or \(M_n\)) for theory-consistent complexity.

**API contract (illustrative):** A function that takes training data, loss, regularization \(\lambda\), tolerance \(\varepsilon\) (multiplicative or additive), and optional max_leaves; returns a list of trees (or tree structures and, if needed, predictions) whose penalized risk is within the specified tolerance of the optimum.

---

## 2. Complexity-constrained Rashomon set

**Theory:** The sieve \(\mathcal{T}_{s_n}\) or \(\mathcal{T}_{M_n}\) satisfies \(M_n \to \infty\) and \(M_n \log n / n \to 0\). The Rashomon set should be compatible with the paper’s rate conditions, so that any tree in the set has complexity that does not exceed the theoretical rate index.

**Requirement:** The implementation must allow restricting the Rashomon set to trees with number of leaves \(\le s_n\) (or \(\le M_n\)), so that the set is compatible with the paper’s rate conditions. This may be via a max_leaves argument or a sieve parameter.

*Note (treefarmr):* An R-side sieve is provided: `get_rashomon_trees(model, max_leaves = M)` and `cross_fitted_rashomon(..., max_leaves = M)` restrict to trees with \(\#\text{leaves} \le M\). Theory suggests \(s_n \asymp n^{d/(2\beta+d)}\) (see paper). The C++ backend also has `depth_budget` (max depth).

---

## 3. Intersection and structure matching

**Theory:** The intersection of Rashomon sets across folds is the set of *tree structures* (splits) such that for every fold \(k\), when that structure is refit on \(\mathcal{D}^{(-k)}\), the resulting tree belongs to \(R_k(\varepsilon)\). For DML, we need for each fold \(k\) the nuisance \(\tilde{\eta}^{(-k)}\) to be the refit of a chosen intersection structure on \(\mathcal{D}^{(-k)}\).

**Requirement:** The implementation must support:
- (i) Comparing trees by *structure* (e.g., canonical representation or JSON), so that the same splits define the same structure even if leaf values differ.
- (ii) Returning the set of structures that appear in every fold’s Rashomon set (intersection).
- (iii) Given a structure and fold \(k\)’s training data, returning the *refit* of that structure on \(\mathcal{D}^{(-k)}\) (i.e., same splits, leaf values optimized on data \(-k\)), for use as \(\tilde{\eta}^{(-k)}\) in the DML score.

If the implementation currently only returns “intersecting” trees from one fold (e.g., fold 1), it must be extended to support fold-specific refit (or fold-specific predictions) so that for each observation \(i\) in fold \(k\), the score uses \(\tilde{\eta}^{(-k)}(X_i)\).

---

## 4. Tuning and theory-consistent defaults

**Theory:** The tolerance \(\varepsilon_n\) should tend to zero in such a way that Rashomon-chosen nuisances still satisfy the \(o_p(n^{-1/4})\) rate. The regularization \(\lambda_n \asymp (\log n)/n\) and the complexity index \(s_n \asymp n^{d/(2\beta+d)}\) are theory-consistent choices.

**Requirement:** The implementation should document or expose \(\varepsilon\), \(\lambda\), and complexity (e.g., max_leaves) as tuning parameters. Theory-consistent defaults (e.g., \(\varepsilon\) small or \(o(1)\), \(\lambda \propto (\log n)/n\)) should be documented so that users can align with the paper’s conditions.

**Theory-consistent defaults (treefarmr):**
- **\(\varepsilon\) (Rashomon tolerance):** Multiplicative: \code{rashomon_bound_multiplier = 0.05} (small); additive: \code{rashomon_bound_adder} (e.g. 0). Theory: \(\varepsilon_n \to 0\).
- **\(\lambda\) (regularization):** \(\lambda_n \asymp (\log n)/n\). Use \code{\link{cv_regularization}} with default grid \code{(log(n)/n) * c(0.25, 0.5, 1, 2, 4)}.
- **Complexity (max_leaves):** Not yet exposed; when added, theory suggests \(s_n \asymp n^{d/(2\beta+d)}\) (see paper).

---

## 5. Loss functions

**Theory:** For ATT, the propensity \(e(X)\) is binary and can use cross-entropy or misclassification loss. The outcome regressions \(m_0(X)\), \(m_1(X)\) may be continuous; the paper’s loss–norm link and rates apply under squared error (and cross-entropy for bounded outcomes). For continuous \(Y\), squared error is the natural choice for \(m_0\), \(m_1\).

**Requirement:** If DML with continuous outcome \(Y\) is a target, the implementation must support squared-error (or regression) loss so that \(m_0\) and \(m_1\) can be fit with the same Rashomon/intersection workflow. At minimum: support for propensity (binary) and for outcome regression (squared error or equivalent).

*Note (treefarmr):* Squared-error (regression) loss is supported: use \code{loss_function = "squared_error"} or \code{"regression"} in \code{treefarms()}, \code{fit_tree()}, \code{fit_rashomon()}, and \code{cross_fitted_rashomon()}. For regression, \code{y} is continuous and prediction returns fitted values (leaf means). The same Rashomon/intersection/refit workflow applies.

---

## 6. DML-oriented prediction

**Theory:** For observation \(i\) in fold \(k\), the orthogonal score must use \(\tilde{\eta}^{(-k)}(X_i)\)—i.e., the nuisance trained on data *not* in fold \(k\). So predictions must be fold-specific: for each observation, the prediction must come from the intersection tree (or chosen Rashomon tree) *refit on that observation’s training fold*, not from a single fold’s model applied to everyone.

**Requirement:** The implementation must support returning or applying **fold-specific** nuisances:
- For each observation \(i\), the prediction must be \(\tilde{\eta}^{(-k(i))}(X_i)\), where \(k(i)\) is the fold containing \(i\) and \(\tilde{\eta}^{(-k)}\) is the nuisance fitted on data not in fold \(k\) (e.g., the refit of the intersection structure on \(\mathcal{D}^{(-k)}\)).
- The API should allow callers to pass fold indices and obtain \(\tilde{\eta}^{(-k(i))}(X_i)\) for each \(i\). Any implementation that currently predicts from a single fold’s model for all observations must be extended to support this for valid DML.

---

## Summary table

| # | Theory requirement | Implementation must provide |
|---|--------------------|-----------------------------|
| 1 | Rashomon set = trees with penalized risk \(\le (1+\varepsilon)\times\) optimum (or \(+ \varepsilon\)) on fold’s training data | Return all trees in set; multiplicative/additive tolerance; optional max_leaves |
| 2 | Sieve/complexity compatible with rate conditions | Restrict Rashomon set to trees with \(\#\text{leaves} \le s_n\) or \(M_n\) |
| 3 | Intersection = structures in every fold’s set; refit per fold for \(\tilde{\eta}^{(-k)}\) | Compare by structure; return intersection; refit structure on \(\mathcal{D}^{(-k)}\) per fold |
| 4 | \(\varepsilon_n \to 0\), \(\lambda_n \asymp (\log n)/n\), \(s_n\) as in paper | Expose and document \(\varepsilon\), \(\lambda\), complexity with theory-consistent defaults |
| 5 | Squared error for \(m_0\), \(m_1\); cross-entropy/misclassification for \(e\) | Support squared-error (regression) loss for outcome nuisances |
| 6 | Score uses \(\tilde{\eta}^{(-k(i))}(X_i)\) (fold-specific nuisance) | Fold-specific predictions: for each \(i\), predict using nuisance fitted on \(\mathcal{D}^{(-k(i))}\) |

---

## treefarmr status (addendum)

| # | Status | Notes / functions |
|---|--------|-------------------|
| 1 | Met | `get_rashomon_trees()`; `rashomon_bound_multiplier` and `rashomon_bound_adder` in `treefarms()`, `fit_rashomon()`, `cross_fitted_rashomon()` |
| 2 | Met | R-side sieve: `get_rashomon_trees(..., max_leaves=M)`, `cross_fitted_rashomon(..., max_leaves=M)`; `count_leaves_tree()`. C++ has `depth_budget`. |
| 3 | Met | Structure-only intersection in `find_tree_intersection()`; `refit_structure_on_data()`; `cf_rashomon` stores `fold_refits` and `intersecting_structures`. |
| 4 | Met | Theory-consistent defaults documented in requirements doc and in `?cross_fitted_rashomon`, `?fit_rashomon`; \(\lambda\) in `cv_regularization()`. |
| 5 | Met | \code{loss_function = "squared_error"} (or \code{"regression"}); continuous \code{y}; prediction returns fitted values. Same workflow for \code{refit_structure_on_data()}, \code{cross_fitted_rashomon()}, \code{predict.cf_rashomon()}. |
| 6 | Met | `predict.cf_rashomon(..., fold_indices=)` and `fold_id_per_row`; fold-specific refits used when `fold_indices` provided. |

*This document can be used by any implementer to align code with the paper. Update the addendum as the codebase changes.*
