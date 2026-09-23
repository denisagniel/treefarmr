# Superseded work

Files here were written against a theory architecture that
`inst/paper/main.tex` no longer describes. Kept for reference (the ideas
may be revisited), not part of the package (`R/`) or its test suite.

## `select_lambda_plateau.R` + `test-select-lambda-plateau.R`

Written ~2026-09-01, never committed. Implements lambda selection via
plateau detection under the sandwich condition `nu_n << lambda_n << mu_n`
that the paper's then-current `prop:parsimony-grid`/`prop:boundary-recovery`
required for topology recovery. The 2026-09-04 architecture revision
replaced that condition with a reversed one, `lambda_n >> 1/r_n`, and the
paper's own Discussion outline flags lambda-selection under the new regime
as an open question -- not yet resolved, so this file is parked rather
than adapted on a guess. Revisit once `sec:implementation`/the R3
simulation (large-lambda vs. small-lambda, collapse off) has something to
say about what a selector for the new regime should look like; under a
one-sided condition the story may turn out to be simpler than plateau
detection, not just a reparameterized version of it.
