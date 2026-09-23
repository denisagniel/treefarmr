# Confirm a max_depth cap resolves the T5/T6 regression explosion under the new
# adaptive default, and find the smallest cap that keeps fits fast. Each fit in a
# fresh killable subprocess (hard 30s cap).
fit_one <- function(n, p, reg, ymode, depth, seed) {
  callr::r(
    function(n, p, reg, ymode, depth, seed) {
      suppressMessages(devtools::load_all(quiet = TRUE))
      set.seed(seed)
      X <- matrix(rnorm(n * p), n, p)
      y <- if (ymode == "norm") rnorm(n, 0, 100) else rnorm(n, 1e6, 1)
      m <- optimaltrees(X = X, y = y, loss_function = "squared_error",
                        regularization = reg, verbose = FALSE, max_depth = depth)
      m@n_trees
    },
    args = list(n = n, p = p, reg = reg, ymode = ymode, depth = depth, seed = seed),
    timeout = 30, show = FALSE
  )
}

cases <- list(
  list(lbl = "T5 n50 p3", n = 50, p = 3, reg = 0.01, ymode = "norm", seed = 161718),
  list(lbl = "T6 n100 p4", n = 100, p = 4, reg = 0.01, ymode = "bigmean", seed = 192021)
)
for (cs in cases) {
  for (depth in c(0L, 5L, 4L, 3L)) {   # 0 = unlimited (baseline)
    t0 <- Sys.time()
    r <- tryCatch(fit_one(cs$n, cs$p, cs$reg, cs$ymode, depth, cs$seed),
                  error = function(e) "TIMEOUT")
    dt <- as.numeric(Sys.time() - t0, units = "secs")
    cat(sprintf("%-10s depth=%d  %6.1fs  -> %s\n", cs$lbl, depth, dt, as.character(r)))
  }
}
