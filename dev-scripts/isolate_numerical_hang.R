# Isolate which optimaltrees() call in test-numerical-edge-cases.R hangs, and
# whether the NEW adaptive discretization default is the cause. Each fit runs in
# a fresh killable subprocess (hard 30s cap). We run each case twice: once with
# the default (adaptive) and once forcing the legacy "log" schedule.

run_fit <- function(n, p, loss, reg, ymode, bins, seed) {
  callr::r(
    function(n, p, loss, reg, ymode, bins, seed) {
      suppressMessages(devtools::load_all(quiet = TRUE))
      set.seed(seed)
      X <- matrix(rnorm(n * p), n, p)
      y <- switch(ymode,
        binom  = rbinom(n, 1, 0.5),
        allone = rep(1, n),
        norm   = rnorm(n, 0, 100),
        bigmean = rnorm(n, 1e6, 1))
      args <- list(X = X, y = y, loss_function = loss,
                   regularization = reg, verbose = FALSE)
      if (!is.null(bins)) args$discretize_bins <- bins
      m <- do.call(optimaltrees, args)
      m@n_trees
    },
    args = list(n = n, p = p, loss = loss, reg = reg, ymode = ymode,
                bins = bins, seed = seed),
    timeout = 30, show = FALSE
  )
}

cases <- list(
  list(lbl = "T1 misclass n50 p3 reg.01",   n = 50, p = 3, loss = "misclassification", reg = 0.01, ymode = "binom",   seed = 789),
  list(lbl = "T3 misclass n80 p4 reg.02",   n = 80, p = 4, loss = "misclassification", reg = 0.02, ymode = "binom",   seed = 101112),
  list(lbl = "T4 logloss  n60 p3 reg.01",   n = 60, p = 3, loss = "log_loss",           reg = 0.01, ymode = "binom",   seed = 131415),
  list(lbl = "T5 sqerr    n50 p3 reg.01",   n = 50, p = 3, loss = "squared_error",      reg = 0.01, ymode = "norm",    seed = 161718),
  list(lbl = "T6 sqerr    n100 p4 reg.01",  n = 100, p = 4, loss = "squared_error",     reg = 0.01, ymode = "bigmean", seed = 192021)
)

for (cs in cases) {
  for (sched in list(NULL, "log")) {
    tag <- if (is.null(sched)) "default(adaptive)" else "log"
    t0 <- Sys.time()
    r <- tryCatch(run_fit(cs$n, cs$p, cs$loss, cs$reg, cs$ymode, sched, cs$seed),
                  error = function(e) paste("TIMEOUT/ERR:", conditionMessage(e)))
    dt <- as.numeric(Sys.time() - t0, units = "secs")
    cat(sprintf("%6.1fs  %-26s [%-16s] -> %s\n", dt, cs$lbl, tag, as.character(r)))
  }
}
