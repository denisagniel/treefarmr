residualize_var <- function(ds, out_var) {
  ds <- ds %>%
    mutate_all(as.numeric)
  this_task <- as_task_regr(
    ds,
    target = out_var,
    id = "ortho_task"
  )
  this_lrnr <- lrn('regr.lm')
  this_lrnr$train(this_task)
  avg_ds <- ds %>%
    mutate_at(vars(-out_var), mean)
  ds[[out_var]] -
    as.vector(this_lrnr$predict(this_task)$response) +
    as.vector(this_lrnr$predict_newdata(avg_ds)$response)
}

cross_fitted_dr <- function(ds, K = 5, seed = 42, lrnr_nm = NULL, ...) {
  set.seed(seed)

  # Create K folds
  n <- nrow(ds)
  fold_indices <- sample(rep(1:K, length.out = n))

  # Initialize dataframe for storing predictions
  results_df <- ds %>%
    mutate(
      pihat = NA_real_,
      muhat_1 = NA_real_,
      muhat_0 = NA_real_
    )

  # Create ds1 and ds0 versions (for counterfactual predictions)
  ds1 <- ds %>% mutate(a = 1)
  ds0 <- ds %>% mutate(a = 0)

  # Loop through each fold
  for (k in 1:K) {
    # Split data into training and validation
    train_indices <- which(fold_indices != k)
    valid_indices <- which(fold_indices == k)

    train_data <- ds[train_indices, ]
    valid_data <- ds[valid_indices, ]

    # Create tasks for this fold
    ps_task <- as_task_regr(
      train_data %>% select(a, starts_with('z')),
      target = "a",
      id = "ps"
    )
    out_task <- as_task_regr(
      train_data %>% select(y, a, starts_with('z')),
      target = "y",
      id = "out"
    )

    # Initialize and train learners
    ps_lrnr <- lrn(lrnr_nm, ...)
    out_lrnr <- lrn(lrnr_nm, ...)

    ps_lrnr$train(ps_task)
    out_lrnr$train(out_task)

    # Make predictions on validation data
    # Propensity score predictions
    valid_ps_task <- as_task_regr(
      valid_data %>% select(a, starts_with('z')),
      target = "a",
      id = "ps_valid"
    )
    results_df$pihat[valid_indices] <- pmin(
      0.99,
      pmax(0.01, as.vector(ps_lrnr$predict(valid_ps_task)$response))
    )

    # Outcome predictions (for a=1 and a=0)
    valid_data1 <- valid_data %>% mutate(a = 1)
    valid_data0 <- valid_data %>% mutate(a = 0)

    results_df$muhat_1[valid_indices] <- as.vector(
      out_lrnr$predict_newdata(valid_data1)$response
    )
    results_df$muhat_0[valid_indices] <- as.vector(
      out_lrnr$predict_newdata(valid_data0)$response
    )
  }

  # Calculate doubly robust estimator
  final_ds <- results_df %>%
    mutate(
      ifn = (a * y - (a - pihat) * muhat_1) /
        pihat -
        ((1 - a) * y - (1 - a - (1 - pihat)) * muhat_0) / (1 - pihat)
    )

  # If true values are available, add them
  if (all(c("pi", "mu1", "mu0") %in% colnames(ds))) {
    final_ds <- final_ds %>%
      mutate(
        true_ifn = (a * y - (a - pi) * mu1) /
          pi -
          ((1 - a) * y - (1 - a - (1 - pi)) * mu0) / (1 - pi)
      )
  }

  return(final_ds)
}

sim_fn <- function(s, lrnr, weird_covars = TRUE, n, ...) {
  print('###################################')
  print(glue::glue('sim {s}, lrnr {lrnr}, n {n}, weird_covars {weird_covars}...'))
  print('###################################')
  set.seed(s)
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  x4 <- rnorm(n)

  z1 <- exp(x1 / 2)
  z2 <- x2 / (1 + exp(x1)) + 10
  z3 <- (x1 * x3 / 25 + 0.6)^3
  z4 <- (x2 + x4 + 20)^2

  pi <- plogis(
    -1 + log(1.75) * x1 + log(1.75) * x2 + log(1.75) * x3 + log(1.75) * x4
  )
  a <- rbinom(n, size = 1, prob = pi)
  mu <- 120 + 6 * a + 3 * x1 + 3 * x2 + 3 * x3 + 3 * x4
  mu1 <- 120 + 6 + 3 * x1 + 3 * x2 + 3 * x3 + 3 * x4
  mu0 <- 120 + 3 * x1 + 3 * x2 + 3 * x3 + 3 * x4
  y <- mu + rnorm(n, sd = 6)

  if (weird_covars) {
    ds <- tibble(x1, x2, x3, x4, z1, z2, z3, z4, pi, a, mu1, mu0, y)
  } else {
    ds <- tibble(
      x1,
      x2,
      x3,
      x4,
      z1 = x1,
      z2 = x2,
      z3 = x3,
      z4 = x4,
      pi,
      a,
      mu1,
      mu0,
      y
    )
  }

  ds1 <- ds %>% mutate(a = 1)
  ds0 <- ds %>% mutate(a = 0)

  # ps_mod <- regression_forest(X = cbind(z1, z2, z3, z4), Y = a, tune.parameters = "all")
  # out_mod <- regression_forest(X = cbind(z1, z2, z3, z4, a), Y = y, tune.parameters = "all")
  # ps_mod <- rpart(a ~ z1 + z2 + z3 + z4, data = ds)
  # out_mod <- rpart(y ~ z1 + z2 + z3 + z4 + a, data =)
  ps_task <- as_task_regr(
    ds %>% select(a, starts_with('z')),
    target = "a",
    id = "ps"
  )
  out_task <- as_task_regr(
    ds %>% select(y, a, starts_with('z')),
    target = "y",
    id = "out"
  )

  ps_lrnr <- lrn(lrnr, ...)
  out_lrnr <- lrn(lrnr, ...)

  ps_lrnr$train(ps_task)
  out_lrnr$train(out_task)

  naive_ds <- ds %>%
    mutate(
      pihat = pmin(
        0.99,
        pmax(0.01, as.vector(ps_lrnr$predict(ps_task)$response))
      ),
      muhat_1 = as.vector(out_lrnr$predict_newdata(ds1)$response),
      muhat_0 = as.vector(out_lrnr$predict_newdata(ds0)$response),
      ifn = (a * y - (a - pihat) * muhat_1) /
        pihat -
        ((1 - a) * y - (1 - a - (1 - pihat)) * muhat_0) / (1 - pihat),
      true_ifn = (a * y - (a - pi) * mu1) /
        pi -
        ((1 - a) * y - (1 - a - (1 - pi)) * mu0) / (1 - pi)
    )

  naive_est <- naive_ds %>%
    summarise(
      thetahat = mean(ifn),
      se_thetahat = sd(ifn) / sqrt(n),
      theta_0 = mean(true_ifn),
      se_theta0 = sd(true_ifn) / sqrt(n),
      theta_00 = mean(mu1 - mu0)
    )

  cf_ds <- cross_fitted_dr(ds = ds, K = 5, seed = s, lrnr_nm = lrnr, ...) %>%
    mutate(ay = a * y, may = (1 - a) * y, )
  # browser()
  cf_est <- cf_ds %>%
    summarise(
      thetahat = mean(ifn),
      se_thetahat = sd(ifn) / sqrt(n),
      theta_0 = mean(true_ifn),
      se_theta0 = sd(true_ifn) / sqrt(n),
      theta_00 = mean(mu1 - mu0)
    )

  # browser()
  cf_ds2 <- cf_ds %>%
    mutate(
      pihat_r = residualize_var(
        cf_ds %>% mutate() %>% select(pihat, a),
        'pihat'
      ),
      muhat1_r = residualize_var(
        cf_ds %>% select(muhat_1, a),
        'muhat_1'
      ),
      muhat0_r = residualize_var(
        cf_ds %>% select(muhat_0, a),
        'muhat_0'
      ),
      z1_r = residualize_var(
        cf_ds %>% select(z1, a),
        'z1'
      ),
      z2_r = residualize_var(
        cf_ds %>% select(z2, a),
        'z2'
      ),
      z3_r = residualize_var(
        cf_ds %>% select(z3, a),
        'z3'
      ),
      z4_r = residualize_var(
        cf_ds %>% select(z4, a),
        'z4'
      )
    )
  cf_ps_task <- as_task_regr(
    cf_ds2 %>% select(pihat_r, z1_r, z2_r, z3_r, z4_r),
    # cf_ds2 %>% select(pihat_r, z1, z2, z3, z4),
    target = "pihat_r",
    id = "cfps"
  )
  cf_ps_naive_task <- as_task_regr(
    cf_ds2 %>% select(pihat, z1, z2, z3, z4),
    target = "pihat",
    id = "cfpsn"
  )
  cf_out1_task <- as_task_regr(
    # cf_ds2 %>% select(muhat1_r, z1_r, z2_r, z3_r, z4_r),
    cf_ds %>% select(muhat_1, z1, z2, z3, z4),
    target = "muhat_1",
    # target = "muhat1_r",
    id = "cfout1"
  )
  cf_out0_task <- as_task_regr(
    # cf_ds2 %>% select(muhat0_r, z1_r, z2_r, z3_r, z4_r),
    cf_ds %>% select(muhat_0, z1, z2, z3, z4),
    target = "muhat_0",
    # target = "muhat0_r",
    id = "cfout0"
  )
  # browser()
  cfps_lrnr <- lrn(lrnr, ...)
  cfpsn_lrnr <- lrn(lrnr, ...)
  cfout1_lrnr <- lrn(lrnr, ...)
  cfout0_lrnr <- lrn(lrnr, ...)

  cfps_lrnr$train(cf_ps_task)
  cfpsn_lrnr$train(cf_ps_naive_task)
  cfout1_lrnr$train(cf_out1_task)
  cfout0_lrnr$train(cf_out0_task)

  cf_plus_ds <- cf_ds %>%
    mutate(
      new_pihat = pmin(
        0.99,
        pmax(0.01, as.vector(cfps_lrnr$predict(cf_ps_task)$response))
      ),
      new_muhat_1 = as.vector(cfout1_lrnr$predict(cf_out1_task)$response),
      new_muhat_0 = as.vector(cfout0_lrnr$predict(cf_out0_task)$response),
      ifn = (a * y - (a - new_pihat) * new_muhat_1) /
        new_pihat -
        ((1 - a) * y - (1 - a - (1 - new_pihat)) * new_muhat_0) /
          (1 - new_pihat),
      true_ifn = (a * y - (a - pi) * mu1) /
        pi -
        ((1 - a) * y - (1 - a - (1 - pi)) * mu0) / (1 - pi)
    )
  # browser()
  cf_plus_est <- cf_plus_ds %>%
    summarise(
      thetahat = mean(ifn),
      se_thetahat = sd(ifn) / sqrt(n),
      theta_0 = mean(true_ifn),
      se_theta0 = sd(true_ifn) / sqrt(n),
      theta_00 = mean(mu1 - mu0)
    )
  cfp_naive_ds <- cf_ds %>%
    mutate(
      new_pihat = pmin(
        0.99,
        pmax(0.01, as.vector(cfpsn_lrnr$predict(cf_ps_naive_task)$response))
      ),
      new_muhat_1 = as.vector(cfout1_lrnr$predict(cf_out1_task)$response),
      new_muhat_0 = as.vector(cfout0_lrnr$predict(cf_out0_task)$response),
      ifn = (a * y - (a - new_pihat) * new_muhat_1) /
        new_pihat -
        ((1 - a) * y - (1 - a - (1 - new_pihat)) * new_muhat_0) /
          (1 - new_pihat),
      true_ifn = (a * y - (a - pi) * mu1) /
        pi -
        ((1 - a) * y - (1 - a - (1 - pi)) * mu0) / (1 - pi)
    )
  # browser()
  cfp_naive_est <- cfp_naive_ds %>%
    summarise(
      thetahat = mean(ifn),
      se_thetahat = sd(ifn) / sqrt(n),
      theta_0 = mean(true_ifn),
      se_theta0 = sd(true_ifn) / sqrt(n),
      theta_00 = mean(mu1 - mu0)
    )
  bind_rows(
    naive_est %>% mutate(type = 'naive'),
    cf_est %>% mutate(type = 'cf'),
    cf_plus_est %>% mutate(type = 'cf_plus'),
    cfp_naive_est %>% mutate(type = 'cf_plus_naive')
  ) %>%
    mutate(n = n, lrnr = lrnr, weird_covars = weird_covars, seed = s)
}
