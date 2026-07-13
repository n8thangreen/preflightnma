test_that("extract_network_data works on mock nma_data object", {
  mock_net <- list(
    agd_arm = data.frame(
      .study = c("Study1", "Study1", "Study2", "Study2"),
      .trt = c("A", "B", "A", "C"),
      .n = c(100, 100, 150, 150),
      .r = c(10, 20, 15, 30),
      mean_age = c(50, 50, 60, 60),
      timepoint_wk = c(12, 12, 12, 12)
    )
  )
  class(mock_net) <- "nma_data"
  
  df <- extract_network_data(mock_net)
  expect_s3_class(df, "data.frame")
  expect_equal(df$study, c("Study1", "Study1", "Study2", "Study2"))
  expect_equal(df$treatment, c("A", "B", "A", "C"))
  expect_equal(df$sample_size, c(100, 100, 150, 150))
  expect_equal(df$events, c(10, 20, 15, 30))
  expect_equal(df$mean_age, c(50, 50, 60, 60))
})

test_that("preflight_sparsity calculates correct network sparsity", {
  mock_net <- list(
    agd_arm = data.frame(
      .study = c("S1", "S1", "S2", "S2", "S3", "S3"),
      .trt = c("A", "B", "B", "C", "A", "C"),
      .n = c(50, 50, 50, 50, 50, 50)
    )
  )
  class(mock_net) <- "nma_data"
  
  # Treatments: A, B, C (3 treatments)
  # Possible: 3 * 2 / 2 = 3 comparisons
  # Observed comparisons: A-B, B-C, A-C (3 comparisons)
  # Sparsity should be 0.0 (0%)
  
  res <- preflight_sparsity(mock_net)
  expect_equal(res$n_treatments, 3)
  expect_equal(res$n_possible_comparisons, 3)
  expect_equal(res$n_observed_comparisons, 3)
  expect_equal(res$sparsity, 0.0)
})

test_that("preflight_connectivity generates plots", {
  mock_net <- list(
    agd_arm = data.frame(
      .study = c("S1", "S1", "S2", "S2"),
      .trt = c("A", "B", "A", "C"),
      .n = c(50, 50, 50, 50),
      timepoint_wk = c(12, 12, 12, 12)
    )
  )
  class(mock_net) <- "nma_data"
  
  plots <- preflight_connectivity(mock_net, time_var = "timepoint_wk")
  expect_type(plots, "list")
  expect_true("Overall_treatment" %in% names(plots))
  expect_s3_class(plots$Overall_treatment, "ggplot")
})

test_that("preflight_transitivity generates plots", {
  mock_net <- list(
    agd_arm = data.frame(
      .study = c("S1", "S1", "S2", "S2"),
      .trt = c("A", "B", "A", "C"),
      .n = c(50, 50, 50, 50),
      mean_age = c(50, 50, 60, 60)
    )
  )
  class(mock_net) <- "nma_data"
  
  plots <- preflight_transitivity(mock_net, covariates = "mean_age")
  expect_type(plots, "list")
  expect_true("mean_age" %in% names(plots))
  expect_s3_class(plots$mean_age, "ggplot")
})

test_that("preflight_covariates generates meta-regression, forest and timeline plots", {
  mock_net <- list(
    agd_arm = data.frame(
      .study = c("S1", "S1", "S2", "S2"),
      .trt = c("A", "B", "A", "C"),
      .n = c(50, 50, 50, 50),
      .r = c(5, 10, 8, 12),
      mean_age = c(50, 50, 60, 60),
      timepoint_wk = c(12, 12, 12, 12),
      val_tx_duration = c(12, 12, 12, 12)
    )
  )
  class(mock_net) <- "nma_data"
  
  plots <- preflight_covariates(
    mock_net, covariate = "mean_age", 
    time_var = "timepoint_wk", tx_duration_var = "val_tx_duration"
  )
  expect_type(plots, "list")
  expect_true("bubble_plot" %in% names(plots))
  expect_true("stratified_forest" %in% names(plots))
  expect_true("study_timeline" %in% names(plots))
  
  expect_s3_class(plots$bubble_plot, "ggplot")
  expect_s3_class(plots$stratified_forest, "ggplot")
  expect_s3_class(plots$study_timeline, "ggplot")
})

test_that("nma_preflight orchestrates all checks", {
  mock_net <- list(
    agd_arm = data.frame(
      .study = c("S1", "S1", "S2", "S2"),
      .trt = c("A", "B", "A", "C"),
      .n = c(50, 50, 50, 50),
      .r = c(5, 10, 8, 12),
      mean_age = c(50, 50, 60, 60),
      timepoint_wk = c(12, 12, 12, 12),
      val_tx_duration = c(12, 12, 12, 12)
    )
  )
  class(mock_net) <- "nma_data"
  
  res <- nma_preflight(
    mock_net, covariates = "mean_age", 
    time_var = "timepoint_wk", tx_duration_var = "val_tx_duration"
  )
  expect_s3_class(res, "nma_preflight")
  expect_s3_class(res$sparsity, "preflight_sparsity")
  expect_type(res$connectivity_plots, "list")
  expect_type(res$transitivity_plots, "list")
  expect_type(res$covariate_plots, "list")
})
