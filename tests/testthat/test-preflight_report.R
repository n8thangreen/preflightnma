test_that("nma_preflight_report creates an HTML report", {
  skip_if_not(quarto::quarto_available())
  
  mock_net <- list(
    agd_arm = data.frame(
      .study = c("S1", "S1", "S2", "S2"),
      .trt = c("A", "B", "A", "C"),
      .n = c(50, 50, 50, 50),
      .r = c(5, 10, 8, 12),
      mean_age = c(50, 50, 60, 60),
      timepoint_wk = c(12, 12, 12, 12)
    )
  )
  class(mock_net) <- "nma_data"
  
  tmp_html <- tempfile(fileext = ".html")
  
  nma_preflight_report(
    mock_net, 
    output_file = tmp_html, 
    covariates = "mean_age", 
    time_var = "timepoint_wk",
    browse = FALSE
  )
  
  expect_true(file.exists(tmp_html))
  
  if (file.exists(tmp_html)) unlink(tmp_html)
})
