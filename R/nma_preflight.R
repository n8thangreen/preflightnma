#' Run Preflight Diagnostics on an NMA Network
#'
#' Orchestrates all preflight checks on the network data structure:
#' connectivity, transitivity (baseline covariate balance), sparsity, and covariate modifier checks.
#'
#' @param network An \code{nma_data} object or list.
#' @param covariates Optional character vector of covariate names to assess balance.
#' @param time_var Optional column name for timepoint/follow-up week (e.g. "timepoint_wk").
#' @param tx_duration_var Optional column name for treatment duration (e.g. "val_tx_duration").
#' @param trt_var Column name for treatments (default: "treatment").
#' @param study_var Column name for studies (default: "study").
#' @param sample_size_var Column name for sample sizes (default: "sample_size").
#' @param trt_combo_var Optional column name for alternative treatment groupings.
#' @param timeline_facet_var Optional column name to facet the timeline plot by.
#' @param reference_trt Optional name of reference treatment.
#' @param ... Additional arguments passed to plotting functions.
#'
#' @return An object of class \code{nma_preflight} containing sparsity, connectivity plots,
#' transitivity plots, and covariate modifier plots.
#' @export
nma_preflight <- function(network, 
                          covariates = NULL, 
                          time_var = NULL, 
                          tx_duration_var = NULL,
                          trt_var = "treatment",
                          study_var = "study",
                          sample_size_var = "sample_size",
                          trt_combo_var = NULL,
                          timeline_facet_var = NULL,
                          reference_trt = NULL,
                          ...) {
  
  cat("Running NMA Preflight Diagnostics...\n")
  
  # 1. Sparsity
  sparsity <- preflight_sparsity(network, trt_var = trt_var, study_var = study_var)
  
  # 2. Connectivity
  cat("Generating network connectivity plots...\n")
  connectivity_plots <- preflight_connectivity(
    network, trt_var = trt_var, trt_combo_var = trt_combo_var,
    study_var = study_var, sample_size_var = sample_size_var,
    show_study_names = TRUE, time_var = time_var
  )
  
  # 3. Transitivity (baseline characteristics balance)
  cat("Generating transitivity balance plots...\n")
  transitivity_plots <- preflight_transitivity(
    network, covariates = covariates, trt_var = trt_var,
    study_var = study_var, facet_var = timeline_facet_var
  )
  
  # 4. Covariates (meta-regression and stratified forest)
  covariate_plots <- list()
  if (!is.null(covariates) && length(covariates) > 0) {
    cat("Generating covariate modifier checks...\n")
    for (cov in covariates) {
      plots_for_cov <- preflight_covariates(
        network, covariate = cov, covariate_label = cov,
        trt_var = trt_var, study_var = study_var, sample_size_var = sample_size_var,
        time_var = time_var, tx_duration_var = tx_duration_var,
        reference_trt = reference_trt, timeline_facet_var = timeline_facet_var,
        ...
      )
      if (length(plots_for_cov) > 0) {
        covariate_plots[[cov]] <- plots_for_cov
      }
    }
  }
  
  res <- list(
    sparsity = sparsity,
    connectivity_plots = connectivity_plots,
    transitivity_plots = transitivity_plots,
    covariate_plots = covariate_plots
  )
  
  class(res) <- "nma_preflight"
  cat("NMA Preflight Diagnostics complete!\n")
  return(res)
}

#' Print NMA Preflight Summary
#'
#' @param x An \code{nma_preflight} object.
#' @param ... Additional arguments.
#' @export
print.nma_preflight <- function(x, ...) {
  cat("=== NMA Preflight Summary ===\n")
  print(x$sparsity)
  cat("\nGenerated Plots:\n")
  cat("- Connectivity plots:  ", length(x$connectivity_plots), "\n")
  cat("- Transitivity plots:  ", length(x$transitivity_plots), "\n")
  cat("- Covariate plots:     ", sum(sapply(x$covariate_plots, length)), "\n")
  invisible(x)
}