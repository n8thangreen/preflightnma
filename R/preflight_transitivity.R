#' Check Network Transitivity and Baseline Covariate Balance
#'
#' Generates baseline risk and patient characteristic balance plots across treatment groups
#' to visually assess the assumption of transitivity (similarity of data sources).
#'
#' @param network An \code{nma_data} object or list.
#' @param covariates Character vector of covariate column names to check. If NULL, auto-detects
#' numeric columns that are not part of the standard NMA structure.
#' @param trt_var Column name for treatments (default: "treatment").
#' @param study_var Column name for studies (default: "study").
#' @param facet_var Optional column name to facet plots by (e.g., "outcome_name").
#'
#' @return A list of ggplot objects (one per covariate).
#' @export
#' @import ggplot2
#' @importFrom dplyr filter mutate sym select_if
preflight_transitivity <- function(network, 
                                   covariates = NULL, 
                                   trt_var = "treatment", 
                                   study_var = "study",
                                   facet_var = NULL) {
  
  data <- extract_network_data(network)
  
  # Auto-detect covariates if not provided
  if (is.null(covariates)) {
    # Find all numeric columns, excluding standard NMA columns
    standard_cols <- c("study", "treatment", "sample_size", "events", "outcome_val", 
                       "timepoint_wk", ".study", ".trt", ".n", ".r", ".y", "proportion", "prop")
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    covariates <- setdiff(numeric_cols, standard_cols)
  }
  
  if (length(covariates) == 0) {
    warning("No covariates found or specified for transitivity check.")
    return(list())
  }
  
  plots <- list()
  
  for (cov in covariates) {
    if (!(cov %in% names(data))) {
      warning("Covariate '", cov, "' not found in the network data.")
      next
    }
    
    # Filter missing values
    plot_data <- data[!is.na(data[[cov]]), ]
    if (nrow(plot_data) == 0) next
    
    # Clean facet names if matching outcome name
    if (!is.null(facet_var) && facet_var %in% names(plot_data)) {
      plot_data <- plot_data |>
        mutate(!!sym(facet_var) := ifelse(!!sym(facet_var) == "AE", "Adverse Events", as.character(!!sym(facet_var))))
    }
    
    p <- ggplot(plot_data, aes(x = .data[[cov]], y = !!sym(trt_var), colour = !!sym(study_var))) +
      geom_point(size = 4, alpha = 0.8) +
      theme_bw() + 
      xlab(cov) +
      ylab("Intervention") + 
      labs(colour = "Study", title = paste("Baseline", cov, "Distribution across Treatments")) +
      theme(
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 12, face = "bold")
      )
    
    if (!is.null(facet_var) && facet_var %in% names(plot_data)) {
      p <- p + facet_wrap(vars(!!sym(facet_var)))
    }
    
    plots[[cov]] <- p
  }
  
  return(plots)
}