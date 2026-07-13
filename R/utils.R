#' Extract data frame from an nma_data object
#'
#' Helper function to extract a tidy data frame from a multinma network object.
#' It handles mapping internal \code{.study}, \code{.trt}, \code{.n}, etc. variables to clean names
#' and consolidates IPD and/or AgD data.
#'
#' @param network A network object of class \code{nma_data} (from \code{multinma}) or a list.
#' @return A data.frame containing the network data with standardized names.
#' @export
#' @importFrom dplyr as_tibble
extract_network_data <- function(network) {
  if (!inherits(network, "nma_data") && !is.list(network)) {
    stop("Input 'network' must be an 'nma_data' object or a list structured similarly.")
  }
  
  df <- NULL
  if (!is.null(network$agd_arm) && nrow(network$agd_arm) > 0) {
    df <- network$agd_arm
  } else if (!is.null(network$ipd) && nrow(network$ipd) > 0) {
    df <- network$ipd
  } else if (!is.null(network$agd_contrast) && nrow(network$agd_contrast) > 0) {
    df <- network$agd_contrast
  }
  
  if (is.null(df) || nrow(df) == 0) {
    stop("No data (ipd, agd_arm, or agd_contrast) found in the network object.")
  }
  
  # Ensure it is a data.frame
  df <- as.data.frame(df)
  
  # Map standard multinma prefixes if they exist and are not already mapped
  if (".study" %in% names(df) && !("study" %in% names(df))) {
    df$study <- df$.study
  }
  if (".trt" %in% names(df) && !("treatment" %in% names(df))) {
    df$treatment <- df$.trt
  }
  if (".n" %in% names(df) && !("sample_size" %in% names(df))) {
    df$sample_size <- df$.n
  }
  if (".r" %in% names(df) && !("events" %in% names(df))) {
    df$events <- df$.r
  }
  if (".y" %in% names(df) && !("outcome_val" %in% names(df))) {
    df$outcome_val <- df$.y
  }
  
  # Add default sample size if missing
  if (!("sample_size" %in% names(df))) {
    df$sample_size <- 1
  }
  
  return(df)
}
