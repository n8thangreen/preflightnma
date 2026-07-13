#' Calculate Network Sparsity
#'
#' Computes the sparsity of the evidence network. Sparsity is defined as the proportion
#' of all possible pairwise treatment comparisons that have no direct study evidence.
#'
#' @param network An \code{nma_data} object or list.
#' @param trt_var Column name for treatments (default: "treatment").
#' @param study_var Column name for studies (default: "study").
#'
#' @return A list containing network sparsity metrics.
#' @export
#' @importFrom dplyr distinct filter group_by summarise ungroup mutate select count n sym
#' @importFrom utils combn
preflight_sparsity <- function(network, trt_var = "treatment", study_var = "study") {
  data <- extract_network_data(network)
  
  std <- sym(study_var)
  grp <- sym(trt_var)
  
  arms <- data |>
    distinct(!!std, arm = !!grp) |>
    filter(!is.na(arm), arm != "")
  
  treatments <- unique(arms$arm)
  n_trt <- length(treatments)
  
  if (n_trt < 2) {
    return(list(
      n_treatments = n_trt,
      n_possible_comparisons = 0,
      n_observed_comparisons = 0,
      sparsity = 1.0,
      message = "Not enough treatments to form a network."
    ))
  }
  
  # Calculate total possible pairwise comparisons
  total_possible <- n_trt * (n_trt - 1) / 2
  
  # Multi-arm studies (where actual comparisons happen)
  multi_arm <- arms |>
    group_by(!!std) |>
    filter(n() >= 2) |>
    ungroup()
  
  if (nrow(multi_arm) == 0) {
    return(list(
      n_treatments = n_trt,
      n_possible_comparisons = total_possible,
      n_observed_comparisons = 0,
      sparsity = 1.0,
      message = "No studies comparing multiple treatments."
    ))
  }
  
  # Find unique edges
  edges <- multi_arm |>
    group_by(!!std) |>
    summarise(pairs = list(combn(sort(unique(arm)), 2, simplify = FALSE)), .groups = "drop") |>
    tidyr::unnest(pairs) |>
    mutate(from = purrr::map_chr(pairs, 1), to = purrr::map_chr(pairs, 2)) |>
    select(from, to) |>
    distinct()
  
  observed <- nrow(edges)
  sparsity_val <- 1 - (observed / total_possible)
  
  res <- list(
    n_treatments = n_trt,
    n_possible_comparisons = total_possible,
    n_observed_comparisons = observed,
    sparsity = sparsity_val,
    treatments = treatments,
    observed_edges = edges
  )
  
  class(res) <- "preflight_sparsity"
  return(res)
}

#' Print Preflight Sparsity Summary
#'
#' @param x A \code{preflight_sparsity} object.
#' @param ... Additional arguments.
#' @export
print.preflight_sparsity <- function(x, ...) {
  cat("=== Network Sparsity Report ===\n")
  cat("Number of treatments:      ", x$n_treatments, "\n")
  cat("Possible comparisons:      ", x$n_possible_comparisons, "\n")
  cat("Observed direct comparisons:", x$n_observed_comparisons, "\n")
  cat("Network Sparsity:          ", sprintf("%.1f%%", x$sparsity * 100), "\n")
  if (!is.null(x$message)) cat("Note:                      ", x$message, "\n")
  invisible(x)
}