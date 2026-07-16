#' Plot Covariate Bubble (Exploratory Meta-Regression)
#'
#' Bubble plot of outcome rate/proportion vs baseline covariate, weighted by sample size.
#'
#' @param data A data frame.
#' @param covariate Character; the covariate column name.
#' @param covariate_label Character; the label for the covariate axis.
#' @param trt_var Character; treatment column name.
#' @param outcome_var Character; outcome column name. If NULL, calculated as \code{events / sample_size}.
#' @param outcome_label Character; outcome label for the y-axis.
#' @param time_var Character; timepoint column name.
#' @param title_text Character; title of the plot.
#'
#' @return A ggplot object.
#' @export
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @importFrom scales percent_format
#' @importFrom dplyr filter mutate n_distinct sym left_join group_by summarise
#' @importFrom stats cor
plot_covariate_bubble <- function(data, 
                                  covariate, 
                                  covariate_label = covariate, 
                                  trt_var = "treatment", 
                                  outcome_var = NULL, 
                                  outcome_label = "Outcome Rate",
                                  time_var = NULL,
                                  title_text = NULL) {
  
  # Prepare outcomes
  if (is.null(outcome_var)) {
    if (!all(c("events", "sample_size") %in% names(data))) {
      stop("To calculate proportions, 'events' and 'sample_size' columns must be present.")
    }
    data$proportion <- data$events / data$sample_size
    y_col <- "proportion"
  } else {
    y_col <- outcome_var
    data$proportion <- data[[y_col]]
  }
  
  # Required variables
  req_cols <- c(covariate, trt_var, "proportion", "sample_size")
  if (!is.null(time_var)) req_cols <- c(req_cols, time_var)
  
  # Drop NAs
  plot_data <- data
  for (col in req_cols) {
    plot_data <- plot_data[!is.na(plot_data[[col]]), ]
  }
  
  if (nrow(plot_data) == 0) return(NULL)
  
  # Add correlation labels
  cor_data <- plot_data |>
    group_by(.data[[trt_var]]) |>
    summarise(
      r_val = suppressWarnings(cor(.data[[covariate]], proportion, use = "complete.obs")), 
      .groups = "drop"
    ) |>
    mutate(
      r_label = ifelse(is.na(r_val), "N/A", sprintf("%.2f", r_val)), 
      legend_label = paste0(.data[[trt_var]], " (r = ", r_label, ")")
    )
  
  plot_data <- plot_data |> left_join(cor_data, by = trt_var)
  plot_data$legend_label <- as.factor(plot_data$legend_label)
  n_tx <- n_distinct(plot_data$legend_label)
  
  if (!is.null(time_var)) {
    plot_data$tp_label <- factor(paste("Timepoint", plot_data[[time_var]]), 
                                 levels = paste("Timepoint", sort(unique(plot_data[[time_var]]))))
  }
  
  p <- ggplot(plot_data, aes(x = .data[[covariate]], y = proportion, fill = legend_label, colour = legend_label)) +
    suppressWarnings(geom_smooth(aes(weight = sample_size), method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed", linewidth = 0.8, alpha = 0.6)) +
    geom_point(aes(size = sample_size), shape = 21, colour = "white", alpha = 0.75)
  
  if ("study" %in% names(plot_data)) {
    p <- p + geom_text_repel(aes(label = study), size = 3, show.legend = FALSE, 
                             color = "grey20", bg.color = "white", bg.r = 0.15, 
                             max.overlaps = 20, min.segment.length = 0)
  }
  
  p <- p +
    scale_size_continuous(range = c(2, 12)) +
    scale_fill_brewer(palette = if (n_tx <= 8) "Dark2" else "Paired", drop = FALSE) +
    scale_colour_brewer(palette = if (n_tx <= 8) "Dark2" else "Paired", drop = FALSE) +
    guides(
      fill = guide_legend(title = "Treatment Group", override.aes = list(shape = 21, size = 4, colour = NA)), 
      colour = "none",
      size = guide_legend(title = "Arm N", override.aes = list(shape = 21, fill = "grey70", colour = "grey30", alpha = 0.8))
    ) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA)) +
    labs(
      title = if (is.null(title_text)) paste("Meta-Regression of", outcome_label, "by", covariate_label) else title_text,
      subtitle = "Trendlines are weighted by arm sample size. 'r' indicates Pearson correlation.",
      x = covariate_label,
      y = outcome_label
    ) +
    theme_bw(base_size = 11) +
    theme(legend.position = "right", plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(colour = "grey40", size = 9), panel.grid.minor = element_blank())
  
  if (!is.null(time_var)) {
    p <- p + facet_wrap(~ tp_label)
  }
  
  return(p)
}

#' Plot Stratified Forest Plot
#'
#' Generates a forest plot of treatment contrasts (e.g. Risk Differences) stratified by baseline covariate categories.
#'
#' @param data A data frame.
#' @param covariate Character; baseline covariate column.
#' @param effect_col Column for treatment effects (default: "risk_difference").
#' @param lower_ci Column for lower CI.
#' @param upper_ci Column for upper CI.
#' @param study_col Column for study names.
#' @param trt_col Column for treatments.
#' @param time_col Column for timepoints.
#' @param breaks Optional numeric breaks to categorize the covariate.
#' @param labels Optional labels for the breaks.
#'
#' @return A ggplot object.
#' @export
#' @import ggplot2
#' @importFrom forcats fct_reorder
plot_stratified_forest <- function(data, 
                                   covariate,
                                   effect_col = "risk_difference", 
                                   lower_ci = "ci_lower", 
                                   upper_ci = "ci_upper",
                                   study_col = "study",
                                   trt_col = "treatment",
                                   time_col = NULL,
                                   breaks = NULL,
                                   labels = NULL) {
  
  # Categorize covariate
  plot_data <- data |> filter(!is.na(!!sym(effect_col)))
  
  if (is.numeric(plot_data[[covariate]])) {
    if (is.null(breaks)) {
      # Default: cut by median
      med <- median(plot_data[[covariate]], na.rm = TRUE)
      breaks <- c(-Inf, med, Inf)
      labels <- c(paste("<=", round(med, 2)), paste(">", round(med, 2)))
    }
    plot_data$stratum <- cut(plot_data[[covariate]], breaks = breaks, labels = labels)
  } else {
    plot_data$stratum <- as.factor(plot_data[[covariate]])
  }
  
  # Reorder studies
  plot_data <- plot_data |>
    mutate(
      !!sym(study_col) := fct_reorder(as.character(!!sym(study_col)), !!sym(effect_col), na.rm = TRUE)
    )
  
  p <- ggplot(plot_data, aes(x = !!sym(effect_col), y = !!sym(study_col))) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
    geom_errorbar(aes(xmin = !!sym(lower_ci), xmax = !!sym(upper_ci)), width = 0.2, color = "gray50") +
    geom_point(aes(size = total_n, color = !!sym(trt_col)), alpha = 0.8) +
    theme_minimal(base_size = 12) +
    labs(
      title = "Treatment Effects Stratified by Baseline Covariate",
      x = "Contrast Effect Size",
      y = NULL,
      size = "Total N",
      color = "Treatment Contrast"
    ) +
    theme(
      strip.text.y.left = element_text(angle = 0, face = "bold"),
      strip.placement = "outside",
      panel.spacing = unit(1, "lines"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray80", fill = NA)
    )
  
  # Add facets
  facet_formula <- if (!is.null(time_col)) {
    as.formula(paste("stratum ~", time_col))
  } else {
    as.formula("~ stratum")
  }
  
  p <- p + facet_grid(facet_formula, scales = "free_y", space = "free_y", switch = "y")
  return(p)
}

#' Plot Study Timeline
#'
#' Visualizes the duration of treatment and follow-up across different studies and arms.
#'
#' @param data A data frame.
#' @param study_var Column name for study IDs.
#' @param trt_var Column name for treatments.
#' @param tx_duration_var Column name for treatment duration.
#' @param followup_var Column name for total follow-up time.
#' @param facet_var Optional column name to facet the timeline plot by.
#'
#' @return A ggplot object.
#' @export
#' @import ggplot2
#' @importFrom dplyr group_by slice_max ungroup rename across all_of
#' @importFrom stats reorder median as.formula
plot_study_timeline <- function(data, 
                                study_var = "study", 
                                trt_var = "treatment", 
                                tx_duration_var = "val_tx_duration", 
                                followup_var = "timepoint_wk",
                                facet_var = NULL) {
  
  # Ensure variables are in dataset
  req_cols <- c(study_var, trt_var, tx_duration_var, followup_var)
  if (!is.null(facet_var)) req_cols <- c(req_cols, facet_var)
  
  missing_cols <- setdiff(req_cols, names(data))
  if (length(missing_cols) > 0) {
    warning("Missing required variables for timeline plot: ", paste(missing_cols, collapse = ", "))
    return(NULL)
  }
  
  plot_data <- data |>
    filter(!is.na(!!sym(tx_duration_var)), !!sym(followup_var) >= 0)
  
  # Deduplicate to get the maximum follow-up per study arm
  group_cols <- c(study_var, trt_var)
  if (!is.null(facet_var)) group_cols <- c(group_cols, facet_var)
  
  plot_data <- plot_data |>
    group_by(across(all_of(group_cols))) |>
    slice_max(order_by = !!sym(followup_var), n = 1, with_ties = FALSE) |>
    ungroup()
  
  # Rename followup variable for plotting
  plot_data$fu_end <- plot_data[[followup_var]]
  plot_data$study_arm <- plot_data[[study_var]]
  
  p <- ggplot(plot_data, aes(y = reorder(study_arm, fu_end))) +
    geom_segment(
      aes(x = 0, xend = !!sym(tx_duration_var), yend = study_arm, color = "Treatment"), 
      linewidth = 2
    ) +
    geom_segment(
      aes(x = !!sym(tx_duration_var), xend = !!sym(tx_duration_var) + fu_end, yend = study_arm, color = "Follow-up"), 
      linewidth = 1.5
    ) +
    geom_point(
      aes(x = !!sym(tx_duration_var) + fu_end, color = "Follow-up"), 
      size = 2, 
      na.rm = TRUE
    ) +
    scale_color_manual(
      name = "Study Phase", 
      values = c("Treatment" = "#2c7fb8", "Follow-up" = "#bdbdbd")
    ) +
    theme_bw() +
    labs(
      x = "Time from baseline (weeks)", 
      y = "Study",
      title = "Study Design Timeline"
    )
  
  if (!is.null(facet_var)) {
    facet_formula <- as.formula(paste(trt_var, "~", facet_var))
    p <- p + facet_grid(facet_formula, scales = "free_y", space = "free_y")
  } else {
    p <- p + facet_grid(as.formula(paste("~", trt_var)), scales = "free_y", space = "free_y")
  }
  
  return(p)
}

#' Run Covariate Diagnostics
#'
#' Orchestrates the generation of bubble plots, stratified forest plots, and timeline plots.
#'
#' @param network An \code{nma_data} object or list.
#' @param covariate Character; the covariate column to examine.
#' @param covariate_label Character; the label for the covariate.
#' @param trt_var Column name for treatments.
#' @param study_var Column name for studies.
#' @param sample_size_var Column name for sample size.
#' @param time_var Column name for follow-up/time variable.
#' @param tx_duration_var Column name for treatment duration.
#' @param reference_trt Optional reference treatment name.
#' @param breaks Optional breaks for stratified forest plot.
#' @param labels Optional labels for breaks.
#' @param outcome_var Optional outcome column name.
#' @param timeline_facet_var Optional facet variable for study timeline.
#' @param ... Additional arguments.
#'
#' @return A list of ggplot objects.
#' @export
preflight_covariates <- function(network,
                                 covariate,
                                 covariate_label = covariate,
                                 trt_var = "treatment",
                                 study_var = "study",
                                 sample_size_var = "sample_size",
                                 time_var = NULL,
                                 tx_duration_var = NULL,
                                 reference_trt = NULL,
                                 breaks = NULL,
                                 labels = NULL,
                                 outcome_var = NULL,
                                 timeline_facet_var = NULL,
                                 ...) {
  
  data <- extract_network_data(network)
  plots <- list()
  
  # 1. Bubble Plot
  p_bubble <- plot_covariate_bubble(
    data, covariate, covariate_label, 
    trt_var = trt_var, outcome_var = outcome_var, 
    time_var = time_var
  )
  if (!is.null(p_bubble)) plots$bubble_plot <- p_bubble
  
  # 2. Stratified Forest Plot (computes risk difference if binomial events/size are present)
  if (all(c("events", "sample_size") %in% names(data))) {
    contrast_data <- suppressWarnings(
      tryCatch({
        compute_empirical_contrasts(
          data, trt_var = trt_var, study_var = study_var, 
          reference_trt = reference_trt, time_var = time_var
        )
      }, error = function(e) NULL)
    )
    
    if (!is.null(contrast_data) && nrow(contrast_data) > 0) {
      p_forest <- plot_stratified_forest(
        contrast_data, covariate = covariate, 
        study_col = study_var, trt_col = "contrast_label", 
        time_col = time_var, breaks = breaks, labels = labels
      )
      if (!is.null(p_forest)) plots$stratified_forest <- p_forest
    }
  }
  
  # 3. Study Timeline Plot
  if (!is.null(tx_duration_var)) {
    p_timeline <- plot_study_timeline(
      data, study_var = study_var, trt_var = trt_var,
      tx_duration_var = tx_duration_var, followup_var = if (is.null(time_var)) "sample_size" else time_var,
      facet_var = timeline_facet_var
    )
    if (!is.null(p_timeline)) plots$study_timeline <- p_timeline
  }
  
  return(plots)
}

#' Helper to compute empirical contrasts (risk difference)
#'
#' @noRd
compute_empirical_contrasts <- function(df, trt_var = "treatment", study_var = "study", reference_trt = NULL, time_var = NULL) {
  if (is.null(reference_trt)) {
    reference_trt <- names(sort(table(df[[trt_var]]), decreasing = TRUE))[1]
  }
  
  df$prop <- df$events / df$sample_size
  
  group_cols <- c(study_var)
  if (!is.null(time_var) && time_var %in% names(df)) {
    group_cols <- c(group_cols, time_var)
  }
  
  df_ref <- df[df[[trt_var]] == reference_trt, ]
  if (nrow(df_ref) == 0) {
    stop("Reference treatment not found.")
  }
  
  ref_merge_cols <- c(group_cols, "events", "sample_size", "prop")
  df_ref_sub <- df_ref[, names(df_ref) %in% ref_merge_cols]
  names(df_ref_sub)[names(df_ref_sub) == "events"] <- "events_ref"
  names(df_ref_sub)[names(df_ref_sub) == "sample_size"] <- "sample_size_ref"
  names(df_ref_sub)[names(df_ref_sub) == "prop"] <- "prop_ref"
  
  df_trt <- df[df[[trt_var]] != reference_trt, ]
  df_contrast <- merge(df_trt, df_ref_sub, by = group_cols)
  
  df_contrast$risk_difference <- df_contrast$prop - df_contrast$prop_ref
  df_contrast$se_rd <- sqrt((df_contrast$prop * (1 - df_contrast$prop) / df_contrast$sample_size) +
                           (df_contrast$prop_ref * (1 - df_contrast$prop_ref) / df_contrast$sample_size_ref))
  df_contrast$se_rd[is.na(df_contrast$se_rd) | is.infinite(df_contrast$se_rd)] <- 0
  
  df_contrast$ci_lower <- df_contrast$risk_difference - (1.96 * df_contrast$se_rd)
  df_contrast$ci_upper <- df_contrast$risk_difference + (1.96 * df_contrast$se_rd)
  df_contrast$total_n <- df_contrast$sample_size + df_contrast$sample_size_ref
  df_contrast$contrast_label <- paste(df_contrast[[trt_var]], "vs", reference_trt)
  
  return(df_contrast)
}
