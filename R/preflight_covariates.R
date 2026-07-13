
#
preflight_covariates <- function(network, time_var = NULL) {
  
}


#
build_bubble_plot <- function(data, x_var, x_label, grp_var, title_text, legend_title, outcome_label) {
  # 1. Filter out missing data, ensuring timepoint_wk is present
  plot_data <- data |>
    filter(!is.na(.data[[x_var]]), !is.na(proportion), !is.na(N_num), !is.na(.data[[grp_var]]), !is.na(timepoint_wk))
  
  if (nrow(plot_data) == 0) return(invisible(NULL))
  
  # 2. Convert timepoint to a factor for ordered, clean facet headers
  plot_data <- plot_data |> 
    mutate(
      tp_label = factor(paste("Week", timepoint_wk), 
                        levels = paste("Week", sort(unique(timepoint_wk)))),
      
      # convert the grouping variable to a factor to lock the colour palette
      !!sym(grp_var) := factor(.data[[grp_var]])
    )
  
  n_tx <- n_distinct(plot_data[[grp_var]])
  
  # 3. Build the faceted plot
  ggplot(plot_data, aes(x = .data[[x_var]], y = proportion, fill = .data[[grp_var]], colour = .data[[grp_var]])) +
    
    # geom_smooth now calculates independent linear models for each treatment *within each time facet*
    suppressWarnings(geom_smooth(aes(weight = N_num), method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.8, alpha = 0.6)) +
    geom_point(aes(size = N_num), shape = 21, colour = "white", alpha = 0.75) +
    geom_text_repel(aes(label = author_year), size = 2.5, show.legend = FALSE, 
                    color = "grey20", bg.color = "white", bg.r = 0.15, 
                    max.overlaps = 15, min.segment.length = 0) +
    
    scale_size_continuous(range = c(2, 10)) +
    scale_fill_brewer(palette = if (n_tx <= 8) "Dark2" else "Paired", drop = FALSE) +
    scale_colour_brewer(palette = if (n_tx <= 8) "Dark2" else "Paired", drop = FALSE) +
    
    guides(
      fill = guide_legend(title = legend_title, override.aes = list(shape = 21, size = 4, colour = NA)), colour = "none",
      size = guide_legend(title = "Study arm\nsample size", override.aes = list(shape = 21, fill = "grey70", colour = "grey30", alpha = 0.8))
    ) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA)) +
    
    # NEW: Facet the plot into a grid based on the timepoint
    facet_wrap(~ tp_label) +
    
    labs(
      title = title_text,
      subtitle = "Trendlines are weighted by N. Stratified by time point to account for repeated measures.",
      x = x_label,
      y = paste0("Proportion of patients with ", outcome_label)
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "right", 
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(colour = "grey40", size = 9), 
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey92", colour = NA),
      strip.text = element_text(face = "bold", size = 10)
    )
}


# =================================================
# HBsAg loss at baseline as effect modifier plots
# =================================================

# --------------------------
# into HBsAg base groups

# Load required libraries
library(ggplot2)
library(dplyr)
library(forcats)
library(tidyr)
library(scales)

plot_stratified_forest <- function(data, 
                                   effect_col = "risk_difference", 
                                   lower_ci = "ci_lower", 
                                   upper_ci = "ci_upper",
                                   study_col = "study_name") {
  
  plot_data <- data %>%
    # Proactively drop rows missing the effect size
    # This prevents sorting errors and cleans up the ggplot input
    filter(!is.na(!!sym(effect_col))) %>%
    mutate(
      hbsag_stratum = cut(baseline_hbsag,
                          breaks = c(-Inf, 1000, 3000, Inf),
                          labels = c("Low (<1000 IU/mL)", 
                                     "Intermediate (1000-3000 IU/mL)", 
                                     "High (>3000 IU/mL)")),
      # 2. FIXED: na.rm = TRUE (without the dot)
      !!sym(study_col) := fct_reorder(as.character(!!sym(study_col)), !!sym(effect_col), na.rm = TRUE)
    )
  
  ggplot(plot_data, aes(x = !!sym(effect_col), y = !!sym(study_col))) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
    geom_errorbarh(aes(xmin = !!sym(lower_ci), xmax = !!sym(upper_ci)), 
                   height = 0.2, color = "gray50") +
    geom_point(aes(size = total_n, color = treatment_arm), alpha = 0.8) +
    facet_grid(hbsag_stratum ~ time_point, scales = "free_y", space = "free_y", switch = "y") +
    theme_minimal(base_size = 14) +
    labs(
      title = "Empirical Treatment Effects Stratified by Baseline HBsAg",
      x = "Risk Difference (Treatment - Control)",
      y = NULL,
      size = "Total N",
      color = "Treatment Arm"
    ) +
    theme(
      strip.text.y.left = element_text(angle = 0, face = "bold"),
      strip.placement = "outside",
      panel.spacing = unit(1, "lines"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray80", fill = NA)
    )
}


plot_dat <- 
  df_hbsag |> 
  select(author_year,
         timepoint_wk,
         std_intervention,
         val_hbsag_cont,
         n_events,
         N_num)


# 1. Define the exact string used for your control arm
control_name <- "NA"

# 2. Extract the Control arm data
df_control <- plot_dat %>%
  filter(std_intervention == control_name) %>%
  select(author_year, 
         timepoint_wk,
         events_c = n_events, 
         n_c = N_num)

# 3. Extract the Treatment arm data (everything except control)
df_treat <- plot_dat %>%
  filter(std_intervention != control_name) %>%
  select(author_year, 
         timepoint_wk,
         treatment_arm = std_intervention,
         baseline_hbsag = val_hbsag_cont, 
         events_t = n_events, 
         n_t = N_num)

# 4. Join and calculate the Risk Differences and CIs
contrast_data <- df_treat %>%
  inner_join(df_control, by = c("author_year", "timepoint_wk")) %>%
  mutate(
    # Calculate event probabilities for each arm
    p_t = events_t / n_t,
    p_c = events_c / n_c,
    
    # Calculate Risk Difference (RD)
    risk_difference = p_t - p_c,
    
    # Calculate Standard Error for the RD
    se_rd = sqrt((p_t * (1 - p_t) / n_t) + (p_c * (1 - p_c) / n_c)),
    
    # Calculate 95% Confidence Intervals
    ci_lower = risk_difference - (1.96 * se_rd),
    ci_upper = risk_difference + (1.96 * se_rd),
    
    # Calculate total sample size for the bubble weights
    total_n = n_t + n_c,
    
    # Create a clean label for the contrast (e.g., "NA + Bepi vs NA")
    contrast_label = paste(treatment_arm, "vs", control_name)
  ) %>%
  # Select and rename columns to perfectly match the plotting function
  select(
    study_name = author_year,
    time_point = timepoint_wk,
    treatment_arm = contrast_label,
    baseline_hbsag,
    total_n,
    risk_difference,
    ci_lower,
    ci_upper
  )

p_forest <- 
  contrast_data %>%
  plot_stratified_forest()

p_forest

ggsave(filename = "plots/HBsAg_loss_risk_difference_by_HBsAg_at_baseline_separate_PEG.png", 
       plot = p_forest, width = 15, height = 10)

# ----------------------------------
# continuous HBsAg loss at baseline

library(ggrepel)

plot_meta_regression_bubble <- function(data) {
  
  # Remove any rows missing the calculated effect size
  plot_data <- data %>% 
    filter(!is.na(risk_difference))
  
  ggplot(plot_data, aes(x = baseline_hbsag, y = risk_difference)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray70", linewidth = 0.8) +
    
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  width = 0, color = "gray60", alpha = 0.5) +
    
    geom_point(aes(size = total_n, color = treatment_arm), alpha = 0.7) +
    
    geom_smooth(aes(weight = total_n), method = "lm", formula = y ~ x,
                color = "black", linetype = "dashed", linewidth = 0.8, fill = "gray90") +
    
    # --- Add the study labels ---
    geom_text_repel(aes(label = study_name), 
                    size = 3.5,
                    color = "gray20",
                    box.padding = 0.6,
                    point.padding = 0.4,
                    min.segment.length = 0,
                    max.overlaps = Inf) +
    
    scale_x_log10(labels = label_comma()) +
    scale_y_continuous(labels = label_percent()) +
    
    facet_wrap(~time_point, labeller = labeller(time_point = function(x) paste("Week", x))) +
    
    theme_minimal(base_size = 14) +
    labs(
      title = "Exploratory Meta-Regression: Baseline HBsAg as an Effect Modifier",
      subtitle = "Dashed lines indicate potential interaction; bubbles sized by total study N",
      x = "Baseline HBsAg (IU/mL, log10 scale)",
      y = "Absolute Risk Difference (Treatment - Control)",
      size = "Total Sample Size (N)",
      color = "Treatment Contrast"
    ) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 12),
      legend.position = "bottom",
      legend.box = "vertical"
    )
}

p_meta_rgn <- 
  contrast_data %>% 
  plot_meta_regression_bubble()

ggsave(filename = "plots/metaregression_plot_HBsAg_RD_by_HBsAg_at_baseline_sep_PEG.png", 
       plot = p_meta_rgn, width = 15, height = 10)

study_timeline_plot <-
  study_data |>
  mutate(outcome_name = ifelse(outcome_name == "AE", "Adverse Events", outcome_name)) |> 
  mutate(followup_wk = ifelse(followup_wk < 0, 0, followup_wk)) |> 
  
  filter(!is.na(val_tx_duration), followup_wk >= 0) |>
  
  # 1. Group by the specific study arm and outcome
  group_by(author_year, std_intervention, outcome_name) |>
  
  # 2. Keep ONLY the single row with the maximum follow-up time for that group
  slice_max(order_by = followup_wk, n = 1, with_ties = FALSE) |>
  ungroup() |>
  
  # 3. Rename it to fu_end so the rest of your ggplot code works perfectly
  rename(fu_end = followup_wk) |>
  
  # 4. Create a unique Y-axis label
  mutate(study_arm = author_year) |>
  # mutate(study_arm = paste(author_year, "-", std_intervention)) |>
  
  # 5. Build the plot
  ggplot(aes(y = reorder(study_arm, fu_end))) +
  facet_grid(
    rows = vars(std_intervention), 
    cols = vars(outcome_name), 
    scales = "free_y", 
    space = "free_y"
  ) +
  
  # Treatment segment
  geom_segment(
    aes(x = 0, xend = val_tx_duration, yend = study_arm, color = "Treatment"), 
    linewidth = 2
  ) +
  
  # Follow-up segment
  geom_segment(
    aes(x = val_tx_duration, xend = val_tx_duration + fu_end, yend = study_arm, color = "Follow-up"), 
    linewidth = 1.5
  ) +
  
  # End cap point
  geom_point(
    aes(x = val_tx_duration + fu_end, color = "Follow-up"), 
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
    y = "Study"
  )

study_timeline_plot

ggsave(plot = study_timeline_plot, filename = "plots/study_timeline_plot.png",
       width = 10, height = 8)

