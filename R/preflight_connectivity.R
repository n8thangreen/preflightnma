
#' Network of evidence plots.
#' 
#' Node-Splitting Forest Plot
#' Assessing the "clash" between evidence sources.
preflight_connectivity <- function() {
  
}

build_network <- function(data, grouping_var, title_text, show_study_names = TRUE) {
  grp <- sym(grouping_var)
  
  arms <- data |>
    distinct(ref_id, author_year, arm = !!grp) |>
    filter(!is.na(arm), arm != "")
  
  multi_arm <- arms |>
    group_by(ref_id) |>
    filter(n() >= 2) |>
    ungroup()
  
  if (nrow(multi_arm) == 0) return(invisible(NULL))
  
  edges <- multi_arm |>
    group_by(ref_id) |>
    summarise(pairs = list(combn(sort(unique(arm)), 2, simplify = FALSE)), .groups = "drop") |>
    unnest(pairs) |>
    mutate(from = map_chr(pairs, 1), to = map_chr(pairs, 2)) |>
    select(from, to) |>
    count(from, to, name = "n_studies")
  
  node_n <- data |>
    rename(arm = !!grp) |>
    filter(!is.na(arm), arm != "") |>
    group_by(arm) |>
    summarise(
      total_N = sum(N_num, na.rm = TRUE),
      
      # Adjust 'width' to make the stack narrower or wider
      study_labels = str_wrap(paste(unique(author_year[!is.na(author_year)]), collapse = ", "), width = 25),
      
      .groups = "drop"
    ) |>
    mutate(
      node_label = if_else(
        show_study_names & study_labels != "", 
        paste0(str_to_upper(arm), "\n(", study_labels, ")"), 
        as.character(str_to_upper(arm))
      )
    )
  
  edge_nodes <- union(edges$from, edges$to)
  node_n <- node_n |> filter(arm %in% edge_nodes)
  
  g <- graph_from_data_frame(edges, directed = FALSE, vertices = node_n |> rename(name = arm))
  
  ggraph(g, layout = "fr") +
    geom_edge_link(aes(width = n_studies, alpha = n_studies), colour = "#4E79A7", show.legend = TRUE) +
    geom_node_point(aes(size = total_N), colour = "#F28E2B") +
    # Update aes(label = ...) to use the pre-calculated node_label
    geom_node_label(aes(label = node_label), repel = TRUE, size = 3, fill = "white", 
                    label.size = 0.2, label.padding = unit(0.15, "lines"), max.overlaps = 50) +
    scale_edge_width_continuous(range = c(0.8, 4), name = "# Studies", breaks = function(x) { b <- pretty(x); b[b %% 1 == 0] }) +
    scale_edge_alpha_continuous(range = c(0.4, 1.0), guide = "none") +
    scale_size_continuous(range = c(3, 14), name = "Total N") +
    labs(title = title_text) +
    theme_graph(base_family = "sans") +
    theme(plot.title = element_text(size = 11, face = "bold"), legend.position = "right", legend.key.width = unit(1.2, "lines"))
}


build_bubble_plot <- function(data, x_var, x_label, grp_var, title_text, legend_title, outcome_label) {
  plot_data <- data |>
    filter(!is.na(.data[[x_var]]), !is.na(proportion), !is.na(N_num), !is.na(.data[[grp_var]]))
  
  if (nrow(plot_data) == 0) return(invisible(NULL))
  
  cor_data <- plot_data |>
    group_by(.data[[grp_var]]) |>
    summarise(r_val = suppressWarnings(cor(.data[[x_var]], proportion, use = "complete.obs")), .groups = "drop") |>
    mutate(r_label = if_else(is.na(r_val), "N/A", sprintf("%.2f", r_val)), 
           legend_label = paste0(.data[[grp_var]], " (r = ", r_label, ")"))
  
  plot_data <- plot_data |> left_join(cor_data, by = grp_var)
  
  # FIX 1: Convert legend_label to a factor so ggplot locks the color mapping order
  plot_data <- plot_data |> mutate(legend_label = as.factor(legend_label))
  
  n_tx <- n_distinct(plot_data$legend_label)
  
  # Suppress the geom_smooth warnings so it doesn't spam your console when 'lm' fails on vertical stacks
  ggplot(plot_data, aes(x = .data[[x_var]], y = proportion, fill = legend_label, colour = legend_label)) +
    suppressWarnings(geom_smooth(aes(weight = N_num), method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.8, alpha = 0.6)) +
    geom_point(aes(size = N_num), shape = 21, colour = "white", alpha = 0.75) +
    
    # NEW: Add ggrepel layer to label the bubbles with Author and Year
    geom_text_repel(aes(label = author_year), size = 3, show.legend = FALSE, 
                    color = "grey20", bg.color = "white", bg.r = 0.15, 
                    max.overlaps = 20, min.segment.length = 0) +
    
    scale_size_continuous(range = c(2, 12)) +
    
    # FIX 2: Add drop = FALSE to ensure skipped lines don't shift the color palette
    scale_fill_brewer(palette = if (n_tx <= 8) "Dark2" else "Paired", drop = FALSE) +
    scale_colour_brewer(palette = if (n_tx <= 8) "Dark2" else "Paired", drop = FALSE) +
    
    guides(
      fill = guide_legend(title = legend_title, override.aes = list(shape = 21, size = 4, colour = NA)), colour = "none",
      size = guide_legend(title = "Study arm\nsample size", override.aes = list(shape = 21, fill = "grey70", colour = "grey30", alpha = 0.8))
    ) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA)) +
    labs(
      title = title_text,
      subtitle = "Trendlines are weighted by sample size (N). 'r' indicates Pearson correlation.",
      x = x_label,
      y = paste0("Proportion of patients with ", outcome_label)
    ) +
    theme_bw(base_size = 11) +
    theme(legend.position = "right", plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(colour = "grey40", size = 9), panel.grid.minor = element_blank())
}

generate_networks <- function(df_filt, outcome_label, output_prefix, show_study_names = TRUE) {
  
  out_dir <- paste0(output_prefix, "_outputs/")
  dir.create(out_dir, showWarnings = FALSE)
  
  network_plots <- list()
  
  for (grp_var in c("std_intervention", "std_combo")) {
    grp_label <- if (grp_var == "std_intervention") "Strategy_1" else "Strategy_2"
    
    # Overall Network (Pass the flag here)
    p <- build_network(df_filt, grp_var, title_text = paste0(outcome_label, " – All Timepoints\n", grp_label), show_study_names = show_study_names)
    if (!is.null(p)) network_plots[[paste0("All_", grp_label)]] <- p
    
    # Timepoint-specific Networks (Pass the flag here)
    for (tp in sort(unique(df_filt$timepoint_wk))) {
      p <- build_network(filter(df_filt, timepoint_wk == tp), grp_var, title_text = paste0(outcome_label, " – Week ", tp, "\n", grp_label), show_study_names = show_study_names)
      if (!is.null(p)) network_plots[[paste0("Week_", tp, "_", grp_label)]] <- p
    }
  }
  
  if (length(network_plots) > 0) {
    # Loop through and save each plot as a separate png
    for (nm in names(network_plots)) {
      
      # Ensure the filename is completely safe for all operating systems
      safe_name <- str_replace_all(nm, "[^A-Za-z0-9_]", "_")
      safe_name <- str_replace_all(safe_name, "_+", "_") # Remove double underscores
      
      file_name <- file.path(out_dir, paste0(output_prefix, "_Network_", safe_name, ".png"))
      
      ggsave(
        filename = file_name,
        plot     = network_plots[[nm]],
        width    = 10, 
        height   = 8, 
        device   = "png"
      )
    }
    cat("Saved", length(network_plots), "individual Network Graphs to ->", out_dir, "\n")
  } else {
    cat("No valid multi-arm studies found to generate networks.\n")
  }
}

build_network_all_timespoints <- function(data, grouping_var, title_text, max_N = NULL, max_studies = NULL, show_study_names = TRUE) {
  grp <- sym(grouping_var)
  
  # Added author_year to the distinct call
  arms <- data |>
    distinct(ref_id, author_year, arm = !!grp) |>
    filter(!is.na(arm), arm != "")
  
  multi_arm <- arms |>
    group_by(ref_id) |>
    filter(n() >= 2) |>
    ungroup()
  
  if (nrow(multi_arm) == 0) return(invisible(NULL))
  
  edges <- multi_arm |>
    group_by(ref_id) |>
    summarise(pairs = list(combn(sort(unique(arm)), 2, simplify = FALSE)), .groups = "drop") |>
    unnest(pairs) |>
    mutate(from = map_chr(pairs, 1), to = map_chr(pairs, 2)) |>
    select(from, to) |>
    count(from, to, name = "n_studies")
  
  # Aggregate total_N and study_labels
  node_n <- data |>
    rename(arm = !!grp) |>
    filter(!is.na(arm), arm != "") |>
    group_by(arm) |>
    summarise(
      total_N = sum(N_num, na.rm = TRUE),
      
      # Adjust 'width' to make the stack narrower or wider.
      study_labels = str_wrap(paste(unique(author_year[!is.na(author_year)]), collapse = ", "), width = 25),
      
      .groups = "drop"
    ) |>
    mutate(
      node_label = if_else(
        show_study_names & study_labels != "", 
        paste0(str_to_upper(arm), "\n(", study_labels, ")"), 
        as.character(str_to_upper(arm))
      )
    )
  
  edge_nodes <- union(edges$from, edges$to)
  node_n <- node_n |> filter(arm %in% edge_nodes)
  
  g <- graph_from_data_frame(edges, directed = FALSE, vertices = node_n |> rename(name = arm))
  
  # Set limits dynamically: Use global limits if provided, otherwise let ggplot decide
  limit_N <- if (!is.null(max_N)) c(0, max_N) else NULL
  limit_edges <- if (!is.null(max_studies)) c(1, max_studies) else NULL
  
  ggraph(g, layout = "fr") +
    geom_edge_link(aes(width = n_studies, alpha = n_studies), colour = "#4E79A7", show.legend = TRUE) +
    geom_node_point(aes(size = total_N), colour = "#F28E2B") +
    # Use the new node_label variable
    geom_node_label(aes(label = node_label), repel = TRUE, size = 3, fill = "white", 
                    label.size = 0.2, label.padding = unit(0.15, "lines"), max.overlaps = 50) +
    # Force unified limits so patchwork merges the legends perfectly
    scale_edge_width_continuous(range = c(0.8, 4), name = "# Studies", limits = limit_edges, breaks = function(x) { b <- pretty(x); b[b %% 1 == 0] }) +
    scale_edge_alpha_continuous(range = c(0.4, 1.0), guide = "none") +
    scale_size_continuous(range = c(3, 14), name = "Total N", limits = limit_N) +
    # Fix the clipping by turning off clip and physically expanding the mapping space
    coord_cartesian(clip = "off") +
    scale_x_continuous(expand = expansion(mult = 0.3)) + 
    scale_y_continuous(expand = expansion(mult = 0.3)) +
    labs(title = title_text) +
    theme_graph(base_family = "sans") +
    theme(
      plot.title = element_text(size = 11, face = "bold"), 
      legend.position = "right", 
      legend.key.width = unit(1.2, "lines"),
      # Add margin padding so repelled labels don't hit the absolute edge of the image
      plot.margin = margin(15, 15, 15, 15) 
    )
}

generate_networks_all_timepoints <- function(df_filt, outcome_label, output_prefix, show_study_names = TRUE) {
  
  out_dir <- paste0(output_prefix, "_outputs/")
  dir.create(out_dir, showWarnings = FALSE)
  
  # =========================================================================
  # 1. OVERALL SIDE-BY-SIDE PLOT (Strategy 1 vs Strategy 2 - All Timepoints)
  # =========================================================================
  
  # Calculate max N across both strategies (ignoring timepoints)
  get_max_n_overall <- function(var) {
    df_filt |> 
      filter(!is.na(.data[[var]]), .data[[var]] != "") |>
      group_by(.data[[var]]) |>
      summarise(tot_N = sum(N_num, na.rm = TRUE), .groups = "drop") |>
      pull(tot_N) |> 
      max(na.rm = TRUE)
  }
  global_max_n_all <- max(get_max_n_overall("std_intervention"), get_max_n_overall("std_combo"), na.rm = TRUE)
  
  # Calculate max edge width across both strategies (ignoring timepoints)
  get_max_studies_overall <- function(var) {
    df_filt |> 
      filter(!is.na(.data[[var]]), .data[[var]] != "") |>
      group_by(ref_id) |> 
      filter(n() >= 2) |>
      summarise(pairs = list(combn(sort(unique(.data[[var]])), 2, simplify = FALSE)), .groups = "drop") |>
      unnest(pairs) |> 
      mutate(from = map_chr(pairs, 1), to = map_chr(pairs, 2)) |>
      count(from, to) |> 
      pull(n) |>
      suppressWarnings() |> 
      max(na.rm = TRUE)
  }
  global_max_studies_all <- max(get_max_studies_overall("std_intervention"), get_max_studies_overall("std_combo"), na.rm = TRUE)
  if (is.infinite(global_max_studies_all)) global_max_studies_all <- 1
  
  # Generate the two overall plots (passing the flag)
  p_strat1_all <- build_network_all_timespoints(
    df_filt, "std_intervention", title_text = "Strategy 1 (Separate PEGs)", 
    max_N = global_max_n_all, max_studies = global_max_studies_all, show_study_names = show_study_names
  )
  p_strat2_all <- build_network_all_timespoints(
    df_filt, "std_combo", title_text = "Strategy 2 (Combined PEGs)", 
    max_N = global_max_n_all, max_studies = global_max_studies_all, show_study_names = show_study_names
  )
  
  # Combine and save the overall plot
  if (!is.null(p_strat1_all) || !is.null(p_strat2_all)) {
    plot_list <- purrr::compact(list(p_strat1_all, p_strat2_all))
    combined_overall <- wrap_plots(plot_list, ncol = 2) +
      plot_annotation(
        title = paste0(outcome_label, " – Overall Network (All Timepoints)"),
        theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
      ) +
      plot_layout(guides = "collect") 
    
    ggsave(
      filename = file.path(out_dir, paste0(output_prefix, "_Network_Overall_Combined.png")),
      plot     = combined_overall, width = 16, height = 8, device = "png"
    )
  }
  
  # =========================================================================
  # 2. TIMEPOINT-SPECIFIC GRIDS (Week 24, 48, etc., for each strategy)
  # =========================================================================
  
  for (grp_var in c("std_intervention", "std_combo")) {
    grp_label <- if (grp_var == "std_intervention") "Strategy_1" else "Strategy_2"
    grp_sym <- sym(grp_var)
    
    # Calculate limits specifically for the timepoints of THIS strategy
    max_n_tp <- df_filt |>
      filter(!is.na(!!grp_sym), !!grp_sym != "") |>
      group_by(timepoint_wk, !!grp_sym) |>
      summarise(tot_N = sum(N_num, na.rm = TRUE), .groups = "drop") |>
      pull(tot_N) |> max(na.rm = TRUE)
    
    max_studies_tp <- df_filt |>
      filter(!is.na(!!grp_sym), !!grp_sym != "") |>
      group_by(timepoint_wk, ref_id) |>
      filter(n() >= 2) |>
      summarise(pairs = list(combn(sort(unique(!!grp_sym)), 2, simplify = FALSE)), .groups = "drop") |>
      unnest(pairs) |> mutate(from = map_chr(pairs, 1), to = map_chr(pairs, 2)) |>
      count(timepoint_wk, from, to) |> pull(n) |> suppressWarnings() |> max(na.rm = TRUE)
    
    if (is.infinite(max_studies_tp)) max_studies_tp <- 1 
    
    # Generate the timepoint-specific networks
    tp_plots <- list()
    
    set.seed(42)
    
    for (tp in sort(unique(df_filt$timepoint_wk))) {
      # Passing the flag down here as well
      p <- build_network_all_timespoints(
        filter(df_filt, timepoint_wk == tp), grp_var, 
        title_text = paste0("Week ", tp),
        max_N = max_n_tp, max_studies = max_studies_tp, show_study_names = show_study_names
      )
      if (!is.null(p)) tp_plots[[paste0("Week_", tp)]] <- p
    }
    
    # Combine into a grid and save
    if (length(tp_plots) > 0) {
      combined_grid <- wrap_plots(tp_plots) +
        plot_annotation(
          title = paste0(outcome_label, " Network Plots by Timepoint"),
          subtitle = grp_label,
          theme = theme(plot.title = element_text(size = 16, face = "bold"))
        ) +
        plot_layout(guides = "collect") 
      
      n_plots <- length(tp_plots)
      n_cols  <- ceiling(sqrt(n_plots))
      n_rows  <- ceiling(n_plots / n_cols)
      
      ggsave(
        filename = file.path(out_dir, paste0(output_prefix, "_Network_Grid_", grp_label, ".png")),
        plot     = combined_grid, width = 5 * n_cols, height = 4 * n_rows,  
        device   = "png", limitsize = FALSE        
      )
    }
  }
  
  cat("Successfully saved all Network Overall and Grid plots for", outcome_label, "->", out_dir, "\n")
}

