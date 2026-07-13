#' Build Connectivity Network Plot
#'
#' Construct and render a network diagram of treatment comparisons. Node sizes represent the total
#' sample size, and edge thicknesses/opacities represent the number of studies comparing those treatments.
#'
#' @param data A data frame containing study-level arm data.
#' @param grouping_var Column name for treatments (e.g., "treatment").
#' @param study_var Column name for study IDs (e.g., "study").
#' @param sample_size_var Column name for sample size (e.g., "sample_size").
#' @param title_text Title of the plot.
#' @param show_study_names Logical; if TRUE, study identifiers are listed next to the nodes.
#' @param max_N Optional numeric value to cap/standardize the node size scale across multiple plots.
#' @param max_studies Optional numeric value to cap/standardize the edge width scale across multiple plots.
#'
#' @return A ggraph/ggplot object representing the network plot.
#' @export
#' @import ggplot2
#' @import ggraph
#' @import igraph
#' @importFrom dplyr distinct filter group_by summarise ungroup mutate rename select count n sym pull
#' @importFrom utils combn
#' @importFrom stringr str_wrap str_to_upper
#' @importFrom purrr map_chr
#' @importFrom tidyr unnest
build_network <- function(data, 
                          grouping_var = "treatment", 
                          study_var = "study", 
                          sample_size_var = "sample_size", 
                          title_text = "", 
                          show_study_names = TRUE,
                          max_N = NULL,
                          max_studies = NULL) {
  
  grp <- sym(grouping_var)
  std <- sym(study_var)
  ss <- sym(sample_size_var)
  
  arms <- data |>
    distinct(!!std, arm = !!grp) |>
    filter(!is.na(arm), arm != "")
  
  multi_arm <- arms |>
    group_by(!!std) |>
    filter(n() >= 2) |>
    ungroup()
  
  if (nrow(multi_arm) == 0) return(NULL)
  
  edges <- multi_arm |>
    group_by(!!std) |>
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
      total_N = sum(.data[[sample_size_var]], na.rm = TRUE),
      study_labels = str_wrap(paste(unique(.data[[study_var]][!is.na(.data[[study_var]])]), collapse = ", "), width = 25),
      .groups = "drop"
    ) |>
    mutate(
      node_label = ifelse(
        show_study_names & study_labels != "", 
        paste0(str_to_upper(arm), "\n(", study_labels, ")"), 
        as.character(str_to_upper(arm))
      )
    )
  
  edge_nodes <- union(edges$from, edges$to)
  node_n <- node_n |> filter(arm %in% edge_nodes)
  
  g <- graph_from_data_frame(edges, directed = FALSE, vertices = node_n |> rename(name = arm))
  
  limit_N <- if (!is.null(max_N)) c(0, max_N) else NULL
  limit_edges <- if (!is.null(max_studies)) c(1, max_studies) else NULL
  
  ggraph(g, layout = "fr") +
    geom_edge_link(aes(width = n_studies, alpha = n_studies), colour = "#4E79A7", show.legend = TRUE) +
    geom_node_point(aes(size = total_N), colour = "#F28E2B") +
    geom_node_label(aes(label = node_label), repel = TRUE, size = 3, fill = "white", 
                    label.size = 0.2, label.padding = unit(0.15, "lines"), max.overlaps = 50) +
    scale_edge_width_continuous(range = c(0.8, 4), name = "# Studies", limits = limit_edges, breaks = function(x) { b <- pretty(x); b[b %% 1 == 0] }) +
    scale_edge_alpha_continuous(range = c(0.4, 1.0), guide = "none") +
    scale_size_continuous(range = c(3, 14), name = "Total N", limits = limit_N) +
    coord_cartesian(clip = "off") +
    scale_x_continuous(expand = expansion(mult = 0.3)) + 
    scale_y_continuous(expand = expansion(mult = 0.3)) +
    labs(title = title_text) +
    theme_graph(base_family = "sans") +
    theme(
      plot.title = element_text(size = 11, face = "bold"), 
      legend.position = "right", 
      legend.key.width = unit(1.2, "lines"),
      plot.margin = margin(15, 15, 15, 15)
    )
}

#' Generate Network Connectivity Diagnostics
#'
#' Entry point for generating connectivity network plots across all timepoints
#' and/or for specific timepoints.
#'
#' @param network An \code{nma_data} object or a list/data.frame containing network data.
#' @param trt_var Column name for treatments (default: "treatment").
#' @param trt_combo_var Optional column name for alternative treatment groupings (e.g. classes).
#' @param study_var Column name for study IDs (default: "study").
#' @param sample_size_var Column name for sample size (default: "sample_size").
#' @param show_study_names Logical; if TRUE, study names are plotted.
#' @param time_var Optional column name for study follow-up time (e.g. "timepoint_wk").
#'
#' @return A list containing network diagnostic plots.
#' @export
#' @import patchwork
#' @importFrom purrr compact
preflight_connectivity <- function(network, 
                                   trt_var = "treatment", 
                                   trt_combo_var = NULL, 
                                   study_var = "study",
                                   sample_size_var = "sample_size",
                                   show_study_names = TRUE,
                                   time_var = NULL) {
  
  data <- extract_network_data(network)
  plots <- list()
  
  # Determine all treatment grouping variables to analyze
  grp_vars <- trt_var
  if (!is.null(trt_combo_var)) {
    grp_vars <- c(grp_vars, trt_combo_var)
  }
  
  for (grp in grp_vars) {
    grp_label <- if (grp == trt_var) "Primary Treatments" else "Alternative Grouping"
    
    # 1. Overall network across all timepoints
    p_all <- build_network(
      data, grouping_var = grp, study_var = study_var, 
      sample_size_var = sample_size_var, 
      title_text = paste(grp_label, "- All Timepoints"),
      show_study_names = show_study_names
    )
    if (!is.null(p_all)) plots[[paste0("Overall_", grp)]] <- p_all
    
    # 2. Timepoint-specific networks
    if (!is.null(time_var) && time_var %in% names(data)) {
      timepoints <- sort(unique(data[[time_var]]))
      if (length(timepoints) > 1) {
        # Calculate maximum N and studies to keep scale consistent
        # Max N per treatment arm at any timepoint
        max_n_val <- data |> 
          filter(!is.na(.data[[grp]]), .data[[grp]] != "") |>
          group_by(.data[[time_var]], .data[[grp]]) |>
          summarise(tot_N = sum(.data[[sample_size_var]], na.rm = TRUE), .groups = "drop") |>
          pull(tot_N) |> 
          max(na.rm = TRUE)
        if (is.infinite(max_n_val)) max_n_val <- NULL
        
        tp_plots <- list()
        for (tp in timepoints) {
          sub_data <- data[data[[time_var]] == tp, ]
          p_tp <- build_network(
            sub_data, grouping_var = grp, study_var = study_var,
            sample_size_var = sample_size_var,
            title_text = paste("Week", tp),
            show_study_names = show_study_names,
            max_N = max_n_val
          )
          if (!is.null(p_tp)) tp_plots[[paste0("Week_", tp)]] <- p_tp
        }
        
        if (length(tp_plots) > 0) {
          combined_tp <- wrap_plots(tp_plots) +
            plot_annotation(
              title = paste(grp_label, "by Timepoint"),
              theme = theme(plot.title = element_text(size = 14, face = "bold"))
            ) +
            plot_layout(guides = "collect")
          plots[[paste0("Timepoint_Grid_", grp)]] <- combined_tp
        }
      }
    }
  }
  
  return(plots)
}
