
#' Similarity of Data Sources
#' 
#' Baseline Risk Distribution Plots	
#' Quantifying clinical heterogeneity visually.
preflight_transitivity <- function(network, covariates = NULL) {
  # Are baseline risks (or patient characteristics) balanced across treatments?
  
  # dot plots
  age_plot <-
    study_data |>
    mutate(outcome_name = ifelse(outcome_name == "AE", "Adverse Events", outcome_name)) |> 
    ggplot(aes(x = mean_age, std_intervention, colour = author_year)) +
    facet_wrap("outcome_name") +
    geom_point(size = 5) +
    theme_bw() + 
    xlab("Mean age") +
    ylab("Intervention") + 
    labs(colour = "Study") +
    theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 12))
  
  age_plot
  
  ggsave(plot = age_plot, filename = "plots/age_plot.png",
         width = 10, height = 8)
  
  # summaries: histograms etc
  
}