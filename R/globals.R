# Declare global variables to avoid R CMD check notes about "no visible binding for global variable"
# when using tidyverse/dplyr non-standard evaluation.
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "arm", "from", "to", "study_labels", "n_studies", "total_N", "node_label", 
    "proportion", "r_val", "r_label", "legend_label", "sample_size", "study", 
    "total_n", "study_arm", "fu_end", "tot_N", "pairs", ":="
  ))
}
