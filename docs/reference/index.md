# Package index

## Main Workflow & Diagnostics

Functions for orchestrating network meta-analysis preflight diagnostics
and HTML reporting.

- [`nma_preflight()`](https://n8thangreen.github.io/preflightnma/reference/nma_preflight.md)
  : Run Preflight Diagnostics on an NMA Network
- [`nma_preflight_report()`](https://n8thangreen.github.io/preflightnma/reference/nma_preflight_report.md)
  : Generate Quarto Report for NMA Preflight Diagnostics

## Diagnostic Checks

Functions assessing individual dimensions of NMA feasibility.

- [`preflight_connectivity()`](https://n8thangreen.github.io/preflightnma/reference/preflight_connectivity.md)
  : Generate Network Connectivity Diagnostics
- [`preflight_covariates()`](https://n8thangreen.github.io/preflightnma/reference/preflight_covariates.md)
  : Run Covariate Diagnostics
- [`preflight_sparsity()`](https://n8thangreen.github.io/preflightnma/reference/preflight_sparsity.md)
  : Calculate Network Sparsity
- [`preflight_transitivity()`](https://n8thangreen.github.io/preflightnma/reference/preflight_transitivity.md)
  : Check Network Transitivity and Baseline Covariate Balance

## Diagnostic Visualizations

Functions for specialized NMA plots and covariate balance
visualisations.

- [`plot_covariate_bubble()`](https://n8thangreen.github.io/preflightnma/reference/plot_covariate_bubble.md)
  : Plot Covariate Bubble (Exploratory Meta-Regression)
- [`plot_stratified_forest()`](https://n8thangreen.github.io/preflightnma/reference/plot_stratified_forest.md)
  : Plot Stratified Forest Plot
- [`plot_study_timeline()`](https://n8thangreen.github.io/preflightnma/reference/plot_study_timeline.md)
  : Plot Study Timeline

## Data Preparation & Utilities

Helper functions for processing network data structures.

- [`build_network()`](https://n8thangreen.github.io/preflightnma/reference/build_network.md)
  : Build Connectivity Network Plot
- [`extract_network_data()`](https://n8thangreen.github.io/preflightnma/reference/extract_network_data.md)
  : Extract data frame from an nma_data object

## Internal Methods

S3 methods and internal printing functions.

- [`print(`*`<nma_preflight>`*`)`](https://n8thangreen.github.io/preflightnma/reference/print.nma_preflight.md)
  : Print NMA Preflight Summary
- [`print(`*`<preflight_sparsity>`*`)`](https://n8thangreen.github.io/preflightnma/reference/print.preflight_sparsity.md)
  : Print Preflight Sparsity Summary
