# Run Covariate Diagnostics

Orchestrates the generation of bubble plots, stratified forest plots,
and timeline plots.

## Usage

``` r
preflight_covariates(
  network,
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
  ...
)
```

## Arguments

- network:

  An `nma_data` object or list.

- covariate:

  Character; the covariate column to examine.

- covariate_label:

  Character; the label for the covariate.

- trt_var:

  Column name for treatments.

- study_var:

  Column name for studies.

- sample_size_var:

  Column name for sample size.

- time_var:

  Column name for follow-up/time variable.

- tx_duration_var:

  Column name for treatment duration.

- reference_trt:

  Optional reference treatment name.

- breaks:

  Optional breaks for stratified forest plot.

- labels:

  Optional labels for breaks.

- outcome_var:

  Optional outcome column name.

- timeline_facet_var:

  Optional facet variable for study timeline.

- ...:

  Additional arguments.

## Value

A list of ggplot objects.
