# Plot Stratified Forest Plot

Generates a forest plot of treatment contrasts (e.g. Risk Differences)
stratified by baseline covariate categories.

## Usage

``` r
plot_stratified_forest(
  data,
  covariate,
  effect_col = "risk_difference",
  lower_ci = "ci_lower",
  upper_ci = "ci_upper",
  study_col = "study",
  trt_col = "treatment",
  time_col = NULL,
  breaks = NULL,
  labels = NULL
)
```

## Arguments

- data:

  A data frame.

- covariate:

  Character; baseline covariate column.

- effect_col:

  Column for treatment effects (default: "risk_difference").

- lower_ci:

  Column for lower CI.

- upper_ci:

  Column for upper CI.

- study_col:

  Column for study names.

- trt_col:

  Column for treatments.

- time_col:

  Column for timepoints.

- breaks:

  Optional numeric breaks to categorize the covariate.

- labels:

  Optional labels for the breaks.

## Value

A ggplot object.
