# Plot Covariate Bubble (Exploratory Meta-Regression)

Bubble plot of outcome rate/proportion vs baseline covariate, weighted
by sample size.

## Usage

``` r
plot_covariate_bubble(
  data,
  covariate,
  covariate_label = covariate,
  trt_var = "treatment",
  outcome_var = NULL,
  outcome_label = "Outcome Rate",
  time_var = NULL,
  title_text = NULL
)
```

## Arguments

- data:

  A data frame.

- covariate:

  Character; the covariate column name.

- covariate_label:

  Character; the label for the covariate axis.

- trt_var:

  Character; treatment column name.

- outcome_var:

  Character; outcome column name. If NULL, calculated as
  `events / sample_size`.

- outcome_label:

  Character; outcome label for the y-axis.

- time_var:

  Character; timepoint column name.

- title_text:

  Character; title of the plot.

## Value

A ggplot object.
