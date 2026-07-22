# Plot Study Timeline

Visualizes the duration of treatment and follow-up across different
studies and arms.

## Usage

``` r
plot_study_timeline(
  data,
  study_var = "study",
  trt_var = "treatment",
  tx_duration_var = "val_tx_duration",
  followup_var = "timepoint_wk",
  facet_var = NULL
)
```

## Arguments

- data:

  A data frame.

- study_var:

  Column name for study IDs.

- trt_var:

  Column name for treatments.

- tx_duration_var:

  Column name for treatment duration.

- followup_var:

  Column name for total follow-up time.

- facet_var:

  Optional column name to facet the timeline plot by.

## Value

A ggplot object.
