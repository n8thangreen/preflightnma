# Generate Network Connectivity Diagnostics

Entry point for generating connectivity network plots across all
timepoints and/or for specific timepoints.

## Usage

``` r
preflight_connectivity(
  network,
  trt_var = "treatment",
  trt_combo_var = NULL,
  study_var = "study",
  sample_size_var = "sample_size",
  show_study_names = TRUE,
  time_var = NULL
)
```

## Arguments

- network:

  An `nma_data` object or a list/data.frame containing network data.

- trt_var:

  Column name for treatments (default: "treatment").

- trt_combo_var:

  Optional column name for alternative treatment groupings (e.g.
  classes).

- study_var:

  Column name for study IDs (default: "study").

- sample_size_var:

  Column name for sample size (default: "sample_size").

- show_study_names:

  Logical; if TRUE, study names are plotted.

- time_var:

  Optional column name for study follow-up time (e.g. "timepoint_wk").

## Value

A list containing network diagnostic plots.
