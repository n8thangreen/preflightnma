# Run Preflight Diagnostics on an NMA Network

Orchestrates all preflight checks on the network data structure:
connectivity, transitivity (baseline covariate balance), sparsity, and
covariate modifier checks.

## Usage

``` r
nma_preflight(
  network,
  covariates = NULL,
  time_var = NULL,
  tx_duration_var = NULL,
  trt_var = "treatment",
  study_var = "study",
  sample_size_var = "sample_size",
  trt_combo_var = NULL,
  timeline_facet_var = NULL,
  reference_trt = NULL,
  ...
)
```

## Arguments

- network:

  An `nma_data` object or list.

- covariates:

  Optional character vector of covariate names to assess balance.

- time_var:

  Optional column name for timepoint/follow-up week (e.g.
  "timepoint_wk").

- tx_duration_var:

  Optional column name for treatment duration (e.g. "val_tx_duration").

- trt_var:

  Column name for treatments (default: "treatment").

- study_var:

  Column name for studies (default: "study").

- sample_size_var:

  Column name for sample sizes (default: "sample_size").

- trt_combo_var:

  Optional column name for alternative treatment groupings.

- timeline_facet_var:

  Optional column name to facet the timeline plot by.

- reference_trt:

  Optional name of reference treatment.

- ...:

  Additional arguments passed to plotting functions.

## Value

An object of class `nma_preflight` containing sparsity, connectivity
plots, transitivity plots, and covariate modifier plots.
