# Check Network Transitivity and Baseline Covariate Balance

Generates baseline risk and patient characteristic balance plots across
treatment groups to visually assess the assumption of transitivity
(similarity of data sources).

## Usage

``` r
preflight_transitivity(
  network,
  covariates = NULL,
  trt_var = "treatment",
  study_var = "study",
  facet_var = NULL
)
```

## Arguments

- network:

  An `nma_data` object or list.

- covariates:

  Character vector of covariate column names to check. If NULL,
  auto-detects numeric columns that are not part of the standard NMA
  structure.

- trt_var:

  Column name for treatments (default: "treatment").

- study_var:

  Column name for studies (default: "study").

- facet_var:

  Optional column name to facet plots by (e.g., "outcome_name").

## Value

A list of ggplot objects (one per covariate).
