# Calculate Network Sparsity

Computes the sparsity of the evidence network. Sparsity is defined as
the proportion of all possible pairwise treatment comparisons that have
no direct study evidence.

## Usage

``` r
preflight_sparsity(network, trt_var = "treatment", study_var = "study")
```

## Arguments

- network:

  An `nma_data` object or list.

- trt_var:

  Column name for treatments (default: "treatment").

- study_var:

  Column name for studies (default: "study").

## Value

A list containing network sparsity metrics.
