# Extract data frame from an nma_data object

Helper function to extract a tidy data frame from a multinma network
object. It handles mapping internal `.study`, `.trt`, `.n`, etc.
variables to clean names and consolidates IPD and/or AgD data.

## Usage

``` r
extract_network_data(network)
```

## Arguments

- network:

  A network object of class `nma_data` (from `multinma`) or a list.

## Value

A data.frame containing the network data with standardized names.
