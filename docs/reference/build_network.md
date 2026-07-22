# Build Connectivity Network Plot

Construct and render a network diagram of treatment comparisons. Node
sizes represent the total sample size, and edge thicknesses/opacities
represent the number of studies comparing those treatments.

## Usage

``` r
build_network(
  data,
  grouping_var = "treatment",
  study_var = "study",
  sample_size_var = "sample_size",
  title_text = "",
  show_study_names = TRUE,
  max_N = NULL,
  max_studies = NULL
)
```

## Arguments

- data:

  A data frame containing study-level arm data.

- grouping_var:

  Column name for treatments (e.g., "treatment").

- study_var:

  Column name for study IDs (e.g., "study").

- sample_size_var:

  Column name for sample size (e.g., "sample_size").

- title_text:

  Title of the plot.

- show_study_names:

  Logical; if TRUE, study identifiers are listed next to the nodes.

- max_N:

  Optional numeric value to cap/standardize the node size scale across
  multiple plots.

- max_studies:

  Optional numeric value to cap/standardize the edge width scale across
  multiple plots.

## Value

A ggraph/ggplot object representing the network plot.
