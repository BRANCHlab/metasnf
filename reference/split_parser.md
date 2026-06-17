# Helper function to determine which row and columns to split on

Helper function to determine which row and columns to split on

## Usage

``` r
split_parser(
  row_split_vector = NULL,
  column_split_vector = NULL,
  row_split = NULL,
  column_split = NULL,
  n_rows,
  n_columns
)
```

## Arguments

- row_split_vector:

  A vector of row indices to split on.

- column_split_vector:

  A vector of column indices to split on.

- row_split:

  Standard parameter of
  [`ComplexHeatmap::Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html).

- column_split:

  Standard parameter of
  [`ComplexHeatmap::Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html).

- n_rows:

  The number of rows in the data.

- n_columns:

  The number of columns in the data.

## Value

"list"-class object containing row_split and column_split character
vectors to pass into ComplexHeatmap::Heatmap.
