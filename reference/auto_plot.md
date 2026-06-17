# Automatically plot features across clusters

Given a single row of a solutions data frame and data provided through a
data list, this function will return a series of bar and/or jitter plots
based on feature types.

## Usage

``` r
auto_plot(
  sol_df_row = NULL,
  dl = NULL,
  cluster_df = NULL,
  return_plots = TRUE,
  save = NULL,
  jitter_width = 6,
  jitter_height = 6,
  bar_width = 6,
  bar_height = 6,
  verbose = FALSE
)
```

## Arguments

- sol_df_row:

  A single row of a solutions data frame.

- dl:

  A data list containing data to plot.

- cluster_df:

  Directly provide a cluster_df rather than a solutions matrix. Useful
  if plotting data from label propagated results.

- return_plots:

  If `TRUE`, the function will return a list of plots. If FALSE, the
  function will instead return the full data frame used for plotting.

- save:

  If a string is provided, plots will be saved and this string will be
  used to prefix plot names.

- jitter_width:

  Width of jitter plots if save is specified.

- jitter_height:

  Height of jitter plots if save is specified.

- bar_width:

  Width of bar plots if save is specified.

- bar_height:

  Height of bar plots if save is specified.

- verbose:

  If TRUE, output progress to console.

## Value

By default, returns a list of plots (class "gg", "ggplot") with one plot
for every feature in the provided data list and/or target list. If
`return_plots` is FALSE, will instead return a single "data.frame"
object containing every provided feature for every observation in long
format.
