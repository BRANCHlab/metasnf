# Manhattan plot of feature-feature association p-values

Manhattan plot of feature-feature association p-values

## Usage

``` r
var_manhattan_plot(
  dl,
  key_var,
  neg_log_pval_thresh = 5,
  threshold = NULL,
  point_size = 5,
  text_size = 20,
  plot_title = NULL,
  hide_x_labels = FALSE,
  bonferroni_line = FALSE
)
```

## Arguments

- dl:

  List of data frames containing data information.

- key_var:

  Feature for which the association p-values of all other features are
  plotted.

- neg_log_pval_thresh:

  Threshold for negative log p-values.

- threshold:

  p-value threshold to plot dashed line at.

- point_size:

  Size of points in the plot.

- text_size:

  Size of text in the plot.

- plot_title:

  Title of the plot.

- hide_x_labels:

  If TRUE, hides x-axis labels.

- bonferroni_line:

  If TRUE, plots a dashed black line at the Bonferroni-corrected
  equivalent of the p-value threshold.

## Value

A Manhattan plot (class "gg", "ggplot") showing the association p-values
of features against one key feature in a data list.

## Examples

``` r
dl <- data_list(
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    list(depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)
#> ℹ 188 observations dropped due to incomplete data.

var_manhattan <- var_manhattan_plot(
    dl,
    key_var = "household_income",
    plot_title = "Correlation of Features with Household Income",
    text_size = 16,
    neg_log_pval_thresh = 3,
    threshold = 0.05
)
```
