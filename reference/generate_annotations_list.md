# Generate annotations list

Intermediate function that takes in formatted lists of features and the
annotations they should be viewed through and returns annotation objects
usable by ComplexHeatmap::Heatmap.

## Usage

``` r
generate_annotations_list(
  df,
  left_bar = NULL,
  right_bar = NULL,
  top_bar = NULL,
  bottom_bar = NULL,
  left_hm = NULL,
  right_hm = NULL,
  top_hm = NULL,
  bottom_hm = NULL,
  show_legend = TRUE,
  annotation_colours = NULL
)
```

## Arguments

- df:

  data frame containing all the data that is specified in the remaining
  arguments.

- left_bar:

  Named list of strings, where the strings are features in df that
  should be used for a barplot annotation on the left of the plot and
  the names are the names that will be used to caption the plots and
  their legends.

- right_bar:

  See left_bar.

- top_bar:

  See left_bar.

- bottom_bar:

  See left_bar.

- left_hm:

  Like left_bar, but with a heatmap annotation instead of a barplot
  annotation.

- right_hm:

  See left_hm.

- top_hm:

  See left_hm.

- bottom_hm:

  See left_hm.

- show_legend:

  Add legends to the annotations.

- annotation_colours:

  Named list of heatmap annotations and their colours.

## Value

annotations_list A named list of all the annotations.
