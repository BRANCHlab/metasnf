# Helper function to stop annotation building when no data was provided

Helper function to stop annotation building when no data was provided

## Usage

``` r
check_dataless_annotations(annotation_requests, data)
```

## Arguments

- annotation_requests:

  A list of requested annotations

- data:

  A data frame with data to build annotations

## Value

Does not return any value. This function just raises an error when
annotations are requested without any provided data for a heatmap.
