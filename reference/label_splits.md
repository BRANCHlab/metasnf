# Convert a vector of partition indices into meta cluster labels

Convert a vector of partition indices into meta cluster labels

## Usage

``` r
label_splits(split_vector, nrow)
```

## Arguments

- split_vector:

  A vector of partition indices.

- nrow:

  The number of rows in the data being partitioned.

## Value

A character vector that expands the split_vector into an nrow-length
sequence of ascending letters of the alphabet. If the split vector is
c(3, 6) and the number of rows is 8, the result will be a vector of two
"A"s (up to the first index, 3), three "B"s (up to the second index, 6),
and three "C"s (up to and including the last index, 8).
