# Adjust the diagonals of a matrix

Adjust the diagonals of a matrix to reduce contrast with off-diagonals
during plotting.

## Usage

``` r
scale_diagonals(matrix, method = "mean")
```

## Arguments

- matrix:

  Matrix to rescale.

- method:

  Method of rescaling. Can be:

  - "mean" (replace diagonals with average value of off-diagonals)

  - "zero" (replace diagonals with 0)

  - "min" (replace diagonals with min value of off-diagonals)

  - "max" (replace diagonals with max value of off-diagonals)

## Value

A "matrix" class object with rescaled diagonals.
