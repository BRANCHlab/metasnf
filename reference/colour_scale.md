# Return a colour ramp for a given vector

Given a numeric vector and min and max colour values, return a colour
ramp that assigns a colour to each element in the vector. This function
is a wrapper for
[`circlize::colorRamp2`](https://rdrr.io/pkg/circlize/man/colorRamp2.html).'

## Usage

``` r
colour_scale(data, min_colour, max_colour)
```

## Arguments

- data:

  Vector of numeric values.

- min_colour:

  Minimum colour value.

- max_colour:

  Maximum colour value.

## Value

A "function" class object that can build a circlize-style colour ramp.
