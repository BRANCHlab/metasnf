# Helper resampling function found in ?sample

Like sample, but when given a single value x, returns back that single
value instead of a random value from 1 to x.

## Usage

``` r
resample(x, ...)
```

## Arguments

- x:

  Vector or single value to sample from

- ...:

  Remaining arguments for base::sample function

## Value

Numeric vector result of running base::sample.
