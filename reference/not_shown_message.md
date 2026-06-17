# Helper function for creating what hidden ft/obs/sols message

Helper function for creating what hidden ft/obs/sols message

## Usage

``` r
not_shown_message(
  hidden_solutions = NULL,
  hidden_observations = NULL,
  hidden_features = NULL
)
```

## Arguments

- hidden_solutions:

  Number of hidden solutions.

- hidden_observations:

  Number of hidden observations.

- hidden_features:

  Number of hidden features.

## Value

If all arguments are NULL or 0, returns NULL. Otherwise, output a neatly
formatted string indicating how many observations, features, and/or
observations were not shown.
