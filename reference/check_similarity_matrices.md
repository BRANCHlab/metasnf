# Check validity of similarity matrices

Check to see if similarity matrices in a list have the following
properties:

1.  The maximum value in the entire matrix is 0.5

2.  Every value in the diagonal is 0.5

## Usage

``` r
check_similarity_matrices(similarity_matrices)
```

## Arguments

- similarity_matrices:

  A list of similarity matrices

## Value

valid_matrices Boolean indicating if properties are met by all
similarity matrices
