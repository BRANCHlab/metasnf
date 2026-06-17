# Built-in clustering algorithms

These functions can be used when building a `metasnf` clustering
functions list. Each function converts a similarity matrix (matrix class
object) to a cluster solution (numeric vector). Note that these
functions (or custom clustering functions) cannot accept number of
clusters as a parameter; this value must be built into the function
itself if necessary.

## Usage

``` r
spectral_eigen(similarity_matrix)

spectral_rot(similarity_matrix)

spectral_eigen_classic(similarity_matrix)

spectral_rot_classic(similarity_matrix)

spectral_two(similarity_matrix)

spectral_three(similarity_matrix)

spectral_four(similarity_matrix)

spectral_five(similarity_matrix)

spectral_six(similarity_matrix)

spectral_seven(similarity_matrix)

spectral_eight(similarity_matrix)

spectral_nine(similarity_matrix)

spectral_ten(similarity_matrix)
```

## Arguments

- similarity_matrix:

  A similarity matrix.

## Value

solution_data A vector of cluster assignments

## Details

- spectral_eigen: Spectral clustering where the number of clusters is
  based on the eigen-gap heuristic

- spectral_rot: Spectral clustering where the number of clusters is
  based on the rotation-cost heuristic

- spectral\_(C): Spectral clustering for a C-cluster solution.
