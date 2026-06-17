# Check if settings_df exceeds bounds of clust_fns_list

Check if settings_df exceeds bounds of clust_fns_list

## Usage

``` r
check_compatible_sdf_cfl(sdf, cfl)
```

## Arguments

- sdf:

  A `settings_df` class object.

- cfl:

  A `clust_fns_list` class object.

## Value

Doesn't return any value. Raises error if sdf calls for a clustering
function outside the range of cfl.
