# Check if settings_df exceeds bounds of dist_fns_list

Check if settings_df exceeds bounds of dist_fns_list

## Usage

``` r
check_compatible_sdf_dfl(sdf, dfl)
```

## Arguments

- sdf:

  A `settings_df` class object.

- dfl:

  A `dist_fns_list` class object.

## Value

Doesn't return any value. Raises error if sdf calls for a distance
function outside the range of dfl.
