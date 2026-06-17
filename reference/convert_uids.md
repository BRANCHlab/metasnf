# Convert unique identifiers of data list to "uid"

Column name "uid" is reserved for the unique identifier of observations.
This function ensures all data frames have their UID set as "uid".

## Usage

``` r
convert_uids(dll, uid)
```

## Arguments

- dll:

  A data list-like `list` class object.

- uid:

  (string) the name of the uid column currently used data

## Value

The provided nested list with "uid" as UID.
