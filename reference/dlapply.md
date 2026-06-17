# Apply-like function for data list objects

This function enables manipulating a `data_list` class object with
lapply syntax without removing that object's `data_list` class
attribute. The function will only preserve this attribute if the result
of the apply call has a valid data list structure.

## Usage

``` r
dlapply(X, FUN, ...)
```

## Arguments

- X:

  A `data_list` class object.

- FUN:

  The function to be applied to each data list component.

- ...:

  Optional arguments to `FUN`.

## Value

If FUN applied to each component of X yields a valid data list, a data
list. Otherwise, a list.

## Examples

``` r
# Convert all UID values to lowercase
dl <- data_list(
    list(abcd_income, "income", "demographics", "discrete"),
    list(abcd_colour, "colour", "likes", "categorical"),
    uid = "patient"
)
#> ℹ 49 observations dropped due to incomplete data.

dl_lower <- dlapply(
    dl,
    function(x) {
        x$"data"$"uid" <- tolower(x$"data"$"uid")
        return(x)
    }
)
```
