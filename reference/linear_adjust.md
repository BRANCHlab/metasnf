# Linearly correct data list by features with unwanted signal

Given a data list to correct and another data list of categorical
features to linearly adjust for, corrects the first data list based on
the residuals of the linear model relating the numeric features in the
first data list to the unwanted signal features in the second data list.

## Usage

``` r
linear_adjust(dl, unwanted_signal_list, sig_digs = NULL)
```

## Arguments

- dl:

  A nested list of input data from
  [`data_list()`](https://branchlab.github.io/metasnf/reference/data_list.md).

- unwanted_signal_list:

  A data list of categorical features that should have their mean
  differences removed in the first data list.

- sig_digs:

  Number of significant digits to round the residuals to.

## Value

A data list ("list") in which each data component has been converted to
contain residuals off of the linear model built against the features in
the unwanted_signal_list.

## Examples

``` r
has_tutor <- sample(c(1, 0), size = 9, replace = TRUE)
math_score <- 70 + 30 * has_tutor + rnorm(9, mean = 0, sd = 5)

math_df <- data.frame(uid = paste0("id_", 1:9), math = math_score)
tutor_df <- data.frame(uid = paste0("id_", 1:9), tutor = has_tutor)

dl <- data_list(
    list(math_df, "math_score", "school", "continuous"),
    uid = "uid"
)

adjustment_dl <- data_list(
    list(tutor_df, "tutoring", "school", "categorical"),
    uid = "uid"
)

adjusted_dl <- linear_adjust(dl, adjustment_dl)

adjusted_dl[[1]]$"data"$"math"
#> [1] -7.3258939 -3.7112970  6.9599194  6.7274772  0.5984167  4.4019625 -1.6826826
#> [8] -0.6687095 -5.2991928

# Equivalent to:
as.numeric(resid(lm(math_score ~ has_tutor)))
#> [1] -7.3258939 -3.7112970  6.9599194  6.7274772  0.5984167  4.4019625 -1.6826826
#> [8] -0.6687095 -5.2991928
```
