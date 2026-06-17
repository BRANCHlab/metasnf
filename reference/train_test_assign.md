# Training and testing split

Given a vector of uid_id and a threshold, returns a list of which
members should be in the training set and which should be in the testing
set. The function relies on whether or not the absolute value of the
Jenkins's one_at_a_time hash function exceeds the maximum possible value
(2147483647) multiplied by the threshold.

## Usage

``` r
train_test_assign(train_frac, uids, seed = 42)
```

## Arguments

- train_frac:

  The fraction (0 to 1) of observations for training

- uids:

  A character vector of UIDs to be distributed into training and test
  sets.

- seed:

  Seed used for Jenkins's one_at_a_time hash function.

## Value

A named list containing the training and testing uid_ids.
