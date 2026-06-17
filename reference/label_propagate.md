# Label propagate cluster solutions to non-clustered observations

Given a solutions data frame containing clustered observations and a
data list containing those clustered observations as well as additional
to-be-clustered observations, this function will re-run SNF to generate
a similarity matrix of all observations and use the label propagation
algorithm to assigned predicted clusters to the non-clustered
observations.

## Usage

``` r
label_propagate(partial_sol_df, full_dl, verbose = FALSE)
```

## Arguments

- partial_sol_df:

  A solutions data frame derived from the training set.

- full_dl:

  A data list containing observations from both the training and testing
  sets.

- verbose:

  If TRUE, output progress to console.

## Value

A data frame with one row per observation containing a column for UIDs,
a column for whether the observation was in the train (original) or test
(held out) set, and one column per row of the solutions data frame
indicating the original and propagated clusters.

## Examples

``` r
# \donttest{
# Function to identify observations with complete data
uids_with_complete_obs <- get_complete_uids(
    list(subc_v, income, pubertal, anxiety, depress),
    uid = "unique_id"
)

# Dataframe assigning 80% of observations to train and 20% to test
train_test_split <- train_test_assign(
    train_frac = 0.8,
    uids = uids_with_complete_obs
)

# Pulling the training and testing observations specifically
train_obs <- train_test_split$"train"
test_obs <- train_test_split$"test"

# Partition a training set
train_subc_v <- subc_v[subc_v$"unique_id" %in% train_obs, ]
train_income <- income[income$"unique_id" %in% train_obs, ]
train_pubertal <- pubertal[pubertal$"unique_id" %in% train_obs, ]
train_anxiety <- anxiety[anxiety$"unique_id" %in% train_obs, ]
train_depress <- depress[depress$"unique_id" %in% train_obs, ]

# Partition a test set
test_subc_v <- subc_v[subc_v$"unique_id" %in% test_obs, ]
test_income <- income[income$"unique_id" %in% test_obs, ]
test_pubertal <- pubertal[pubertal$"unique_id" %in% test_obs, ]
test_anxiety <- anxiety[anxiety$"unique_id" %in% test_obs, ]
test_depress <- depress[depress$"unique_id" %in% test_obs, ]

# Find cluster solutions in the training set
train_dl <- data_list(
    list(train_subc_v, "subc_v", "neuroimaging", "continuous"),
    list(train_income, "household_income", "demographics", "continuous"),
    list(train_pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)

# We'll pick a solution that has good separation over our target features
train_target_dl <- data_list(
    list(train_anxiety, "anxiety", "behaviour", "ordinal"),
    list(train_depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)

sc <- snf_config(
    train_dl,
    n_solutions = 5,
    min_k = 10,
    max_k = 30
)
#> ℹ No distance functions specified. Using defaults.
#> ℹ No clustering functions specified. Using defaults.

train_sol_df <- batch_snf(
    train_dl,
    sc,
    return_sim_mats = TRUE
)

ext_sol_df <- extend_solutions(
    train_sol_df,
    train_target_dl
)

# Determining solution with the lowest minimum p-value
lowest_min_pval <- min(ext_sol_df$"min_pval")
which(ext_sol_df$"min_pval" == lowest_min_pval)
#> [1] 2
top_row <- ext_sol_df[1, ]

# Propagate that solution to the observations in the test set
# data list below has both training and testing observations
full_dl <- data_list(
    list(subc_v, "subc_v", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)
#> ℹ 175 observations dropped due to incomplete data.

# Use the solutions data frame from the training observations and the data list
# from the training and testing observations to propagate labels to the test observations
propagated_labels <- label_propagate(top_row, full_dl)

propagated_labels_all <- label_propagate(ext_sol_df, full_dl)

head(propagated_labels_all)
#>                    uid     group 1 2 3 4 5
#> 1 uid_NDAR_INV0567T2Y9 clustered 1 1 1 3 6
#> 2 uid_NDAR_INV0J4PYA5F clustered 4 2 1 4 3
#> 3 uid_NDAR_INV10OMKVLE clustered 3 2 2 7 4
#> 4 uid_NDAR_INV15FPCW4O clustered 4 2 1 6 4
#> 5 uid_NDAR_INV19NB4RJK clustered 3 2 2 1 4
#> 6 uid_NDAR_INV1HLGR738 clustered 3 2 2 1 5
tail(propagated_labels_all)
#>                      uid       group 1 2 3 4 5
#> 95  uid_NDAR_INVJEV61XIU unclustered 3 2 2 4 5
#> 96  uid_NDAR_INVJR3S271G unclustered 4 2 1 7 4
#> 97  uid_NDAR_INVK9ULDQA2 unclustered 1 1 1 1 1
#> 98  uid_NDAR_INVKYH529RD unclustered 1 1 1 4 8
#> 99  uid_NDAR_INVL045Z1TY unclustered 3 1 2 7 4
#> 100 uid_NDAR_INVLDQH8ATK unclustered 3 2 1 7 4
# }
```
