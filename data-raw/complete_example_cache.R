library(metasnf)

df_list <- list(
    anxiety,
    depress,
    cort_t,
    cort_sa,
    subc_v,
    income,
    pubertal
)

# The number of rows in each data frame:
lapply(df_list, dim)

# Whether or not each data frame has missing values:
lapply(df_list,
    function(x) {
        any(is.na(x))
    }
)

complete_uids <- get_complete_uids(df_list, uid = "unique_id")

print(length(complete_uids))

# Reducing data frames to only common observations with no missing data
anxiety <- anxiety[anxiety$"unique_id" %in% complete_uids, ]
depress <- depress[depress$"unique_id" %in% complete_uids, ]
cort_t <- cort_t[cort_t$"unique_id" %in% complete_uids, ]
cort_sa <- cort_sa[cort_sa$"unique_id" %in% complete_uids, ]
subc_v <- subc_v[subc_v$"unique_id" %in% complete_uids, ]
income <- income[income$"unique_id" %in% complete_uids, ]
pubertal <- pubertal[pubertal$"unique_id" %in% complete_uids, ]

# (data = ..., name = ..., etc.)
input_dl <- data_list(
    list(
        data = cort_t,
        name = "cortical_thickness",
        domain = "neuroimaging",
        type = "continuous"
    ),
    list(
        data = cort_sa,
        name = "cortical_surface_area",
        domain = "neuroimaging",
        type = "continuous"
    ),
    list(
        data = subc_v,
        name = "subcortical_volume",
        domain = "neuroimaging",
        type = "continuous"
    ),
    list(
        data = income,
        name = "household_income",
        domain = "demographics",
        type = "continuous"
    ),
    list(
        data = pubertal,
        name = "pubertal_status",
        domain = "demographics",
        type = "continuous"
    ),
    uid = "unique_id"
)

target_dl <- data_list(
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    list(depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)

set.seed(42)
my_sc <- snf_config(
    dl = input_dl,
    n_solutions = 20,
    min_k = 20,
    max_k = 50
)

cache_a_complete_example_sol_df <- batch_snf(input_dl, my_sc)

usethis::use_data(cache_a_complete_example_sol_df, overwrite = TRUE)

sol_df <- cache_a_complete_example_sol_df

sol_aris <- calc_aris(sol_df)

meta_cluster_order <- get_matrix_order(sol_aris)

mc_sol_df <- label_meta_clusters(
    sol_df,
    order = meta_cluster_order,
    split_vector = split_vec
)

cache_a_complete_example_ext_sol_df <- extend_solutions(
    mc_sol_df,
    dl = input_dl,
    target_dl = target_dl
)

usethis::use_data(cache_a_complete_example_ext_sol_df, overwrite = TRUE)

ext_sol_df

#------------------------------------------------------------------------------

# Label propagation chunk:
# All the observations present in all data frames with no NAs
all_observations <- uids(input_dl)

all_observations

# Remove the "uid_" prefix to allow merges with the original data
all_observations <- gsub("uid_", "", all_observations)

# data frame assigning 80% of observations to train and 20% to test
assigned_splits <- train_test_assign(train_frac = 0.8, uids = all_observations)

# Pulling the training and testing observations specifically
train_obs <- assigned_splits$"train"
test_obs <- assigned_splits$"test"

# Partition a training set
train_cort_t <- cort_t[cort_t$"unique_id" %in% train_obs, ]
train_cort_sa <- cort_sa[cort_sa$"unique_id" %in% train_obs, ]
train_subc_v <- subc_v[subc_v$"unique_id" %in% train_obs, ]
train_income <- income[income$"unique_id" %in% train_obs, ]
train_pubertal <- pubertal[pubertal$"unique_id" %in% train_obs, ]
train_anxiety <- anxiety[anxiety$"unique_id" %in% train_obs, ]
train_depress <- depress[depress$"unique_id" %in% train_obs, ]

# Partition a test set
test_cort_t <- cort_t[cort_t$"unique_id" %in% test_obs, ]
test_cort_sa <- cort_sa[cort_sa$"unique_id" %in% test_obs, ]
test_subc_v <- subc_v[subc_v$"unique_id" %in% test_obs, ]
test_income <- income[income$"unique_id" %in% test_obs, ]
test_pubertal <- pubertal[pubertal$"unique_id" %in% test_obs, ]
test_anxiety <- anxiety[anxiety$"unique_id" %in% test_obs, ]
test_depress <- depress[depress$"unique_id" %in% test_obs, ]

# A data list with just training observations
train_dl <- data_list(
    list(train_cort_t, "cortical_thickness", "neuroimaging", "continuous"),
    list(train_cort_sa, "cortical_sa", "neuroimaging", "continuous"),
    list(train_subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(train_income, "household_income", "demographics", "continuous"),
    list(train_pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)

# A data list with training and testing observations
full_dl <- data_list(
    list(cort_t, "cortical_thickness", "neuroimaging", "continuous"),
    list(cort_sa, "cortical_surface_area", "neuroimaging", "continuous"),
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)

# Construct the target lists
train_target_dl <- data_list(
    list(train_anxiety, "anxiety", "behaviour", "ordinal"),
    list(train_depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)

# Find a clustering solution in your training data
set.seed(42)
my_sc <- snf_config(
    train_dl,
    n_solutions = 5,
    min_k = 10,
    max_k = 30
)

train_sol_df <- batch_snf(
    train_dl,
    my_sc
)

cache_a_complete_example_lp_ext_sol_df <- extend_solutions(
    train_sol_df,
    train_target_dl
)

usethis::use_data(cache_a_complete_example_lp_ext_sol_df, overwrite = TRUE)

# The first row had the lowest minimum p-value across our outcomes
lowest_min_pval <- min(ext_sol_df$"min_pval")
which(ext_sol_df$"min_pval" == lowest_min_pval)

# Keep track of your top solution
top_row <- ext_sol_df[1, ]

# Use the solutions data frame from the training observations and the data list from
# the training and testing observations to propagate labels to the test observations
propagated_labels <- label_propagate(top_row, full_dl)

head(propagated_labels)
tail(propagated_labels)
