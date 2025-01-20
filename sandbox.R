devtools::load_all()
library(sloop)
library(testthat)

set.seed(42)
input_dl <- data_list(
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)

sc <- snf_config(input_dl, n_solutions = 5)

sol_df <- batch_snf(input_dl, sc)

aris <- calc_aris(sol_df)

aris

ext_sol_df <- extend_solutions(sol_df, dl = input_dl)

ext_sol_df2 <- extend_solutions(sol_df, target_dl = input_dl)


#' # my_dl <- data_list(
#' #     list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
#' #     list(income, "household_income", "demographics", "continuous"),
#' #     list(pubertal, "pubertal_status", "demographics", "continuous"),
#' #     uid = "unique_id"
#' # )
#' # 
#' # sc <- snf_config(my_dl, n_solutions = 5, max_k = 40)
#' # 
#' # sol_df <- batch_snf(my_dl, sc)
#' # 
#' # my_dl_subsamples <- subsample_dl(
#' #     my_dl,
#' #     n_subsamples = 20,
#' #     subsample_fraction = 0.85
#' # )
#' # 
#' # batch_subsample_results <- batch_snf_subsamples(
#' #     my_dl_subsamples,
#' #     sc,
#' #     verbose = TRUE
#' # )
#' # 
#' # subsample_cluster_solutions <- batch_subsample_results[["cluster_solutions"]]
#' # 
#' # pairwise_aris <- subsample_pairwise_aris(
#' #     subsample_cluster_solutions,
#' #     return_raw_aris = TRUE,
#' #     verbose = TRUE
#' # )
#' # 
#' # coclustering_results <- calculate_coclustering(
#' #     subsample_cluster_solutions,
#' #     sol_df,
#' #     verbose = TRUE
#' # )
#' # 
#' # coclustering_results$"cocluster_summary"
#' # 
#' # cocluster_dfs <- coclustering_results$"cocluster_dfs"
#' # 
#' # cocluster_density(cocluster_dfs[[1]])
#' # 
#' # cocluster_heatmap(
#' #     cocluster_dfs[[1]],
#' #     dl = my_dl,
#' #     top_hm = list(
#' #         "Income" = "household_income",
#' #         "Pubertal Status" = "pubertal_status"
#' #     ),
#' #     annotation_colours = list(
#' #         "Pubertal Status" = colour_scale(
#' #             c(1, 4),
#' #             min_colour = "black",
#' #             max_colour = "purple"
#' #         ),
#' #         "Income" = colour_scale(
#' #             c(0, 4),
#' #             min_colour = "black",
#' #             max_colour = "red"
#' #         )
#' #     )
#' # )


# Function to identify obervations with complete data

df_list <- list(cort_t, cort_sa, subc_v, income, pubertal, anxiety, depress)

uids_with_complete_obs <- get_complete_uids(df_list, uid = "unique_id")

# Dataframe assigning 80% of subjects to train and 20% to test
train_test_split <- train_test_assign(
    train_frac = 0.8,
    subjects = uids_with_complete_obs
)

# Pulling the training and testing subjects specifically
train_subs <- train_test_split$"train"
test_subs <- train_test_split$"test"

# Partition a training set
train_cort_t <- cort_t[cort_t$"unique_id" %in% train_subs, ]
train_cort_sa <- cort_sa[cort_sa$"unique_id" %in% train_subs, ]
train_subc_v <- subc_v[subc_v$"unique_id" %in% train_subs, ]
train_income <- income[income$"unique_id" %in% train_subs, ]
train_pubertal <- pubertal[pubertal$"unique_id" %in% train_subs, ]
train_anxiety <- anxiety[anxiety$"unique_id" %in% train_subs, ]
train_depress <- depress[depress$"unique_id" %in% train_subs, ]

# Partition a test set
test_cort_t <- cort_t[cort_t$"unique_id" %in% test_subs, ]
test_cort_sa <- cort_sa[cort_sa$"unique_id" %in% test_subs, ]
test_subc_v <- subc_v[subc_v$"unique_id" %in% test_subs, ]
test_income <- income[income$"unique_id" %in% test_subs, ]
test_pubertal <- pubertal[pubertal$"unique_id" %in% test_subs, ]
test_anxiety <- anxiety[anxiety$"unique_id" %in% test_subs, ]
test_depress <- depress[depress$"unique_id" %in% test_subs, ]

# Find cluster solutions in the training set
train_dl <- data_list(
    list(train_cort_t, "cort_t", "neuroimaging", "continuous"),
    list(train_cort_sa, "cortical_sa", "neuroimaging", "continuous"),
    list(train_subc_v, "subc_v", "neuroimaging", "continuous"),
    list(train_income, "household_income", "demographics", "continuous"),
    list(train_pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)

train_target_dl <- data_list(
    list(train_anxiety, "anxiety", "behaviour", "ordinal"),
    list(train_depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)

set.seed(42)
sc <- snf_config(train_dl, n_solutions = 5, min_k = 10, max_k = 30)

train_sol_df <- batch_snf(train_dl, sc)

debug(extend_solutions)

ext_sol_df <- extend_solutions(train_sol_df, train_target_dl)

lowest_min_pval <- min(ext_sol_df$"min_pval")
min_idx <- which(ext_sol_df$"min_pval" == lowest_min_pval)
top_row <- ext_sol_df[min_idx, ]

full_dl <- data_list(
    list(cort_t, "cort_t", "neuroimaging", "continuous"),
    list(cort_sa, "cort_sa", "neuroimaging", "continuous"),
    list(subc_v, "subc_v", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)

propagated_labels <- label_propagate(top_row, full_dl)

head(propagated_labels)
tail(propagated_labels)
