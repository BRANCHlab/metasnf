folder_path <- "data"
files <- list.files(path = folder_path, pattern = "\\.rda$", full.names = TRUE)
lapply(files, load, envir = globalenv())

library("metasnf")

# `data_list` class object stores data frames and metadata

mock_data_list <- data_list(
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(cort_sa, "cortical_sa", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)

#usethis::use_data(mock_data_list, overwrite = TRUE)

set.seed(42)
mock_snf_config <- snf_config(
    mock_data_list,
    n_solutions = 20,
    max_k = 40,
    clust_fns = list(
        "two_cluster_spectral" = spectral_two,
        "five_cluster_spectral" = spectral_five
    ),
    cnt_dist_fns = list(
        "standard_norm_euclidean" = sn_euclidean_distance
    ),
    dsc_dist_fns = list(
        "standard_norm_euclidean" = sn_euclidean_distance
    ),
    weights_fill = "uniform",
    use_default_dist_fns = TRUE,
    use_default_clust_fns = TRUE
)

#usethis::use_data(mock_snf_config, overwrite = TRUE)

mock_settings_df <- mock_snf_config$"settings_df"
mock_dist_fns_list <- mock_snf_config$"dist_fns_list"
mock_clust_fns_list <- mock_snf_config$"clust_fns_list"
mock_weights_matrix <- mock_snf_config$"weights_matrix"

#usethis::use_data(mock_settings_df, overwrite = TRUE)
#usethis::use_data(mock_dist_fns_list, overwrite = TRUE)
#usethis::use_data(mock_clust_fns_list, overwrite = TRUE)
#usethis::use_data(mock_weights_matrix, overwrite = TRUE)

mock_solutions_df <- batch_snf(
    mock_data_list,
    mock_snf_config
)

progressr::with_progress({
    mock_solutions_df2 <- batch_snf(
        mock_data_list,
        mock_snf_config,
        return_sim_mats = TRUE
    )
})

mock_sim_mats_list <- sim_mats_list(mock_solutions_df2)

usethis::use_data(mock_sim_mats_list, overwrite = TRUE)

#usethis::use_data(mock_solutions_df, overwrite = TRUE)

mock_t_solutions_df <- t(mock_solutions_df)

#usethis::use_data(mock_t_solutions_df, overwrite = TRUE)

mock_aris <- calc_aris(mock_solutions_df)

#usethis::use_data(mock_aris, overwrite = TRUE)

meta_cluster_order <- get_matrix_order(mock_aris)
meta_cluster_order

mock_ari_hm <- meta_cluster_heatmap(
    mock_aris,
    order = meta_cluster_order
)

#usethis::use_data(mock_ari_hm, overwrite = TRUE)

shiny_annotator(mock_ari_hm)

split_vec <- c(7, 12, 15)
mock_ari_mc_hm <- meta_cluster_heatmap(
   mock_aris,
   order = meta_cluster_order,
   split_vector = split_vec
)
usethis::use_data(mock_ari_mc_hm, overwrite = TRUE)

mock_mc_solutions_df <- label_meta_clusters(
    mock_solutions_df,
    order = meta_cluster_order,
    split_vector = split_vec
)
mock_mc_solutions_df

usethis::use_data(mock_mc_solutions_df, overwrite = TRUE)

### Identifying representative solutions

mock_rep_solutions_df <- get_representative_solutions(
    mock_aris,
    mock_mc_solutions_df
)
mock_rep_solutions_df

usethis::use_data(mock_rep_solutions_df, overwrite = TRUE)

mock_ext_solutions_df <- extend_solutions(
    mock_rep_solutions_df,
    target_dl = mock_data_list
)

mock_ext_solutions_df

usethis::use_data(mock_ext_solutions_df, overwrite = TRUE)

dl2 <- mock_data_list

start <- Sys.time()
progressr::with_progress({
    feature_nmis <- calc_nmis(
        dl = dl2,
        sol_df = mock_ext_solutions_df,
        processes = "max"
    )
})
print(Sys.time() - start)

feature_nmis

as.data.frame(dl2) |> colnames()

features(dl2)

feature_nmis2

feature_nmis

summary(dl2, "feature")

as.data.frame(dl2)

identical(feature_nmis, feature_nmis2)

config_hm <- config_heatmap(
    sc = my_sc,
    order = meta_cluster_order,
    hide_fixed = TRUE
)
config_hm
#save_heatmap(
#    heatmap = config_hm,
#    path = "config_hm.png",
#    width = 400,
#    height = 475,
#    res = 75
#)

sol_df_with_sim_mats <- batch_snf(
   dl = my_dl,
   sc = my_sc,
   return_sim_mats = TRUE
)

silhouette_scores <- calculate_silhouettes(sol_df_with_sim_mats)
dunn_indices <- calculate_dunn_indices(sol_df_with_sim_mats)
db_indices <- calculate_db_indices(sol_df_with_sim_mats)


set.seed(42)
my_dl_subsamples <- subsample_dl(
    my_dl,
    n_subsamples = 50,
    subsample_fraction = 0.85
)

# Corresponding to MCs C and B
top_sol_df <- rep_sol_df_2[c(2, 3), ]
top_config <- attr(top_sol_df, "snf_config")
batch_subsample_results <- batch_snf_subsamples(
    my_dl_subsamples,
    top_config,
    verbose = TRUE
)
names(batch_subsample_results)

pairwise_aris <- subsample_pairwise_aris(
    batch_subsample_results,
    verbose = TRUE
)
names(pairwise_aris)
pairwise_aris[["ari_summary"]]

inter_ss_ari_hm_1 <- ComplexHeatmap::Heatmap(
    pairwise_aris[["raw_aris"]][["s1"]],
    col = circlize::colorRamp2(
        c(0, 0.5, 1),
        c("blue", "white", "red")
    ),
    heatmap_legend_param = list(
        color_bar = "continuous",
        title = "Inter-Subsample\nARI",
        at = c(0, 0.5, 1)
    ),
    show_column_names = FALSE,
    show_row_names = FALSE
)
inter_ss_ari_hm_2 <- ComplexHeatmap::Heatmap(
    pairwise_aris[["raw_aris"]][["s2"]],
    col = circlize::colorRamp2(
        c(0, 0.5, 1),
        c("blue", "white", "red")
    ),
    heatmap_legend_param = list(
        color_bar = "continuous",
        title = "Inter-Subsample\nARI",
        at = c(0, 0.5, 1)
    ),
    show_column_names = FALSE,
    show_row_names = FALSE
)
#save_heatmap(
#    heatmap = inter_ss_ari_hm_1,
#    path = "inter_ss_ari_hm_1.png",
#    width = 400,
#    height = 300,
#    res = 70
#)
#save_heatmap(
#    heatmap = inter_ss_ari_hm_2,
#    path = "inter_ss_ari_hm_2.png",
#    width = 400,
#    height = 300,
#    res = 70
#)

coclustering_results <- calculate_coclustering(
    batch_subsample_results,
    top_sol_df,
    verbose = TRUE
)
names(coclustering_results)

cocluster_dfs <- coclustering_results[["cocluster_dfs"]]
head(cocluster_dfs[[1]])

cocluster_summary <- coclustering_results[["cocluster_summary"]]
cocluster_summary

density_plot <- cocluster_density(cocluster_dfs[[2]])
density_plot

cocluster_hm <- cocluster_heatmap(
    cocluster_dfs[[1]],
    dl = my_dl,
    top_hm = list(
        "Income" = "household_income",
        "Pubertal Status" = "pubertal_status"
    ),
    annotation_colours = list(
        "Pubertal Status" = colour_scale(
            c(1, 4),
            min_colour = "black",
            max_colour = "purple"
        ),
        "Income" = colour_scale(
            c(0, 4),
            min_colour = "black",
            max_colour = "red"
        )
    )
)
cocluster_hm
#save_heatmap(
#    heatmap = cocluster_hm,
#    path = "cocluster_hm.png",
#    width = 600,
#    height = 400,
#    res = 100
#)

## Evaluating generalizability through label propagation

# Dataframe assigning 80% of observations to train and 20% to test
train_test_split <- train_test_assign(
    train_frac = 0.8,
    uids = complete_uids,
    seed = 42
)

# Pulling the training and testing observations specifically
train_obs <- train_test_split$"train"
test_obs <- train_test_split$"test"

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

# Find cluster solutions in the training set
train_dl <- data_list(
    list(train_cort_t, "cort_t", "neuroimaging", "continuous"),
    list(train_cort_sa, "cortical_sa", "neuroimaging", "continuous"),
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

set.seed(42)
sc <- snf_config(
    train_dl,
    n_solutions = 5,
    min_k = 10,
    max_k = 30
)

train_sol_df <- batch_snf(train_dl, sc)

ext_sol_df <- extend_solutions(train_sol_df, train_target_dl)

# Determining solution with the lowest minimum p-value
lowest_min_pval <- min(ext_sol_df$"min_pval")
which(ext_sol_df$"min_pval" == lowest_min_pval)
top_sol_df <- ext_sol_df[1, ]

# Propagate that solution to the observations in the test set
# data list below has both training and testing observations
full_dl <- data_list(
    list(cort_t, "cort_t", "neuroimaging", "continuous"),
    list(cort_sa, "cort_sa", "neuroimaging", "continuous"),
    list(subc_v, "subc_v", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)

# Use the solutions data frame from the training observations and the data list from
# the training and testing observations to propagate labels to the test observations
propagated_labels <- label_propagate(top_sol_df, full_dl)

head(propagated_labels)
tail(propagated_labels)

## Additional plots

### Feature plots

dl <- data_list(
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(fav_colour, "favourite_colour", "misc", "categorical"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    list(depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)
set.seed(42)
sc <- snf_config(
    dl = dl,
    n_solutions = 2,
    min_k = 20,
    max_k = 50
)
sol_df <- batch_snf(dl, sc)
# Plotting just the first solution
top_sol_df <- sol_df[1, ]
plot_list <- auto_plot(sol_df_row = top_sol_df, dl = dl)

plot_list[["colour"]]
plot_list[["smri_vol_scs_csf"]]
plot_list[["colour"]] +
    ggplot2::labs(
        fill = "Favourite Colour",
        x = "Cluster",
        title = " Favourite Colour by Cluster"
    ) +
    ggplot2::scale_fill_manual(
        values = c(
            "green" = "forestgreen",
            "red" = "firebrick3",
            "yellow" = "darkgoldenrod1"
        )
    )

### Correlation plots

cort_sa_minimal <- cort_sa[, 1:5]
dl <- data_list(
    list(cort_sa_minimal, "cortical_sa", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "ordinal"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    list(fav_colour, "favourite_colour", "demographics", "categorical"),
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    list(depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)
assoc_pval_matrix <- calc_assoc_pval_matrix(dl)
ap_heatmap <- assoc_pval_heatmap(assoc_pval_matrix)
ap_heatmap
#save_heatmap(
#    ap_heatmap,
#    "ap_hm.png",
#    width = 650,
#    height = 500,
#    res = 100
#)

ap_heatmap2 <- assoc_pval_heatmap(
    assoc_pval_matrix,
    confounders = list(
        "Colour" = "colour",
        "Pubertal Status" = "pubertal_status"
    ),
    out_of_models = list(
        "Income" = "household_income"
    ),
    dl = dl,
    split_by_domain = TRUE
)
ap_heatmap2
#save_heatmap(
#    ap_heatmap2,
#    "ap_hm2.png",
#    width = 700,
#    height = 500,
#    res = 100
#)

### Alluvial plots 

my_dl <- data_list(
    list(
        data = expression_df,
        name = "genes_1_and_2_exp",
        domain = "gene_expression",
        type = "continuous"
    ),
    list(
        data = methylation_df,
        name = "genes_1_and_2_meth",
        domain = "gene_methylation",
        type = "continuous"
    ),
    list(
        data = gender_df,
        name = "gender",
        domain = "demographics",
        type = "categorical"
    ),
    list(
        data = diagnosis_df,
        name = "diagnosis",
        domain = "clinical",
        type = "categorical"
    ),
    uid = "patient_id"
)
set.seed(42)
my_sc <- snf_config(
    my_dl,
    n_solutions = 1,
    max_k = 40
)
sol_df <- batch_snf(
    dl = my_dl,
    sc = my_sc,
    return_sim_mats = TRUE
)
sim_mats <- sim_mats_list(sol_df)
similarity_matrix <- sim_mats[[1]]
cluster_sequence <- list(
    spectral_two,
    spectral_three,
    spectral_four
)
alluvial_cluster_plot(
    cluster_sequence = cluster_sequence,
    similarity_matrix = similarity_matrix,
    dl = my_dl,
    key_outcome = "gender",
    key_label = "Gender",
    extra_outcomes = "diagnosis",
    title = "Gender Across Cluster Counts"
)

### Similarity matrix heatmaps

similarity_matrix_hm <- similarity_matrix_heatmap(
    similarity_matrix = similarity_matrix,
    cluster_solution = sol_df[1, ],
    heatmap_height = grid::unit(10, "cm"),
    heatmap_width = grid::unit(10, "cm")
)
similarity_matrix_hm
#save_heatmap(
#    heatmap = similarity_matrix_hm,
#    path = "similarity_matrix_hm.png",
#    width = 410,
#    height = 330,
#    res = 80
#)

### Other manhattan plots

full_dl <- data_list(
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    list(depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)
dl <- full_dl[1:3]
target_dl <- full_dl[4:5]
set.seed(42)
sc <- snf_config(
    dl = dl,
    n_solutions = 20,
    min_k = 20,
    max_k = 50
)
sol_df <- batch_snf(dl, sc)
ext_sol_df <- extend_solutions(
    sol_df,
    dl = dl,
    target = target_dl,
    min_pval = 1e-10 # p-values below 1e-10 will be thresholded to 1e-10
)
esm_manhattan <- esm_manhattan_plot(
    ext_sol_df[1:5, ],
    neg_log_pval_thresh = 5,
    threshold = 0.05,
    point_size = 3,
    jitter_width = 0.1,
    jitter_height = 0.1,
    plot_title = "Feature-Solution Associations",
    text_size = 14,
    bonferroni_line = TRUE
)
var_manhattan <- var_manhattan_plot(
    dl,
    key_var = "household_income",
    plot_title = "Correlation of Features with Household Income",
    text_size = 16,
    neg_log_pval_thresh = 3,
    threshold = 0.05
)
esm_manhattan
var_manhattan

## Computational runtimes

library("ggplot2")
library("progressr")
library("metasnf")
library("readr")

simulate_batch_snf <- function(n_ft, n_obs, n_snf) {
    # Mock set of domain names
    domain_names <- letters[seq_len(3)]
    # Initialize a list of random data frames
    list_of_lists <- lapply(
        seq_len(6),
        function(x) {
            if (x <= 3) {
                dom <- domain_names[x]
            } else {
                dom <- sample(domain_names, 1)
            }
            df <- as.data.frame(matrix(rnorm(n_obs * n_ft), ncol = n_ft))
            colnames(df) <- paste0("ft_", x, "_", 1:n_ft)
            df <- df |>
                dplyr::mutate("uid" = paste0("obs_", 1:n_obs)) |>
                dplyr::relocate("uid", .before = colnames(df)[1])
            list(
                df,
                paste0("df_", x),
                dom,
                "continuous"
            )
        }
    )
    dl <- data_list(list_of_lists, uid = "uid")
    sc <- snf_config(dl, n_solutions = n_snf)
    with_progress({
        batch_snf(dl, sc)
    })
}

profiler <- function(ft_vec = c(5),
                     obs_vec = c(100),
                     snf_vec = c(10)) {
    counter = 0
    profile_df <- data.frame()
    for (n_ft in ft_vec) {
        for (n_obs in obs_vec) {
            for (n_snf in snf_vec) {
                counter <- counter + 1
                print(
                    paste0(
                        "Running simulation ",
                        counter, " of ", length(ft_vec) *
                        length(obs_vec) *
                        length(snf_vec)
                    )
                )
                start <- proc.time()
                sol_df <- simulate_batch_snf(
                    n_ft = n_ft,
                    n_obs = n_obs,
                    n_snf = n_snf
                )
                batch_snf_time <- (proc.time() - start)['elapsed']
                if (n_snf > 1) {
                    start <- proc.time()
                    calc_aris(sol_df)                            
                    calc_aris_time <- (proc.time() - start)['elapsed']
                } else {
                    calc_aris_time <- 0
                }
                profile_df <- rbind(
                    profile_df,
                    data.frame(
                        n_ft = n_ft,
                        n_obs = n_obs,
                        n_snf = n_snf,
                        fn = "batch_snf",
                        time = batch_snf_time
                    )
                )
                profile_df <- rbind(
                    profile_df,
                    data.frame(
                        n_ft = n_ft,
                        n_obs = n_obs,
                        n_snf = n_snf,
                        fn = "calc_aris",
                        time = calc_aris_time
                    )
                )
            }
        }
    }
    rownames(profile_df) <- NULL
    return(profile_df)
}

# Profiling # of SNF runs
n_snf_profile <- profiler(snf_vec = c(128, 64, 32, 16, 8, 4, 2, 1))

write_csv(n_snf_profile, "n_snf_profile.csv")

n_obs_profile <- profiler(obs_vec = c(100, 200, 300, 400, 500, 600))
write_csv(n_obs_profile, "n_obs_profile.csv")

n_ft_profile <- profiler(ft_vec = c(10, 50, 100, 200, 400))
write_csv(n_ft_profile, "n_ft_profile.csv")

# SNF: number of solutions vs. runtime
batch_snf_solutions_plot <- n_snf_profile |>
    dplyr::filter(fn == "batch_snf") |>
    ggplot(aes(x = n_snf, y = time)) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_point(size = 1.5) + 
    scale_x_continuous(breaks = seq(0, 130, by = 20)) + 
    theme_classic() +
    theme(text = element_text(size = 11)) +
    xlab("Number of Solutions") +
    ylab("Runtime (s)")
batch_snf_solutions_plot
ggsave(
    "batch_snf_solutions_runtime_plot.png",
    batch_snf_solutions_plot,
    width = 4,
    height = 3
)

# SNF: observations vs. runtime
batch_snf_obs_plot <- n_obs_profile |>
    dplyr::filter(fn == "batch_snf") |>
    ggplot(aes(x = n_obs, y = time)) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_point(size = 1.5) + 
    theme_classic() +
    theme(text = element_text(size = 11)) +
    xlab("Observations") +
    ylab("Runtime (s)")
batch_snf_obs_plot
ggsave(
    "batch_snf_obs_runtime_plot.png",
    batch_snf_obs_plot,
    width = 4,
    height = 3
)

# SNF: features vs. runtime
batch_snf_ft_plot <- n_ft_profile |>
    dplyr::filter(fn == "batch_snf") |>
    ggplot(aes(x = n_ft, y = time)) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_point(size = 1.5) + 
    theme_classic() +
    scale_x_continuous(breaks = seq(0, 410, by = 50)) + 
    theme(text = element_text(size = 11)) +
    xlab("Features") +
    ylab("Runtime (s)")
batch_snf_ft_plot
ggsave(
    "batch_snf_ft_runtime_plot.png",
    batch_snf_ft_plot,
    width = 4,
    height = 3
)

# ARIs: solutions vs. runtime
calc_aris_solutions_plot <- n_snf_profile |>
    dplyr::filter(fn == "calc_aris") |>
    ggplot(aes(x = n_snf, y = time)) +
    geom_smooth(
        method = "lm",
        se = FALSE,
        formula = y ~ poly(x, 2)
    ) +
    geom_point(size = 1.5) + 
    theme_classic() +
    theme(text = element_text(size = 11)) +
    scale_x_continuous(breaks = seq(0, 130, by = 20)) + 
    xlab("Number of Solutions") +
    ylab("Runtime (s)")
calc_aris_solutions_plot
ggsave(
    "calc_aris_solutions_runtime_plot.png",
    calc_aris_solutions_plot,
    width = 4,
    height = 3
)


## Helper functions 

### Data list summaries

my_dl <- data_list(
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)

summary(my_dl)

summary(my_dl, scope = "feature")

### Conversion to data frames

class(as.data.frame(my_dl))

my_sc <- snf_config(my_dl, n_solutions = 5)

sol_df <- batch_snf(input_dl, sc)

colnames(as.data.frame(sol_df))[1:20]

colnames(as.data.frame(sol_df, keep_attributes = TRUE))[1:20]

## Alternative formats for data list generation 

### Named nested components

heart_rate_df <- data.frame(
    unique_id = c("1", "2", "3"),
    var1 = c(0.04, 0.1, 0.3),
    var2 = c(30, 2, 0.3)
)
personality_test_df <- data.frame(
    unique_id = c("1", "2", "3"),
    var3 = c(900, 1990, 373),
    var4 = c(509, 2209, 83)
)
survey_response_df <- data.frame(
    unique_id = c("1", "2", "3"),
    var5 = c(1, 3, 3),
    var6 = c(2, 3, 3)
)
city_df <- data.frame(
    unique_id = c("1", "2", "3"),
    var7 = c("toronto", "montreal", "vancouver")
)
my_dl <- data_list(
    list(
        data = heart_rate_df,
        name = "heart_rate",
        domain = "clinical",
        type = "continuous"
    ),
    list(
        data = personality_test_df,
        name = "personality_test",
        domain = "surveys",
        type = "continuous"
    ),
    list(
        data = survey_response_df,
        name = "survey_response",
        domain = "surveys",
        type = "ordinal"
    ),
    list(
        data = city_df,
        name = "city",
        domain = "location",
        type = "categorical"
    ),
    uid = "unique_id"
)

### List of lists

list_of_lists <- list(
    list(heart_rate_df, "data1", "domain1", "continuous"),
    list(personality_test_df, "data2", "domain2", "continuous")
)
my_dl <- data_list(
    list_of_lists,
    uid = "unique_id"
)

## Clustering algorithms 

### The default clustering algorithms list

my_dl <- data_list(
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)

my_sc <- snf_config(my_dl, n_solutions = 5)

my_sc[["clust_fns_list"]]

my_sc[["clust_fns_list"]][[1]]

my_sc$"settings_df"

### Customizing the clustering algorithms list

#### Pre-defined algorithms

my_sc2 <- snf_config(
    dl = my_dl,
    n_solutions = 5,
    clust_fns = list(
        "two_cluster_spectral" = spectral_two,
        "five_cluster_spectral" = spectral_five
    )
)

my_sc2[["clust_fns_list"]]

my_sc3 <- snf_config(
    dl = my_dl,
    n_solutions = 5,
    clust_fns = list(
        "two_cluster_spectral" = spectral_two,
        "five_cluster_spectral" = spectral_five
    ),
    use_default_clust_fns = TRUE
)

my_sc3[["clust_fns_list"]]

### Non-automated clustering

sol_df <- batch_snf(
    my_dl,
    my_sc,
    return_sim_mats = TRUE
)

dim(sim_mats_list(sol_df)[[1]])

## Distance metrics 

my_dl <- data_list(
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)

my_sc <- snf_config(my_dl, n_solutions = 5)

my_sc[["dist_fns_list"]]

### Using custom distance metrics

#### Pre-defined distance metrics functions

my_sc2 <- snf_config(
    dl = my_dl,
    n_solutions = 5,
    cnt_dist_fns = list(
        "standard_norm_euclidean" = sn_euclidean_distance
    ),
    dsc_dist_fns = list(
        "standard_norm_euclidean" = sn_euclidean_distance
    )
)
my_sc2[["dist_fns_list"]]

my_sc3 <- snf_config(
    dl = my_dl,
    n_solutions = 5,
    cnt_dist_fns = list(
        "standard_norm_euclidean" = sn_euclidean_distance
    ),
    dsc_dist_fns = list(
        "standard_norm_euclidean" = sn_euclidean_distance
    ),
    use_default_dist_fns = TRUE
)
my_sc3[["dist_fns_list"]]

## Feature weighting 

my_dl <- data_list(
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)

my_sc <- snf_config(my_dl, n_solutions = 5)

as.matrix(my_sc[["weights_matrix"]])

set.seed(42)
my_sc2 <- snf_config(
    my_dl,
    n_solutions = 5,
    weights_fill = "uniform"
)
as.matrix(my_sc2[["weights_matrix"]])

set.seed(42)
my_sc3 <- snf_config(
    my_dl,
    n_solutions = 5,
    weights_fill = "exponential"
)
as.matrix(my_sc3[["weights_matrix"]])

## Association boosted similarity network fusion 

# Build data list with held-out feature "fav_colour"
dl <- data_list(
    list(income, "income", "demographics", "ordinal"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    list(fav_colour, "favourite_colour", "demographics", "categorical"),
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    uid = "unique_id"
)

# Calculation of all pairwise p-values
assoc_pval_matrix <- calc_assoc_pval_matrix(dl)

# Extract p-values related to colour
colour_pk <- assoc_pval_matrix["colour", ]

# Drop self p-value
colour_pk <- colour_pk[!names(colour_pk) == "colour"]

# Convert p-values to feature weights
feature_weights <- -log10(colour_pk)/sum(-log10(colour_pk))

# Drop held-out component from data list
dl["favourite_colour"] <- NULL

# Apply feature weights to weights matrix
sc <- snf_config(dl, n_solutions = 1)
sc[["weights_matrix"]][1, ] <- feature_weights

# Run SNF
batch_snf(dl, sc)

## Imputation of missing data as a meta clustering parameter 

# First hypothetically imputed dataset
dl_imp1 <- data_list(
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    list(depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)

# Second hypothetically imputed dataset
dl_imp2 <- data_list(
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    list(depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)

# Config construction is not sensitive to the data list values,
# so any imputed data set will lead to same result
set.seed(42)
my_sc <- snf_config(
    dl = dl_imp1,
    n_solutions = 10,
    min_k = 20,
    max_k = 50
)

# Generation of 20 cluster solutions
sol_df_imp1 <- batch_snf(dl_imp1, sc)
sol_df_imp2 <- batch_snf(dl_imp2, sc)

# Create a stacked solution matrix that stores solutions from both imputations
# Solutions 1:10 are for imputation 1, 11:20 are for imputation 2
sol_df <- rbind(sol_df_imp1, sol_df_imp2, reset_indices = TRUE)

# Calculate pairwise similarities across all solutions
# (Including across imputations)
sol_aris <- calc_aris(sol_df)

my_dl <- data_list(
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)

sc <- snf_config(my_dl, n_solutions = 5, max_k = 40)

my_dl_subsamples <- subsample_dl(
    my_dl,
    n_subsamples = 20,
    subsample_fraction = 0.85
)

batch_subsample_results <- batch_snf_subsamples(
    my_dl_subsamples,
    sc
)

pairwise_aris <- subsample_pairwise_aris(
    batch_subsample_results,
    verbose = TRUE
)

# Visualize ARIs 
ComplexHeatmap::Heatmap(
    pairwise_aris$"raw_aris"[[1]],
    heatmap_legend_param = list(
        color_bar = "continuous",
        title = "Inter-Subsample\nARI",
        at = c(0, 0.5, 1)
    ),
    show_column_names = FALSE,
    show_row_names = FALSE
)









library(metasnf)

dl <- data_list(
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    list(depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)

set.seed(42)
sc <- snf_config(
    dl = dl,
    n_solutions = 20,
    min_k = 20,
    max_k = 50
)

# Generation of 20 cluster solutions
sol_df <- batch_snf(dl, sc)

# Let's just calculate NMIs of the anxiety and depression data types for the
# first 5 cluster solutions to save time:
feature_nmis <- calc_nmis(dl[4:5], sol_df[1:5, ])

print(feature_nmis)

