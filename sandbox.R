devtools::load_all()

library(metasnf)

dl <- data_list(
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    list(depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)

# The default list:
sc <- snf_config(
    dl = dl,
    n_solutions = 5,
    use_default_clust_fns = TRUE
)

sc

sc$"clust_fns_list"

# Adding algorithms provided by the package
sc <- snf_config(
    dl = dl,
    n_solutions = 3,
    clust_fns = list(
        "a" = spectral_two,
        "b" = spectral_three,
        "c" = spectral_four,
        "d" = spectral_five
    )
)


sc$"settings_df"$"clust_alg"

sc

sc

sc

?clust_fns_list

# Note that this one has the default algorithms as well as the newly added ones
sc$"clust_fns_list"

# This list has only the newly added ones
my <- snf_config(
    dl = dl,
    n_solutions = 50,
    clust_fns = list(
        "two_cluster_spectral" = spectral_two,
        "five_cluster_spectral" = spectral_five
    ),
    use_default_clust_fns = T
)

my_set

my_settings$"settings_df"$"clust_alg" <- sample(
    1:length(my_settings$"clust_fns_list"),
    size = nrow(my_settings$"settings_df"),
    replace = TRUE
)

sc$"settings_df"



