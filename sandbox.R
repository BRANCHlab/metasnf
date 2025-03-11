devtools::load_all()

# `data_list` class object stores data frames and metadata
dl <- data_list(
    list(cort_sa, "cortical_sa", "neuroimaging", "continuous"),
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)

set.seed(42)
config <- snf_config(
    dl,
    n_solutions = 50,
    dropout_dist = "none",
    max_k = 40
)


devtools::load_all()
start <- Sys.time()
sol_df <- batch_snf(dl, config)
print(Sys.time() - start)

nested_list <- lapply(
  split(dl, sapply(dl, `[[`, "domain")),
  function(lst) lapply(lst, `[[`, "data")
)

nested_list[[1]]
