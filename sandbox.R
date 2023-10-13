library(metasnf)

data_list <- generate_data_list(
    list(abcd_cort_t, "cortical_thickness", "neuroimaging", "numeric"),
    list(abcd_cort_sa, "cortical_surface_area", "neuroimaging", "numeric"),
    list(abcd_subc_v, "subcortical_volume", "neuroimaging", "numeric"),
    list(abcd_income, "household_income", "demographics", "numeric"),
    list(abcd_pubertal, "pubertal_status", "demographics", "numeric"),
    old_uid = "patient"
)

length(data_list)

settings_matrix <- generate_settings_matrix(data_list, nrow = 15, seed = 42)

new_settings_matrix <- generate_settings_matrix(data_list, nrow = 15, seed = 42)

new_settings_matrix <- generate_settings_matrix(data_list, nrow = 15, seed = 42, dropout_dist = "exponential")

identical(settings_matrix, new_settings_matrix)

new_settings_matrix

z1 <- generate_settings_matrix(data_list, nrow = 15, seed = 42)

generate_settings_matrix(data_list, nrow = 15, dropout_dist = "uniform")

generate_settings_matrix(data_list, nrow = 15, dropout_dist = "exponential")

generate_settings_matrix(data_list, nrow = 15, dropout_dist = "none")


z1

z2 <- generate_settings_matrix(data_list, nrow = 15, seed = 42)

settings_matrix <- generate_settings_matrix(
    data_list,
    nrow = 15,
    seed = 42,
    min_alpha = 0.3,
    max_alpha = 0.8,
    possible_k = seq(10, 100, by = 10),
    possible_snf_schemes = c(1, 2)
)

sample(20:30, 1)


rownames(new_settings_matrix) <- NULL

colnames(new_settings_matrix)

q <- t(data.frame(colnames(new_settings_matrix)))

colnames(q) <- colnames(new_settings_matrix)

rownames(q) <- NULL

str(q)

data.frame(t(rep(1, 5)))
