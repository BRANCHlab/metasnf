library(abcdutils)
library(readr)

# Neuroimaging variables ======================================================
smrip10201 <- read_delim(
    "~/documents/research/data/abcd/raw/abcd_smrip10201.txt"
)

mrisdp10201 <- read_delim(
    "~/documents/research/data/abcd/raw/abcd_mrisdp10201.txt"
)

mock_smrip10201 <- smrip10201 |> generate_mock_data(n = 300, seed = 42)
mock_mrisdp10201 <- mrisdp10201 |> generate_mock_data(n = 300, seed = 43)

## Subcortical volumes
abcd_subc_v <- abcdutils::get_subc_v(mock_smrip10201, t = 0)

## Cortical thickness
abcd_cort_t <- abcdutils::get_cort_t(mock_mrisdp10201, t = 0)

## Cortical volumes
abcd_cort_sa <- abcdutils::get_cort_sa(mock_mrisdp10201, t = 0)

# Demographic variables =======================================================
pdem02 <- read_delim(
    "~/documents/research/data/abcd/raw/pdem02.txt"
)

ssphp01 <- read_delim(
    "~/documents/research/data/abcd/raw/abcd_ssphp01.txt"
)

ssphy01 <- read_delim(
    "~/documents/research/data/abcd/raw/abcd_ssphy01.txt"
)

mock_pdem02 <- pdem02 |> generate_mock_data(n = 300, seed = 44)
mock_ssphp01 <- ssphp01 |> generate_mock_data(n = 3000, seed = 45)
mock_ssphy01 <- ssphy01 |> generate_mock_data(n = 3000, seed = 46)


# These two need to align for the pubertal status function to work
mock_ssphy01$"subjectkey" <- mock_ssphp01$"subjectkey"
mock_ssphy01$"sex" <- mock_ssphp01$"sex"

## Household income
abcd_income <- abcdutils::get_income(mock_pdem02, t = 0)

## Pubertal status
abcd_pubertal <- abcdutils::get_pubertal_status(
    mock_ssphp01,
    mock_ssphy01,
    t = 0
)

# Two outcome variables: depression and anxiety ===============================
abcd_cbcls01 <- read_delim(
    "~/documents/research/data/abcd/raw/abcd_cbcls01.txt"
)

mock_abcd_cbcls01 <- generate_mock_data(abcd_cbcls01, n = 3000, seed = 47)

abcd_depress <- get_cbcl_depress(mock_abcd_cbcls01, t = 0)
abcd_anxiety <- get_cbcl_anxiety(mock_abcd_cbcls01, t = 0)

abcd_depress <- abcd_depress[1:275, ]
abcd_anxiety <- abcd_anxiety[1:275, ]

# Matching the subjectkeys ====================================================
(abcd_subc_v$"subjectkey" <- abcd_pubertal$"subjectkey"[1:nrow(abcd_subc_v)])

(abcd_cort_t$"subjectkey" <- abcd_pubertal$"subjectkey"[1:nrow(abcd_cort_t)])

(abcd_cort_sa$"subjectkey" <- abcd_pubertal$"subjectkey"[1:nrow(abcd_cort_sa)])

(abcd_income$"subjectkey" <- abcd_pubertal$"subjectkey"[1:nrow(abcd_income)])

(abcd_pubertal$"subjectkey" <- abcd_pubertal$"subjectkey"[1:nrow(abcd_pubertal)])

(abcd_depress$"subjectkey" <- abcd_pubertal$"subjectkey"[1:nrow(abcd_depress)])

(abcd_anxiety$"subjectkey" <- abcd_pubertal$"subjectkey"[1:nrow(abcd_anxiety)])

# Mimicking data that does not have "subjectkey" as the initial UID
abcd_subc_v <- abcd_subc_v |> dplyr::rename("patient" = "subjectkey")
abcd_cort_t <- abcd_cort_t |> dplyr::rename("patient" = "subjectkey")
abcd_cort_sa <- abcd_cort_sa |> dplyr::rename("patient" = "subjectkey")
abcd_income <- abcd_income |> dplyr::rename("patient" = "subjectkey")
abcd_pubertal <- abcd_pubertal |> dplyr::rename("patient" = "subjectkey")
abcd_depress <- abcd_depress |> dplyr::rename("patient" = "subjectkey")
abcd_anxiety <- abcd_anxiety |> dplyr::rename("patient" = "subjectkey")

# Creating an example of categorical data
abcd_colour <- abcd_depress |>
    dplyr::mutate(
        colour = dplyr::case_when(
            cbcl_depress_r > 1 ~ "red",
            cbcl_depress_r == 1 ~ "yellow",
            cbcl_depress_r == 0 ~ "green"
        )
    ) |>
    dplyr::select(patient, colour)

usethis::use_data(abcd_subc_v, overwrite = TRUE)
usethis::use_data(abcd_cort_t, overwrite = TRUE)
usethis::use_data(abcd_cort_sa, overwrite = TRUE)
usethis::use_data(abcd_income, overwrite = TRUE)
usethis::use_data(abcd_pubertal, overwrite = TRUE)
usethis::use_data(abcd_depress, overwrite = TRUE)
usethis::use_data(abcd_anxiety, overwrite = TRUE)
usethis::use_data(abcd_colour, overwrite = TRUE)

################################################################################
# 2023-10-31 Removing NAs from the subjectkey column of abcd_income
library(metasnf)

abcd_h_income <- abcd_income
abcd_h_income <- abcd_h_income[!is.na(abcd_h_income$"patient"), ]

usethis::use_data(abcd_h_income, overwrite = TRUE)
###############################################################################
# 2024-07-11 Replacing "patient" uid to simply "unique_id"

library(metasnf)

subc_v <- abcd_subc_v |> dplyr::rename("unique_id" = "patient")
cort_t <- abcd_cort_t |> dplyr::rename("unique_id" = "patient")
cort_sa <- abcd_cort_sa |> dplyr::rename("unique_id" = "patient")
income <- abcd_h_income |> dplyr::rename("unique_id" = "patient")
pubertal <- abcd_pubertal |> dplyr::rename("unique_id" = "patient")
depress <- abcd_depress |> dplyr::rename("unique_id" = "patient")
anxiety <- abcd_anxiety |> dplyr::rename("unique_id" = "patient")
fav_colour <- abcd_colour |> dplyr::rename("unique_id" = "patient")

usethis::use_data(subc_v, overwrite = TRUE)
usethis::use_data(cort_t, overwrite = TRUE)
usethis::use_data(cort_sa, overwrite = TRUE)
usethis::use_data(income, overwrite = TRUE)
usethis::use_data(pubertal, overwrite = TRUE)
usethis::use_data(depress, overwrite = TRUE)
usethis::use_data(anxiety, overwrite = TRUE)
usethis::use_data(fav_colour, overwrite = TRUE)
