library(SNFtool)

###############################################################################
# Extending SNF Data1 and Data2
###############################################################################
# Load in data from SNFtool
data(Data1)
data(Data2)

# More descriptive column names
colnames(Data1) <- c("gene_1_expression", "gene_2_expression")
colnames(Data2) <- c("gene_1_methylation", "gene_2_methylation")

# Assign unique patient identifiers to each patient
set.seed(42)
random_ids <- sample(100:999)[1:200]

Data1$"patient_id" <- random_ids
Data2$"patient_id" <- random_ids

expression_df <- Data1
methylation_df <- Data2

###############################################################################
# Fake gender, diagnosis, and atopic asthma variables to accompany above data
###############################################################################
# Set the possible values
gender <- c("female", "male")
diagnosis <- c("definite asthma", "possible asthma", "no asthma")
atopic_asthma <- c("Yes", "No")

# C1: mostly F, mostly "definite asthma", mostly "Yes" to atopic asthma
c1_genders <- sample(gender, 100, replace = TRUE, prob = c(3, 1))
c1_diagnosis <- sample(diagnosis, 100, replace = TRUE, prob = c(3, 2, 1))
c1_asthma <- sample(atopic_asthma, 100, replace = TRUE, prob = c(3, 1))
c1_ages <- sample(c(12:18), 100, replace = TRUE, prob = c(1:7))

c2_genders <- sample(gender, 100, replace = TRUE, prob = c(1, 3))
c2_diagnosis <- sample(diagnosis, 100, replace = TRUE, prob = c(1, 2, 3))
c2_asthma <- sample(atopic_asthma, 100, replace = TRUE, prob = c(1, 3))
c2_ages <- sample(c(18:30), 100, replace = TRUE,)

genders <- c(c1_genders, c2_genders)
diagnoses <- c(c1_diagnosis, c2_diagnosis)
asthmas <- c(c1_asthma, c2_asthma)
ages <- c(c1_ages, c2_ages)

gender_df <- data.frame(
    patient_id = random_ids,
    gender = genders
)

diagnosis_df <- data.frame(
    patient_id = random_ids,
    diagnosis = diagnoses
)

age_df <- data.frame(
    patient_id = random_ids,
    age = ages
)

dim(expression_df)

###############################################################################
# Export
###############################################################################
usethis::use_data(expression_df, overwrite = TRUE)
usethis::use_data(methylation_df, overwrite = TRUE)
usethis::use_data(gender_df, overwrite = TRUE)
usethis::use_data(diagnosis_df, overwrite = TRUE)
usethis::use_data(age_df, overwrite = TRUE)


###############################################################################
# Tacking on cancer type df to be the same thing as diagnosis_df
###############################################################################
library(metasnf)

cancer_diagnosis_df <- diagnosis_df |>
    dplyr::mutate(
        diagnosis = dplyr::case_when(
            diagnosis == "definite asthma" ~ "1",
            diagnosis == "possible asthma" ~ "2",
            diagnosis == "no asthma" ~ "3"
        )
    )

usethis::use_data(cancer_diagnosis_df, overwrite = TRUE)
