library(metasnf)

heart_rate_df <- data.frame(
    patient_id = c("1", "2", "3"),
    var1 = c(0.04, 0.1, 0.3),
    var2 = c(30, 2, 0.3)
)

personality_test_df <- data.frame(
    patient_id = c("1", "2", "3"),
    var3 = c(900, 1990, 373),
    var4 = c(509, 2209, 83)
)

survey_response_df <- data.frame(
    patient_id = c("1", "2", "3"),
    var5 = c(1, 3, 3),
    var6 = c(2, 3, 3)
)

city_df <- data.frame(
    patient_id = c("1", "2", "3"),
    var7 = c("toronto", "montreal", "vancouver")
)


# Compact loading
dl <- generate_data_list(
    list(heart_rate_df, "heart_rate", "clinical", "continuous"),
    list(personality_test_df, "personality_test", "surveys", "continuous"),
    list(survey_response_df, "survey_response", "surveys", "ordinal"),
    list(city_df, "city", "location", "categorical"),
    uid = "patient_id"
)


class(data_list)

data_list <- generate_data_list(
    list(city_df, "city", "location", "categorical"),
    uid = "patient_id"
)

long_df <- data.frame(
    patient_id = as.character(1:100),
    var7 = c(1:100)
)

data_list <- generate_data_list(
    list(long_df, "city", "location", "categorical"),
    uid = "patient_id"
)

class(data_list)


inherits(data_list, "list")

library(tibble)


datalist
