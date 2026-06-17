# Build a `data_list` class object

`data_list()` constructs a data list object which inherits from classes
`data_list` and `list`. This object is the primary way in which features
to be used along the `metasnf` clustering pipeline are stored. The data
list is fundamentally a 2-level nested list object where each inner list
contains a data frame and associated metadata for that data frame. The
metadata includes the name of the data frame, the 'domain' of that data
frame (the broader source of information that the input data frame is
capturing, determined by user's domain knowledge), and the type of
feature stored in the data frame (continuous, discrete, ordinal,
categorical, or mixed).

## Usage

``` r
data_list(..., uid)
```

## Arguments

- ...:

  Any number of lists formatted as (df, "df_name", "df_domain",
  "df_type") and/or any number of lists of lists formatted as (df,
  "df_name", "df_domain", "df_type").

- uid:

  (character) the name of the uid column currently used data. data
  frame.

## Examples

``` r
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

# Explicitly (Name each nested list element):
dl <- data_list(
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
    uid = "patient_id"
)

# Compact loading
dl <- data_list(
    list(heart_rate_df, "heart_rate", "clinical", "continuous"),
    list(personality_test_df, "personality_test", "surveys", "continuous"),
    list(survey_response_df, "survey_response", "surveys", "ordinal"),
    list(city_df, "city", "location", "categorical"),
    uid = "patient_id"
)

# Printing data list summaries
summary(dl)
#>               name        type   domain length width
#> 1       heart_rate  continuous clinical      3     2
#> 2 personality_test  continuous  surveys      3     2
#> 3  survey_response     ordinal  surveys      3     2
#> 4             city categorical location      3     1

# Alternative loading: providing a single list of lists
list_of_lists <- list(
    list(heart_rate_df, "data1", "domain1", "continuous"),
    list(personality_test_df, "data2", "domain2", "continuous")
)

dl <- data_list(
    list_of_lists,
    uid = "patient_id"
)
```
