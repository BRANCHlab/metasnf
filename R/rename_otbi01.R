#' Rename ambiguous columns in otbi01 file.
#'
#' Renames column names of ABCD's Ohio TBI Screen data to meaningful names that are easier to work with.
#' Use 'original_otbi_names()' to print out a conversion table of old and new column names.
#'
#' @param otbi01 The complete file abcd_otbi01.txt
#'
#' @return renamed_otbi01 A modified form of otbi01 with clearer column names
#' @export
#'
#' @examples
#' # Mock abcd_otbi01.txt
#' abcd_otbi01 <- data.frame(matrix(NA, nrow = 2, ncol = 49))
#'
#' colnames(abcd_otbi01) <- c("collection_id", "abcd_otbi01_id", "dataset_id", 
#'     "subjectkey", "src_subject_id", "interview_date", "interview_age", "sex", 
#'     "eventname", "tbi_select_language___1", "tbi_1", "tbi_1b", "tbi_1c",
#'     "tbi_1d", "tbi_2", "tbi_2b", "tbi_2c", "tbi_2d", "tbi_3", "tbi_3b",
#'     "tbi_3c", "tbi_3d", "tbi_4", "tbi_4b", "tbi_4c", "tbi_4d", "tbi_5",
#'     "tbi_5b", "tbi_5c", "tbi_5d", "tbi_6o", "tbi_6p", "tbi_6q", "tbi_6r",
#'     "tbi_6s", "tbi_7a", "tbi_7c1", "tbl_7c2", "tbi_7e", "tbi_7f", "tbi_7g",
#'     "tbi_7i", "tbi_7k", "tbi_7l", "tbi_8g", "tbi_8i", "tbi_8k", "tbi_8l",
#'     "collection_title")
#'
#' otbi01_renamed <- rename_otbi01(abcd_otbi01)
rename_otbi01 <- function(otbi01) {
    renamed_otbi01 <- otbi01 |>
        dplyr::rename(
            "hosp_er_inj" = "tbi_1", # ever hospitalized/ER for head/neck injury?
            "hosp_er_loc" = "tbi_1b", # if LOC, how long?
            "hosp_er_mem_daze" = "tbi_1c", # were they dazed or have memory gap?
            "hosp_er_age" = "tbi_1d", # how old were they?
            "vehicle_inj" = "tbi_2", # ever injured in a vehicle accident?
            "vehicle_loc" = "tbi_2b", # if LOC, how long?
            "vehicle_mem_daze" = "tbi_2c", # were they dazed or have memory gap?
            "vehicle_age" = "tbi_2d", # how old were they?
            "fall_hit_inj" = "tbi_3", # ever injured head/neck from fall or hit?
            "fall_hit_loc" = "tbi_3b", # if LOC, how long?
            "fall_hit_mem_daze" = "tbi_3c", # were they dazed or have memory gap?
            "fall_hit_age" = "tbi_3d", # how old were they?
            "violent_hit_inj" = "tbi_4", # ever injure head/neck from violence?
            "violent_hit_loc" = "tbi_4b", # if LOC, how long?
            "violent_hit_mem_daze" = "tbi_4c", # were they dazed or have memory gap?
            "violent_hit_age" = "tbi_4d", # how old were they?
            "blast_inj" = "tbi_5", # ever injure head or neck from blast?
            "blast_loc" = "tbi_5b", # if LOC, how long?
            "blast_mem_daze" = "tbi_5c", # were they dazed or have memory gap?
            "blast_age" = "tbi_5d", # how old were they?
            "other_loc_inj" = "tbi_6o", # any other injuries with LOC?
            "other_loc_num" = "tbi_6p", # how many more?
            "other_loc_max_loc_mins" = "tbi_6q", # how long was longest LOC?
            "other_loc_num_over_30" = "tbi_6r", # how many were >= 30 min?
            "other_loc_min_age" = "tbi_6s", # what was their youngest age?
            "multi_inj" = "tbi_7a", # did they have a period of multiple injuries?
            "multi_loc" = "tbi_7c1", # if LOC, how long?
            "multi_mem_daze" = "tbl_7c2", # were they dazed or have memory gap?
            "multi_effect_start_age" = "tbi_7e", # at what age did the effects begin?
            "multi_effect_end_age" = "tbi_7f", # at what age did the effects end?
            "other_multi_inj" = "tbi_7g", # was there another multiple injury period?
            "other_multi_effect_type" = "tbi_7i", # typical effect of the injury?
            "other_multi_effect_start_age" = "tbi_7k", # start age of those effects?
            "other_multi_effect_end_age" = "tbi_7l", # end age of those effects?
            "other_other_multi_inj" = "tbi_8g", # another period of multiple inj?
            "other_other_multi_effect_type" = "tbi_8i", # typical effects?
            "other_other_multi_effect_start_age" = "tbi_8k", # start age of effects?
            "other_other_multi_effect_end_age" = "tbi_8l" # end age of effects?
        )
}


#' Return conversion table of original and new otbi names
#'
#' @export
original_otbi_names <- function() {
    print("| New name                           | Old name | Description                                  |")
    print("|------------------------------------+----------+----------------------------------------------|")
    print("| hosp_er_inj                        | tbi_1    | ever hospitalized/ER for head/neck injury?   |")
    print("| hosp_er_loc                        | tbi_1b   | if LOC, how long?                            |")
    print("| hosp_er_mem_daze                   | tbi_1c   | were they dazed or have memory gap?          |")
    print("| hosp_er_age                        | tbi_1d   | how old were they?                           |")
    print("| vehicle_inj                        | tbi_2    | ever injured in a vehicle accident?          |")
    print("| vehicle_loc                        | tbi_2b   | if LOC, how long?                            |")
    print("| vehicle_mem_daze                   | tbi_2c   | were they dazed or have memory gap?          |")
    print("| vehicle_age                        | tbi_2d   | how old were they?                           |")
    print("| fall_hit_inj                       | tbi_3    | ever injured head/neck from fall or hit?     |")
    print("| fall_hit_loc                       | tbi_3b   | if LOC, how long?                            |")
    print("| fall_hit_mem_daze                  | tbi_3c   | were they dazed or have memory gap?          |")
    print("| fall_hit_age                       | tbi_3d   | how old were they?                           |")
    print("| violent_hit_inj                    | tbi_4    | ever injure head/neck from violence?         |")
    print("| violent_hit_loc                    | tbi_4b   | if LOC, how long?                            |")
    print("| violent_hit_mem_daze               | tbi_4c   | were they dazed or have memory gap?          |")
    print("| violent_hit_age                    | tbi_4d   | how old were they?                           |")
    print("| blast_inj                          | tbi_5    | ever injure head or neck from blast?         |")
    print("| blast_loc                          | tbi_5b   | if LOC, how long?                            |")
    print("| blast_mem_daze                     | tbi_5c   | were they dazed or have memory gap?          |")
    print("| blast_age                          | tbi_5d   | how old were they?                           |")
    print("| other_loc_inj                      | tbi_6o   | any other injuries with LOC?                 |")
    print("| other_loc_num                      | tbi_6p   | how many more?                               |")
    print("| other_loc_max_loc_mins             | tbi_6q   | how long was longest LOC?                    |")
    print("| other_loc_num_over_30              | tbi_6r   | how many were >= 30 min?                     |")
    print("| other_loc_min_age                  | tbi_6s   | what was their youngest age?                 |")
    print("| multi_inj                          | tbi_7a   | did they have a period of multiple injuries? |")
    print("| multi_loc                          | tbi_7c1  | if LOC, how long?                            |")
    print("| multi_mem_daze                     | tbl_7c2  | were they dazed or have memory gap?          |")
    print("| multi_effect_start_age             | tbi_7e   | at what age did the effects begin?           |")
    print("| multi_effect_end_age               | tbi_7f   | at what age did the effects end?             |")
    print("| other_multi_inj                    | tbi_7g   | was there another multiple injury period?    |")
    print("| other_multi_effect_type            | tbi_7i   | typical effect of the injury?                |")
    print("| other_multi_effect_start_age       | tbi_7k   | start age of those effects?                  |")
    print("| other_multi_effect_end_age         | tbi_7l   | end age of those effects?                    |")
    print("| other_other_multi_inj              | tbi_8g   | another period of multiple inj?              |")
    print("| other_other_multi_effect_type      | tbi_8i   | typical effects?                             |")
    print("| other_other_multi_effect_start_age | tbi_8k   | start age of effects?                        |")
    print("| other_other_multi_effect_end_age   | tbi_8l   | end age of effects?                          |")
}
