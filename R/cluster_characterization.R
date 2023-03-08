#' Heatmap p-value matrix
#'
#' @param p_val_matrix matrix of p-values
#' @param file_path where to store heatmap
#'
#' @export
heatmap_pvals <- function(p_val_matrix, file_path = NA) {
    my_colors <- grDevices::colorRampPalette(c("cyan", "deeppink3"))
    pheatmap::pheatmap(p_val_matrix, col = rev(my_colors(100)),
        filename = file_path)
    pheatmap::pheatmap(p_val_matrix, col = rev(my_colors(100)))
}


#' Select all output_matrix columns except for subjects
#'
#' @description
#' Return a dataframe without any subject columns
#'
#' @param df Datframe
#'
#' @return df_no_subs Dataframe without subjects
#'
#' @export
no_subs <- function(df) {
    df_no_subs <- df |> dplyr::select(!(dplyr::starts_with("NDAR")))
    return(df_no_subs)
}


#' Select only output_matrix columns containing subjects
#'
#' @description
#' Return a dataframe with only subject columns
#'
#' @param df Datframe
#'
#' @return df_subs Dataframe without subjects
#'
#' @export
subs <- function(df) {
    df_subs <- df |> dplyr::select(
        "row_id",
        dplyr::starts_with("NDAR"))
    return(df_subs)
}

#' Select specific row_ids from an output matrix
#'
#' @param om output matrix
#' @param row_ids vector of row_id values to be selected
#'
#' @return selected_om
#'
#' @export
select_om <- function(om, row_ids) {
    selected_om <- om[om$"row_id" %in% row_ids, ]
    return(selected_om)
}
