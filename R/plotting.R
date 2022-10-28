#' Wrapper for geom_hist
#'
#' @param df A dataframe
#' @param xval x-value for histogram passed as a string
#' @param xlabel x-axis label
#' @param size x-axis label
#'
#' @return renamed_tbi A modified form of tbi_df with clearer column names
#'
#' @export
#'
branch_hist <- function(df, xval, xlabel, size = 20) {
    ggplot2::ggplot(df, ggplot2::aes_string(x = xval)) +
        ggplot2::geom_histogram(color = "darkblue", fill = "lightblue") +
        ggplot2::ggtitle("Histogram of times since latest mTBI") +
        ggplot2::xlab(xlabel) +
        ggplot2::ylab("Count") +
        ggplot2::theme_light(base_size = size)
}

branch_ggsave <- function(path, plotname) {
    carbon_path <- paste0(path, plotname)
    repo_path <- paste0("figures/abcd/", plotname)
    print("Warning: This will overwrite files located at:")
    print(paste0("[1]: ", here::here(carbon_path)))
    print(paste0("[2]: ", here::here(repo_path)))
    proceed <- readline(prompt = "Proceed? [N/y]: ")
    if (proceed == "y") {
        ## For carbon
        ggplot2::ggsave(here::here(carbon_path),
            width = 10,
            height = 10)

        # For this repo
        ggplot2::ggsave(here::here(repo_path),
            width = 10,
            height = 10)
    }
}
