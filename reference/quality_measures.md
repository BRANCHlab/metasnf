# Quality metrics

These functions calculate conventional metrics of cluster solution
quality.

## Usage

``` r
calculate_silhouettes(sol_df)

calculate_dunn_indices(sol_df)

calculate_db_indices(sol_df)
```

## Arguments

- sol_df:

  A `solutions_df` class object created by
  [`batch_snf()`](https://branchlab.github.io/metasnf/reference/batch_snf.md)
  with the parameter `return_sim_mats = TRUE`.

## Value

A list of `silhouette` class objects, a vector of Dunn indices, or a
vector of Davies-Bouldin indices depending on which function was used.

## Details

calculate_silhouettes: A wrapper for
[`cluster::silhouette`](https://rdrr.io/pkg/cluster/man/silhouette.html)
that calculates silhouette scores for all cluster solutions in a
provided solutions data frame. Silhouette values range from -1 to +1 and
indicate an overall ratio of how close together observations within a
cluster are to how far apart observations across clusters are. You can
learn more about interpreting the results of this function by calling
[`?cluster::silhouette`](https://rdrr.io/pkg/cluster/man/silhouette.html).

calculate_dunn_indices: Calculates Dunn indices for all cluster
solutions in a provided solutions data frame.

calculate_db_indices: Calculates Davies-Bouldin indices for all cluster
solutions in a provided solutions data frame.

For Dunn and Davies-Bouldin index calculations, a grid of values is
presented based on multiple combinations of intra-cluster distance
methods (complete or average distance) and inter-cluster distance
methods (single-linkage, complete-linkage, average distance, and
Hausdorff distance).

## Examples

``` r
if (FALSE) { # \dontrun{
input_dl <- data_list(
    list(gender_df, "gender", "demographics", "categorical"),
    list(diagnosis_df, "diagnosis", "clinical", "categorical"),
    uid = "patient_id"
)

sc <- snf_config(input_dl, n_solutions = 5)

sol_df <- batch_snf(input_dl, sc, return_sim_mats = TRUE)

# calculate Davies-Bouldin indices
davies_bouldin_indices <- calculate_db_indices(sol_df)

# calculate Dunn indices
dunn_indices <- calculate_dunn_indices(sol_df)

# calculate silhouette scores
silhouette_scores <- calculate_silhouettes(sol_df)
} # }
```
