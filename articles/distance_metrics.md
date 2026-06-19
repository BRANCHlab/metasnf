# Distance Metrics

Download a copy of the vignette to follow along here:
[distance_metrics.Rmd](https://raw.githubusercontent.com/BRANCHlab/metasnf/main/vignettes/distance_metrics.Rmd)

## Distance functions

metasnf enables users to customize what distance metrics are used in the
SNF pipeline. All information about distance metrics are stored in a
`dist_fns_list` object.

When no relevant arguments are provided,
[`snf_config()`](https://branchlab.github.io/metasnf/reference/snf_config.md)
will create its own `dist_fns_list` class object by calling the
[`dist_fns_list()`](https://branchlab.github.io/metasnf/reference/dist_fns_list.md)
function with an argument indicating that the list should be populated
with default distance functions.

``` r

library(metasnf)

dl <- data_list(
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    list(depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)
#> ℹ 38 observations dropped due to incomplete data.

sc <- snf_config(
    dl = dl,
    n_solutions = 5
)
#> ℹ No distance functions specified. Using defaults.
#> ℹ No clustering functions specified. Using defaults.

sc$"dist_fns_list"
#> Continuous (1):
#> [1] euclidean_distance
#> Discrete (1):
#> [1] euclidean_distance
#> Ordinal (1):
#> [1] euclidean_distance
#> Categorical (1):
#> [1] gower_distance
#> Mixed (1):
#> [1] gower_distance
```

The list is a list of functions
([`euclidean_distance()`](https://branchlab.github.io/metasnf/reference/dist_fns.md)
and
[`gower_distance()`](https://branchlab.github.io/metasnf/reference/dist_fns.md)
in this case).

These lists can contain any number of distance metrics for each of the 5
recognized types of features: continuous, discrete, ordinal,
categorical, and mixed (any combination of the previous four).

By default, continuous, discrete, and ordinal data are converted to
distance matrices using simple Euclidean distance. Categorical and mixed
data are handled using Gower’s formula as implemented by the cluster
package (see
[`?cluster::daisy`](https://rdrr.io/pkg/cluster/man/daisy.html)).

## How the dist_fns_list is used

To show how the dist_fns_list is used, we’ll start by extending our
dist_fns_list beyond just the default options. metasnf provides a
Euclidean distance function that applies standard normalization first,
[`sn_euclidean_distance()`](https://branchlab.github.io/metasnf/reference/dist_fns.md)
(a wrapper around
[`SNFtool::standardNormalization`](https://rdrr.io/pkg/SNFtool/man/standardNormalization.html) +
[`stats::dist`](https://rdrr.io/r/stats/dist.html)). Here’s how we can
create a custom dist_fns_list that includes this metric for continuous
and discrete features.

``` r

sc <- snf_config(
    dl = dl,
    n_solutions = 10,
    cnt_dist_fns = list("standard_norm_euclidean" = sn_euclidean_distance),
    dsc_dist_fns = list("standard_norm_euclidean" = sn_euclidean_distance),
    use_default_dist_fns = TRUE
)
#> ℹ No clustering functions specified. Using defaults.

sc
#> Settings Data Frame:
#>                1    2    3    4    5    6    7    8    9   10
#> SNF hyperparameters:
#> alpha        0.5  0.7  0.8  0.4  0.7  0.5  0.7  0.4  0.3  0.6
#> k             70   81   16   33   81   69   22   21   30   25  
#> t             20   20   20   20   20   20   20   20   20   20  
#> SNF scheme:
#>                3    1    2    3    3    1    3    3    1    2  
#> Clustering functions:
#>                2    2    2    1    2    2    1    2    1    1  
#> Distance functions:
#> CNT            1    2    2    1    1    1    1    1    2    1  
#> DSC            1    1    2    1    2    1    1    2    2    2  
#> ORD            1    1    1    1    1    1    1    1    1    1  
#> CAT            1    1    1    1    1    1    1    1    1    1  
#> MIX            1    1    1    1    1    1    1    1    1    1  
#> Component dropout:
#> anxiety        ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
#> depressed      ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
#> Distance Functions List:
#> Continuous (2):
#> [1] euclidean_distance
#> [2] standard_norm_euclidean
#> Discrete (2):
#> [1] euclidean_distance
#> [2] standard_norm_euclidean
#> Ordinal (1):
#> [1] euclidean_distance
#> Categorical (1):
#> [1] gower_distance
#> Mixed (1):
#> [1] gower_distance
#> Clustering Functions List:
#> [1] spectral_eigen
#> [2] spectral_rot
#> Weights Matrix:
#> Weights defined for 10 cluster solutions.
#> $ cbcl_anxiety_r  1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ cbcl_depress_r  1, 1, 1, 1, 1, 1, 1, 1, 1, 1
```

The rows in the settings data frame part of the SNF config show the
continuous (CNT) and discrete (DSTC) distance metrics values randomly
fluctuate between 1 and 2, where 1 means that the first metric
([`euclidean_distance()`](https://branchlab.github.io/metasnf/reference/dist_fns.md))
will be used and 2 means that the second metric
(`sn_euclidean_distance`) will be used.

The settings data frame within the config stores pointers to which
function should be used, while the distance functions list in the config
stores the actual functions.

Once you have an SNF config that contains a distance functions list to
your liking, you can call
[`batch_snf()`](https://branchlab.github.io/metasnf/reference/batch_snf.md)
as usual:

``` r

sol_df <- batch_snf(dl, sc)
```

## Removing the default distance_metrics

There are two ways to avoid using the default distance metrics.

The first way is to use the `use_default_dist_fns` parameter:

``` r

sc <- snf_config(
    dl = dl,
    n_solutions = 10,
    cnt_dist_fns = list("standard_norm_euclidean" = sn_euclidean_distance),
    dsc_dist_fns = list("standard_norm_euclidean" = sn_euclidean_distance),
    ord_dist_fns = list("standard_norm_euclidean" = sn_euclidean_distance),
    mix_dist_fns = list("standard_norm_euclidean" = gower_distance),
    use_default_dist_fns = FALSE
)
#> ℹ No clustering functions specified. Using defaults.

sc
#> Settings Data Frame:
#>                1    2    3    4    5    6    7    8    9   10
#> SNF hyperparameters:
#> alpha        0.4  0.7  0.7  0.6  0.7  0.8  0.7  0.5  0.3  0.8
#> k             36   89   87   79   43   50   97   74   41   25  
#> t             20   20   20   20   20   20   20   20   20   20  
#> SNF scheme:
#>                2    3    2    3    1    2    2    1    3    1  
#> Clustering functions:
#>                1    1    1    2    2    2    1    2    1    2  
#> Distance functions:
#> CNT            1    1    1    1    1    1    1    1    1    1  
#> DSC            1    1    1    1    1    1    1    1    1    1  
#> ORD            1    1    1    1    1    1    1    1    1    1  
#> CAT            0    0    0    0    0    0    0    0    0    0  
#> MIX            1    1    1    1    1    1    1    1    1    1  
#> Component dropout:
#> anxiety        ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
#> depressed      ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
#> Distance Functions List:
#> Continuous (1):
#> [1] standard_norm_euclidean
#> Discrete (1):
#> [1] standard_norm_euclidean
#> Ordinal (1):
#> [1] standard_norm_euclidean
#> Categorical (0):
#> Mixed (1):
#> [1] standard_norm_euclidean
#> Clustering Functions List:
#> [1] spectral_eigen
#> [2] spectral_rot
#> Weights Matrix:
#> Weights defined for 10 cluster solutions.
#> $ cbcl_anxiety_r  1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ cbcl_depress_r  1, 1, 1, 1, 1, 1, 1, 1, 1, 1
```

The second way is to explicitly specify which indices you want to sample
from during SNF config generation:

``` r

sc <- snf_config(
    dl = dl,
    n_solutions = 10,
    cnt_dist_fns = list(
        "standard_norm_euclidean" = sn_euclidean_distance,
        "some_other_metric" = sn_euclidean_distance
    ),
    dsc_dist_fns = list(
        "standard_norm_euclidean" = sn_euclidean_distance,
        "some_other_metric" = sn_euclidean_distance
    ),
    use_default_dist_fns = TRUE,
    continuous_distances = 1,
    discrete_distances = c(2, 3)
)
#> ℹ No clustering functions specified. Using defaults.

sc
#> Settings Data Frame:
#>                1    2    3    4    5    6    7    8    9   10
#> SNF hyperparameters:
#> alpha        0.5  0.5  0.6  0.8  0.8  0.4  0.8  0.5  0.4  0.3
#> k             14   23   62   35   80   80   19   64   35   60  
#> t             20   20   20   20   20   20   20   20   20   20  
#> SNF scheme:
#>                2    2    3    2    3    1    3    1    3    2  
#> Clustering functions:
#>                2    1    1    2    2    1    1    2    1    2  
#> Distance functions:
#> CNT            1    1    1    1    1    1    1    1    1    1  
#> DSC            2    2    2    2    3    2    2    3    3    3  
#> ORD            1    1    1    1    1    1    1    1    1    1  
#> CAT            1    1    1    1    1    1    1    1    1    1  
#> MIX            1    1    1    1    1    1    1    1    1    1  
#> Component dropout:
#> anxiety        ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
#> depressed      ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
#> Distance Functions List:
#> Continuous (3):
#> [1] euclidean_distance
#> [2] standard_norm_euclidean
#> [3] some_other_metric
#> Discrete (3):
#> [1] euclidean_distance
#> [2] standard_norm_euclidean
#> [3] some_other_metric
#> Ordinal (1):
#> [1] euclidean_distance
#> Categorical (1):
#> [1] gower_distance
#> Mixed (1):
#> [1] gower_distance
#> Clustering Functions List:
#> [1] spectral_eigen
#> [2] spectral_rot
#> Weights Matrix:
#> Weights defined for 10 cluster solutions.
#> $ cbcl_anxiety_r  1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ cbcl_depress_r  1, 1, 1, 1, 1, 1, 1, 1, 1, 1
```

This can save you the trouble of needing to manage several distinct
distance metrics lists or manage your solution space over separate runs
of `batch_snf`.

## Supplying weights to distance metrics

Some distance metric functions can accept weights. Usually, weights will
be applied by direct scaling of specified features. In some cases
(e.g. categorical distance metric functions), the way in which weights
are applied may be somewhat less intuitive. The bottom of this vignette
outlines the available distance metric functions grouped by whether or
not they accept weights. You can examine the documentation of those
weighted functions to learn more about how the weights you provide will
be used.

An important note on providing weights for a run of SNF is that the
specific form of the data may not be what you expect by the time it is
ready to be converted into a distance metric function. The “individual”
and “two-step” SNF schemes involve distance metrics only being applied
to the input data frames in the data list as they are. The “domain”
scheme, however, concatenates data within a domain before converting
that larger data frame into a distance matrix. Anytime you have more
than one data frame with the same domain label and you use the domain
SNF scheme, all the columns associated with that domain will be in a
single data frame when the distance metric function is applied.

By default,
[`snf_config()`](https://branchlab.github.io/metasnf/reference/snf_config.md)
also defines the `weights_matrix` class object that stores feature
weights. Without any user provided weights, these weights are just
initialized to 1.

``` r

sc$"weights_matrix"
#> Weights defined for 10 cluster solutions.
#> $ cbcl_anxiety_r  1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ cbcl_depress_r  1, 1, 1, 1, 1, 1, 1, 1, 1, 1
```

To actually use the matrix during SNF, you’ll need to make sure that the
number of rows in the weights matrix is the same as the number of rows
in your settings data frame.

You can either replace the 1s with your own weights that you’ve
calculated outside of the package, or use some random weights following
a uniform or exponential distribution.

``` r

# random weights:
sc <- snf_config(
    dl = dl,
    n_solutions = 10,
    weights_fill = "uniform" # or fill = "exponential"
)

sc

# custom weights
fts <- features(dl)
custom_wm <- matrix(nrow = 10, ncol = length(fts), rnorm(10 * length(fts))^2)
colnames(custom_wm) <- fts
custom_wm <- as_weights_matrix(custom_wm)

sc <- snf_config(
    dl = dl,
    n_solutions = 10,
    wm = custom_wm
)

sc
```

The default metrics (simple Euclidean for continuous, discrete, and
ordinal data and Gower’s distance for categorical and mixed data) are
both capable of applying weights to data before distance matrix
generation.

## Custom distance metrics

The remainder of this vignette deals with supplying custom distance
metrics (including custom feature weighting). Making use of this
functionality will require a good understanding of working with
functions in R.

You can also supply your own custom distance metrics. Looking at the
code from one of the package-provided distance functions shows some of
the essential aspects of a well-formated distance function.

``` r

euclidean_distance
#> function (df, weights_row) 
#> {
#>     weights <- diag(weights_row, nrow = length(weights_row))
#>     weighted_df <- as.matrix(df) %*% weights
#>     distance_matrix <- as.matrix(stats::dist(weighted_df, method = "euclidean"))
#>     return(distance_matrix)
#> }
#> <bytecode: 0x55e91d904570>
#> <environment: namespace:metasnf>
```

The function should accept two arguments: `df` and `weights_row`, and
only give one output, `distance_matrix`. The function doesn’t actually
need to make use of those weights if you don’t want it to.

By the time your data reaches a distance metric function, it (referred
to as `df`) will always:

1.  have no UID column
2.  have at least one feature column
3.  have no missing values
4.  be a data.frame (not a tibble)

The feature column names won’t be altered from the values they had when
they were loaded into the data_list.

For example, consider the `anxiety` raw data supplied by metasnf:

``` r

head(anxiety)
#> # A tibble: 6 × 2
#>   unique_id        cbcl_anxiety_r
#>   <chr>                     <dbl>
#> 1 NDAR_INV0567T2Y9              3
#> 2 NDAR_INV0GLZNC2W              1
#> 3 NDAR_INV0IZ157F8             NA
#> 4 NDAR_INV0J4PYA5F              0
#> 5 NDAR_INV0OYE291Q              2
#> 6 NDAR_INV0SM1JLXQ              2
```

Here’s how to make it look more like what the distance metric functions
will expect to see:

``` r

processed_anxiety <- anxiety |>
    na.omit() |> # no NAs
    dplyr::rename("uid" = "unique_id") |>
    data.frame(row.names = "uid")

head(processed_anxiety)
#>                  cbcl_anxiety_r
#> NDAR_INV0567T2Y9              3
#> NDAR_INV0GLZNC2W              1
#> NDAR_INV0J4PYA5F              0
#> NDAR_INV0OYE291Q              2
#> NDAR_INV0SM1JLXQ              2
#> NDAR_INV0Z87UJDR              0
```

If we want to have a distance metric that calculates Euclidean distance,
but also scales the resulting matrix down such that the biggest allowed
distance is a 1, it would look like this:

``` r

my_scaled_euclidean <- function(df, weights_row) {
    # this function won't apply the weights it is given
    distance_matrix <- df |>
        stats::dist(method = "euclidean") |>
        as.matrix() # make sure it's formatted as a matrix
    distance_matrix <- distance_matrix / max(distance_matrix)
    return(distance_matrix)
}
```

You’ll need to be mindful of any edge cases that your function will run
into. For example, this function will fail if the pairwise distances for
all patients is 0 (a division by 0 will occur). If that specific
situation ever happens, there’s probably something quite wrong with the
data.

Once you’re happy that you distance function is working as you’d like it
to:

``` r

my_scaled_euclidean(processed_anxiety)[1:5, 1:5]
#>                  NDAR_INV0567T2Y9 NDAR_INV0GLZNC2W NDAR_INV0J4PYA5F
#> NDAR_INV0567T2Y9              0.0              0.2              0.3
#> NDAR_INV0GLZNC2W              0.2              0.0              0.1
#> NDAR_INV0J4PYA5F              0.3              0.1              0.0
#> NDAR_INV0OYE291Q              0.1              0.1              0.2
#> NDAR_INV0SM1JLXQ              0.1              0.1              0.2
#>                  NDAR_INV0OYE291Q NDAR_INV0SM1JLXQ
#> NDAR_INV0567T2Y9              0.1              0.1
#> NDAR_INV0GLZNC2W              0.1              0.1
#> NDAR_INV0J4PYA5F              0.2              0.2
#> NDAR_INV0OYE291Q              0.0              0.0
#> NDAR_INV0SM1JLXQ              0.0              0.0
```

You can load it into the distance functions list during SNF config
construction:

``` r

sc <- snf_config(
    n_solutions = 10,
    dl = dl,
    cnt_dist_fns = list(
        "my_scaled_euclidean" = my_scaled_euclidean
    ),
    use_default_dist_fns = TRUE
)
#> ℹ No clustering functions specified. Using defaults.

sc
#> Settings Data Frame:
#>                1    2    3    4    5    6    7    8    9   10
#> SNF hyperparameters:
#> alpha        0.3  0.4  0.7  0.6  0.5  0.6  0.3  0.4  0.6  0.7
#> k             12   18   29   47   23   93   93   84   61   41  
#> t             20   20   20   20   20   20   20   20   20   20  
#> SNF scheme:
#>                2    3    3    1    3    1    2    1    1    2  
#> Clustering functions:
#>                1    1    2    1    2    2    2    1    1    1  
#> Distance functions:
#> CNT            1    1    1    1    1    2    2    1    1    2  
#> DSC            1    1    1    1    1    1    1    1    1    1  
#> ORD            1    1    1    1    1    1    1    1    1    1  
#> CAT            1    1    1    1    1    1    1    1    1    1  
#> MIX            1    1    1    1    1    1    1    1    1    1  
#> Component dropout:
#> anxiety        ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
#> depressed      ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
#> Distance Functions List:
#> Continuous (2):
#> [1] euclidean_distance
#> [2] my_scaled_euclidean
#> Discrete (1):
#> [1] euclidean_distance
#> Ordinal (1):
#> [1] euclidean_distance
#> Categorical (1):
#> [1] gower_distance
#> Mixed (1):
#> [1] gower_distance
#> Clustering Functions List:
#> [1] spectral_eigen
#> [2] spectral_rot
#> Weights Matrix:
#> Weights defined for 10 cluster solutions.
#> $ cbcl_anxiety_r  1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ cbcl_depress_r  1, 1, 1, 1, 1, 1, 1, 1, 1, 1
```

## Requesting metrics

If there’s a metric you’d like to see added as a prewritten option
included in the package, feel free to post an issue or make a pull
request on the package’s GitHub.

## List of prewritten distance metrics functions

These metrics can be used as is. They are all capable of accepting and
applying custom weights provided by a `weights_matrix`.

- `euclidean_distance` (Euclidean distance)
  - applies to continuous, discrete, or ordinal data
- `sn_euclidean_distance` (Standard normalized Euclidean distance)
  - Standard normalize data, then use Euclidean distance
  - applies to continuous, discrete, or ordinal data
- `gower_distance` (Gower’s distance)
  - applies to any data
- `siw_euclidean_distance` (Squared (including weights) Euclidean
  distance)
  - Apply weights to data frame, then calculate Euclidean distance, then
    square the results
- `sew_euclidean_distance` (Squared (excluding weights) Euclidean
  distance)
  - Apply *square root* of weights to data frame, then calculate
    Euclidean distance, then square the results
- `hamming_distance` (Hamming distance)
  - Distance between patients is (1 \* feature weight) summed over all
    features

## References

Wang, Bo, Aziz M. Mezlini, Feyyaz Demir, Marc Fiume, Zhuowen Tu, Michael
Brudno, Benjamin Haibe-Kains, and Anna Goldenberg. 2014. “Similarity
Network Fusion for Aggregating Data Types on a Genomic Scale.” Nature
Methods 11 (3): 333–37. <https://doi.org/10.1038/nmeth.2810>.
