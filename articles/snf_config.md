# The SNF Config

Download a copy of the vignette to follow along here:
[snf_config.Rmd](https://raw.githubusercontent.com/BRANCHlab/metasnf/main/vignettes/snf_config.Rmd)

This vignette outlines how to construct and use the SNF config, an
object storing all the settings and hyperparameters required to convert
data in a `data_list` class object into a space of cluster solutions.

## Creating a default SNF config

The most minimal SNF config (`snf_config` class object) can be obtained
by providing a data list into the
[`snf_config()`](https://branchlab.github.io/metasnf/reference/snf_config.md)
function.

``` r

library(metasnf)

dl <- data_list(
    list(cort_t, "cortical_thickness", "neuroimaging", "continuous"),
    list(cort_sa, "cortical_surface_area", "neuroimaging", "continuous"),
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)
```

    ## ℹ 175 observations dropped due to incomplete data.

``` r

sc <- snf_config(dl, n_solutions = 5)
```

    ## ℹ No distance functions specified. Using defaults.

    ## ℹ No clustering functions specified. Using defaults.

``` r

sc
```

    ## Settings Data Frame:
    ##                            1    2    3    4    5
    ## SNF hyperparameters:
    ## alpha                    0.6  0.8  0.4  0.6  0.6
    ## k                         39   75   51   77   25  
    ## t                         20   20   20   20   20  
    ## SNF scheme:
    ##                            1    2    2    1    3  
    ## Clustering functions:
    ##                            2    2    2    2    2  
    ## Distance functions:
    ## CNT                        1    1    1    1    1  
    ## DSC                        1    1    1    1    1  
    ## ORD                        1    1    1    1    1  
    ## CAT                        1    1    1    1    1  
    ## MIX                        1    1    1    1    1  
    ## Component dropout:
    ## cortical_thickness         ✔    ✔    ✔    ✔    ✖  
    ## cortical_surface_area      ✔    ✔    ✖    ✔    ✔  
    ## subcortical_volume         ✔    ✔    ✔    ✔    ✔  
    ## household_income           ✔    ✔    ✔    ✖    ✔  
    ## pubertal_status            ✔    ✔    ✔    ✔    ✔  
    ## Distance Functions List:
    ## Continuous (1):
    ## [1] euclidean_distance
    ## Discrete (1):
    ## [1] euclidean_distance
    ## Ordinal (1):
    ## [1] euclidean_distance
    ## Categorical (1):
    ## [1] gower_distance
    ## Mixed (1):
    ## [1] gower_distance
    ## Clustering Functions List:
    ## [1] spectral_eigen
    ## [2] spectral_rot
    ## Weights Matrix:
    ## Weights defined for 5 cluster solutions.
    ## $ mrisdp_1 1, 1, 1, 1, 1 
    ## $ mrisdp_2 1, 1, 1, 1, 1 
    ## $ mrisdp_3 1, 1, 1, 1, 1 
    ## $ mrisdp_4 1, 1, 1, 1, 1 
    ## $ mrisdp_5 1, 1, 1, 1, 1 
    ## …and 329 more features.

Similarity network fusion-based clustering pipelines require the
following steps:

1.  Selecting a set of data frames to integrate
2.  Converting those data frames into distance matrices using a distance
    function
3.  Converting those distance matrices to similarity matrices using the
    `SNFtool` package’s
    [`affinityMatrix()`](https://rdrr.io/pkg/SNFtool/man/affinityMatrix.html)
    function
4.  Integrating the similarity matrices into one final similarity matrix
    using the `SNFtool` package’s
    [`SNF()`](https://rdrr.io/pkg/SNFtool/man/SNF.html) function
5.  Converting that final similarity matrix into a cluster solution
    using a clustering function

The SNF config is made up of four parts that all address various parts
of that pipeline:

1.  The *settings data frame* (class `settings_df`, extends class
    `data.frame`), which contains information about SNF-specific
    hyperparameters (step 4), which distance and clustering functions
    will be used (steps 2 and 5), and if any components of the data list
    (data frames) will be excluded on a particular run (step 1). Each
    row of the data frame corresponds to a complete set of settings that
    can yield a single cluster solution from the data list.
2.  The *distance functions list* (class `dist_fns_list`, extends class
    `list`), which stores the actual distance functions that are
    referenced in the settings data frame (step 2)
3.  The *clustering functions list* (class `clust_fns_list`, extends
    class `list`), which similarly stores clustering functions (step 5)
4.  The *weights matrix* (class `weights_matrix`, extends classes
    `matrix`, `array`), which contains feature weights to account for
    during the data to distance matrix conversion step (step 2).

### The settings data frame

You can view the settings data frame in closer detail as follows:

``` r

sc$"settings_df"
```

    ##                            1    2    3    4    5
    ## SNF hyperparameters:
    ## alpha                    0.6  0.8  0.4  0.6  0.6
    ## k                         39   75   51   77   25  
    ## t                         20   20   20   20   20  
    ## SNF scheme:
    ##                            1    2    2    1    3  
    ## Clustering functions:
    ##                            2    2    2    2    2  
    ## Distance functions:
    ## CNT                        1    1    1    1    1  
    ## DSC                        1    1    1    1    1  
    ## ORD                        1    1    1    1    1  
    ## CAT                        1    1    1    1    1  
    ## MIX                        1    1    1    1    1  
    ## Component dropout:
    ## cortical_thickness         ✔    ✔    ✔    ✔    ✖  
    ## cortical_surface_area      ✔    ✔    ✖    ✔    ✔  
    ## subcortical_volume         ✔    ✔    ✔    ✔    ✔  
    ## household_income           ✔    ✔    ✔    ✖    ✔  
    ## pubertal_status            ✔    ✔    ✔    ✔    ✔

``` r

# Printed as a regular data frame
sc$"settings_df" |> as.data.frame()
```

    ##   solution alpha  k  t snf_scheme clust_alg cnt_dist dsc_dist ord_dist cat_dist
    ## 1        1   0.6 39 20          1         2        1        1        1        1
    ## 2        2   0.8 75 20          2         2        1        1        1        1
    ## 3        3   0.4 51 20          2         2        1        1        1        1
    ## 4        4   0.6 77 20          1         2        1        1        1        1
    ## 5        5   0.6 25 20          3         2        1        1        1        1
    ##   mix_dist inc_cortical_thickness inc_cortical_surface_area
    ## 1        1                      1                         1
    ## 2        1                      1                         1
    ## 3        1                      1                         0
    ## 4        1                      1                         1
    ## 5        1                      0                         1
    ##   inc_subcortical_volume inc_household_income inc_pubertal_status
    ## 1                      1                    1                   1
    ## 2                      1                    1                   1
    ## 3                      1                    1                   1
    ## 4                      1                    0                   1
    ## 5                      1                    1                   1

The columns in a `settings_df` class object include:

- `solution`: A label to keep track of each generated cluster solution.
- `alpha`: The alpha (also referred to as sigma or eta in the original
  SNF paper) hyperparameter in SNF. This hyperparameter plays a role in
  converting distance matrices into similarity matrices. The process by
  which
  [`SNFtool::affinityMatrix()`](https://rdrr.io/pkg/SNFtool/man/affinityMatrix.html)
  does this conversion essentially involves plugging the distance value
  as the x-coordinate of a normal distribution and pulling out the
  density at that point as the similarity. The thickness of the normal
  distribution is regulated by alpha, where a larger alpha leads to a
  broader normal distribution and a greater sensitivity to
  discriminating distances.
- `k`: The k (nearest neighbours) hyperparameter in the distance matrix
  to similarity matrix conversion as well as in similarity network
  fusion. In the distance matrix to similarity matrix conversion
  ([`SNFtool::affinityMatrix()`](https://rdrr.io/pkg/SNFtool/man/affinityMatrix.html)),
  k controls how many nearest neighbours to consider when calculating
  how similar each observation is to its nearest neighbours. The closer
  an observation is to its k nearest neighbours, the broader the normal
  distribution that is used for the distance to similarity conversion.
  For the similarity network fusion step
  ([`SNFtool::SNF()`](https://rdrr.io/pkg/SNFtool/man/SNF.html)), k
  controls how intensely all the matrices should be sparsified before
  information is passed between them. With a very small k, say, k = 1,
  all the values in all the matrices will be reduced to 0 with the
  exception of one value between each observation and that observation’s
  most similar neighbour.
- `t`: The T (number of iterations) hyperparameter used in SNF. A larger
  `t` results in more rounds of information passing between similarity
  matrices. SNF eventually converges, so overshooting this value offers
  no benefit but undershooting can yield inaccurate results. The
  original SNF developers recommend leaving this value at 20.
- `snf_scheme`: Which SNF “scheme” is being used to convert the initial
  provided data frames into a final fused network (more on this in the
  [SNF schemes
  vignette](https://branchlab.github.io/metasnf/articles/snf_schemes.html)).
- `clust_alg`: Which clustering algorithm function from the *clustering
  functions list* of the config will be applied to the final fused
  network. You can learn more about using this parameter in the
  [clustering algorithnms
  vignette](https://branchlab.github.io/metasnf/articles/clustering_algorithms.html).
- Columns ending in `dist`: Which distance metric function from the
  *distance functions list* of the config will be used for each of the
  various types of features in the data list (more on this in the
  [distance metrics
  vignette](https://branchlab.github.io/metasnf/articles/distance_metrics.html)).
- Columns starting with `inc`: Whether or not the corresponding data
  frame will be included (1) or excluded (0) from this row.

By default, the `alpha` and `k` hyperparameters are randomly varied from
0.3 to 0.8 and 10 to 100 respectively based on suggestions from the
original SNF paper. The `t` hyperparameter by default stays fixed at 20.
The `snf_scheme` column varies randomly from 1 to 3, corresponding to
each of the three differente schemes that are available. The `clust_alg`
randomly varies between 1 and 2 for the two default clustering algoritm
functions: (1) spectral clustering using the eigen-gap heuristic to
calculate the number of clusters and (2) spectral clustering using the
rotation cost heuristic. The distance columns will always be 1 by
default, as there is only one default distance metric function per
variable type: simple Euclidean for anything numeric and Gower’s
distance for anything mixed or categorical.

### The distance functions list

The distance functions list is simply a list of functions capable of
converting a data frame into a distance matrix. Distance functions
within the list are organized based on what type of variable they deal
with: continuous, discrete, ordinal, categorical, or mixed (any
combination of the former 4).

``` r

dfl <- sc$"dist_fns_list"

dfl
```

    ## Continuous (1):
    ## [1] euclidean_distance
    ## Discrete (1):
    ## [1] euclidean_distance
    ## Ordinal (1):
    ## [1] euclidean_distance
    ## Categorical (1):
    ## [1] gower_distance
    ## Mixed (1):
    ## [1] gower_distance

``` r

names(dfl)
```

    ## [1] "cnt_dist_fns" "dsc_dist_fns" "ord_dist_fns" "cat_dist_fns" "mix_dist_fns"

``` r

dfl$"cnt_dist_fns"[[1]]
```

    ## function (df, weights_row) 
    ## {
    ##     weights <- diag(weights_row, nrow = length(weights_row))
    ##     weighted_df <- as.matrix(df) %*% weights
    ##     distance_matrix <- as.matrix(stats::dist(weighted_df, method = "euclidean"))
    ##     return(distance_matrix)
    ## }
    ## <bytecode: 0x558fdd7c5e50>
    ## <environment: namespace:metasnf>

You can learn more about customizing distance metrics in the [distance
metrics
vignette](https://branchlab.github.io/metasnf/articles/distance_metrics.html).

### The clustering functions list

The clustering functions list is similarly a list of functions capable
of converting a similarity matrix into a cluster solution (numeric
vector).

``` r

cfl <- sc$"clust_fns_list"

cfl
```

    ## [1] spectral_eigen
    ## [2] spectral_rot

``` r

names(cfl)
```

    ## [1] "spectral_eigen" "spectral_rot"

``` r

cfl[[1]]
```

    ## function (similarity_matrix) 
    ## {
    ##     estimated_n <- estimate_nclust_given_graph(W = similarity_matrix, 
    ##         NUMC = 2:10)
    ##     nclust_estimate <- estimated_n$`Eigen-gap best`
    ##     solution <- SNFtool::spectralClustering(similarity_matrix, 
    ##         nclust_estimate)
    ##     return(solution)
    ## }
    ## <bytecode: 0x558fdddf5fc0>
    ## <environment: namespace:metasnf>

You can learn more about customizing clustering functions in the
[clustering algorithnms
vignette](https://branchlab.github.io/metasnf/articles/clustering_algorithms.html).

### The weights matrix

``` r

wm <- sc$"weights_matrix"

wm
```

    ## Weights defined for 5 cluster solutions.
    ## $ mrisdp_1 1, 1, 1, 1, 1 
    ## $ mrisdp_2 1, 1, 1, 1, 1 
    ## $ mrisdp_3 1, 1, 1, 1, 1 
    ## $ mrisdp_4 1, 1, 1, 1, 1 
    ## $ mrisdp_5 1, 1, 1, 1, 1 
    ## …and 329 more features.

``` r

class(wm) <- "matrix"

wm[1:5, 1:5]
```

    ##      mrisdp_1 mrisdp_2 mrisdp_3 mrisdp_4 mrisdp_5
    ## [1,]        1        1        1        1        1
    ## [2,]        1        1        1        1        1
    ## [3,]        1        1        1        1        1
    ## [4,]        1        1        1        1        1
    ## [5,]        1        1        1        1        1

There’s one row in the weights matrix corresponding to every row in the
settings data frame and one column for every feature in the data list.
By default, all the weights are set to 1, so no weighting occurs.

## Customizing an SNF config

When not specifying any parameters beyond the number of rows that are
created, the function will randomly vary most configurable values in the
config within sensible default ranges.

``` r

sc <- snf_config(
    dl,
    n_solutions = 100
)
```

    ## ℹ No distance functions specified. Using defaults.

    ## ℹ No clustering functions specified. Using defaults.

``` r

sc
```

    ## Settings Data Frame:
    ##                            1    2    3    4    5    6    7    8    9   10
    ## SNF hyperparameters:
    ## alpha                    0.6  0.8  0.3  0.7  0.7  0.8  0.6  0.7  0.8  0.3
    ## k                         27   84   50   57   16   80   65   32   95   56  
    ## t                         20   20   20   20   20   20   20   20   20   20  
    ## SNF scheme:
    ##                            2    2    3    3    1    3    2    1    3    2  
    ## Clustering functions:
    ##                            2    1    1    1    1    1    1    1    1    2  
    ## Distance functions:
    ## CNT                        1    1    1    1    1    1    1    1    1    1  
    ## DSC                        1    1    1    1    1    1    1    1    1    1  
    ## ORD                        1    1    1    1    1    1    1    1    1    1  
    ## CAT                        1    1    1    1    1    1    1    1    1    1  
    ## MIX                        1    1    1    1    1    1    1    1    1    1  
    ## Component dropout:
    ## cortical_thickness         ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
    ## cortical_surface_area      ✔    ✔    ✔    ✔    ✔    ✖    ✔    ✔    ✔    ✔  
    ## subcortical_volume         ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
    ## household_income           ✔    ✔    ✔    ✖    ✔    ✔    ✔    ✖    ✔    ✔  
    ## pubertal_status            ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
    ## …and settings defined to create 90 more cluster solutions.
    ## Distance Functions List:
    ## Continuous (1):
    ## [1] euclidean_distance
    ## Discrete (1):
    ## [1] euclidean_distance
    ## Ordinal (1):
    ## [1] euclidean_distance
    ## Categorical (1):
    ## [1] gower_distance
    ## Mixed (1):
    ## [1] gower_distance
    ## Clustering Functions List:
    ## [1] spectral_eigen
    ## [2] spectral_rot
    ## Weights Matrix:
    ## Weights defined for 100 cluster solutions.
    ## $ mrisdp_1 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## $ mrisdp_2 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## $ mrisdp_3 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## $ mrisdp_4 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## $ mrisdp_5 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## …and 329 more features.

### Alpha, k, and t

You can control any of these parameters either by providing a vector of
values you’d like to randomly sample from or by specifying a minimum and
maximum range.

``` r

# Through minimums and maximums
sc <- snf_config(
    dl,
    n_solutions = 100,
    min_k = 10,
    max_k = 60,
    min_alpha = 0.3,
    max_alpha = 0.8
)
```

    ## ℹ No distance functions specified. Using defaults.

    ## ℹ No clustering functions specified. Using defaults.

``` r

# Through specific value sampling
sc <- snf_config(
    dl,
    n_solutions = 20,
    k_values = c(10, 25, 50),
    alpha_values = c(0.4, 0.8)
)
```

    ## ℹ No distance functions specified. Using defaults.
    ## ℹ No clustering functions specified. Using defaults.

### Inclusion columns and data frame dropout

Bounds on the number of input data frames removed as well as the way in
which the number removed is chosen can be controlled.

By default, the `settings_df` generated during the call to
[`snf_config()`](https://branchlab.github.io/metasnf/reference/snf_config.md)
will pick a random value between 0 (printed as a red X) and 1 (printed
as a green checkmark) less than the total number of available data
frames in the data list based on an exponential probability
distribution. The exponential distribution makes it so that it is very
likely that a small number of data frames will be dropped and much less
likely that a large number of data frames will be dropped.

You can control the distribution by changing the `dropout_dist` value to
“uniform” (which will result in a much higher number of data frames
being dropped on average) or “none” (which will result in no data frames
being dropped).

``` r

# Exponential dropping
sc <- snf_config(
    dl,
    n_solutions = 20,
    dropout_dist = "exponential" # the default behaviour
)
```

    ## ℹ No distance functions specified. Using defaults.

    ## ℹ No clustering functions specified. Using defaults.

``` r

sc
```

    ## Settings Data Frame:
    ##                            1    2    3    4    5    6    7    8    9   10
    ## SNF hyperparameters:
    ## alpha                    0.4  0.7  0.5  0.8  0.5  0.4  0.8  0.3  0.4  0.5
    ## k                         46   64   27   33   74   32   85   63   10   17  
    ## t                         20   20   20   20   20   20   20   20   20   20  
    ## SNF scheme:
    ##                            2    2    1    2    1    1    3    3    1    3  
    ## Clustering functions:
    ##                            2    2    1    1    1    2    2    2    1    1  
    ## Distance functions:
    ## CNT                        1    1    1    1    1    1    1    1    1    1  
    ## DSC                        1    1    1    1    1    1    1    1    1    1  
    ## ORD                        1    1    1    1    1    1    1    1    1    1  
    ## CAT                        1    1    1    1    1    1    1    1    1    1  
    ## MIX                        1    1    1    1    1    1    1    1    1    1  
    ## Component dropout:
    ## cortical_thickness         ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
    ## cortical_surface_area      ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
    ## subcortical_volume         ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
    ## household_income           ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
    ## pubertal_status            ✔    ✔    ✔    ✖    ✔    ✔    ✔    ✔    ✔    ✔  
    ## …and settings defined to create 10 more cluster solutions.
    ## Distance Functions List:
    ## Continuous (1):
    ## [1] euclidean_distance
    ## Discrete (1):
    ## [1] euclidean_distance
    ## Ordinal (1):
    ## [1] euclidean_distance
    ## Categorical (1):
    ## [1] gower_distance
    ## Mixed (1):
    ## [1] gower_distance
    ## Clustering Functions List:
    ## [1] spectral_eigen
    ## [2] spectral_rot
    ## Weights Matrix:
    ## Weights defined for 20 cluster solutions.
    ## $ mrisdp_1 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## $ mrisdp_2 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## $ mrisdp_3 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## $ mrisdp_4 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## $ mrisdp_5 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## …and 329 more features.

``` r

# Uniform dropping
sc <- snf_config(
    dl,
    n_solutions = 20,
    dropout_dist = "uniform"
)
```

    ## ℹ No distance functions specified. Using defaults.
    ## ℹ No clustering functions specified. Using defaults.

``` r

sc
```

    ## Settings Data Frame:
    ##                            1    2    3    4    5    6    7    8    9   10
    ## SNF hyperparameters:
    ## alpha                    0.4  0.5  0.4  0.8  0.8  0.3  0.5  0.4  0.6  0.3
    ## k                         23   68   14   50   91   61   95   61   86   26  
    ## t                         20   20   20   20   20   20   20   20   20   20  
    ## SNF scheme:
    ##                            2    3    1    3    2    2    3    2    1    2  
    ## Clustering functions:
    ##                            2    1    1    2    2    1    1    2    1    1  
    ## Distance functions:
    ## CNT                        1    1    1    1    1    1    1    1    1    1  
    ## DSC                        1    1    1    1    1    1    1    1    1    1  
    ## ORD                        1    1    1    1    1    1    1    1    1    1  
    ## CAT                        1    1    1    1    1    1    1    1    1    1  
    ## MIX                        1    1    1    1    1    1    1    1    1    1  
    ## Component dropout:
    ## cortical_thickness         ✔    ✖    ✔    ✖    ✔    ✔    ✖    ✖    ✔    ✔  
    ## cortical_surface_area      ✔    ✔    ✔    ✖    ✖    ✔    ✖    ✔    ✔    ✔  
    ## subcortical_volume         ✔    ✖    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
    ## household_income           ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
    ## pubertal_status            ✔    ✖    ✔    ✔    ✔    ✔    ✔    ✖    ✔    ✔  
    ## …and settings defined to create 10 more cluster solutions.
    ## Distance Functions List:
    ## Continuous (1):
    ## [1] euclidean_distance
    ## Discrete (1):
    ## [1] euclidean_distance
    ## Ordinal (1):
    ## [1] euclidean_distance
    ## Categorical (1):
    ## [1] gower_distance
    ## Mixed (1):
    ## [1] gower_distance
    ## Clustering Functions List:
    ## [1] spectral_eigen
    ## [2] spectral_rot
    ## Weights Matrix:
    ## Weights defined for 20 cluster solutions.
    ## $ mrisdp_1 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## $ mrisdp_2 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## $ mrisdp_3 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## $ mrisdp_4 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## $ mrisdp_5 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## …and 329 more features.

``` r

# No dropping
sc <- snf_config(
    dl,
    n_solutions = 20,
    dropout_dist = "none"
)
```

    ## ℹ No distance functions specified. Using defaults.
    ## ℹ No clustering functions specified. Using defaults.

``` r

sc
```

    ## Settings Data Frame:
    ##                            1    2    3    4    5    6    7    8    9   10
    ## SNF hyperparameters:
    ## alpha                    0.8  0.4  0.8  0.4  0.7  0.6  0.8  0.4  0.3  0.7
    ## k                         80   53   27   67   63   11   53   14   33   27  
    ## t                         20   20   20   20   20   20   20   20   20   20  
    ## SNF scheme:
    ##                            1    2    1    1    1    3    2    2    1    2  
    ## Clustering functions:
    ##                            1    2    2    1    2    1    2    1    2    2  
    ## Distance functions:
    ## CNT                        1    1    1    1    1    1    1    1    1    1  
    ## DSC                        1    1    1    1    1    1    1    1    1    1  
    ## ORD                        1    1    1    1    1    1    1    1    1    1  
    ## CAT                        1    1    1    1    1    1    1    1    1    1  
    ## MIX                        1    1    1    1    1    1    1    1    1    1  
    ## Component dropout:
    ## cortical_thickness         ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
    ## cortical_surface_area      ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
    ## subcortical_volume         ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
    ## household_income           ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
    ## pubertal_status            ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
    ## …and settings defined to create 10 more cluster solutions.
    ## Distance Functions List:
    ## Continuous (1):
    ## [1] euclidean_distance
    ## Discrete (1):
    ## [1] euclidean_distance
    ## Ordinal (1):
    ## [1] euclidean_distance
    ## Categorical (1):
    ## [1] gower_distance
    ## Mixed (1):
    ## [1] gower_distance
    ## Clustering Functions List:
    ## [1] spectral_eigen
    ## [2] spectral_rot
    ## Weights Matrix:
    ## Weights defined for 20 cluster solutions.
    ## $ mrisdp_1 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## $ mrisdp_2 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## $ mrisdp_3 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## $ mrisdp_4 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## $ mrisdp_5 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## …and 329 more features.

The bounds on the number of data frames that can be dropped can be
controlled using the `min_removed_inputs` and `max_removed_inputs`:

``` r

sc <- snf_config(
    dl,
    n_solutions = 20,
    min_removed_inputs = 3
)
```

    ## ℹ No distance functions specified. Using defaults.

    ## ℹ No clustering functions specified. Using defaults.

``` r

# No row will exclude fewer than 3 data frames during SNF
sc
```

    ## Settings Data Frame:
    ##                            1    2    3    4    5    6    7    8    9   10
    ## SNF hyperparameters:
    ## alpha                    0.5  0.7  0.8  0.3  0.6  0.4  0.4  0.7  0.3  0.3
    ## k                         82   44   24   56   96   10   94   27   34   61  
    ## t                         20   20   20   20   20   20   20   20   20   20  
    ## SNF scheme:
    ##                            1    1    1    2    2    3    2    1    2    2  
    ## Clustering functions:
    ##                            1    2    2    1    1    1    2    1    1    1  
    ## Distance functions:
    ## CNT                        1    1    1    1    1    1    1    1    1    1  
    ## DSC                        1    1    1    1    1    1    1    1    1    1  
    ## ORD                        1    1    1    1    1    1    1    1    1    1  
    ## CAT                        1    1    1    1    1    1    1    1    1    1  
    ## MIX                        1    1    1    1    1    1    1    1    1    1  
    ## Component dropout:
    ## cortical_thickness         ✖    ✔    ✖    ✖    ✖    ✖    ✖    ✔    ✔    ✖  
    ## cortical_surface_area      ✖    ✖    ✔    ✖    ✖    ✔    ✖    ✔    ✖    ✔  
    ## subcortical_volume         ✔    ✖    ✖    ✔    ✔    ✖    ✔    ✖    ✔    ✖  
    ## household_income           ✔    ✖    ✖    ✔    ✔    ✖    ✔    ✖    ✖    ✖  
    ## pubertal_status            ✖    ✔    ✔    ✖    ✖    ✔    ✖    ✖    ✖    ✔  
    ## …and settings defined to create 10 more cluster solutions.
    ## Distance Functions List:
    ## Continuous (1):
    ## [1] euclidean_distance
    ## Discrete (1):
    ## [1] euclidean_distance
    ## Ordinal (1):
    ## [1] euclidean_distance
    ## Categorical (1):
    ## [1] gower_distance
    ## Mixed (1):
    ## [1] gower_distance
    ## Clustering Functions List:
    ## [1] spectral_eigen
    ## [2] spectral_rot
    ## Weights Matrix:
    ## Weights defined for 20 cluster solutions.
    ## $ mrisdp_1 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## $ mrisdp_2 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## $ mrisdp_3 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## $ mrisdp_4 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## $ mrisdp_5 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1… 
    ## …and 329 more features.

## Grid searching

If you are interested in grid searching over perhaps just a specific set
of alpha and k values, you may want to consider varying those parameters
and keeping everything else fixed:

``` r

sc <- snf_config(
    dl,
    n_solutions = 10,
    alpha_values = c(0.3, 0.5, 0.8),
    k_values = c(20, 40, 60),
    dropout_dist = "none"
)
```

    ## ℹ No distance functions specified. Using defaults.

    ## ℹ No clustering functions specified. Using defaults.

``` r

sc
```

    ## Settings Data Frame:
    ##                            1    2    3    4    5    6    7    8    9   10
    ## SNF hyperparameters:
    ## alpha                    0.8  0.5  0.3  0.8  0.3  0.3  0.3  0.3  0.8  0.3
    ## k                         40   20   40   40   60   40   20   20   20   40  
    ## t                         20   20   20   20   20   20   20   20   20   20  
    ## SNF scheme:
    ##                            3    3    3    3    2    2    3    2    2    2  
    ## Clustering functions:
    ##                            2    2    2    1    1    1    1    1    2    2  
    ## Distance functions:
    ## CNT                        1    1    1    1    1    1    1    1    1    1  
    ## DSC                        1    1    1    1    1    1    1    1    1    1  
    ## ORD                        1    1    1    1    1    1    1    1    1    1  
    ## CAT                        1    1    1    1    1    1    1    1    1    1  
    ## MIX                        1    1    1    1    1    1    1    1    1    1  
    ## Component dropout:
    ## cortical_thickness         ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
    ## cortical_surface_area      ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
    ## subcortical_volume         ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
    ## household_income           ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
    ## pubertal_status            ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔    ✔  
    ## Distance Functions List:
    ## Continuous (1):
    ## [1] euclidean_distance
    ## Discrete (1):
    ## [1] euclidean_distance
    ## Ordinal (1):
    ## [1] euclidean_distance
    ## Categorical (1):
    ## [1] gower_distance
    ## Mixed (1):
    ## [1] gower_distance
    ## Clustering Functions List:
    ## [1] spectral_eigen
    ## [2] spectral_rot
    ## Weights Matrix:
    ## Weights defined for 10 cluster solutions.
    ## $ mrisdp_1 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 
    ## $ mrisdp_2 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 
    ## $ mrisdp_3 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 
    ## $ mrisdp_4 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 
    ## $ mrisdp_5 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 
    ## …and 329 more features.

## Assembling an SNF config in pieces

Rather than varying everything equally all at once, you may be
interested in looking at “chunks” of solution spaces that are based on
distinct SNF configs. For example, you may want to look at 25 solutions
generated with k = 50 and look at another 25 solutions generated with k
= 80. You can build two separate SNF configs and join them using the
[`merge()`](https://rdrr.io/r/base/merge.html) function.

``` r

set.seed(42)
sc_1 <- snf_config(
    dl,
    n_solutions = 25,
    k_values = 50
)
```

    ## ℹ No distance functions specified. Using defaults.

    ## ℹ No clustering functions specified. Using defaults.

``` r

sc_2 <- snf_config(
    dl,
    n_solutions = 25,
    k_values = 80
)
```

    ## ℹ No distance functions specified. Using defaults.
    ## ℹ No clustering functions specified. Using defaults.

``` r

full_sc <- merge(sc_1, sc_2)
```

## “`settings_df` building failed to converge”

[`snf_config()`](https://branchlab.github.io/metasnf/reference/snf_config.md)
will never build duplicate rows. A consequence of this is that if you
request a very large number of rows over a very small range of possible
values to vary over, it will be impossible for the matrix to be built.
For example, there’s no way to generate 10 unique rows when the only
varying parameter is which of two clustering algorithms is used - only 2
rows could ever be created. If you encounter the error “Matrix building
failed”, try to generate fewer rows or to be a little less strict with
what values are allowed.
