# Feature Weighting

Download a copy of the vignette to follow along here:
[feature_weights.Rmd](https://raw.githubusercontent.com/BRANCHlab/metasnf/main/vignettes/feature_weights.Rmd)

## Generating and Using the Weights Matrix

The [distance
metrics](https://branchlab.github.io/metasnf/articles/distance_metrics.html)
used in `metasnf` are all capable of applying custom weights to included
features. The code below outlines how to generate and use a
weights_matrix (data frame containing feature weights) object.

``` r

library(metasnf)

# Make sure to throw in all the data you're interested in visualizing for this
# data_list, including out-of-model measures and confounding features.
dl <- data_list(
    list(income, "household_income", "demographics", "ordinal"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    list(fav_colour, "favourite_colour", "demographics", "categorical"),
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    list(depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)
#> ℹ 139 observations dropped due to incomplete data.

summary(dl)
#>               name        type       domain length width
#> 1 household_income     ordinal demographics    136     1
#> 2  pubertal_status  continuous demographics    136     1
#> 3 favourite_colour categorical demographics    136     1
#> 4          anxiety     ordinal    behaviour    136     1
#> 5        depressed     ordinal    behaviour    136     1

set.seed(42)
sc <- snf_config(
    dl,
    n_solutions = 20,
    min_k = 20,
    max_k = 50
)
#> ℹ No distance functions specified. Using defaults.
#> ℹ No clustering functions specified. Using defaults.

sc$"weights_matrix"
#> Weights defined for 20 cluster solutions.
#> $ household_income 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,… 
#> $ pubertal_status 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,… 
#> $ colour 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,… 
#> $ cbcl_anxiety_r 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,… 
#> $ cbcl_depress_r 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
```

By default, the weights are all 1. This is what `batch_snf` uses when no
weights_matrix is supplied.

If you have custom feature weights you’d like to be used you can
manually populate this data frame. There’s one column per feature (no
need to worry about column orders) and the number of rows should match
the number of rows in the SNF config.

If you are just looking to broaden the space of cluster solutions you
generate, you can use some of the built-in randomization options for the
weights:

``` r

# Random uniformly distributed values
sc <- snf_config(
    dl,
    n_solutions = 20,
    min_k = 20,
    max_k = 50,
    weights_fill = "uniform"
)
#> ℹ No distance functions specified. Using defaults.
#> ℹ No clustering functions specified. Using defaults.

sc$"weights_matrix"
#> Weights defined for 20 cluster solutions.
#> $ household_income 0.08161542, 0.40378037, 0.83551451, 0.59499701, 0.351… 
#> $ pubertal_status 0.39553669, 0.95934650, 0.11323819, 0.23559680, 0.510… 
#> $ colour 0.07772700, 0.76282288, 0.15224422, 0.93752397, 0.745… 
#> $ cbcl_anxiety_r 0.15473680, 0.20350748, 0.14351450, 0.61093017, 0.516… 
#> $ cbcl_depress_r 0.02428327, 0.60967042, 0.47194860, 0.10858025, 0.864…

# Random exponentially distributed values
sc <- snf_config(
    dl,
    n_solutions = 20,
    min_k = 20,
    max_k = 50,
    weights_fill = "exponential"
)
#> ℹ No distance functions specified. Using defaults.
#> ℹ No clustering functions specified. Using defaults.

sc$"weights_matrix"
#> Weights defined for 20 cluster solutions.
#> $ household_income 2.27094485, 0.16919903, 0.18456918, 0.33487595, 1.167… 
#> $ pubertal_status 0.1528647, 0.4143784, 0.7710409, 1.7987298, 1.4162029… 
#> $ colour 1.54348805, 0.82288484, 2.54124213, 0.60145639, 1.089… 
#> $ cbcl_anxiety_r 0.21682983, 3.68043243, 1.25420312, 1.15417982, 0.102… 
#> $ cbcl_depress_r 0.1307026, 1.8525605, 4.1633331, 1.2926046, 1.6980694…
```

Once you’re happy with your weights_matrix, you can pass it into
batch_snf:

``` r

batch_snf(dl = dl, sc = sc)
```

## The Nitty Gritty of How Weights are Used

The specific implementation of the weights during distance matrix
calculations is dependent on the distance metric used, which you can
learn more about in the [distance metrics
vignette](https://branchlab.github.io/metasnf/articles/distance_metrics.html).

The other aspect to understand if you want to know precisely how your
weights are being used is related to the SNF schemes. Depending on which
scheme is specified in the corresponding `settings_df` of the SNF
config, the feature columns that are involved at each distance matrix
calculation can differ substantially.

For example, in the domain scheme, all features of the same domain are
concatenated prior to distance matrix calculation. If you have any
domains with multiple types of features (e.g., continuous and
categorical), that will mean that the mixed distance metric (Gower’s
method by default) will be used, and weights will be applied but only on
a per-domain basis.

Here’s a more concrete example on how data set-up and SNF scheme can
influence the feature weighting process: consider generating a data list
where every single input data frame contains only 1 input feature. If
that data list is processed exclusively using the “individual” SNF
scheme in this set-up, *feature weights won’t matter*. This is because
the individual SNF scheme calculates individual distance metrics for
every input data frame separately before fusing them together with SNF.
Anytime a distance matrix is calculated, it’ll be for a single feature
only, and the purpose of feature weighting (changing the relative
contributions of input features during the distance matrix calculations)
will be lost.
