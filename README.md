
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metaSNF: Meta clustering with Similarity Network Fusion

<!-- badges: start -->
<!-- badges: end -->

## Brief Overview

metaSNF is an R package that facilitates usage of the meta clustering
paradigm described in ([Caruana et al. 2006](#ref-caruanaMeta2006)) with
the similarity network fusion (SNF) data integration procedure developed
in ([Wang et al. 2014](#ref-wangSimilarity2014)). The package offers a
comprehensive suite of tools to assist users in transforming multi-modal
tabular data into cluster solutions, decision making in the clustering
process, and visualization along the way with a strong emphasis on
context-specific utility and principled validation of results.

## Installation

You will need R version 4.1.0 or higher to install this package. metaSNF
can be installed from CRAN:

``` r
install.packages("metasnf")
```

Development versions can be installed from GitHub:

``` r
# Latest development version
devtools::install_github("BRANCHlab/metasnf")

# Install a specific tagged version
devtools::install_github("BRANCHlab/metasnf@v2.0.0")
```

## Quick Start

Minimal usage of the package looks like this:

``` r
# Load the package
library(metasnf)

# Setting up the data
dl <- data_list(
    list(abcd_cort_t, "cortical_thickness", "neuroimaging", "continuous"),
    list(abcd_cort_sa, "cortical_surface_area", "neuroimaging", "continuous"),
    list(abcd_subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(abcd_income, "household_income", "demographics", "continuous"),
    list(abcd_pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "patient"
)
#> ℹ 176 observations dropped due to incomplete data.

# Specifying 5 different sets of settings for SNF
set.seed(42)
sc <- snf_config(
    dl,
    n_solutions = 5,
    max_k = 40
)
#> ℹ No distance functions specified. Using defaults.
#> ℹ No clustering functions specified. Using defaults.

# This matrix has clustering solutions for each of the 5 SNF runs!
sol_df <- batch_snf(dl, sc)

sol_df
#> 5 cluster solutions of 101 observations:
#> solution nclust mc uid_NDAR_INV0567T2Y9 uid_NDAR_INV0IZ157F8 
#>        1      2  .                    1                    2 
#>        2      2  .                    2                    1 
#>        3     10  .                    1                    9 
#>        4      2  .                    2                    1 
#>        5      8  .                    1                    7 
#> 98 observations not shown.
#> Use `print(n = ...)` to change the number of rows printed.
#> Use `t()` to view compact cluster solution format.

t(sol_df)
#> 5 cluster solutions of 100 observations:
#>                  uid      s1    s2    s3    s4    s5 
#> uid_NDAR_INV0567T2Y9       1     2     1     2     1
#> uid_NDAR_INV0IZ157F8       2     1     9     1     7
#> uid_NDAR_INV0J4PYA5F       1     2     7     2     3
#> uid_NDAR_INV10OMKVLE       2     1     8     1     4
#> uid_NDAR_INV15FPCW4O       1     2     2     2     5
#> uid_NDAR_INV19NB4RJK       1     2     9     2     7
#> uid_NDAR_INV1HLGR738       1     2     9     2     7
#> uid_NDAR_INV1KR0EZFU       2     1     9     2     7
#> uid_NDAR_INV1L3Y9EOP       1     2     2     2     5
#> uid_NDAR_INV1TCP5GNM       1     2     8     2     4
#> 90 observations not shown.
```

Check out the tutorial vignettes below to learn about how the package
can be used:

- [Simple usage of the
  package](https://branchlab.github.io/metasnf/articles/a_simple_example.html)
- [Complex usage of the
  package](https://branchlab.github.io/metasnf/articles/a_complete_example.html)

And more tutorials can be found under the “articles” section of the
documentation home page:
<https://branchlab.github.io/metasnf/index.html>

## Background

**Why use meta clustering?**

Clustering algorithms seek solutions where members of the same cluster
are very similar to each other and members of distinct clusters are very
dissimilar to each other. In sufficiently noisy datasets where many
qualitatively distinct solutions with similar scores of clustering
quality exist, it is not necessarily the case that the top solution
selected by a clustering algorithm will also be the most useful one for
the user’s context.

To address this issue, the original meta clustering procedure [Caruana
et al., 2006](https://doi.org/10.1109/ICDM.2006.103) involved generating
a large number of reasonable clustering solutions, clustering those
solutions into qualitatively similar ones, and having the user examine
those “meta clusters” to find something that seems like it’ll be the
most useful.

**Why use SNF?**

In the clinical data setting, we often have access to patient data
across a wide range of domains, such as imaging, genetics, biomarkers,
demographics. When trying to extract subtypes out of all this
information, direct concatenation of the data followed by cluster
analysis can result in a substantial amount of lost (valuable) signal
contained in each individual domain. Empirically, SNF has been
demonstrated to effectively integrate highly diverse patient data for
the purposes of clinical subtyping.

## Documentation

### Example workflows

- [Simple](https://branchlab.github.io/metasnf/articles/a_simple_example.html)
- [Complex](https://branchlab.github.io/metasnf/articles/a_complete_example.html)

### Essential objects

- [SNF
  config](https://branchlab.github.io/metasnf/articles/snf_config.html)
- [Data
  list](https://branchlab.github.io/metasnf/articles/data_list.html)

### Further customization of generated solutions

- [SNF
  schemes](https://branchlab.github.io/metasnf/articles/snf_schemes.html)
- [Distance
  metrics](https://branchlab.github.io/metasnf/articles/distance_metrics.html)
- [Clustering
  algorithms](https://branchlab.github.io/metasnf/articles/clustering_algorithms.html)
- [Feature
  weighting](https://branchlab.github.io/metasnf/articles/feature_weights.html)

### Additional functionality

- [Stability measures and consensus
  clustering](https://branchlab.github.io/metasnf/articles/stability_measures.html)
- [Removing unwanted
  signal](https://branchlab.github.io/metasnf/articles/confounders.html)
- [Parallel
  processing](https://branchlab.github.io/metasnf/articles/parallel_processing.html)
- [Label
  propagation](https://branchlab.github.io/metasnf/articles/label_propagation.html)
- [Imputations](https://branchlab.github.io/metasnf/articles/imputations.html)
- [NMI
  scores](https://branchlab.github.io/metasnf/articles/nmi_scores.html)

### Plotting

- [Correlation
  plots](https://branchlab.github.io/metasnf/articles/correlation_plots.html)
- [Similarity matrix
  heatmaps](https://branchlab.github.io/metasnf/articles/similarity_matrix_heatmap.html)
- [Manhattan
  plots](https://branchlab.github.io/metasnf/articles/manhattan_plots.html)
- [Alluvial
  plots](https://branchlab.github.io/metasnf/articles/alluvial_plots.html)
- [Feature
  plots](https://branchlab.github.io/metasnf/articles/feature_plots.html)

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-caruanaMeta2006" class="csl-entry">

Caruana, Rich, Mohamed Elhawary, Nam Nguyen, and Casey Smith. 2006.
“Meta Clustering.” In *Sixth International Conference on Data Mining
(ICDM’06)*, 107–18. <https://doi.org/10.1109/ICDM.2006.103>.

</div>

<div id="ref-wangSimilarity2014" class="csl-entry">

Wang, Bo, Aziz M. Mezlini, Feyyaz Demir, Marc Fiume, Zhuowen Tu, Michael
Brudno, Benjamin Haibe-Kains, and Anna Goldenberg. 2014. “Similarity
Network Fusion for Aggregating Data Types on a Genomic Scale.” *Nature
Methods* 11 (3): 333–37. <https://doi.org/10.1038/nmeth.2810>.

</div>

</div>
