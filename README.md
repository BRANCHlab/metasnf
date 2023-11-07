
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Meta clustering with Similarity Network Fusion

<!-- badges: start -->
<!-- badges: end -->

## Requirements

Necessary:

-   R version 4.1.0 or higher
-   `devtools` package (`install.packages("devtools")`) for installation

## Installation

**This package is currently under active development.**

You will need R version 4.1.0 or higher to install this package. We
recommend installing the latest development version of the package (it
has the best stuff!):

``` r
devtools::install_github("BRANCHlab/metasnf")
```

If you require a stable version of the package, please keep track of
which commit you want when installing.

``` r
devtools::install_github("BRANCHlab/metasnf@8badfd4d7acaaf3b3c7af62d52b5c9324b82cc6c")
```

## Brief Overview

*metasnf* is a package that facilitates usage of the meta clustering
paradigm described in ([Caruana et al. 2006](#ref-caruanaMeta2006)) with
the similarity network fusion (SNF) data integration procedure developed
in ([Wang et al. 2014](#ref-wangSimilarity2014)). The package offers a
comprehensive suite of tools to assist users in transforming raw patient
data into patient subtypes, decision making in the subtyping process,
and visualization along the way with a strong emphasis on
context-specific utility and principled validation of results.

## Quick Start

An extremely minimal usage of the package looks something like this:

``` r
# Load the package
library(metasnf)

# Setting up the data
data_list <- generate_data_list(
    list(abcd_cort_t, "cortical_thickness", "neuroimaging", "continuous"),
    list(abcd_cort_sa, "cortical_surface_area", "neuroimaging", "continuous"),
    list(abcd_subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(abcd_income, "household_income", "demographics", "continuous"),
    list(abcd_pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "patient"
)

# Specifying 5 different sets of settings for SNF
settings_matrix <- generate_settings_matrix(
    data_list,
    nrow = 5,
    max_k = 40,
    seed = 42
)
#> [1] "The global seed has been changed!"

# This matrix has clustering solutions for each of the 5 SNF runs!
solutions_matrix <- batch_snf(data_list, settings_matrix)
#> [1] "Row: 1/5 | Time remaining: 1 seconds"
#> [1] "Row: 2/5 | Time remaining: 0 seconds"
#> [1] "Row: 3/5 | Time remaining: 0 seconds"
#> [1] "Row: 4/5 | Time remaining: 0 seconds"
#> [1] "Row: 5/5 | Time remaining: 0 seconds"
#> [1] "Total time taken: 1 seconds."
```

Check out the tutorial vignettes below to learn about how the package
can be used:

-   [Simple usage of the
    package](https://branchlab.github.io/metasnf/articles/a_simple_example.html)
-   [Complex usage of the
    package](https://branchlab.github.io/metasnf/articles/a_less_simple_example.html)

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

## Package Structure

The major structures are:

### The `settings_matrix`

The `settings_matrix` is a dataframe that includes all tunable
clustering settings as columns. Each row represents a distinct set of
clustering settings that will be used to produce a single clustering
solution by SNF and spectral clustering.

### The `data_list`

The `data_list` is a nested list that contains all preprocessed input
data that is ready to be used for clustering. Each list within a
`data_list` contains (1) a dataframe that will be used for clustering,
(2) the name of that dataframe, (3) the **domain** of that dataframe,
and (4) the type of data (continuous, ordinal, discrete, nominal
(categorical), or mixed) of the features within that dataframe.

Domains are user-specified labels that indicate which dataframes
describe similar sources of data. For example, structural MRI data and
diffusion MRI data may be grouped within a neuroimaging domain.

### The `target_list`

An optional data structure that closely resembles the `data_list`, but
contains data sources which are not meant to be included as direct
inputs for clustering. Data within the `target_list` is reserved for
out-of-model measures for comparing completed clustering solutions.

### The `solutions_matrix`

The `solutions_matrix` is a large dataframe that appends cluster
solution columns (one column per subject) to the `settings_matrix`. It
contains all the clustering results generated by the different settings
included in the `settings_matrix`.

An `solutions_matrix` can be convereted to an
`extended_solutions_matrix` which contains overall regression p-values
for any measures stored in an `target_list`. The
`extended_solutions_matrix` is useful in pipelines where separation
across one or several variables stored in the `target_list` serve as the
primary metric for automating the selection of a top clustering
solution.

Alternatively, a top clustering solution can be selected from a regular
`solutions_matrix` using the traditional meta clustering approach of
clustering cluster solutions and manually examining a few solutions from
the qualitatively distinct “meta clusters” that emerge.

## Recent updates:

-   [x] Changes in function names
-   [x] Capability of defining custom clustering algorithms
-   [x] Added flexibility during generation of the settings_matrix
-   [x] Capability of calculating solution stability across patient
    resamplings for all cluster solutions
-   [x] Capability of calculating a wide range of quality metrics for
    all cluster solutions
-   [x] Capability of writing affinity matrices to disk
-   [x] Capability of not running the clustering on every run of SNF
    (only makes sense when paired with the previous change)
-   [x] Capability of defining custom distance metrics (including
    feature weights)

## Work in progress:

-   [ ] New visualization functions and improvements to existing
    visualization functions
-   [ ] Updated vignette(s) to fully document basic and advance usage of
    the package

### Lower priority:

-   [ ] Adapt parallel processing variant of batch_snf to reflect the
    above changes
-   [ ] Add tests / make package CRAN ready

## Contributions

Contributions to the package are very welcome!

If you have collaborator access, please create a new branch based on
main OR a fork of the repo, make your changes, and submit a pull
request. If you do not have collaborator access, please make a fork of
the repo, make your changes, and submit a pull request.

You are also welcome to simply post suggested changes in the issues tab
of the repo.

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
