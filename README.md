
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Meta clustering with Similarity Network Fusion

<!-- badges: start -->
<!-- badges: end -->

## Installation

**This package is currently under active development.**

We recommend installing the latest development version of the package
(it has the best stuff!):

``` r
devtools::install_github("BRANCHlab/metasnf")
```

If you require a stable version of the package, please keep track of
which commit you want when installing.

``` r
devtools::install_github("BRANCHlab/metasnf@8badfd4d7acaaf3b3c7af62d52b5c9324b82cc6c")
```

## Overview

*metasnf* is a package that facilitates usage of the meta clustering
paradigm described in ([Caruana et al. 2006](#ref-caruanaMeta2006)) with
the similarity network fusion (SNF) data integration procedure developed
in ([Wang et al. 2014](#ref-wangSimilarity2014)).

This package enables repeated iterations of SNF with distinct clustering
hyperparameters and combinations of input variables by making use of a
few key data structures.

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

## Work in progress:

-   [ ] Change in package name
-   [ ] Capability of defining custom distance metrics (including
    feature weights)
-   [ ] Capability of defining custom SNF-level weights
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
