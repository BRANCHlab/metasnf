
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Meta clustering with Similarity Network Fusion (metasnf)

<!-- badges: start -->
<!-- badges: end -->

**NOTE:** Repo is currently undergoing major changes in the branch to
become ABCD-independent and generally more accessible.

To-do list (in order of priority):

-   Make a vignette/minimal example of walking through the main steps of
    the pipeline
-   Allow multi-feature dataframes to be used when generating an outcome
    list
-   Write tests for non-trivial functions
-   Add check to make sure no columns end in “\_p” prior to extending
    the OM
-   Share repo!
-   Create a github pages
-   Allow categorical comparisons in outcome list
-   More customization of design matrix
-   More snf digestion schemes
-   Zipf-distribution scaling as described in meta-clustering paper
    (this may not make sense for SNF?)
-   ~~Build design matrix off of data list~~
-   ~~Make outcome list optional~~
-   ~~Consider moving reduce_dl_to_common / arrange_dl to start of
    execute_dm~~
-   ~~Cleaner organization of functions across .R files~~
-   ~~Account for different identifier than ABCD’s “subjectkey”~~

## Overview

*metasnf* is a package that facilitates usage of the meta clustering
paradigm described in ([Caruana et al. 2006](#ref-caruanaMeta2006)) with
the similarity network fusion (SNF) data integration procedure developed
in ([Wang et al. 2014](#ref-wangSimilarity2014)).

This package enables repeated iterations of SNF with distinct clustering
hyperparameters and combinations of input variables by making use of a
few key data structures.

Because I didn’t know what object-oriented programming was and nobody
talks about [object-oriented programming in
R](https://adv-r.hadley.nz/oo.html), these structures are all just lots
of nested lists : ).

The major structures are:

### The `design_matrix`

The `design_matrix` is a dataframe that includes all tunable clustering
settings as columns. Each row represents a distinct set of clustering
settings that will be used to produce a single clustering solution by
SNF and spectral clustering.

### The `data_list`

The `data_list` is a nested list that contains all preprocessed input
data that is ready to be used for clustering. Each list within a
`data_list` contains (1) a dataframe that will be used for clustering,
(2) the name of that dataframe, (3) the **domain** of that dataframe,
(4) the **subdomain** of that dataframe, and (5) the type of data
(numerical, categorical, or mixed) of the features within that
dataframe.

Domains are user-specified labels that indicate which dataframes
describe similar sources of data. For example, structural MRI data and
diffusion MRI data may be grouped within a neuroimaging domain.

Subdomains are user-specified labels for further granularity. For
example, a cortical thickness dataframe and a subcortical volume
dataframe may be grouped within a structural MRI subdomain.

### The `outcome_list`

An optional data structure that closely resembles the `data_list`, but
contains data sources which are not meant to be included as direct
inputs for clustering. Data within the `outcome_list` is reserved for
out-of-model measures for comparing completed clustering solutions.

### The `output_matrix`

The `output_matrix` is a large dataframe that appends cluster solution
columns (one column per subject) to the `design_matrix`. It contains all
the clustering results generated by the different settings included in
the `design_matrix`.

An `output_matrix` can be convereted to an `extended_output_matrix`
which contains overall regression p-values for any measures stored in an
`outcome_list`. The `extended_output_matrix` is useful in pipelines
where separation across one or several variables stored in the
`outcome_list` serve as the primary metric for automating the selection
of a top clustering solution.

Alternatively, a top clustering solution can be selected from a regular
`output_matrix` using the traditional meta clustering approach of
clustering cluster solutions and manually examining a few solutions from
the qualitatively distinct “meta clusters” that emerge.

## Installation

You can install the development version of metasnf with:

``` r
# install.packages("devtools")
devtools::install_github("BRANCHlab/metasnf")
```

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
