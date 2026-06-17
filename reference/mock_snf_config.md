# Mock example of a `snf_config` metasnf object

Mock example of a `snf_config` metasnf object

## Usage

``` r
mock_snf_config
```

## Format

### `mock_snf_config`

An SNF config containing hyperparameters and functions defined for
generating 20 cluster solutions from a data list. The config has been
specified to: - limit the `k` hyperparameter to 40 - make use of
uniformly distributed random weights - randomly select between using
spectral clustering where the number of clusters can be 2, 5, decided by
the eigen-gap heuristic, or decided by the rotation cost heuristic - use
Gower distance for categorical and mixed data, Euclidean distance for
ordinal data, and randomly select from Euclidean distance or
standard/normalized Euclidean distance for continuous and discrete data
The config was built using the `mock_data_list` loaded into the
namespace after calling
[`library("metasnf")`](https://branchlab.github.io/metasnf/). Used as an
example of an `snf_config` metasnf object.

## Source

This data comes from the metasnf package.
