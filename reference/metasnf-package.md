# metasnf: Meta Clustering with Similarity Network Fusion

Framework to facilitate patient subtyping with similarity network fusion
and meta clustering. The similarity network fusion (SNF) algorithm was
introduced by Wang et al. (2014) in
[doi:10.1038/nmeth.2810](https://doi.org/10.1038/nmeth.2810) . SNF is a
data integration approach that can transform high-dimensional and
diverse data types into a single similarity network suitable for
clustering with minimal loss of information from each initial data
source. The meta clustering approach was introduced by Caruana et al.
(2006) in
[doi:10.1109/ICDM.2006.103](https://doi.org/10.1109/ICDM.2006.103) .
Meta clustering involves generating a wide range of cluster solutions by
adjusting clustering hyperparameters, then clustering the solutions
themselves into a manageable number of qualitatively similar solutions,
and finally characterizing representative solutions to find ones that
are best for the user's specific context. This package provides a
framework to easily transform multi-modal data into a wide range of
similarity network fusion-derived cluster solutions as well as to
visualize, characterize, and validate those solutions. Core package
functionality includes easy customization of distance metrics,
clustering algorithms, and SNF hyperparameters to generate diverse
clustering solutions; calculation and plotting of associations between
features, between patients, and between cluster solutions; and standard
cluster validation approaches including resampled measures of cluster
stability, standard metrics of cluster quality, and label propagation to
evaluate generalizability in unseen data. Associated vignettes guide the
user through using the package to identify patient subtypes while
adhering to best practices for unsupervised learning.

## See also

Useful links:

- <https://branchlab.github.io/metasnf/>

- <https://github.com/BRANCHlab/metasnf/>

- Report bugs at <https://github.com/BRANCHlab/metasnf/issues>

## Author

**Maintainer**: Prashanth S Velayudhan <psvelayu@gmail.com>

Authors:

- Xiaoqiao Xu

- Prajkta Kallurkar

- Ana Patricia Balbon

- Maria T Secara

- Adam Taback

- Denise Sabac

- Nicholas Chan

- Shihao Ma

- Bo Wang

- Daniel Felsky

- Stephanie H Ameis

- Brian Cox

- Colin Hawco

- Lauren Erdman

- Anne L Wheeler <anne.wheeler@sickkids.ca> \[thesis advisor\]
