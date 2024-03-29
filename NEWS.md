## v0.2.1

* Class `Duplicates` used to be a base R reference class (defined by
`setRefClass()`), it is now an R6 class (defined by `R6::R6Class()`).
Precondition to use the functionality of roxygen2 for documenting R6 classes.
* Field with ngrams matrix of the `Duplicates` class has been dropped: The
matrix may be bulky and is used only temporarily. No need to store it beyond
duplicate detection workflow to save memory space.
* Fields `whatToCompare` and `similarityMatrix` dropped from Duplicates class to
improve memory efficiency.
* Method `Duplicates$detectDuplicates()` is `Duplicates$detect()` now.
* Method `Duplicates$makeAnnotation()` renamed as `Duplicates$annotate()`.
* New and required argument `corpus` when initializing `Duplicates` class.

## v0.2.0

* Method `Duplicates$detectDuplicates()` now has arguments `n` (passed into 
polmineR::ngrams) and `character_selection`. Values were hard-coded previously.
Default values are aligned with Kliche et al. 2014.
* Remaining use of `CQI$struc2cpos()` has been replaced by RcppCWB functionality
(`get_region_matrix()`) #5.
* Method `nchars()` is implemented for `subcorpus` and `subcorpus_bundle` objects 
now and will be available for `plpr_subcorpus` by inheritance #3.
* An outdated example für the `Duplicates` class using a `duplicates()` method
has been removed #7.
* `Duplicates$detectDuplicates()` works for `n` = 0 (same-day comparisons only)
#11.

## v0.1.9

* `partition_bundle` replaces `partitionBundle` in code and documentation (#9).
* `store()` method removed - storing mallet objects is intended usage, so code
is captured in an issue of biglda package.
* `papply()` is removed: `pbapply::pblapply()` is the consolidated state-of-the-art.
* Documentation etc fixed so that R CMD check yields OK.