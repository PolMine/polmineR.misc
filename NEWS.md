## v0.1.9.9001 

* Method `Duplicates$detectDuplicates()` now has arguments `n` (passed into 
polmineR::ngrams) and `character_selection`. Values were hard-coded previously.
Default values are aligned with Kliche et al. 2014.

## v0.1.9

* `partition_bundle` replaces `partitionBundle` in code and documentation (#9).
* `store()` method removed - storing mallet objects is intended usage, so code
is captured in an issue of biglda package.
* `papply()` is removed: `pbapply::pblapply()` is the consolidated state-of-the-art.
* Documentation etc fixed so that R CMD check yields OK.