# mouser

A simple package to define some colour schemes and constants useful for mouse genetics, with a focus on the [Collaborative Cross](http://csbio.unc.edu/CCstatus/index.py) and [Diversity Outbred](http://churchill.jax.org/research/cc/doresources.shtml) populations.


## Features

* Chromosome lengths:
	* `chromsizes()`, to which the next two are shortcuts
	* `chromsizes_mm9()`, old assembly ("build 37"/UCSC mm9)
	* `chromsizes_mm10()`, current assembly ("build 38"/UCSC mm10)
* Position of the pseudoautosomal boundary (PAR; ie. the part of the X chromosome that might be heterozygous in males):
	* `pseudoautosomal_boundary()`
	* `PAR_mm9()`
	* `PAR_mm10()`
* Strain names and strain codes for the CC/DO founders:
	* `cc_strains()`, for full strain names ("A/J", "C57BL/6J", ...)
	* `CC_STRAINS()`, for strain codes (`A`,`B`, ...)
* Canonical colour schemes for the CC/DO founders, corresponding to the full strain names or codes:
	* `cc_colors()`
	* `CC_COLORS()`
* Canonical colour schemes for subspecies of *Mus musuclus* and colours for related speices in *Mus*:
	* `mus_taxa()`
	* `mus_colors()`
	
We also define a set of convience functions using these colour schemes with `ggplot2`:

* `scale_color_cc()`
* `scale_fill_cc()`
* `scale_color_mus()`
* `scale_fill_mus()`
* `scale_color_sex()`
* `scale_fill_sex()`

## Notes

For chromosome names, both the UCSC ("chr1", ..., "chrX", "chrM") and Ensembl ("1", ..., "X","MT") naming conventions are supported.  The desired convention is guessed from input arguments: `chromsizes("mm10")` will return UCSC names, while `chromsizes("GRCm38")` will return Ensembl names.  For users of the Bioconductor stack (especially the [`GenomicRanges`](http://bioconductor.org/packages/release/bioc/html/GenomicRanges.html) package), they can be returned as a `Seqinfo` object by passing `as.seqinfo = TRUE` to any of the `chromsizes()` family of functions.

> **A note about "build 37" coordinates:** these are provided for backwards compatibility, but users should really consider upgrading to the newer genome assembly.