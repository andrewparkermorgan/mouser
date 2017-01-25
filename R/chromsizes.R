# chromsizes.R
# chromosome sizes, in various genome builds and with different flavours of chromosome names

#' Shortcut to chromosome lengths for genome build mm10.
#' @param ... passed through to \code{chromsizes()}
#' @return either a named vector of chromosome lengths or a \code{Seqinfo} object; see \code{?chromsizes}
#' @details Chromosome names are in the "chr1", ..., "chrM" (UCSC) style.  If you prefer Ensembl ("1", ..., "MT) style,
#' 	use \code{chromsizes("38")}.
#' @seealso \code{\link{chromsizes}}
#' @export
chromsizes_mm10 <- function(...) {
	chromsizes("mm10", ...)
}

#' Shortcut to chromosome lengths for genome build mm9.
#' @param ... passed through to \code{chromsizes()}
#' @return either a named vector of chromosome lengths or a \code{Seqinfo} object; see \code{?chromsizes}
#' @details Chromosome names are in the "chr1", ..., "chrM" (UCSC) style.  If you prefer Ensembl ("1", ..., "MT) style,
#' 	use \code{chromsizes("37")}.
#' 	PS: Should you really still be using mm9?  Consider updating to the post-2011 genome assembly.
#' @seealso \code{\link{chromsizes}}
#' @export
chromsizes_mm9 <- function(...) {
	chromsizes("mm10", ...)
}

#' Get chromosome lengths for a specific genome build.
#' @param build name of a genome build: one of \code{"mm9", "mm10", "37", "38"}
#' @param as.seqinfo logical; if \code{TRUE}, return a \code{Seqinfo} object
#' @param ... ignored
#' @return either a named vector of chromosome lengths or a \code{Seqinfo} object for use with Bioconductor stack
#' @details Chromosome names for \code{"mm9"} and \code{"mm10"} are in the "chr1", ..., "chrM" (UCSC) style.  If you prefer Ensembl
#' 	("1", ..., "MT) style, use \code{"37"} or \code{"38"}, respectively.
#' 	If a \code{Seqinfo} object is requested but the appropriate package is not installed, the function will default
#' 	to a named vector but issue a warning.
#' @seealso \code{\link[GenomeInfoDb]{Seqinfo}}
#' @export
chromsizes <- function(build = c("mm9","mm10","37","38","NCBIm37","GRCm38","cf3","hg19"), as.seqinfo = FALSE, ...) {

	seqnames <- chromnames("mouse")
	is.circ <- c(rep(FALSE, 21),TRUE)
	
	.return.sizes <- function(nm, sz, circ, genome) {
		if (requireNamespace("GenomeInfoDb", quietly = TRUE)) {
			GenomeInfoDb::Seqinfo(nm, sz, circ, genome)
		}
		else {
			warning("Package 'GenomeInfoDb' is required to return a Seqinfo object, but it couldn't be loaded.")
			return( setNames(sz, nm) )
		}
	}
	
	.build <- match.arg(build)
	if (.build %in% c("mm10","38","GRCm38")) {
		seqlengths <- c(195471971,182113224,160039680,156508116,151834684,
						149736546,145441459,129401213,124595110,130694993,
						122082543,120129022,120421639,124902244,104043685,
						98207768,94987271,90702639,61431566,
						171031299,91744698,16299)
		if (.build %in% c("38","GRCm38")) {
			seqnames <- gsub("^chr","", seqnames)
			seqnames[22] <- "MT"
			genome <- "GRCm38"
		}
		else {
			genome <- "mm10"
		}
			
		if (as.seqinfo)
			.return.sizes(seqnames, seqlengths, is.circ, genome)
		else
			return( setNames(seqlengths, seqnames) )
	}
	else if (.build %in% c("mm9","37","NCBIm37")) {
		seqlengths <- c(197195432, 181748087, 159599783, 155630120, 152537259,
						149517037, 152524553, 131738871, 124076172, 129993255,
						121843856, 121257530, 120284312, 125194864, 103494974,
						98319150, 95272651, 90772031, 61342430,
						166650296, 91744698, 16299)
		if (.build %in% c("37","NCBIm37")) {
			seqnames <- gsub("^chr","", seqnames)
			seqnames[22] <- "MT"
			genome <- "NCBIm37"
		}
		else {
			genome <- "mm9"
		}
		
		if (as.seqinfo)
			.return.sizes(seqnames, seqlengths, is.circ, genome)
		else
			return( setNames(seqlengths, seqnames) )
	}
	else if (.build == "cf3") {
		# cf3 lives in sysdata.rda
		genome <- "cf3"
		is.circ <- grepl("chrM", names(cf3))
		if (as.seqinfo)
			.return.sizes(names(cf3), cf3, is.circ, genome)
		else
			return(cf3)
	}
	else if (.build == "hg19") {
		# hg19 lives in sysdata.rda
		genome <- "hg19"
		is.circ <- grepl("chrM", names(hg19))
		if (as.seqinfo)
			.return.sizes(names(hg19), hg19, is.circ, genome)
		else
			return(hg19)
	}
	else {
		stop("Genome build not recognized.")
	}
	
}

#' Chromosome lengths in cM.
#' @param ... ignored
#' @return vector of chromosome lenghs in cM
#' @details Lengths are from Collaborative Cross G2:F1 recombination map.
#' @export
chromsizes_cM <- function(...) {
	structure(c(98.5452, 103.9064, 82.6975, 88.6454, 90.2329, 79.0238, 
				89.0678, 76.2476, 75.0692, 77.9816, 88.0028, 63.9015, 67.2537, 
				66.4371, 59.0785, 57.8389, 61.2641, 59.4257, 56.923, 79.4368),
				.Dim = 20L,
				.Dimnames = list(
					c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", 
					  "chr8", "chr9", "chr10", "chr11", "chr12", "chr13", "chr14", 
					  "chr15", "chr16", "chr17", "chr18", "chr19", "chrX")))
}

#' Just return chromosome names.
#' @param species return chromosome names for this species (default is mouse)
#' @param ... ignored
#' @return character vector of chromosome names in the UCSC style: chr1, ..., chrX, chrY, chrM
#' @export
chromnames <- function(species = c("mouse","dog","human"), ...) {
	.species <- match.arg(species)
	if (.species == "mouse")
		paste0("chr", c(1:19, "X","Y","M"))
	else if (.species == "dog")
		paste0("chr", c(1:38, "X","M")) #NB: dog Y chromosome not assembled yet
	else if (.species == "human")
		paste0("chr", c(1:22, "X","Y","M"))
	else
		stop("Species not recognized (yet).")
}

#' Create a factor whose levels are chromosome names.
#' @param x a vector of values
#' @param species return chromosome names for this species (default is mouse)
#' @param ... ignored
#' @return factor whose levels are chromosome names
#' @export
factor_chrom <- function(x, species = c("mouse","dog","human"), ...) {
	
	if (!any(grepl("^chr", x)))
		x <- paste0("chr", x)
	
	ll <- chromnames(species)
	factor( as.character(x), ll )
}

#' Classify chromosmes as X or autosome
#' @param x a vector of values
#' @param include.Y logical; if \code{TRUE}, treat Y chromosome as separate class
#' @param ... ignored
#' @return factor whose levels are "X" (X chromosome) "A" (autosome)
#' @details Chromsomes which are not X or autosome will be set to NA.
#' @export
factor_chromtype <- function(x, include.Y = FALSE, ...) {
	
	groups <- c("A","X")
	if (include.Y)
		groups <- c(groups, "Y")
	
	xx <- vector("character", length(x))
	xx[ grepl("X", x) ] <- "X"
	xx[ !grepl("[YM]", x) & !grepl("X", x) ] <- "A"
	if (include.Y)
		xx[ grepl("Y", x) ] <- "Y"
	xx[ grepl("JH", x) | grepl("GL", x) | grepl("PATCH", x) | grepl("MG", x) ] <- NA
	xx[ is.na(x) ] <- NA
	factor(xx, levels = groups)
	
}

#' Classify sex in systematic way
#' @param x a vector of sexes, encoded in any of the acceptable ways (see Details)
#' @param ... ignored
#' @return a factor with one level for each sex
#' @export
factor_sex <- function(x, ...) {
	x <- as.character(x)
	if (all(c("F","M") %in% x)) {
		factor(x, c("F","M"))
	}
	else if (all(c("f","m") %in% x)) {
		factor(x, c("f","m"))
	}
	else if (all(c("female","male") %in% tolower(x))) {
		x <- tolower(x)
		factor(x, c("female","male"))
	}
	else if (all(c("XX","XY") %in% x)) {
		factor(x, c("XX","XY"))
		if (all(c("XX","XY","XO") %in% x)) {
			factor(x, c("XX","XY","XO"))
		}
	}
	else if (all(c("1","2") %in% x)) {
		factor(x, levels = c("2","1","0"), labels = c("female","male","unknown"))
	}
	else {
		warning("Can't understand sex encoding; values returned unchanged.")
		return(x)
	}
}

#' Return position of pseudoautosomal boundary on X chromosome
#' @param build genome build: see \code{?chromsizes} for options
#' @param ... ignored
#' @return a vector of length two, with the canonical boundary of pseudoautosomal region (PAR) as well as its shifted boundary
#	in the strain CAST/EiJ
#' @references
#' 	White MW, Ikeda A, Payseur BA (2011) A pronounced evolutionary shift of the pseudoautosomal region boundary in house mice. Mammalian Genome 23: 455-466.
#' 	doi:10.1007/s00335-012-9403-5
#' @export
pseudoautosomal_boundary <- function(build = c("mm9","mm10","37","38","NCBIm37","GRCm38"), ...) {
	.build <- match.arg(build)
	if (.build %in% c("mm9","37","NCBIm37"))
		return( c(166407691, 165980013) )
	else if (.build %in% c("mm10","38","GRCm38"))
		return( c(169969759, 169542082) )
}

#' @export
#' @rdname pseudoautosomal_boundary
PAR_mm9 <- function(...) pseudoautosomal_boundary("mm9")

#' @export
#' @rdname pseudoautosomal_boundary
PAR_mm10 <- function(...) pseudoautosomal_boundary("mm10")

#' @export
#' @rdname pseudoautosomal_boundary
scale_x_Mb <- function(name = "position (Mb)", ...) {
	ggplot2::scale_x_continuous(name = name, labels = function(x) x/1e6, ...)
}

#' @export
#' @rdname pseudoautosomal_boundary
scale_y_Mb <- function(name = "position (Mb)", ...) {
	ggplot2::scale_y_continuous(name = name, labels = function(x) x/1e6, ...)
}