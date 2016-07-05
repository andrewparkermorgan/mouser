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
chromsizes <- function(build = c("mm9","mm10","37","38","NCBIm37","GRCm38"), as.seqinfo = FALSE, ...) {

	seqnames <- paste0("chr", c(1:19, "X","Y","M"))
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
		seqlengths <- c(197195432, 181748087, 159599783, 155630120, 152537259,
					   149517037, 152524553, 131738871, 124076172, 129993255,
					   121843856, 121257530, 120284312, 125194864, 103494974,
					   98319150, 95272651, 90772031, 61342430,
					   166650296, 91744698, 16299)
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
		seqlengths <- c(195471971,182113224,160039680,156508116,151834684,
						149736546,145441459,129401213,124595110,130694993,
						122082543,120129022,120421639,124902244,104043685,
						98207768,94987271,90702639,61431566,
						171031299,91744698,16299)
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
	else {
		stop("Genome build not recognized.")
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
		return( c(166.41e6, 165980013) )
	else if (.build %in% c("mm10","38","GRCm38"))
		return( c(169969759, 169542082) )
}

#' @export
#' @rdname pseudoautosomal_boundary
PAR_mm9 <- function(...) pseudoautosomal_boundary("mm9")

#' @export
#' @rdname pseudoautosomal_boundary
PAR_mm10 <- function(...) pseudoautosomal_boundary("mm10")
