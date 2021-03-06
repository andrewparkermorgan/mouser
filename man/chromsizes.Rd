% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chromsizes.R
\name{chromsizes}
\alias{chromsizes}
\title{Get chromosome lengths for a specific genome build.}
\usage{
chromsizes(build = c("mm9", "mm10", "37", "38", "NCBIm37", "GRCm38", "cf3",
  "hg19"), as.seqinfo = FALSE, ...)
}
\arguments{
\item{build}{name of a genome build: one of \code{"mm9", "mm10", "37", "38"}}

\item{as.seqinfo}{logical; if \code{TRUE}, return a \code{Seqinfo} object}

\item{...}{ignored}
}
\value{
either a named vector of chromosome lengths or a \code{Seqinfo} object for use with Bioconductor stack
}
\description{
Get chromosome lengths for a specific genome build.
}
\details{
Chromosome names for \code{"mm9"} and \code{"mm10"} are in the "chr1", ..., "chrM" (UCSC) style.  If you prefer Ensembl
	("1", ..., "MT) style, use \code{"37"} or \code{"38"}, respectively.
	If a \code{Seqinfo} object is requested but the appropriate package is not installed, the function will default
	to a named vector but issue a warning.
}
\seealso{
\code{\link[GenomeInfoDb]{Seqinfo}}
}

