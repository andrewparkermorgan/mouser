% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colours.R
\name{cc_strains}
\alias{CC_STRAINS}
\alias{cc_strains}
\title{Get the list of founder strains of the Collaborative Cross and Diversity Outbred}
\usage{
cc_strains(misspell = FALSE, ...)

CC_STRAINS(...)
}
\arguments{
\item{misspell}{use the common misspelling (NZO/H*I*LtJ: I as in indigo) of the proper strain name for NZO}

\item{...}{ignored}
}
\value{
a vector of full strain names, in proper nomenclature

a named vector of full strain names, indexed by one-letter strain code (A, ..., H)
}
\description{
Get the list of founder strains of the Collaborative Cross and Diversity Outbred

Get the strain codes for founder strains of the Collaborative Cross and Diversity Outbred
}
\references{
Churchill GA, Collaborative Cross Consortium (2004) Nature Genenetics 36: 1133-1137. doi:10.1038/ng1104-1133
	
	Collaborative Cross Consortium (2012) Genetics 190: 389-401. doi:10.1534/genetics.111.132639
	
	\url{http://csbio.unc.edu/CCstatus/index.py}
}

