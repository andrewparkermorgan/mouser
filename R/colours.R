# colours.R
# colour schemes for mouse genetics

#' Canonical colour scheme for Mus taxa
#' @param ... ignored
#' @return a named vector of colours
#' @export
mus_colors <- function(...) {
	
	taxa <- mus_taxa()
	cols <- c("#377eb8","#e41a1c","#4daf4a","#e41a1c", "#377eb8","#4daf4a",
			  "brown","grey",
			  "grey40","cadetblue4","darkgoldenrod3","darkolivegreen",
			  "darkorange4","burlywood4","aquamarine4",
			  "black","black")
	return( setNames(cols, taxa) )
	
}

#' Get a list of taxa in the genus Mus
#' @param ... ignored
#' @return a vector of taxa names or abbreviations; these are the last piece in the Linnean binomial (for full species)
#' 	or trinomial (for subspecies) nomenclature
#' @details For the three house mouse subspecies, abbreviations ("dom","mus","cas") are also included.
#' @export
mus_taxa <- function(...) {
	return(c("dom","mus","cas","musculus","domesticus","castaneus",
			 "molossinus","gentilulus",
			 "spretus","spicilegus","cypriacus","macedonicus",
			 "famulus","caroli","pahari",
			 "cookii","fragilicauda"))
}

#' Canonical colour scheme for denoting male/female sex
#' @param ... ignored
#' @return a named vector of colours
#' @details several different notations for sex are covered here: "f"/"m", "F"/"M", "femalee"/"male", "XX"/"XY"
#' @export
sex_colors <- function(...) {
	# colors are pink,blue
	cols <- c("#F0027F","#386CB0")
	sexes <- c("f","m","F","M","female","male","FEMALE","MALE","XX","XY")
	cols2 <- setNames( rep(cols, 5), sexes )
	cols2 <- c( cols2, setNames( rev(cols), c("1","2") ) )
	return(cols2)
}

#' Get the list of founder strains of the Collaborative Cross and Diversity Outbred
#' @param misspell use the common misspelling (NZO/H*I*LtJ: I as in indigo) of the proper strain name for NZO
#' @return a vector of full strain names, in proper nomenclature
#' @references 
#' 	Churchill GA, Collaborative Cross Consortium (2004) Nature Genenetics 36: 1133-1137. doi:10.1038/ng1104-1133
#' 	
#' 	Collaborative Cross Consortium (2012) Genetics 190: 389-401. doi:10.1534/genetics.111.132639
#' 	
#' 	\url{http://csbio.unc.edu/CCstatus/index.py}
#' @export
cc_strains <- function(misspell = FALSE, ...) {
	
	strains <- c("A/J","C57BL/6J","129S1/SvImJ","NOD/ShiLtJ","NZO/HlLtJ",
				 "CAST/EiJ","PWK/PhJ","WSB/EiJ")
	if (misspell)
		strains[5] <- "NZO/HILtJ"
	
	return(strains)
}

#' Get the strain codes for founder strains of the Collaborative Cross and Diversity Outbred
#' @param ... ignored
#' @return a named vector of full strain names, indexed by one-letter strain code (A, ..., H)
#' @export
#' @rdname cc_strains
CC_STRAINS <- function(...) {
	return( setNames( cc_strains(), LETTERS[1:8] ) )
}

#' Canonical colour scheme for the founder strains of the Collaborative Cross and Diversity Outbred
#' @param ... ignored
#' @return a named, vector of hex colors, indexed by full strain names
#' @seealso \code{\link{cc_strains}}
#' @export
cc_colors <- function(...) {
	strains <- cc_strains()
	cols <- c("#DAA520","#404040","#F08080","#1010F0","#00A0F0","#00A000","#F00000","#9000E0")
	return( setNames(cols, strains) )
}

#' @return a named, vector of hex colors, indexed by one-letter strain codes
#' @export
#' @rdname cc_colors
CC_COLORS <- function(...) {
	cols <- cc_colors()
	names(cols) <- LETTERS[1:8]
	cols2 <- cols
	names(cols2) <- paste0(LETTERS[1:8], LETTERS[1:8])
	return( c(cols, cols2) )
}

#' Color palettes related to mouse genetics for use with \code{ggplot2}.
#' 
#' @param ... passed through to underlying \code{ggplot2} functions
#' @details Palettes with lowercase names ('cc') are indexed by full strain names; those indexed by uppercase names ('CC'), by strain codes.
#' 	Both one-letter and two-letter strain codes are supported.
#' 	
#' 	The default behavior of \code{ggplot2} scales is to remove from the legend all factor levels not present in the data, and to hide from the plot
#' 	any values which are not represented in the scale (which are coerced to \code{NA}s.)  Use \code{na.value} to override this behavior.
#' 	
#' 	All \code{scale_colour_*} functions are just aliases to their \code{scale_color_*} counterparts, to be friendly to UK-style spellers.
#' @seealso \code{\link[ggplot2]{scale_manual}}
#' @name palettes
NULL

#' @param misspell logical; if \code{TRUE}, use common misspelling for name of strain 'NZO'
#' @export
#' @rdname palettes
scale_color_cc <- function(..., misspell = FALSE) {
	ggplot2::scale_colour_manual(..., values = cc_colors(misspell))
}

#' @export
#' @rdname palettes
scale_color_CC <- function(...) {
	ggplot2::scale_colour_manual(..., values = CC_COLORS())
}

#' @export
#' @rdname palettes
scale_fill_cc <- function(..., misspell = FALSE) {
	ggplot2::scale_fill_manual(..., values = cc_colors(misspell))
}

#' @export
#' @rdname palettes
scale_fill_CC <- function(...) {
	ggplot2::scale_fill_manual(..., values = CC_COLORS())
}

#' @export
#' @rdname palettes
scale_color_mus <- function(...) {
	ggplot2::scale_colour_manual(..., values = mus_colors())
}

#' @export
#' @rdname palettes
scale_fill_mus <- function(...) {
	ggplot2::scale_fill_manual(..., values = mus_colors())
}

#' @export
#' @rdname palettes
scale_color_sex <- function(...) {
	ggplot2::scale_color_manual(..., values = sex_colors())
}

#' @export
#' @rdname palettes
scale_fill_sex <- function(...) {
	ggplot2::scale_fill_manual(..., values = sex_colors())
}

# aliases for UK-style spellings
#' @export
#' @rdname mus_colors
mus_colours <- function(...) mus_colours(...)

#' @export
#' @rdname cc_colors
cc_colours <- function(...) cc_colors(...)

#' @export
#' @rdname cc_colors
CC_COLOURS <- function(...) CC_COLORS(...)

#' @export
#' @rdname palettes
scale_colour_cc <- function(...) scale_color_cc(...)

#' @export
#' @rdname palettes
scale_colour_mus <- function(...) scale_color_mus(...)

#' @export
#' @rdname palettes
scale_colour_sex <- function(...) scale_color_sex(...)