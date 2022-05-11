# colours.R
# colour schemes for mouse genetics

#' Canonical colour scheme for Mus taxa
#' @param ... ignored
#' @return a named vector of colours
#' @export
mus_colors <- function(...) {
	
	args <- unlist(list(...))
	taxa <- mus_taxa()
	cols <- c("#377eb8","#e41a1c","#4daf4a","#e41a1c", "#377eb8","#4daf4a",
			  "brown","grey40","grey40",
			  "khaki4","cadetblue4","darkgoldenrod3","darkolivegreen",
			  "darkorange4","burlywood4","aquamarine4",
			  "black","black","grey")
	thecols <- setNames(cols, taxa)
	if (is.null(args))
		return(thecols)
	else
		return(thecols[ intersect(args, names(thecols)) ])
	
}

#' Get a list of taxa in the genus Mus
#' @param ... ignored
#' @return a vector of taxa names or abbreviations; these are the last piece in the Linnean binomial (for full species)
#' 	or trinomial (for subspecies) nomenclature
#' @details For the three house mouse subspecies, abbreviations ("dom","mus","cas") are also included.
#' @export
mus_taxa <- function(...) {
	return(c("dom","mus","cas","musculus","domesticus","castaneus",
			 "molossinus","gentilulus","gen",
			 "spretus","spicilegus","cypriacus","macedonicus",
			 "famulus","caroli","pahari",
			 "cookii","fragilicauda","lab"))
}

#' Get a list of taxa in the genus Mus, with genus-species-subspecies names
#' @param ... ignored
#' @param gentilulus_undefined rename 'gentilulus' to 'undefined', given its uncertain taxonomic status (default TRUE)
#' @return a vector of taxa names or abbreviations; these are the last piece in the Linnean binomial (for full species)
#' 	or trinomial (for subspecies) nomenclature
#' @details For the three house mouse subspecies, abbreviations ("dom","mus","cas") are also included.
#' @export
mus_taxa_formal <- function(..., gentilulus_undefined = TRUE) {
	
	rez <- c("dom"=  "M. m. domesticus",
			 "mus" = "M. m. musculus",
			 "cas" = "M. m. castaneus",
			 "gen" = "M. m. gentilulus",
			 "gentilulus" = "M. m. gentilulus",
			 "molossinus" = "M. m. molossinus",
			 "gentilulus" = "M. m. gentilulus",
			 "spretus" = "M. spretus",
			 "spicilegus" = "M. spicilegus",
			 "cypriacus" = "M. cypriacus",
			 "macedonicus" = "M. macedonicus",
			 "famulus" = "M. famulus",
			 "caroli" = "M. caroli")
	if (gentilulus_undefined) {
		rez["gen"] <- "M. m. undefined"
		rez["gentilulus"] <- "M. m. undefined"
	}
	return(rez)
	
}

#' Test if the listed taxa are outgroups with respect to Mus musculus
#' @param x a vector of values to test (will be coerced to character)
#' @return a logical vector
#' @export
is_outgroup <- function(x, ..) {
	x <- as.character(x)
	x %in% setdiff(mus_taxa(), c("lab","classical",mus_taxa()[1:7]))
}

factor_mus <- function(x, ...) {
	x <- as.character(x)
	factor(x, levels = mus_taxa())
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
	cols2 <- c( cols2, setNames(rep("grey60",2), c("XO","0")) )
	return(cols2)
}

#' @export
sex_shapes <- function(...) {
	# shapes are circle=F, square=M
	cols <- c(19,15)
	sexes <- c("f","m","F","M","female","male","FEMALE","MALE","XX","XY")
	cols2 <- setNames( rep(cols, 5), sexes )
	cols2 <- c( cols2, setNames( rev(cols), c("1","2") ) )
	# unknown or XO is diamond
	cols2 <- c( cols2, setNames(rep(18,2), c("XO","0")) )
	return(cols2)
}

#' Lighter colour scheme for denoting male/female sex
#' @param ... ignored
#' @return a named vector of colours
#' @details several different notations for sex are covered here: "f"/"m", "F"/"M", "femalee"/"male", "XX"/"XY"
#' @export
sex_colors_lighter <- function(...) {
	# colors are pink,blue
	cols <- c("#f4c8df","#b5cded")
	sexes <- c("f","m","F","M","female","male","FEMALE","MALE","XX","XY")
	cols2 <- setNames( rep(cols, 5), sexes )
	cols2 <- c( cols2, setNames( rev(cols), c("1","2") ) )
	cols2 <- c( cols2, setNames(rep("grey80",2), c("XO","0")) )
	return(cols2)
}

#' Canonical colour scheme for denoting autosomes vs X chromosome
#' @param include.Y logical; if \code{TRUE}, give Y chrom its own color
#' @param ... ignored
#' @return a named vector of colours
#' @export
chrom_colors <- function(include.Y = FALSE, ...) {
	
	if (include.Y)
		cols <- c("A" = "black", "X" = unname(sex_colors()["female"]), "Y" = unname(sex_colors()["male"]))
	else
		cols <- c("A" = "#377EB8", "X" = "#E41A1C")
	
	return(cols)
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

#' Return CC diplotypes in canonical order
#' @param strains which strains to include in diplotype states; default is all 8
#' @param phased logical; if FALSE (the default), return unordered combinations; otherwise respect order
#' @param ... ignored
#' @return a character vector of diplotype states as 2-letter codes
#' @export
cc_diplotypes <- function(strains = LETTERS[1:8], phased = FALSE, ...) {
	hom <- paste0(strains, strains)
	het <- apply(combn(strains, 2), 2, paste0, collapse = "")
	if (phased) {
		x <- outer(strains, strains, paste0)
		het <- sort(c(as.vector(x[ upper.tri(x) ]), as.vector(x[ lower.tri(x) ])))
	}
	return( c(hom, het) )
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

#' @return Color palette for Y chromosome haplogroups in CC and DO
#' @export
#' @rdname cc_colors
ychrom_colors <- function(...) {
	cols <- unname(CC_COLORS()[1:8])
	ycols <- c("ABCE" = cols[2], "DH" = cols[4], "F" = cols[6], "G" = cols[7])
	return(ycols)
}

alt_chrom_colors <- function(chroms = chromnames()) {
	setNames( rep_len(c("grey30","grey65"), length(chroms)), chroms )
}

alt_chrom_colours <- function(...) alt_chrom_colors(...)

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
scale_color_cc <- function(..., na.value = "grey", misspell = FALSE) {
	ggplot2::scale_colour_manual(..., values = cc_colors(misspell), na.value = na.value)
}

#' @export
#' @rdname palettes
scale_color_CC <- function(..., na.value = "grey") {
	ggplot2::scale_colour_manual(..., values = CC_COLORS(), na.value = na.value)
}

#' @export
#' @rdname palettes
scale_color_CC_withf1 <- function(..., na.value = "grey90") {
	cols <- CC_COLORS()[9:16]
	labs <- cc_diplotypes()
	cols <- unname( c(cols, rep("grey70", 28)) )
	ggplot2::scale_colour_manual(..., values = cols, labels = labs, na.value = na.value)
}

#' @export
#' @rdname palettes
scale_fill_cc <- function(..., misspell = FALSE, na.value = "grey") {
	ggplot2::scale_fill_manual(..., values = cc_colors(misspell), na.value = na.value)
}

#' @export
#' @rdname palettes
scale_fill_CC <- function(..., na.value = "grey") {
	ggplot2::scale_fill_manual(..., values = CC_COLORS(), na.value = na.value)
}

#' @export
#' @rdname palettes
scale_color_ychrom <- function(..., na.value = "grey") {
	ggplot2::scale_colour_manual(..., values = ychrom_colors(), na.value = na.value)
}

#' @export
#' @rdname palettes
scale_colour_ychrom <- function(...) {
	scale_color_ychrom(...)
}

#' @export
#' @rdname palettes
scale_fill_ychrom <- function(..., na.value = "grey") {
	ggplot2::scale_fill_manual(..., values = ychrom_colors(), na.value = na.value)
}

#' @export
#' @rdname palettes
scale_color_mus <- function(..., na.value = "grey") {
	ggplot2::scale_colour_manual(..., values = mus_colors(), na.value = na.value)
}

#' @export
#' @rdname palettes
scale_fill_mus <- function(..., na.value = "grey") {
	ggplot2::scale_fill_manual(..., values = mus_colors(), na.value = na.value)
}

#' @export
#' @rdname palettes
scale_color_sex <- function(..., na.value = "grey", lighter = FALSE) {
	colfn <- if (!lighter) sex_colors else sex_colors_lighter
	ggplot2::scale_color_manual(..., values = colfn(), na.value = na.value)
}

#' @export
#' @rdname palettes
scale_fill_sex <- function(..., na.value = "grey", lighter = FALSE) {
	colfn <- if (!lighter) sex_colors else sex_colors_lighter
	ggplot2::scale_fill_manual(..., values = colfn(), na.value = na.value)
}

#' @export
#' @rdname palettes
scale_shape_sex <- function(..., na.value = 18) {
	ggplot2::scale_shape_manual(..., values = sex_shapes(), na.value = na.value)
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

#' @export
#' @rdname palettes
scale_color_chromtype <- function(..., include.Y = FALSE) {
	ggplot2::scale_color_manual(..., values = chrom_colors(include.Y), na.value = "grey")
}

#' @export
#' @rdname palettes
scale_colour_chromtype <- function(...) scale_color_chromtype(...)

#' @export
#' @rdname palettes
scale_fill_chromtype <- function(..., include.Y = FALSE) {
	ggplot2::scale_fill_manual(..., values = chrom_colors(include.Y), na.value = "grey")
}

#' @export
chrom_labeller <- function(labs, ...) {
	lapply(labs, function(f) gsub("^chr", "", f))
}

#' @export
taxon_labeller <- function(x, full_names = FALSE, gentilulus_undefined = TRUE) {
	
	if (full_names) {
		mus_names <- c("M. m. domesticus","M. m. musculus","M. m. castaneus",
					   "M. m. gentilulus","M. m. molossinus")
		if (gentilulus_undefined)
			mus_names[4] <- "M. m. undefined"
	}
	else {
		mus_names <- c("domesticus","musculus","castaneus","gentilulus","molossinus")
		if (gentilulus_undefined)
			mus_names[4] <- "undefined"
	}
	
	renamer <- c( setNames( mus_names, c("dom","mus","cas","gen","mol") ),
				 c("spretus" = "M. spretus", "spicilegus" = "M. spicilegus", "caroli" = "M. caroli") )
	
	.do_rename <- function(n) {
		n <- as.character(n)
		nn <- renamer[ n ]
		nas <- is.na(nn)
		nn[ nas ] <- n[ nas ]
		return(nn)
	}
	
	lapply(x, .do_rename)
	
}

#' @export
taxon_labeller_full <- function(x) taxon_labeller(x, TRUE)

#' The "classic" scheme of `ggplot2`, with axis-line issue fixed and nicer facet labels
#' @export
theme_classic2 <- function(...) {
	ggplot2::theme_classic(...) +
		ggplot2::theme(axis.line.x = ggplot2::element_line(),
					   axis.line.y = ggplot2::element_line(),
					   strip.text = ggplot2::element_text(face = "bold"),
					   strip.background = ggplot2::element_blank(),
					   legend.background = ggplot2::element_blank(),
					   legend.key.size = grid::unit(0.9, "lines"),
					   plot.title = ggplot2::element_text(hjust = 0.5))
}

theme_gbrowse <- function(...) {
	theme_classic2(...) + ggplot2::theme(axis.line.y =ggplot2:: element_blank())
}

#' Format axis labels as powers of 10
#' @export
scale_y_power10 <- function(...) {
	scale_y_log10(...,
			  breaks = scales::trans_breaks("log10", function(x) 10^x),
			  labels = scales::trans_format("log10", scales::math_format(10^.x)))
}

#' Format axis labels as powers of 10
#' @export
scale_x_power10 <- function(...) {
	scale_x_log10(...,
				  breaks = scales::trans_breaks("log10", function(x) 10^x),
				  labels = scales::trans_format("log10", scales::math_format(10^.x)))
}
#scientific_10 <- function(x) {
#	parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
#}

#' Move the legend inside plotting area
#' @export
legend_inside <- function(where = c("bottomright","bottomleft","topleft","topright"), ...) {
	
	.where <- match.arg(where)
	if (.where == "bottomleft") {
		pos <- c(0,0)
		bjust <- "left"
	} else if (.where == "bottomright") {
		pos <- c(1,0)
		bjust <- "left"
	} else if (.where == "topleft") {
		pos <- c(0,1)
		bjust <- "left"
	} else if (.where == "topright") {
		pos <- c(1,1)
		bjust <- "left"
	} else {
		return(NULL)
	}
	
	ggplot2::theme(legend.position = pos, legend.justification = pos,
				   legend.box.just = bjust)

}

#' Lay chromosomes out end-to-end and re-calculate position of genomic intervals/points
#' @export
linearize_genome <- function(df, chrlen = NULL, space = 7, ...) {
	
	if (is.null(chrlen)) {
		chrlen <- chromsizes_mm10()
		df$chr <- factor_chrom(df$chr)
	}
	else {
		df$chr <- factor(df$chr, names(chrlen))
	}
	
	#chrom <- as.character(df$chr)
	adj <- c(0, cumsum(chrlen/1e6+space))
	for (col in c("start","end","pos")) {
		if (all(col %in% colnames(df))) {
			newcol <- paste0(".", col)
			df[ ,newcol ] <- df[ ,col ]/1e6 + adj[ as.numeric(df$chr) ]
		}
	}
	
	altcolor <- setNames( rep_len(c(0,1), nlevels(df$chr)), levels(df$chr) )
	df$.colour <- factor(altcolor[ as.character(df$chr) ])
	return(df)
	
}

#' Draw x-axis for concatenated chromosomes
#' @export
scale_x_linearized_genome <- function(..., chrlen = NULL, space = 7) {
	
	if (is.null(chrlen))
		chrlen <- chromsizes_mm10()
	adj <- c(0, cumsum(chrlen/1e6+space))
	ticks <- adj[ -length(adj) ] + diff(adj)/2
	xlim <- c(0, max(adj)+0.05*max(adj))
	ggplot2::scale_x_continuous(..., limits = xlim, breaks = ticks,
					   labels = gsub("^chr", "", names(chrlen)))
	
}

#' @export
par_boundary_refline <- function(..., lty = "dashed", colour = "red", idx = 1) {
	ggplot2::geom_vline(xintercept = pseudoautosomal_boundary("mm10")[idx], lty = lty, colour = colour, ...)
}

#' @export
scale_x_par <- function(..., distal = 170.02e6) {
	scale_x_Mb(limits = c(pseudoautosomal_boundary("mm10")[2], distal), ...)
}

#' @export
scale_color_genocall <- function(..., na.value = "grey") {
	cols <- RColorBrewer::brewer.pal(11, "Spectral")[ c(1,11,10) ]
	names(cols) <- c("0","1","2")
	labs <- function(f) {
		f[ is.na(f) ] <- 4
		newlab <- c("HOM_A1","HET", "HOM_A2","NA")[ as.integer(f)+1 ]
		return(newlab)
	}
	ggplot2::scale_color_manual(..., values = cols, labels = labs, na.value = na.value)
}

#' @export
scale_fill_genocall <- function(..., na.value = "grey") {
	cols <- RColorBrewer::brewer.pal(11, "Spectral")[ c(1,11,10) ]
	names(cols) <- c("0","1","2")
	labs <- function(f) {
		f[ is.na(f) ] <- 4
		newlab <- c("HOM_A1","HET", "HOM_A2","NA")[ as.integer(f)+1 ]
		return(newlab)
	}
	ggplot2::scale_fill_manual(..., values = cols, labels = labs, na.value = na.value)
}

#' @export
color_to_hexcode <- function(col, ...) {
	
	m <- col2rgb(col) 
	m <- t(m)
	hex <- rgb(m, maxColorValue = 255)
	names(hex) <- names(col)
	hex[ is.na(col) ] <- NA
	return(hex)
	
}