# helpers.R
# add-ons for interacting with other packages, especially the old `argyle`

#' Decompose an \code{argyle::genotypes} object to tidy data frame
#' @export
as_tidy_genotypes <- function(g, ...) {
	
	sm <- dplyr::as_tibble( attr(g, "ped") )
	mk <- dplyr::as_tibble( attr(g, "map")[ ,1:4 ] )
	
	.melt_intensity <- function(x, what) {
		xi <- dplyr::as_tibble( reshape2::melt(x) )
		colnames(xi) <- c("marker","iid", what)
		return(xi)
	}
	
	geno <- .melt_intensity(g, "call")
	if (is.numeric(g)) {
		geno$call <- factor(geno$call, c(0,1,2))
	}
	else if (is.character(g)) {
		geno$call <- factor(geno$call, c("A","C","G","T","H","N"))
	}
	
	geno <- dplyr::left_join(geno, sm) %>%
		dplyr::left_join(mk)
	
	if (!is.null(attr(g, "intensity"))) {
		intens <- attr(g, "intensity")
		xi <- .melt_intensity(intens$x, "x")
		yi <- .melt_intensity(intens$y, "y")
		intens <- cbind(xi, yi[ ,-(1:2) ])
		geno <- dplyr::left_join(geno, intens)
	}
	
	return(geno)
	
}

#' @export
geno_grid_plot <- function(g, label = FALSE, ...) {
	
	g <- as_tidy_genotypes(g)
	g$marker <- reorder(g$marker, g$pos)
	p <- ggplot2::ggplot(g) +
		ggplot2::geom_tile(ggplot2::aes(x = marker, y = iid, fill = call)) +
		scale_fill_genocall() +
		ggplot2::theme_bw() +
		ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
	if (!label)
		p <- p + ggplot2::theme(axis.text.y = ggplot2::element_blank())
	return(p)
	
}

#' @export
plot_marker_map <- function(obj, label_size = 2, label_at = 1, label = TRUE, ...) {
	
	#obj <- unique(obj[ ,c("marker","pos") ])
	obj$marker <- factor(as.character(obj$marker))
	obj$marker <- with(obj, reorder(marker, pos))
	mk <- levels(obj$marker)
	
	xrange <- range(obj$pos)
	sp <- diff(xrange)*(seq_along(mk)-1)/length(mk) + min(obj$pos)
	repos <- setNames( sp, mk )
	obj$.pos <- repos[ as.character(obj$marker) ]
	
	p <- ggplot2::ggplot(obj) +
		par_boundary_refline(idx = 1:2)
	if (label)
		 p <- p +
		ggplot2::geom_text(ggplot2::aes(x = .pos, y = label_at, label = marker), size = label_size, angle = 90, hjust = 0)
	
	p <- p +
		ggplot2::geom_segment(ggplot2::aes(x = .pos, y = label_at, xend = pos, yend = 0), colour = "grey70") +
		ggplot2::geom_segment(ggplot2::aes(x = pos, y = 0, xend = pos, yend = -0.05)) +
		ggplot2::geom_hline(yintercept = 0) +
		ggplot2::scale_y_continuous(limits = c(-0.1, 2*label_at)) +
		scale_x_Mb(limits = xrange, expand = ggplot2::expand_scale(mult = 0.1, add = 0))
		#ggplot2::scale_x_continuous(limits = xrange, breaks = repos, labels = names(repos),
		#							expand = ggplot2::expand_scale(mult = 0.1, add = 0))
	attr(p, "bounds") <- xrange
	attr(p, "repos") <- repos
	return(p)
	
}

#' @export
geno_dot_plot <- function(g, label = FALSE, rel_heights = c(3,1), label_size = 2, sort_marker = NULL, plot_it = TRUE, ...) {
	
	if (inherits(g, "genotypes"))
		g <- as_tidy_genotypes(g)
	
	if (!is.null(sort_marker)) {
		iids <- unique(g$iid)
		gg <- subset(g, marker == sort_marker)
		ll <- levels(reorder(factor(gg$iid), as.integer(as.character(gg$call))))
		g$iid <- factor(g$iid, ll)
	}
	
	mkplot <- plot_marker_map(g, label_size = label_size, label = FALSE)
	repos <- attr(mkplot, "repos")
	xrange <- attr(mkplot, "bounds")
	g$.pos <- repos[ as.character(g$marker) ]
	g$call <- factor(g$call, c(0,1,2))
	
	p <- ggplot2::ggplot(g) +
		ggplot2::geom_point(ggplot2::aes(x = .pos, y = iid, color = call), pch = 15, size = 1.5) +
		#ggplot2::geom_tile(ggplot2::aes(x = .pos, y = iid, fill = call)) +
		scale_color_genocall(guide = FALSE) +
		#scale_fill_genocall(guide = FALSE) +
		ggplot2::scale_x_continuous(limits = xrange, breaks = repos, labels = names(repos),
									expand = ggplot2::expand_scale(mult = 0.1, add = 0)) +
		ggplot2::theme_bw() +
		ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
					   axis.title.x = ggplot2::element_blank(),
					   axis.title.y = ggplot2::element_blank())
	if (!label)
		p <- p + ggplot2::theme(axis.text.y = ggplot2::element_blank())
	
	mkplot <- mkplot +
		theme_gbrowse() +
		ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
					   axis.text.y = ggplot2::element_blank(),
					   axis.title.y = ggplot2::element_blank())
	
	if (plot_it) {
		.geno_grid_layout(p, mkplot)
	}
	else {
		return( list(map = mkplot, geno = p) )
	}
	
	
}

.geno_grid_layout <- function(geno, mkplot, rel_heights = c(3,1)) {
	cowplot::plot_grid(geno,
					   mkplot + ggplot2::theme(plot.margin = ggplot2::margin(0, 11/2, 11/2, 11/2, "pt")),
					   nrow = 2, align = "v",
					   rel_heights = rel_heights)
}