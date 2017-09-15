#' ggplot Percent Labels
#'
#' This function allows for custom percentage labels in ggplot2.
#'
#' @param theargument label for ggplot
#' @param siglevel significant figures
#'
#' @return label for ggplot2
#'
#'
#' @export
ggplot_percent_label <-function(theargument, siglevel=3) { 
 	stopifnot(is.numeric(theargument)) 
 	paste(signif(theargument, siglevel) * 100, "%", sep="") 
} 
