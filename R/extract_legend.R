#' Extract Legend 
#'
#' Extracts the legend from a ggplot object
#'
#' @param a.gplot a ggplot2 figure
#'
#' @return legend legend extracted from ggplot2
#'
#'
#' @export
extract_legend <-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 
